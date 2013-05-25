module Distribution.Simple.HaskellSuite where

import Control.Monad
import Control.Applicative
import System.FilePath
import Data.Maybe
import Data.Version

import Distribution.Simple.Program
import Distribution.Simple.Compiler as Compiler
import Distribution.Simple.Utils
import Distribution.Simple.BuildPaths
import Distribution.Verbosity
import Distribution.Text
import Distribution.Package
import Distribution.InstalledPackageInfo hiding (includeDirs)
import Distribution.Simple.PackageIndex as PackageIndex
import Distribution.PackageDescription
import Distribution.Simple.LocalBuildInfo
import Distribution.System (Platform)
import Distribution.Compat.Exception
import Language.Haskell.Extension
import Distribution.Simple.Program.Builtin (haskellSuiteProgram)

hstoolProg :: Program
hstoolProg = simpleProgram "haskell-suite-tool"

hspkgProg :: Program
hspkgProg = simpleProgram "haskell-suite-pkg"

configure
  :: Verbosity -> Maybe FilePath -> Maybe FilePath
  -> ProgramConfiguration -> IO (Compiler, Maybe Platform, ProgramConfiguration)
configure verbosity mbHcPath hcPkgPath conf0 = do

  -- We have no idea how a haskell-suite tool is named, so we require at
  -- least some information from the user.
  hcPath <-
    let msg = "You have to provide name or path of a haskell-suite tool (-w PATH)"
    in maybe (die msg) return mbHcPath

  when (isJust hcPkgPath) $
    warn verbosity "--with-hc-pkg option is ignored for haskell-suite"

  -- Unlike many other compilers, here it makes sense to allow hcPath be
  -- just a name of executable (since it's not known a priori).
  let (hcPathDir, hcPathFile) = splitFileName hcPath
      pathSpecified = not $ null hcPathDir

      prog =
        (simpleProgram hcPathFile)
          { programFindVersion = numericVersion }

      conf1 = addKnownProgram prog conf0

      conf2 =
        if pathSpecified
          then userSpecifyPath hcPathFile hcPath conf1
          else conf1

  (configuredProg, conf3) <- requireProgram verbosity prog conf2

  extensions <- getExtensions verbosity configuredProg
  languages  <- getLanguages  verbosity configuredProg
  (compName, compVersion) <-
    getCompilerVersion verbosity configuredProg

  -- Now rename the program to so that we can find it later. Could't do
  -- that earlier, since 'configureProgram' would attempt to find it under
  -- this meaningless name.
  --
  -- Actually, the current Distribution.Simple.Program.Db interface only
  -- allows us to add a new name for an existing program. We can't delete
  -- the original name. Well, that should be good enough.
  let conf4 =
        updateProgram
          configuredProg { programId = programName hstoolProg }
          conf3

  -- Register our package tool. It's the same executable, qualified with
  -- the "pkg" subcommand.
      conf5 =
        updateProgram
          configuredProg
            { programId = programName hspkgProg
            , programDefaultArgs = ["pkg"]
            }
          conf4

      comp = Compiler {
        compilerId             = CompilerId (HaskellSuite compName) compVersion,
        compilerLanguages      = languages,
        compilerExtensions     = extensions
      }
  return (comp, Nothing, conf5)

hstoolVersion :: Verbosity -> FilePath -> IO (Maybe Version)
hstoolVersion = findProgramVersion "--hspkg-version" id

numericVersion :: Verbosity -> FilePath -> IO (Maybe Version)
numericVersion = findProgramVersion "--compiler-version" (last . words)

getCompilerVersion :: Verbosity -> ConfiguredProgram -> IO (String, Version)
getCompilerVersion verbosity prog = do
  output <- rawSystemStdout verbosity (programPath prog) ["--compiler-version"]
  let
    parts = words output
    name = concat $ init parts -- there shouldn't be any spaces in the name anyway
    versionStr = last parts
  version <-
    maybe (die "haskell-suite: couldn't determine compiler version") return $
      simpleParse versionStr
  return (name, version)

getExtensions :: Verbosity -> ConfiguredProgram -> IO [(Extension, Compiler.Flag)]
getExtensions verbosity prog = do
  extStrs <-
    lines <$>
    rawSystemStdout verbosity (programPath prog) ["--supported-extensions"]
  return
    [ (ext, "-X" ++ display ext) | Just ext <- map simpleParse extStrs ]

getLanguages :: Verbosity -> ConfiguredProgram -> IO [(Language, Compiler.Flag)]
getLanguages verbosity prog = do
  langStrs <-
    lines <$>
    rawSystemStdout verbosity (programPath prog) ["--supported-languages"]
  return
    [ (ext, "-G" ++ display ext) | Just ext <- map simpleParse langStrs ]

-- Other compilers do some kind of a packagedb stack check here. Not sure
-- if we need something like that as well.
getInstalledPackages :: Verbosity -> PackageDBStack -> ProgramConfiguration
                     -> IO PackageIndex
getInstalledPackages verbosity packagedbs conf =
  liftM (PackageIndex.fromList . concat) $ forM packagedbs $ \packagedb ->
    do str <-
        rawSystemProgramStdoutConf verbosity hspkgProg conf
                ["dump", packageDbOpt packagedb]
         `catchExit` \_ -> die $ "pkg dump failed"
       case parsePackages str of
         Right ok -> return ok
         _       -> die "failed to parse output of 'pkg dump'"

  where
    parsePackages str =
      let parsed = map parseInstalledPackageInfo (splitPkgs str)
       in case [ msg | ParseFailed msg <- parsed ] of
            []   -> Right [ pkg | ParseOk _ pkg <- parsed ]
            msgs -> Left msgs

    splitPkgs :: String -> [String]
    splitPkgs = map unlines . splitWith ("---" ==) . lines
      where
        splitWith :: (a -> Bool) -> [a] -> [[a]]
        splitWith p xs = ys : case zs of
                           []   -> []
                           _:ws -> splitWith p ws
          where (ys,zs) = break p xs

buildLib
  :: Verbosity -> PackageDescription -> LocalBuildInfo
  -> Library -> ComponentLocalBuildInfo -> IO ()
buildLib verbosity _pkg_descr lbi lib clbi = do
  -- In future, there should be a mechanism for the compiler to request any
  -- number of the above parameters (or their parts) — in particular,
  -- pieces of PackageDescription.
  --
  -- For now, we only pass those that we know are used.

  let odir = buildDir lbi
      bi = libBuildInfo lib
      srcDirs = hsSourceDirs bi ++ [odir]
      dbStack = withPackageDB lbi
      language = fromMaybe Haskell98 (defaultLanguage bi)

  (hstool, _) <- requireProgram verbosity hstoolProg (withPrograms lbi)

  -- a dummy tool for which the user can specify the options
  (haskellSuite, _) <- requireProgram verbosity haskellSuiteProgram (withPrograms lbi)
  let userOptions = programOverrideArgs haskellSuite

  runProgramInvocation verbosity $
    programInvocation hstool $
      [ "compile", "--build-dir", odir ] ++
      concat [ ["-i", d] | d <- srcDirs ] ++
      concat [ ["-I", d] | d <- [autogenModulesDir lbi, odir] ++ includeDirs bi ] ++
      [ packageDbOpt pkgDb | pkgDb <- dbStack ] ++
      concat [ ["--package-id", display ipkgid ]
             | (ipkgid, _) <- componentPackageDeps clbi ] ++
      ["-G", display language] ++
      concat [ ["-X", display ex] | ex <- usedExtensions bi ] ++
      userOptions ++
      [ display modu | modu <- libModules lib ]



installLib
  :: Verbosity
  -> LocalBuildInfo
  -> FilePath  -- ^install location
  -> FilePath  -- ^install location for dynamic librarys
  -> FilePath  -- ^Build location
  -> PackageDescription
  -> Library
  -> IO ()
installLib verbosity lbi targetDir dynlibTargetDir builtDir pkg lib = do
  (hspkg, _) <- requireProgram verbosity hspkgProg (withPrograms lbi)
  runProgramInvocation verbosity $
    programInvocation hspkg $
      [ "install-library"
      , "--build-dir", builtDir
      , "--target-dir", targetDir
      , "--dynlib-target-dir", dynlibTargetDir
      , "--package-id", display $ packageId pkg
      ] ++ map display (libModules lib)

registerPackage
  :: Verbosity
  -> InstalledPackageInfo
  -> PackageDescription
  -> LocalBuildInfo
  -> Bool
  -> PackageDBStack
  -> IO ()
registerPackage verbosity installedPkgInfo _pkg lbi _inplace packageDbs = do
  (hspkg, _) <- requireProgram verbosity hspkgProg (withPrograms lbi)

  runProgramInvocation verbosity $
    (programInvocation hspkg
      ["update", packageDbOpt $ last packageDbs])
      { progInvokeInput = Just $ showInstalledPackageInfo installedPkgInfo }

packageDbOpt :: PackageDB -> String
packageDbOpt GlobalPackageDB        = "--global"
packageDbOpt UserPackageDB          = "--user"
packageDbOpt (SpecificPackageDB db) = "--package-db=" ++ db
