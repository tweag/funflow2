{-# LANGUAGE Arrows #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Arrow
import Control.Exception (SomeException (..))
import Control.Exception.Safe (try)
import Control.Kernmantle.Error (tryE)
import Control.Kernmantle.Rope (AnyRopeWith)
import Control.Monad (guard)
import Control.Monad.IO.Class (MonadIO)
import qualified Data.ByteString as BS
import Data.CAS.ContentHashable (ContentHashable)
import Data.CAS.ContentStore (Content (..))
import qualified Data.CAS.ContentStore as CS
import qualified Data.CAS.RemoteCache as RC
import Data.List (uncons)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.Typeable (Typeable)
import Funflow
  ( Flow,
    RequiredCore,
    RequiredStrands,
    RunFlowConfig (..),
    dockerFlow,
    ioFlow,
    pureFlow,
    runFlowWithConfig,
  )
import Funflow.Config (Configurable (..))
import qualified Funflow.Tasks.Docker as DE
import Parse
  ( parseMakeFile,
    parsecMakeFile,
    regularParse,
  )
import Path
  ( Abs,
    Dir,
    File,
    Path,
    Rel,
    absdir,
    filename,
    fromAbsDir,
    fromAbsFile,
    fromRelFile,
    parent,
    parseRelFile,
    parseAbsDir, 
    parseAbsFile,
    reldir,
    relfile,
    toFilePath,
    (</>),
  )
import Path.IO (getCurrentDir)
import System.Directory (getCurrentDirectory)
import System.Posix.Files (accessModes, createLink, setFileMode)
import Types

type Set = Set.Set
type Map = Map.Map
type FileName = String
type FileContent = String

main :: IO ()
main = do
  perhapsMakeFile <- getValidMakeFile
  case perhapsMakeFile of
    Right (MFError errMsg) ->
      putStrLn $ "Invalid make file:\n" ++ errMsg
    Left mfile -> do
      cwd <- getCurrentDir
      let contentStore = cwd </> [reldir|makefiletest/store|]
          defGoalRule = defaultGoal mfile
          tfName = mkRuleTarNm defGoalRule
          runCfg = getRunConfigWithoutFile contentStore
      --let unsafeBuildFlow = buildTarget contentStore mfile defGoalRule :: Flow () (Content File)
          --safeBuildFlow = tryE @SomeException unsafeBuildFlow :: Flow () (Either SomeException (Content File))
      putStrLn ("Attempting build:\n" ++ show defGoalRule)
      result <- runFlowWithConfig runCfg (tryE @SomeException (buildTarget contentStore mfile defGoalRule)) () :: IO (Either SomeException (Content File))
      putStrLn "Build attempt complete"
      case result of
        Left ex -> putStrLn $ "\n\nFailed, target failed:\n\n" ++ (show ex)
        Right execFile -> do
          let outpath = (fromAbsDir cwd) ++ "/" ++ tfName
          readByteString contentStore execFile >>= BS.writeFile outpath
          setFileMode outpath accessModes -- chmod +x
          putStrLn "\n\nSuccess, target executable made."

----------------------------------------------------------------------------------------
-- These types/wrappers for overcoming challenge of typelevel polymorphism in kernmantle
-- In particular, this is for beating "illegal polymorphic type..."
-- related to impredicative polymorphism
----------------------------------------------------------------------------------------
newtype Id a b = Id{ unId :: (Flow a b) }
type FlowList a b = [Id a b]

type MyExpFlow input output =
  forall m.
  (MonadIO m) =>
  AnyRopeWith
    RequiredStrands
    (RequiredCore m)
    input
    output

flowJoin :: FlowList a b -> Flow [a] [b]
flowJoin [] = pureFlow (\_ -> [])
flowJoin ff@(f:fs) = proc aa@(a:as) -> do
  () <- guardFlow -< (length ff == length aa)
  b <- unId f -< a
  bs <- flowJoin fs -< as
  returnA -< (b:bs)
----------------------------------------------------------------------------------------

-- | Extract valid makefile
--------------------------------------------------------------------------------
getValidMakeFile :: IO (Either MakeFile MFError)
getValidMakeFile = do
  readEither <- tryReadMakeFile
  case readEither of
    Left errMsg ->
      return $ Right $ MFError errMsg
    Right fileRead ->
      case parseMakeFile fileRead of
        Left errorMsg ->
          return $ Right $ MFError $ show errorMsg
        Right mkFile ->
          -- for now, ignoring checking it's valid
          return $ Left mkFile

tryReadMakeFile :: IO (Either String String)
tryReadMakeFile = do
  cwd <- getCurrentDirectory
  let makeFileLoc = cwd ++ "/Makefile"
  tryRead <- try $ readFile makeFileLoc
  case tryRead of
    Left (_ :: SomeException) -> return (Left makeFileLoc)
    Right file -> return (Right file)

-- | Building A Target
--------------------------------------------------------------------------------
-- Note: assuming the makefile is valid at this point!
buildTarget :: Path Abs Dir -> MakeFile -> MakeRule -> Flow () (Content File)
buildTarget storeRoot mkfile target@(MakeRule targetNm deps cmd) = let
   srcfiles = sourceFiles mkfile
   neededTargets = Set.toList $ Set.difference deps srcfiles
   neededSources  = Set.toList $ deps `Set.intersection` srcfiles
   maybeFindDepRules = findRules mkfile neededTargets  
 in case maybeFindDepRules of
   Nothing -> failNow
   --Just (depRules :: [MakeRule]) -> failWith "called!" DEBUG
   Just (depRules :: [MakeRule]) -> let
       grabSrcsActions = mapM (readFile . ("./" ++))
     in proc _ -> do
       --error "called!" DEBUG
       msgFlow ("Current rule: " ++ show target) -< ()
       () <- guardFlow -< (target `Set.member` (allrules mkfile))
       contentSrcFiles <- (ioFlow grabSrcsActions) -< neededSources
      --  compFile <- failNow -< ()
      --  returnA -< compFile
       depFiles <- flowJoin [Id{ unId = buildTarget storeRoot mkfile r } | r <- depRules] -< (replicate (length depRules) ())
       --let depFiles = [] DEBUG
       let fullSrcFiles = Map.fromList $ zip neededSources contentSrcFiles
       compFile <- (compileFile storeRoot) -< (targetNm, fullSrcFiles, depFiles, cmd)
       returnA -< compFile

-- | Compiles a C file in a docker container.
compileFile :: Path Abs Dir -> Flow (TargetFile, Map.Map SourceFile String, [Content File], Command) (Content File)
compileFile root = proc (tf, srcDeps, tarDeps, cmd) -> do
  relpathCompiledFile <- (ioFlow parseRelFile) -< tf
  -- resDir <- failWith -< ("relpath: " ++ show relpathCompiledFile)
  -- returnA -< resDir
  srcsInStore <- write2Store root -< srcDeps
  let inputFilesInStore = srcsInStore ++ tarDeps
  -- resDir <- failWith -< ("srcsInStore: " ++ show inputFilesInStore)
  -- returnA -< resDir
  inputDir <- mergeFiles root -< inputFilesInStore
  let scriptSrc = "#!/usr/bin/env bash\n\
                  \cd $1 \n"
                  ++ cmd ++ " -o /output/" ++ tf
  compileScript <- writeExecutableString root -< (scriptSrc, [relfile|script.sh|])
  resDir <- makeFlow -< (inputDir, compileScript)
  returnA -< resDir CS.:</> relpathCompiledFile
    where makeFlow :: Flow (Content Dir, Content File) CS.Item
          makeFlow = ioFlow ( \(indir, compile) -> runFlowWithConfig RunFlowConfig{configFile=Nothing, storePath=root} (dockerFlow (dockerConfig indir compile)) (mempty :: DE.DockerTaskInput) )
          dockerConfig :: Content Dir -> Content File -> DE.DockerTaskConfig
          dockerConfig indir compile = DE.DockerTaskConfig
            { DE.image = "gcc:7.3.0"
            , DE.command = Text.pack . toFilePath $ root </> (CS.contentFilename compile)
            , DE.args = [DE.Arg . Literal . Text.pack . toFilePath $ CS.itemRelPath (CS.contentItem indir)]
            }

-- Note: type SourceFile = String.
-- Note: TargetFile is the name of the file.

mergeFilesRaw :: Path Abs Dir -> [Content File] -> IO (Content Dir)
mergeFilesRaw root fs = CS.withStore root (\s -> merge s fs)
    where merge store files = let absFiles = map (CS.contentPath store) files
                                  linkIn d = mapM_ ( \f -> createLink (toFilePath f) (toFilePath $ d </> filename f) )
                              in CS.All <$> CS.putInStore store RC.NoCache (\_ -> return (error "uh-oh!")) linkIn absFiles

mergeFiles :: Path Abs Dir -> Flow [Content File] (Content Dir)
mergeFiles root = ioFlow (mergeFilesRaw root)

ioFixSrcFileData :: (FileName, FileContent) -> IO (FileContent, Path Rel File)
ioFixSrcFileData (x,y) = (\y' -> (y,y')) <$> parseRelFile x

-- flowFixSrcFileData :: Flow (FileName, FileContent) (FileContent, Path Rel File)
-- flowFixSrcFileData = ioFlow ioFixSrcFileData

putInStoreAtRaw ::
  (ContentHashable IO a, Typeable t) =>
  Path Abs Dir ->
  (Path Abs t -> a -> IO ()) ->
  (a, Path Rel t) ->
  IO (CS.Item, CS.Content t)
putInStoreAtRaw root put (a, p) =
  CS.withStore
    root
    ( \store -> do
        item <- CS.putInStore store RC.NoCache (\_ -> return ()) (\d a -> put (d </> p) a) a
        return (item, item CS.:</> p)
    )
                       
writeExecStrRaw :: Path Abs Dir -> (String, Path Rel File) -> IO (CS.Content File)
writeExecStrRaw d arg@(str,path) =  snd <$> putInStoreAtRaw d (\p x -> do
  writeFile (fromAbsFile p) x
  setFileMode (fromAbsFile p) accessModes) arg
  
writeExecutableString :: Path Abs Dir -> Flow (String, Path Rel File) (Content File)
writeExecutableString root = ioFlow (writeExecStrRaw root)

getRunConfigWithoutFile :: Path Abs Dir -> RunFlowConfig
getRunConfigWithoutFile d = RunFlowConfig {storePath = d, configFile = Nothing}
    
putInStoreAt :: (ContentHashable IO a, Typeable t) => Path Abs Dir -> (Path Abs t -> a -> IO ()) -> Flow (a, Path Rel t) (Content t)
putInStoreAt root action = ioFlow (\x@(a,p) -> snd <$> putInStoreAtRaw root action x)

-- | Map an arrow over a list.
mapA :: ArrowChoice a => a b c -> a [b] [c]
mapA f = arr (maybe (Left ()) Right . uncons) >>> (arr (const []) ||| ((f *** mapA f) >>> arr (uncurry (:))))

write1 :: Path Abs Dir -> Flow (String, Path Rel File) (Content File)
write1 d = putInStoreAt d (writeFile . fromAbsFile)

-- write2Store :: Path Abs Dir -> Flow (Map.Map FileName FileContent) [Content File]
-- write2Store root = proc files -> do
--   let fileList = Map.toList files
--       --n = length fileList
--   --filesWithPaths <- flowJoin (replicate n flowFixSrcFileData) -< (Map.toList files)
--   --filesWithPaths <- mapA flowFixSrcFileData -< (Map.toList files)
--   filesWithPaths <- ioFlow (mapM (\x -> do
--     y@(fc, p) <- ioFixSrcFileData x
--     snd <$> putInStoreAtRaw root (writeFile . fromAbsFile) y )) -< fileList
--   --contents <- mapA (write1 root) -< filesWithPaths
--   contents <- flowJoin (replicate n (write1 root)) -< filesWithPaths
--   returnA -< contents

write2Store :: Path Abs Dir -> Flow (Map.Map FileName FileContent) [Content File]
write2Store root = ioFlow (\files -> 
  mapM (\x -> do
    y <- ioFixSrcFileData x
    (_, z) <- putInStoreAtRaw root (writeFile . fromAbsFile) y
    return z
    ) (Map.toList files) )

-- findRules :: MakeFile -> [TargetFile] -> Maybe [MakeRule]
-- findRules _ _ = Nothing
findRules :: MakeFile -> [TargetFile] -> Maybe [MakeRule]
findRules MakeFile{ allrules = rules } !ts = do
 guard . null $ Set.difference tfileSet ruleTarNmSet
 let targetRules = Set.filter ((`Set.member` tfileSet) . mkRuleTarNm) rules
 return $ Set.toList targetRules
 where
   ruleTarNmSet = Set.map mkRuleTarNm rules
   tfileSet = Set.fromList ts

readByteString :: Path Abs Dir -> Content File -> IO BS.ByteString
readByteString store f = CS.withStore store (\s -> BS.readFile . fromAbsFile $ CS.contentPath s f)

guardFlow :: Flow Bool ()
guardFlow = proc test -> do
  case test of
    True -> returnA -< ()
    False -> do
      () <- failNow -< ()
      returnA -< ()

failNow :: Flow () a
failNow = proc _ -> do
  (ioFlow err) -< ()

failWith :: Flow String a
failWith = ioFlow (\msg -> do error msg)

err :: () -> IO a
err () = do
  error "error"

msgFlow :: String -> Flow () ()
msgFlow msg = ioFlow (\_ -> putStrLn msg)

-- | Strictly testing
--------------------------------------------------------------------------------
testMakeFileParse :: IO ()
testMakeFileParse = do
  Right mkfilestr <- tryReadMakeFile
  putStrLn "Testing make file parsing:"
  putStrLn "Readfile:"
  putStrLn mkfilestr
  putStrLn "Parsing:"
  print $ regularParse parsecMakeFile mkfilestr
