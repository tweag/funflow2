{-|
Implements a simple WordCount pipeline similar to the
examples in the Apache Beam tutorials:
https://beam.apache.org/get-started/wordcount-example/.

Expects the `words.txt` file to exist in the current working
directory.
-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.List
import qualified Data.Map as Map
import Data.Ord (comparing)
import qualified Data.Text.IO as T
import qualified Data.Text as T
import Text.Printf (printf)
import Text.Regex.Posix

import Funflow


------------------ UTIL FUNCTIONS -----------------------
type TextCount = (T.Text, Int)

-- | Counts members in a list of text. Also works for lazy lists (e.g. data from readFile)
countWords :: [T.Text] -> [TextCount]
countWords ws = let
        tally :: Map.Map T.Text Int -> T.Text -> Map.Map T.Text Int
        tally m k = if Map.member k m 
                    then Map.adjust (+1) k m
                    else Map.insert k 1 m
    in 
        Map.toList $ foldl tally Map.empty ws

-- | Removes punctuation marks from a text
removePunctuation :: T.Text -> T.Text
removePunctuation = 
    let
        punctuation = ",.?!:;\"\'" :: String
    in T.filter (not . (`elem` punctuation))

 -- | Filters words which are not comprised of latin characters (hyphens are allowed)
filterWords :: [T.Text] -> [T.Text]
filterWords = 
    let 
        wordsRegex = "[A-Za-z\']+" :: String
    in filter $ (=~ wordsRegex) . T.unpack

-- | Like countWords, but with sorted results
countWordsSortedDesc :: [T.Text] -> [TextCount]
countWordsSortedDesc = sortCountsDesc . countWords

-- | Sorts a list of word counts in descending order
sortCountsDesc :: [TextCount] -> [TextCount]
sortCountsDesc = sortBy (flip $ comparing snd)

-- | Prepare word counts for printing
formatCounts :: [TextCount] -> [T.Text]
formatCounts = map (\(w,c) -> T.pack $ printf "%s: %d" (T.unpack w) c)


------------------ PIPELINE DEFINITION ---------------
-- Individual task definitions (each task is also a full "Flow")
readDocument :: Flow String T.Text
readDocument = ioFlow T.readFile

doWordCount :: Flow T.Text T.Text
doWordCount = pureFlow (T.unlines . formatCounts . countWordsSortedDesc . filterWords. T.words . removePunctuation)

writeResult :: Flow (String, T.Text) ()
writeResult = ioFlow (\(f, countText) -> T.writeFile f countText)

-- Build the final pipeline using the task Flows defined above
--   Note: Using arrow syntax to control which inputs get passed to
--   which pipeline tasks, i.e. result_name <- task <- task_input
flow :: Flow (String, String) ()
flow = proc (documentFilePath, outputSummaryFilePath) -> do
    documentText <- readDocument -< documentFilePath
    countSummary <- doWordCount -< documentText
    writeResult -< (outputSummaryFilePath, countSummary)

-----------------------------------------------------
main :: IO ()
main = do
    let input_document = "words.txt" :: String
        output_counts = "counts.txt" :: String
    putStrLn $ printf "Starting WordCount pipeline for input document '%s'..." input_document
    runFlow defaultExecutionConfig flow (input_document, output_counts) :: IO ()
    putStrLn $ printf "Run complete! Results can be found in '%s'" output_counts
