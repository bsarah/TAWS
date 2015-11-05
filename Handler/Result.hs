{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Result where

import Import
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Text as DT
import qualified Data.List as DL (head)
import System.Directory   
import System.IO (readFile)
--import Data.List.Split (splitOn)
import Control.Monad
import Data.Csv
import Data.Char
import qualified Data.Vector as V
import Data.Either.Unwrap
--import Text.ParserCombinators.Parsec 

getResultR :: Handler Html
getResultR = do
    result <- getRequest
    let params = reqGetParams result
    let sessionIdjs = snd (DL.head params)
    let sessionId = DT.unpack sessionIdjs
    let currentApproot = DT.pack "http://kronos.tbi.univie.ac.at:3000" --TODO change later
    let outputPath = "/scr/kronos/sberkemer/tmp/"  --- TODO change paths later
    let temporaryDirectoryPath = (outputPath) ++ sessionId ++ "/"
    let tempDirectoryRootURL = "http://nibiru.tbi.univie.ac.at/taws_tmp/taws/"
    let tempDirectoryURL = tempDirectoryRootURL ++ sessionId ++ "/"
    let tempDirectoryURLjs = DT.pack ("../taws_tmp/taws/" ++ sessionId ++ "/")
    blaststarted <- liftIO (doesFileExist (temporaryDirectoryPath ++ "blastbegin"))
    blastdone <- liftIO (doesFileExist (temporaryDirectoryPath ++ "blastdone"))  
    started <- liftIO (doesFileExist (temporaryDirectoryPath ++ "begin"))
    done <- liftIO (doesFileExist (temporaryDirectoryPath ++ "done"))  
    let unfinished = not done
    resultmsg <- liftIO $ buildResultMsg done started blastdone blaststarted
    resultstring <-liftIO (retrieveResultCsv done temporaryDirectoryPath)
    if started
       then do
         if done
           then do
             defaultLayout $ do
               aDomId <- newIdent
               setTitle "TAWS - Results"
               $(widgetFile "result")
           else do
             defaultLayout $ do
               aDomId <- newIdent
               setTitle "TAWS - Results"
               $(widgetFile "result")
       else do
         defaultLayout $ do
               aDomId <- newIdent
               setTitle "TAWS - Results"
               $(widgetFile "result")

retrieveResultCsv :: Bool -> String -> IO String
retrieveResultCsv done temporaryDirectoryPath = do
  if done
     then do
       let myOptions = defaultDecodeOptions {
         decDelimiter = fromIntegral (ord '\t')
         }
       let tawsCSVPath = temporaryDirectoryPath ++ "result.txt"
       tawsCSV <- B.readFile tawsCSVPath
       let decodedCsvOutput = V.toList (fromRight (decodeWith myOptions HasHeader (tawsCSV) :: Either String (V.Vector (String,String,String,String,String,String,String,String,String))))
       let insidetable = concatMap constructTableLineContent decodedCsvOutput
       let tableHeader = "<tr>"++"<th>"++ "Query" ++ "</th>"
                                       ++"<th>"++ "Target" ++ "</th>"
                                       ++"<th>"++ "Score" ++ "</th>"
                                       ++"<th>"++ "Alignment length" ++ "</th>"
                                       ++"<th>"++ "Average score" ++ "</th>"
                                       ++"<th>"++ "Query start" ++ "</th>"
                                       ++"<th>"++ "Query end" ++ "</th>"
                                       ++"<th>"++ "Target start" ++ "</th>"
                                       ++"<th>"++ "Target end" ++ "</th>"
                                       ++"</tr>"
       let resultstring = "<table>"++ tableHeader ++ insidetable ++ "</table>"
       return resultstring
     else do
         return "Your job is still running."
         



constructTableLineContent :: (String,String,String,String,String,String,String,String,String) -> String
constructTableLineContent (a,b,c,d,e,f,g,h,i) = "<tr>"++"<th>"++ a ++ "</th>"
                                                 ++"<th>"++ b ++ "</th>"
                                                 ++"<th>"++ c ++ "</th>"
                                                 ++"<th>"++ d ++ "</th>"
                                                 ++"<th>"++ e ++ "</th>"
                                                 ++"<th>"++ f ++ "</th>"
                                                 ++"<th>"++ g ++ "</th>"
                                                 ++"<th>"++ h ++ "</th>"
                                                 ++"<th>"++ i ++ "</th>"++" </tr>"

buildResultMsg :: Bool -> Bool -> Bool -> Bool -> IO String
buildResultMsg done started blastdone blaststarted = do
    if done then return "Job completed!"
            else if started then return "Blast run completed. Transalign is still running."
	                    else if blastdone then return "Blast run completed."
		                              else if blaststarted then return "Blast is still running."
				                                   else do return "Your job is still running."
