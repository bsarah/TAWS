{-# Language DoAndIfThenElse #-}


module Handler.Home where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3,
                              withSmallInput, withLargeInput)
import Yesod.Form.Fields (textareaField)
import Yesod.Form.Types (FormResult(..))
import Data.Int (Int16)
import System.Process
import System.Random
import System.Directory
import qualified System.IO as SI (writeFile)
import Data.Maybe
import qualified Data.ByteString as B
import qualified Data.Text.Encoding as DTE
import qualified Data.Text as DT

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getHomeR :: Handler Html
getHomeR = do
     (formWidget, formEnctype) <- generateFormPost inputForm
     (sampleWidget, sampleEnctype) <- generateFormPost sampleForm
     defaultLayout $ do
        aDomId <- newIdent
        setTitle "Welcome To TAWS!"
        $(widgetFile "homepage") --template

postHomeR :: Handler Html
postHomeR = do
    ((result, _), _) <- runFormPost inputForm
    ((sampleresult,_),_) <- runFormPost sampleForm
    app <- getYesod
    let handlerName = "postHomeR" :: Text
    let inputsubmission = case result of
            FormSuccess (fasta,taxid) -> Just (fasta,taxid)
            _ -> Nothing
    let samplesubmission = case sampleresult of
            FormSuccess (filename) -> Just (filename)
            _ -> Nothing
    if ((isJust inputsubmission) || (isJust samplesubmission))
        then do
            --Create tempdir and session-Id
            sessionId <- liftIO createSessionId  --session ID
            -- include revprox
            let outputPath = "/scr/kronos/sberkemer/tmp/"  --- TODO change paths later
            let geQueueName = "c_highmem.q"
            let temporaryDirectoryPath = (outputPath) ++ sessionId ++ "/"  
            let tawsLogPath = temporaryDirectoryPath ++ "Log"
            let tawsresultPath = temporaryDirectoryPath ++ "result.txt"
            let bigcachePath = "/scratch/sberkemer/U50_vs_SP.xml"
            let programPath = "/scr/kronos/sberkemer/"
            liftIO (createDirectory temporaryDirectoryPath)
            let inpType = whichWay inputsubmission
            if(inpType == 1 || inpType == 2) then do liftIO (writesubmissionData temporaryDirectoryPath inputsubmission)
                                             else do return ()
            --run blast to create xml
            let blastpath = programPath ++ "blast/bin/"
	    let unirefpath = "/scr/kronos/sberkemer/uniref50.fasta"
	    let smallcachePath = temporaryDirectoryPath ++ "Inp_vs_U50.xml"
	    let taerrorPath = temporaryDirectoryPath ++ "errorMsg.txt"
            let inputPath = if (inpType == 3) then DT.unpack (fromJust samplesubmission)
	                                      else temporaryDirectoryPath ++ "input.fa"
	    let blastcommand = blastpath ++ "blastx -query "++ inputPath ++" -db " ++ unirefpath ++ " -evalue 1e-4 -num_threads 8 -outfmt 5 -out " ++ smallcachePath ++ "\n"
            ----------------
            --Submit RNAlien Job to SGE
            --continue with samlesubmission xml file TODO change later!!!
            let tacommand = programPath ++ "transalign "++ smallcachePath ++ " " ++  bigcachePath  ++ " > " ++ tawsresultPath ++ " 2> " ++ taerrorPath ++ "\n"
            let archivecommand = "zip -9 -r " ++  temporaryDirectoryPath ++ "result.zip " ++ temporaryDirectoryPath ++ "\n"
	    let blastdonecommand = "touch " ++ temporaryDirectoryPath ++ "/blastdone \n"
	    let blastbegincommand = "touch " ++ temporaryDirectoryPath ++ "/blastbegin \n"
            let donecommand = "touch " ++ temporaryDirectoryPath ++ "/done \n"
	    let begincommand = "touch " ++ temporaryDirectoryPath ++ "/begin \n"
	    let delcommand = "rm " ++ smallcachePath ++ "\n"
	    let delcommanderr = "rm " ++ taerrorPath ++ "\n"
	    let blastdbpath = "export BLASTDB=/scr/kronos/sberkemer/uniref50.fasta \n"
            --sun grid engine settings
            let qsubLocation = "/usr/bin/qsub"
            let geErrorDir = temporaryDirectoryPath ++ "gelog"
            let geLogOutputDir = temporaryDirectoryPath ++ "gelog"
            let bashscriptpath = temporaryDirectoryPath ++ "qsub.sh"
            let bashheader = "#!/bin/bash\n"
            let bashLDLibrary = "#$ -v LD_LIBRARY_PATH=/scr/kronos/sberkemer/"
            let bashmemrequest = "#$ -l mem_free=40G\n"
	    let bashhostrequest = "#$ -l hostname=\"picard\"\n" --TODO change again!!!!
            let parallelenv = "#$ -pe para 5\n"
            let bashPath = "#$ -v PATH=" ++ programPath ++ ":/usr/bin/:/bin/:$PATH\n"
            let bashcontent = bashheader ++ bashLDLibrary ++ bashmemrequest ++ bashhostrequest ++parallelenv ++ bashPath ++ blastdbpath ++ blastbegincommand ++ blastcommand ++ blastdonecommand ++ begincommand ++tacommand ++ delcommand ++ delcommanderr ++ archivecommand ++ donecommand
            let qsubcommand = qsubLocation ++ " -N " ++ sessionId ++ " -l h_vmem=12G " ++ " -q " ++ (DT.unpack geQueueName) ++ " -e " ++ geErrorDir ++ " -o " ++  geLogOutputDir ++ " " ++ bashscriptpath ++ " > " ++ temporaryDirectoryPath ++ "GEJobid"
            liftIO (SI.writeFile geErrorDir "")
            liftIO (SI.writeFile tawsLogPath "")
            liftIO (SI.writeFile bashscriptpath bashcontent)
            _ <- liftIO (runCommand (qsubcommand))
            --Render page            
            defaultLayout $ do
                aDomId <- newIdent
                -- TODO if revprox set uncommand here!!!!
                let approotjs = DT.pack "http://kronos.tbi.univie.ac.at:3000"
                let sessionIdInsert =  DT.pack sessionId
                let sessionIdjs = sessionId                       
                $(widgetFile "calc")
                setTitle "Welcome To TAWS!"
        else do getHomeR  


--custom the input form
inputForm :: Form (Maybe FileInfo, Maybe Textarea)
inputForm = renderBootstrap3 BootstrapBasicForm $ (,)
    <$> fileAFormOpt "Upload a fasta file"
    <*> aopt textareaField (withSmallInput "or paste sequences in fasta format:") Nothing


sampleForm :: Form Text
sampleForm = renderBootstrap3 BootstrapBasicForm (areq hiddenField (withSmallInput "") (Just "/scr/kronos/sberkemer/452.fasta")) --TODO: change later!!!
--    <*> areq hiddenField (withSmallInput "") (Just "")

--sampleForm :: Form (Text,Text)
--sampleForm = renderBootstrap3 BootstrapBasicForm $ (,)
--    <$> areq hiddenField (withSmallInput "") (Just "/scr/kronos/sberkemer/data/452.xml")
--    <*> areq hiddenField (withSmallInput "") (Just "")




-- Auxiliary functions:
-- | Adds cm prefix to pseudo random number
randomid :: Int16 -> String
randomid number = "taws" ++ (show number)

createSessionId :: IO String                  
createSessionId = do
  randomNumber <- randomIO :: IO Int16
  let sessionId = randomid (abs randomNumber)
  return sessionId


writesubmissionData :: [Char] -> Maybe (Maybe FileInfo,Maybe Textarea) -> IO()
writesubmissionData temporaryDirectoryPath inputsubmission = do
   if(isJust inputsubmission)
     then do 
       let (filepath,pastestring) = fromJust inputsubmission
       if(isJust filepath) then do liftIO (fileMove (fromJust filepath) (temporaryDirectoryPath ++ "input.fa"))
                           else do liftIO (B.writeFile (temporaryDirectoryPath ++ "input.fa") (DTE.encodeUtf8 $ unTextarea (fromJust  pastestring)))
     else return ()

--check fasta format
checkSubmission :: FormResult (Maybe FileInfo,Maybe Text) -> Maybe (Maybe FileInfo,Maybe Text) 
checkSubmission (FormSuccess (a,b)) = Just (a,b)
checkSubmission _ = Nothing

--check fasta format
checkInput :: (Maybe FileInfo,Maybe Textarea) -> Int
checkInput (res,something)
  | isJust res = 1
  | isJust something = 2
  | otherwise = 3

-- check if some input exists if not return 3 (=samplesubmission).
-- if input exists: if fasta file: return 1, if pasted sequence return two
whichWay :: Maybe (Maybe FileInfo,Maybe Textarea) -> Int
whichWay inputsubmission
   | isJust inputsubmission = checkInput $ fromJust inputsubmission
   | otherwise = 3
