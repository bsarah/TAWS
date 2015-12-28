{-# Language DoAndIfThenElse #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Handler.Home where

import Import hiding ((<|>),many,optional)
import Prelude (read)
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3,
                              withSmallInput)
import Data.Int (Int16)
import System.Process
import System.Random
import System.Directory
import qualified System.IO as SI (writeFile)
import Data.Maybe
import qualified Data.ByteString as B
import qualified Data.Text.Encoding as DTE
import qualified Data.Text as DT
import Text.Parsec
import Text.Parsec.ByteString
import Data.Either.Unwrap
import Data.List.Split hiding (oneOf)

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
    let errorMsg = DT.pack ""
    setTitle "Welcome To TAWS!"
    $(widgetFile "homepage") 

postHomeR :: Handler Html
postHomeR = do
  ((formResult, _), _) <- runFormPost inputForm
  ((sampleResult,_),_) <- runFormPost sampleForm
  app <- getYesod -- contains all the generally set path variables
  --Create tempdir and session-Id
  sessionId <- liftIO createSessionId  --session ID
  let outputPath = DT.unpack $ appTempDir $ appSettings app
  let temporaryDirectoryPath = outputPath ++ sessionId ++ "/"
  let geQueueName = DT.unpack $ appGeQueueName $ appSettings app
  let tawsresultPath = temporaryDirectoryPath ++ "result.txt"
  let dataPath = DT.unpack $ appDataDir $ appSettings app
  let bigcachePath = dataPath ++ "U50_vs_SP.xml"
  let programPath = DT.unpack $ appProgramDir $ appSettings app
  liftIO (createDirectory temporaryDirectoryPath)
  liftIO (writesubmissionData formResult temporaryDirectoryPath)
  let inputPath = case sampleResult of FormSuccess sample -> (dataPath ++ (DT.unpack sample))
                                       _ -> temporaryDirectoryPath ++ "input.fa"
  uploadedFile <- liftIO (B.readFile inputPath)
  let validatedInput = validateInput formResult uploadedFile sampleResult
  if (isRight validatedInput)
    then do
      --run blast to create xml
      let blastpath = programPath ++ "blast/bin/"
      let unirefpath = dataPath ++ "/uniref50.fasta"
      let smallcachePath = temporaryDirectoryPath ++ "Inp_vs_U50.xml"
      let taerrorPath = temporaryDirectoryPath ++ "errorMsg.txt"
      let blastcommand = blastpath ++ "blastx -query "++ inputPath ++" -db " ++ unirefpath ++ " -evalue 1e-4 -num_threads 5 -outfmt 5 -out " ++ smallcachePath ++ "\n"
      ----------------
      --Submit RNAlien Job to SGE
      --continue with samlesubmission xml file TODO change later!!!
      let blastfilter = setBlastFilter formResult
      let tacommand = programPath ++ "transalign +RTS -C -w -A100M -RTS " ++ blastfilter  ++ " " ++ smallcachePath ++ " " ++  bigcachePath  ++ " > " ++ tawsresultPath ++ " 2> " ++ taerrorPath ++ "\n"
      let archivecommand = "zip -9 -r " ++  temporaryDirectoryPath ++ "result.zip " ++ temporaryDirectoryPath ++ "\n"
      let blastdonecommand = "touch " ++ temporaryDirectoryPath ++ "/blastdone \n"
      let blastbegincommand = "touch " ++ temporaryDirectoryPath ++ "/blastbegin \n"
      let donecommand = "touch " ++ temporaryDirectoryPath ++ "/done \n"
      let begincommand = "touch " ++ temporaryDirectoryPath ++ "/begin \n"
      let delcommand = "rm -r " ++ smallcachePath ++ "\n"
      let delcommanderr = "rm " ++ taerrorPath ++ "\n"
      let blastdbpath = "export BLASTDB=" ++ dataPath ++ "uniref50.fasta \n"
      --sun grid engine settings
      let qsubLocation = "/usr/bin/qsub"
      let geErrorDir = temporaryDirectoryPath ++ "gelog"
      let geLogOutputDir = temporaryDirectoryPath ++ "gelog"
      let bashscriptpath = temporaryDirectoryPath ++ "qsub.sh"
      let bashheader = "#!/bin/bash\n"
      let geJoinErrorsSwitch = "#$ -j y\n"
      let geErrorPathSwitch = "#$ -e " ++ geErrorDir ++ "\n"
      let geOutputPathSwitch = "#$ -o " ++ geLogOutputDir ++ "\n"            
      let bashLDLibrary = "#$ -v LD_LIBRARY_PATH=" ++ dataPath ++ "\n"
      let bashmemrequest = "#$ -l mem_free=25G\n"
      --let bashhostrequest = "#$ -l hostname=\"picard\"\n" 
      let parallelenv = "#$ -pe para 5\n"
      let bashPath = "#$ -v PATH=" ++ programPath ++ ":/usr/bin/:/bin/:$PATH\n"
      let bashcontent = bashheader ++ bashLDLibrary ++ geJoinErrorsSwitch ++ geErrorPathSwitch ++ geOutputPathSwitch ++ bashmemrequest ++ parallelenv ++ bashPath ++ blastdbpath ++ blastbegincommand ++ blastcommand ++ blastdonecommand ++ begincommand ++ tacommand ++ delcommand ++ delcommanderr ++ archivecommand ++ donecommand
      let qsubcommand = qsubLocation ++ " -N " ++ sessionId ++ " -l h_vmem=25G " ++ " -q " ++ (geQueueName) ++ " " ++ bashscriptpath ++ " > " ++ temporaryDirectoryPath ++ "GEJobid"
      liftIO (SI.writeFile geErrorDir "")
      liftIO (SI.writeFile bashscriptpath bashcontent)
      _ <- liftIO (runCommand (qsubcommand))
      --Render page
      defaultLayout $ do
        aDomId <- newIdent
        let approotjs = appRoot $ appSettings app
        let sessionIdInsert =  DT.pack sessionId
        let sessionIdjs = sessionId
        $(widgetFile "calc")
        setTitle "Welcome To TAWS!"
    else do
      (formWidget, formEnctype) <- generateFormPost inputForm
      (sampleWidget, sampleEnctype) <- generateFormPost sampleForm
      defaultLayout $ do
        aDomId <- newIdent
        let parsingErrors = fromLeft validatedInput
        let errorMsg = DT.pack ("<div class=\"alert alert-danger\" role=\"alert\">" ++ parsingErrors ++ "</div><br>")
        setTitle "Welcome To TAWS!"
        $(widgetFile "homepage") 

inputForm :: Form (Maybe FileInfo, Maybe Textarea, Maybe Text)
inputForm = renderBootstrap3 BootstrapBasicForm $ (,,)
    <$> fileAFormOpt "Upload a fasta file"
    <*> aopt textareaField (withSmallInput "or paste sequences in fasta format: \n") Nothing
    <*> aopt textField (withSmallInput "Set optional blastfilter score: \n") Nothing

sampleForm :: Form Text
sampleForm = renderBootstrap3 BootstrapBasicForm (areq hiddenField (withSmallInput "") (Just "452.fa"))

-- Auxiliary functions:
-- | Adds cm prefix to pseudo random number
randomid :: Int16 -> String
randomid int = "taws" ++ (show int)

createSessionId :: IO String
createSessionId = do
  randomNumber <- randomIO :: IO Int16
  let sessionId = randomid (abs randomNumber)
  return sessionId

writesubmissionData :: FormResult (Maybe FileInfo,Maybe Textarea,Maybe Text) -> String -> IO()
writesubmissionData (FormSuccess (filepath,pastestring,_)) temporaryDirectoryPath = do
  if (isJust filepath) then do liftIO (fileMove (fromJust filepath) (temporaryDirectoryPath ++ "input.fa"))
                       else do liftIO (B.writeFile (temporaryDirectoryPath ++ "input.fa") (DTE.encodeUtf8 $ unTextarea (fromJust  pastestring)))
writesubmissionData _ _ = return ()

setBlastFilter :: FormResult (Maybe FileInfo,Maybe Textarea,Maybe Text) -> String
setBlastFilter (FormSuccess (_,_,blastfilter)) = maybe "" (\b -> "--blastfilter " ++ DT.unpack b ++ " ") blastfilter
setBlastFilter _ = ""

validateInput :: FormResult (Maybe FileInfo,Maybe Textarea,Maybe Text) -> B.ByteString -> FormResult Text -> Either String String
validateInput formInput fastaFileContent sampleResult
  | (isRight checkedForm) && (isRight checkedBlastFilter) = Right "Input ok"
  | (isRight checkedSample) && (isRight checkedBlastFilter) = Right "Input ok"
  | otherwise = Left (convertErrorMessagetoHTML((unwrapEither checkedForm) ++ (unwrapEither checkedSample) ++ (unwrapEither checkedBlastFilter)))
  where checkedForm =  either (\a -> Left (show a)) (\_ -> Right ("Input ok" :: String)) (parseFasta fastaFileContent)
        checkedSample = validateSampleResult sampleResult
        checkedBlastFilter = checkBlastFilter formInput

unwrapEither :: Either String String -> String
unwrapEither eithervalue = either (\a -> (show a) ++ "<br>") (\_ -> ("" :: String)) eithervalue 

convertErrorMessagetoHTML :: String -> String
convertErrorMessagetoHTML errorMessage = htmlMessage
        where replacedquotes = intercalate "<br>" . splitOn "\\n" $ errorMessage
              replacedlinebreaks = intercalate " " . splitOn "\"" $ replacedquotes
              htmlMessage = intercalate " " . splitOn "\\" $ replacedlinebreaks

checkTextArea :: Maybe Textarea -> Either String Fasta
checkTextArea filearea = do
  if isJust filearea
    then do
       let bytes = DTE.encodeUtf8 $ unTextarea (fromJust filearea)
       let parsingresult = either (\a -> Left (show a)) (\b -> Right b) (parseFasta bytes)
       parsingresult
    else (Left "")

checkBlastFilter :: FormResult (Maybe FileInfo,Maybe Textarea,Maybe Text) -> Either String String
checkBlastFilter (FormSuccess (_,_,blastfilter)) = checkBlastFilterValue blastfilter
checkBlastFilter _ = Right ("")

checkBlastFilterValue :: Maybe Text -> Either String String
checkBlastFilterValue (Just blastfilter) = either (\a -> Left (show a)) (\_ -> Right "Float ok") (parse float "Error in blastfilter:" (DTE.encodeUtf8 blastfilter))
checkBlastFilterValue _ = Right ("")
        
validateSampleResult :: FormResult Text -> Either String String
validateSampleResult (FormSuccess sample) = if (sample == (DT.pack "452.fa"))
                                              then Right ("Sample input ok" :: String)
                                              else Left "Incorrect sample requested."
validateSampleResult _ = Left ""

(<++>) :: forall (f :: * -> *) b. (Applicative f, Monoid b) => f b -> f b -> f b
(<++>) a b = (++) <$> a <*> b

(<:>) :: forall (f :: * -> *) a. Applicative f => f a -> f [a] -> f [a]
(<:>) a b = (:) <$> a <*> b

number :: forall s u (m :: * -> *). Stream s m Char => ParsecT s u m [Char]
number = many1 digit

plus :: forall s u (m :: * -> *). Stream s m Char => ParsecT s u m [Char]
plus = char '+' *> number

minus :: forall s u (m :: * -> *). Stream s m Char => ParsecT s u m [Char]
minus = char '-' <:> number

integer :: forall s u (m :: * -> *). Stream s m Char => ParsecT s u m [Char]
integer = plus <|> minus <|> number

float :: forall s u (m :: * -> *). Stream s m Char => ParsecT s u m Float
float = fmap rd $ integer <++> decimal <++> _exponent

    where rd       = read :: String -> Float
          decimal  = option "" $ char '.' <:> number
          _exponent = option "" $ oneOf "eE" <:> integer

genParserFasta :: GenParser ByteString st Fasta
genParserFasta = do
  _ <- string (">") 
  _header <- many1 (noneOf "\n")                
  _ <- newline
  _sequence <- many genParserSequenceFragments
  eof
  return $ Fasta _header (concat _sequence)

genParserSequenceFragments :: GenParser ByteString st String
genParserSequenceFragments = do
  _sequencefragment <- many1 (oneOf "AaCcGgTtUuRrYyKkMmSsWwBbDdHhVvNn-")
  _ <- optional newline
  return $ _sequencefragment
  
-- | parse Fasta
parseFasta :: ByteString -> Either ParseError Fasta
parseFasta input = parse genParserFasta "Error in fasta input:" input

data Fasta = Fasta
  { 
    header :: String,
    seq :: String
  }
  deriving (Show, Eq)
