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
import System.IO (writeFile)
import Data.Maybe

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getHomeR :: Handler Html
getHomeR = do
     (formWidget, formEnctype) <- generateFormPost sampleForm
     defaultLayout $ do
        aDomId <- newIdent
        setTitle "Welcome To Yesod!"
        $(widgetFile "homepage") --template

postHomeR :: Handler Html
postHomeR = do
    ((result, formWidget), formEnctype) <- runFormPost sampleForm
    let handlerName = "postHomeR" :: Text
    let checkedSub = checkSubmission result
    if (isJust (checkedSub))
      then do
        if (checkInput (fromJust checkedSub))
          then do
            --Create tempdir and session-Id
            sessionId <- liftIO createSessionId  --session ID
            defaultLayout $ do
              aDomId <- newIdent
              setTitle "Welcome To Yesod!"
              $(widgetFile "homepage")

          else do 
            defaultLayout $ do
              aDomId <- newIdent
              setTitle "Welcome To Yesod!"
              $(widgetFile "homepage")
            
      else do 
        defaultLayout $ do
          aDomId <- newIdent
          setTitle "Welcome To Yesod!"
          $(widgetFile "homepage")

--custom the input form
sampleForm :: Form (Maybe FileInfo, Maybe Text)
sampleForm = renderBootstrap3 BootstrapBasicForm $ (,)
    <$> fileAFormOpt "Upload a fasta file"
    <*> aopt textField (withSmallInput "or paste sequences in fasta format:") Nothing


-- Auxiliary functions:
-- | Adds cm prefix to pseudo random number
randomid :: Int16 -> String
randomid number = "cm" ++ (show number)

createSessionId :: IO String                  
createSessionId = do
  randomNumber <- randomIO :: IO Int16
  let sessionId = randomid (abs randomNumber)
  return sessionId


--check fasta format
checkSubmission :: FormResult (Maybe FileInfo,Maybe Text) -> Maybe (Maybe FileInfo,Maybe Text) 
checkSubmission (FormSuccess (a,b)) = Just (a,b)
checkSubmission _ = Nothing

--check fasta format
checkInput :: (Maybe FileInfo,Maybe Text) -> Bool
checkInput (res,something)
  | isJust res = True
  | isJust something = True
  | otherwise = False
