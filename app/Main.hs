module Main where

import Control.Monad.Trans.Reader (ReaderT (runReaderT), ask)
import Control.Monad.Trans.Writer (WriterT, execWriterT, runWriterT, tell)
import Data.List
import Helper (MaybeT, liftMaybeT, maybeReadInt, prompt, runMaybeT)
import Module.Item (LogItem (UnknownItem), addNewItem, description, itemId, dateInput, itemName, parseItem, parseLogItem, medAmount, unitQuantity, deleteItem)
import Module.Message (LogMessage, makeLogMessage, parseLogMessage)
import System.IO (hFlush, stdout)

runProgram :: [LogItem] -> [LogMessage] -> IO ()
runProgram items messages = do
    putStrLn "\n\n\n=============== Personal Recipe Record Apps ==============="
    putStrLn $ replicate 59 '='
    putStrLn "(a) Show all Recipe Taken  (b) Add new medicine  (c) Remove medicine  (d) Exit program"
    choice <- prompt "Input choice: "
    case choice of
        "a" -> do
            putStrLn $ showAllItem items
            empty <- prompt "Press enter to go back"
            runProgram items messages
        "b" -> do
            putStrLn "\nYou're about to add new medicine, please fill the information below: "
            date <- prompt "Date Input (DDMMYYYY): "
            name <- prompt "Medicine name: "
            putStr "Quantity: "
            hFlush stdout
            medAmount <- do
                result <- runMaybeT maybeReadInt
                case result of
                    (Just a) -> return a
                    Nothing -> return 0
            unit <- prompt "Quantity Unit: "
            description <- prompt "Description: "
            newItems <- addNewItem items date name medAmount unit description
            parseLogItem newItems
            logMessage <- makeLogMessage (last newItems) "NEW"
            parseLogMessage logMessage
            emptyPrompt <- prompt "Successfully added new item! Press enter to continue."
            runProgram newItems messages
        "c" -> do
            putStrLn "You're about to remove medicine: "
            -- Insert ItemID
            putStr "Insert ItemID: "
            hFlush stdout
            choice <- do
                result <- runMaybeT maybeReadInt
                case result of
                    (Just a) -> return a
                    Nothing -> return 0

            updatedItems <- deleteItem items choice
            parseLogItem updatedItems

            let changedItem = find (\item -> itemId item == choice) updatedItems
                extractItem :: Maybe LogItem -> LogItem
                extractItem (Just a) = a
                extractItem Nothing = UnknownItem

            let extractedItem = extractItem changedItem

            logMessage <-
                if extractedItem == UnknownItem
                    then makeLogMessage extractedItem "ERR"
                    else makeLogMessage extractedItem "OUT"
            parseLogMessage logMessage
            emptyPrompt <- prompt "Press enter to continue."
            runProgram updatedItems messages
        "d" -> do
            putStrLn "Exiting program..."
            putStrLn "Goodbye!"
        _ -> do
            empty <- prompt "Wrong input! Press enter to try again."
            runProgram items messages

showAllItem :: [LogItem] -> String
showAllItem [] = replicate 58 '='
showAllItem (item : rest) =
    "ID: " ++ show (itemId item)
        ++ "\nDate: "
        ++ dateInput item
        ++ "\nName: "
        ++ itemName item
        ++ "\nQuantity: "
        ++ show (medAmount item)
        ++ "\nQuantity Unit: "
        ++ unitQuantity item
        ++ "\nDescription: "
        ++ description item
        ++ "\n"
        ++ replicate 29 '-'
        ++ "\n"
        ++ showAllItem rest

main :: IO ()
main = do
    items <- fmap parseItem (readFile "log/items.log")
    runProgram items []