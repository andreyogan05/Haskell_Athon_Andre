module Module.Item where

import Control.Monad.Trans.Reader (ReaderT (runReaderT), ask)
import Control.Monad.Trans.Writer (WriterT, execWriterT, runWriterT, tell)
import Data.List
import Helper (MaybeT, liftMaybeT, maybeReadInt, prompt, runMaybeT)
import System.IO (hFlush, stdout)

data LogItem
    = LogItem
        { itemId :: Int
        , dateInput :: String
        , itemName :: String
        , medAmount :: Int
        , unitQuantity :: String
        , description :: String
        }
    | UnknownItem
    deriving (Show, Eq)

addNewItem :: [LogItem] -> String -> String -> Int -> String -> String -> IO [LogItem]
addNewItem oldLogItemList date name medAmount unit description = do
    let lastId =
            if null oldLogItemList
                then 0
                else itemId $ last oldLogItemList
        newId = lastId + 1
        newLogItem =
            LogItem
                { itemId = newId
                , dateInput = date
                , itemName = name
                , medAmount = medAmount
                , unitQuantity = unit
                , description = description
                }
    let newLogItemList = oldLogItemList ++ [newLogItem]
    return newLogItemList

deleteItem :: [LogItem] -> Int -> IO [LogItem]
deleteItem oldLogItemList choice = do
    let itemExist = find (\item -> (itemId item) == choice) oldLogItemList

        extractItem :: Maybe LogItem -> LogItem
        extractItem (Just a) = a
        extractItem Nothing = UnknownItem

        removeItem :: [LogItem] -> LogItem -> [LogItem]
        removeItem [] chosenItem = []
        removeItem (item : rest) chosenItem
            | item == chosenItem = removeItem rest chosenItem
            | otherwise = [item] ++ removeItem rest chosenItem

    let updatedLogItemList =
            if (extractItem itemExist) == UnknownItem
                then oldLogItemList
                else removeItem oldLogItemList (extractItem itemExist)

    if (extractItem itemExist) == UnknownItem
        then putStrLn "Item not found. Please check your ItemID"
        else putStrLn "Successfully remove medicine!"

    return updatedLogItemList

parseLogItem :: [LogItem] -> IO ()
parseLogItem logItemList = do
    let convertToLog :: [LogItem] -> String
        convertToLog [] = ""
        convertToLog (logItem : rest) =
            show (itemId logItem)
                ++ " "
                ++ dateInput logItem
                ++ " "
                ++ itemName logItem
                ++ " "
                ++ show (medAmount logItem)
                ++ " "
                ++ unitQuantity logItem
                ++ " "
                ++ description logItem
                ++ "\n"
                ++ convertToLog rest
    let parsedLogItem = init $ convertToLog logItemList -- using init to remove the last \n at the end of the .log
    writeFile "log/items.txt" parsedLogItem

parseItem :: String -> [LogItem]
parseItem rawContent = map parseSingleItem (lines rawContent)

parseSingleItem :: String -> LogItem
parseSingleItem str = case words str of
    (i : t : n : m : u : d) -> makeItem i t n m u d
    _ -> UnknownItem

makeItem :: String -> String -> String -> String -> String -> [String] -> LogItem
makeItem itemId dateInput itemName medAmount unitQuantity description =
    LogItem
        { itemId = read itemId
        , dateInput = dateInput
        , itemName = itemName
        , medAmount = read medAmount
        , unitQuantity = unitQuantity
        , description = unwords description
        }