{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log

split :: Char -> String -> [String]
split c s = case rest of
  [] -> [chunk]
  _ : rest -> chunk : split c rest
  where
    (chunk, rest) = break (== c) s

parseMessage :: String -> LogMessage
parseMessage msg
  | msgType == "E" = LogMessage (Error severity) errTimeStamp errMsgString
  | msgType == "W" = LogMessage Warning timeStamp msgString
  | msgType == "I" = LogMessage Info timeStamp msgString
  | otherwise = Unknown unknownMsg
  where
    splitBySpace = split ' ' msg
    afterSpace = tail splitBySpace
    afterSpace' = tail afterSpace
    msgType = head splitBySpace
    timeStamp = read (head afterSpace) :: Int
    msgString = unwords afterSpace'
    severity = timeStamp
    errTimeStamp = read (head afterSpace') :: Int
    errMsgString = unwords $ tail afterSpace'
    unknownMsg = "This is not in the right format"

parse :: String -> [LogMessage]
parse str = map parseMessage $ lines str

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) mTree = mTree
insert lMsg Leaf = Node Leaf lMsg Leaf
insert lMsg@(LogMessage _ ts _) mTree@(Node leftT lMsg'@(LogMessage _ ts' _) rightT)
  | ts < ts' = Node (insert lMsg leftT) lMsg' rightT
  | ts > ts' = Node leftT lMsg' (insert lMsg rightT)
  | otherwise = mTree

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

-- build [] = Leaf
-- build (lMsg:lMsgs) = insert lMsg $ build lMsgs

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node leftT lMsg rightT) = inOrder leftT ++ [lMsg] ++ inOrder rightT

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong [] = []
whatWentWrong lMsgs = map getLogString $ filter (\lMsg -> isMTError lMsg && getSeverity lMsg > 50) (inOrder $ build lMsgs)

isMTError :: LogMessage -> Bool
isMTError (LogMessage (Error _) _ _) = True
isMTError _ = False

getSeverity :: LogMessage -> Int
getSeverity (LogMessage (Error severity) _ _) = severity
getSeverity _ = -1

getLogString :: LogMessage -> String
getLogString (LogMessage _ _ str) = str
getLogString _ = ""