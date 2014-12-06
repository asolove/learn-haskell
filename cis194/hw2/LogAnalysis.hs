{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
import Log
import Data.Maybe(fromMaybe)

-- Ex 1 : parse log messages
parse :: String -> [LogMessage]
parse = (map parseMessage) . lines

parseMessage :: String -> LogMessage
parseMessage s = fromMaybe (Unknown s) (parseMessageMaybe s)

parseMessageMaybe :: String -> Maybe LogMessage
parseMessageMaybe s = do
  (messageType, rest) <- parseMessageType (words s)
  (timeString, message) <- firstAndRest rest
  return $ LogMessage messageType (parseTimestamp timeString) (unwords message)

firstAndRest :: [a] -> Maybe (a, [a])
firstAndRest (x:xs) = Just (x, xs)
firstAndRest _ = Nothing
        
parseMessageType :: [String] -> Maybe (MessageType, [String])
parseMessageType ("I":rest) = Just (Info, rest)
parseMessageType ("W":rest) = Just (Warning, rest)
parseMessageType ("E":rest) = Just (Error level, rest')
  where level = read $ head rest
        rest' = tail rest
parseMessageType _ = Nothing

parseTimestamp :: String -> Int
parseTimestamp = read

-- Ex 2: create tree of messages
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) t = t
insert x Leaf = Node Leaf x Leaf
insert x@(LogMessage _ xt _) (Node t1 y@(LogMessage _ yt _) t2) | xt > yt = Node t1 y (insert x t2)
insert x (Node t1 y t2) = Node (insert x t1) y t2

-- Ex 3: build tree of messages
build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

-- Ex 4: list messages in tree by order
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node t1 m t2) = inOrder t1 ++ [m] ++ inOrder t2

-- Ex 5: what went weong
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map message . filter relevant . inOrder . build
  where relevant (LogMessage (Error s) _ _) | s >= 50 = True
        relevant _ = False

message :: LogMessage -> String
message (LogMessage _ _ m) = m
message (Unknown m) = m
        
-- Ex 6: logs in order
logsInOrder :: [LogMessage] -> [String]
logsInOrder = map message . inOrder . build
