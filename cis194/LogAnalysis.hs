{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
import Log

--Exercise 1
parseMessage :: String -> LogMessage
parseMessage s 
  | prefix == "I"  = LogMessage Info (read (splitted !! 1)) (unwords (tail (tail splitted)))
  | prefix == "W" = LogMessage Warning (read (splitted !! 1)) (unwords (tail (tail splitted)))
  | prefix == "E" = LogMessage (Error (read (splitted !! 1))) (read (splitted !! 2)) (unwords (tail (tail (tail splitted))))
  | otherwise = Unknown s
  where splitted = words s
        prefix = splitted !! 0


parse :: String -> [LogMessage]
parse input = map parseMessage logline
  where logline = lines input

--Exercise 2
insert :: LogMessage -> MessageTree -> MessageTree
insert message@(LogMessage _ _ _) Leaf = Node Leaf message Leaf
insert message@(LogMessage _ ts1 _) (Node treeLeft (LogMessage _ ts2 _) treeRight)
  | ts1 < ts2 = insert message treeLeft
  | ts1 >= ts2 = insert message treeRight
insert _ tree = tree

--Exercise 3
build :: [LogMessage] -> MessageTree
build msgs 
 | null msgs = Leaf
 | otherwise = insert (head msgs) (build (tail msgs))
 
--Exercise 4
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node treeLeft message treeRight) = inOrder treeLeft ++ message : inOrder treeRight
 
--Exercise 5
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong msgs
  | null msgs = []
  | isRelevant(head msgs) = (getMessage (head msgs)) : whatWentWrong (tail msgs)
  | otherwise = whatWentWrong (tail msgs)
  
isRelevant :: LogMessage -> Bool
isRelevant (Unknown _) = False
isRelevant (LogMessage (Error severity) _ _) 
  |severity > 50  = True
  |otherwise = False
isRelevant _ = False
  
getMessage :: LogMessage -> String
getMessage (LogMessage _ _ text) = text
getMessage _ = ""