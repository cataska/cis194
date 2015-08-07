{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

parseInfoMessage :: [String] -> LogMessage
parseInfoMessage ws = LogMessage Info timeStamp message
    where timeStamp = read (head ws)
          message = unwords (tail ws)

parseErrorMessage :: [String] -> LogMessage
parseErrorMessage ws = LogMessage (Error level) timeStamp message
    where level = read (head ws)
          timeStamp = read (head (tail ws))
          message = unwords (drop 2 ws)

parseWarningMessage :: [String] -> LogMessage
parseWarningMessage ws = LogMessage Warning timeStamp message
    where timeStamp = read (head ws)
          message = unwords (tail ws)
    
parseMessage :: String -> LogMessage
parseMessage str = case t of
                     'I' -> parseInfoMessage body
                     'E' -> parseErrorMessage body
                     'W' -> parseWarningMessage body
                     _   -> Unknown (unwords ws)
                   where ws = words str
                         t = head (head ws)
                         body = tail ws

parse :: String -> [LogMessage]
parse logs = map parseMessage $ lines logs

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert msg Leaf = Node Leaf msg Leaf
insert msg@(LogMessage _ ts _) tree@(Node _ treeMsg _) =
    case treeMsg of
      Unknown _                            -> Leaf
      LogMessage Info      treeTimeStamp _ -> buildTree treeTimeStamp ts msg tree
      LogMessage Warning   treeTimeStamp _ -> buildTree treeTimeStamp ts msg tree
      LogMessage (Error _) treeTimeStamp _ -> buildTree treeTimeStamp ts msg tree
    where
      buildTree treeTimeStamp t m tr = if treeTimeStamp > t then
                                           Node tr m Leaf
                                       else
                                           Node Leaf m tr

build :: [LogMessage] -> MessageTree
build msgs = _build (head msgs) Leaf (tail msgs)
    where
      _build msg tree ms = if length ms == 0 then tree
                           else
                               _build (head ms) (insert msg tree) (tail ms)

inOrder :: MessageTree -> [LogMessage]
inOrder tree = case tree of
                 Leaf -> []
                 Node left node right -> (inOrder left) ++ node : (inOrder right)

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong msgs = map extractString (filter errorGreaterThan50 (inOrder (build msgs)))
    where
      errorGreaterThan50 x = case x of
                               LogMessage (Error level) _ _ -> if level >= 50 then True else False
                               _ -> False
      extractString l = case l of
                          LogMessage _ _ str -> str
                          Unknown str -> str
