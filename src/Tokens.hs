{-# LANGUAGE RecordWildCards #-}
module Tokens (Token, scanTokens) where

import Control.Monad.State

data Token
-- Single character
  = LEFT_PAREN
  | RIGHT_PAREN
  | LEFT_BRACE
  | RIGHT_BRACE
  | COMMA
  | DOT
  | MINUS
  | PLUS
  | SEMICOLON
  | SLASH
  | STAR
-- One or two characters
  | BANG
  | BANG_EQUAL
  | EQUAL
  | EQUAL_EQUAL
  | GREATER
  | GREATER_EQUAL
  | LESS
  | LESS_EQUAL
-- Literals
  | IDENTIFIER String
  | STRING String
  | NUMBER String
-- Keywords
  | AND
  | CLASS
  | ELSE
  | FALSE
  | FUN
  | FOR
  | IF
  | NIL
  | OR
  | PRINT
  | RETURN
  | SUPER
  | THIS
  | TRUE
  | VAR
  | WHILE
-- EOF
  | EOF
  deriving (Show)

data TokenState = TokenState {
  current :: Char,
  code :: String,
  tokens :: [Token]
  }

type Tokenizer a = State TokenState a

getCurrent :: Tokenizer Char
getCurrent = do
  ts <- get
  return $ current ts

addToken :: Token -> Tokenizer ()
addToken t = do
  modify (\TokenState {tokens=_tokens,..} -> TokenState {tokens=_tokens++[t], ..})
  return ()

peek :: Tokenizer Char
peek = do
  ts <- get
  let s = code ts
  case s of
    [] -> return '\0'
    (x:_) -> return x

peekTwo :: Tokenizer (Char, Char)
peekTwo = do
  ts <- get
  let s = code ts
  case s of
    (x:y:_) -> return (x,y)
    _ -> return ('\0','\0')

advance :: Tokenizer Char
advance = do
  ts <- get
  let s = code ts
  case s of
    [] -> return '\0'
    (x:xs) -> do
      modify (\TokenState {..} -> TokenState {current=x, code=xs, ..})
      return x

scanSingleCharToken :: Tokenizer ()
scanSingleCharToken = do
  c <- getCurrent
  case c of
    '(' -> addToken LEFT_PAREN
    ')' -> addToken RIGHT_PAREN
    '{' -> addToken LEFT_BRACE
    '}' -> addToken RIGHT_BRACE
    ',' -> addToken COMMA
    '.' -> addToken DOT
    '-' -> addToken MINUS
    '+' -> addToken PLUS
    ';' -> addToken SEMICOLON
    '*' -> addToken STAR
  return ()

scanToken :: Tokenizer ()
scanToken = do
  c <- advance
  if c == '\0' then return () else do
    scanSingleCharToken
    scanToken

scanTokens :: String -> [Token]
scanTokens s = tokens ts
  where
    ts = execState scanToken TokenState {current='\0', code=s, tokens=[]}
