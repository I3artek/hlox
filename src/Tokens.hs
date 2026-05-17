{-# LANGUAGE RecordWildCards #-}

module Tokens (Token, scanTokens) where

import Control.Monad.State

data Token
  = -- Single character
    LEFT_PAREN
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
  | -- One or two characters
    BANG
  | BANG_EQUAL
  | EQUAL
  | EQUAL_EQUAL
  | GREATER
  | GREATER_EQUAL
  | LESS
  | LESS_EQUAL
  | -- Literals
    IDENTIFIER String
  | STRING String
  | NUMBER String
  | -- Keywords
    AND
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
  | -- EOF
    EOF
  deriving (Show)

data TokenState = TokenState
  { current :: Char,
    code :: String,
    tokens :: [Token],
    syntaxErrors :: [String]
  }

type Tokenizer a = State TokenState a

syntaxError :: String -> Tokenizer ()
syntaxError s = do
  modify (\TokenState {syntaxErrors = _syntaxErrors, ..} -> TokenState {syntaxErrors = _syntaxErrors ++ [s], ..})
  return ()

isAtEnd :: Tokenizer Bool
isAtEnd = do
  ts <- get
  let s = code ts
  case s of
    [] -> return True
    _ -> return False

getCurrent :: Tokenizer Char
getCurrent = do
  ts <- get
  return $ current ts

addToken :: Token -> Tokenizer ()
addToken t = do
  modify (\TokenState {tokens = _tokens, ..} -> TokenState {tokens = _tokens ++ [t], ..})
  return ()

peek :: Tokenizer Char
peek = do
  ts <- get
  return $ current ts

-- We use [Char] instead of a tuple, so we can use string comparison
peekTwo :: Tokenizer [Char]
peekTwo = do
  ts <- get
  first <- peek
  let s = code ts
  case s of
    [] -> return [first, '\0']
    (x : _) -> return [first, x]

advance :: Tokenizer Char
advance = do
  ts <- get
  let curr = current ts
      s = code ts
  case s of
    [] -> do
      modify (\TokenState {..} -> TokenState {current = '\0', ..})
      return curr
    (x : xs) -> do
      modify (\TokenState {..} -> TokenState {current = x, code = xs, ..})
      return curr

scanToken :: Tokenizer ()
scanToken = do
  end <- isAtEnd
  if end
    then return ()
    else do
      c <- advance
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
        _ -> syntaxError $ "Unrecognized symbol: "++[c]
      scanToken

scanTokens :: String -> [Token]
scanTokens [] = []
scanTokens s = tokens ts
  where
    ts = execState scanToken TokenState {current = '\0', code = s, tokens = [], syntaxErrors = []}
