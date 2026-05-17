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
    line :: Int,
    pos :: Int,
    syntaxErrors :: [String]
  }

type Tokenizer a = State TokenState a

syntaxError :: Tokenizer ()
syntaxError = do
  ts <- get
  let c = current ts
      l = line ts
      p = pos ts
  let msg = "Unrecognized symbol " ++ [c] ++ " on line " ++ show l ++ " pos " ++ show p
  modify
    ( \TokenState {syntaxErrors = _syntaxErrors, ..} ->
        TokenState {syntaxErrors = _syntaxErrors ++ [msg], ..}
    )
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
      modify
        ( \TokenState {pos = _pos, ..} ->
            TokenState {current = x, code = xs, pos = _pos + 1, ..}
        )
      return curr

match :: Char -> Token -> Token -> Tokenizer ()
match expected tMatch tNoMatch = do
  next <- peek
  if next == expected
    then do
      _ <- advance
      addToken tMatch
    else
      addToken tNoMatch

comment :: Tokenizer ()
comment = do
  next <- peek
  end <- isAtEnd
  if next == '\n' || end
    then
      return ()
    else do
      _ <- advance
      comment

skip :: Tokenizer ()
skip = return ()

newline :: Tokenizer ()
newline = do
  modify
    ( \TokenState {line = _line, pos = _, ..} ->
        TokenState {line = _line + 1, pos = 0, ..}
    )

scanToken :: Tokenizer ()
scanToken = do
  end <- isAtEnd
  curr <- getCurrent
  if end && curr == '\0'
    then return ()
    else do
      c <- advance
      case c of
        -- Single character tokens
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
        -- One ot two character tokens
        '!' -> match '=' BANG_EQUAL BANG
        '=' -> match '=' EQUAL_EQUAL EQUAL
        '>' -> match '=' GREATER_EQUAL GREATER
        '<' -> match '=' LESS_EQUAL LESS
        '/' -> do
          next <- peek
          if next == '/' then comment else addToken SLASH
        -- Ignore all the whitespaces
        ' ' -> skip
        '\r' -> skip
        '\t' -> skip
        '\n' -> newline
        _ -> syntaxError
      scanToken

scanTokens :: String -> [Token]
scanTokens [] = []
scanTokens s = tokens ts
  where
    ts =
      execState
        scanToken
        TokenState
          { current = '\0',
            code = s,
            tokens = [],
            line = 0,
            pos = 0,
            syntaxErrors = []
          }
