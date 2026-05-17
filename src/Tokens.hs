{-# LANGUAGE RecordWildCards #-}

module Tokens (Token, scanTokens) where

import Control.Monad.State
import Data.Char (isDigit)

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
  | NUMBER Double
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
    word :: String,
    line :: Int,
    pos :: Int,
    syntaxErrors :: [String]
  }

type Tokenizer a = State TokenState a

syntaxError :: Char -> Tokenizer ()
syntaxError c = do
  ts <- get
  let l = line ts
      p = pos ts
  let msg = "Unrecognized symbol '" ++ [c] ++ "' on line " ++ show l ++ " pos " ++ show p
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

startLongToken :: Char -> Tokenizer ()
startLongToken c = modify (\TokenState {..} -> TokenState {word = [c], ..})

addToLongToken :: Char -> Tokenizer ()
addToLongToken c =
  modify
    ( \TokenState {word = _word, ..} ->
        TokenState {word = _word ++ [c], ..}
    )

integer :: Tokenizer ()
integer = do
  c <- peek
  if isDigit c
    then do
      addToLongToken c
      _ <- advance
      integer
    else return ()

number :: Tokenizer ()
number = do
  integer
  c <- peek
  if c == '.'
    then do
      addToLongToken '.'
      _ <- advance
      integer
      addToken $ NUMBER 0.5
    else
      addToken $ NUMBER 1.0

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
        '\0' -> skip
        _ -> do
          if isDigit c
            then do
              startLongToken c
              number
            else
              syntaxError c
      scanToken

-- list of tokens and a list of errors
scanTokens :: String -> ([Token], [String])
scanTokens [] = ([], [])
scanTokens s = (tokens ts, syntaxErrors ts)
  where
    ts =
      execState
        scanToken
        TokenState
          { current = '\0',
            code = s,
            tokens = [],
            word = "",
            line = 0,
            pos = 0,
            syntaxErrors = []
          }
