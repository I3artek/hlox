{-# LANGUAGE RecordWildCards #-}

module Tokens (Token, scanTokens) where

import Control.Monad.State
import Data.Char (isAlpha, isAlphaNum, isDigit)
import Data.Map

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
    keywords :: Map String Token,
    tokens :: [Token],
    word :: String,
    line :: Int,
    pos :: Int,
    syntaxErrors :: [String]
  }

type Tokenizer a = State TokenState a

syntaxError :: String -> Tokenizer ()
syntaxError msg =
  modify
    ( \TokenState {syntaxErrors = _syntaxErrors, ..} ->
        TokenState {syntaxErrors = _syntaxErrors ++ [msg], ..}
    )

unrecognizedSymbol :: Char -> Tokenizer ()
unrecognizedSymbol c = do
  ts <- get
  let l = line ts
      p = pos ts
  let msg = "Unrecognized symbol '" ++ [c] ++ "' on line " ++ show l ++ " pos " ++ show p
  syntaxError msg

isAtEnd :: Tokenizer Bool
isAtEnd = do
  ts <- get
  curr <- peek
  let s = code ts
  case s of
    [] -> return $ curr == '\0'
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

peekNext :: Tokenizer Char
peekNext = do
  ts <- get
  let s = code ts
  case s of
    [] -> return '\0'
    (x : _) -> return x

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

clearLongToken :: Tokenizer ()
clearLongToken = modify (\TokenState {..} -> TokenState {word = "", ..})

addToLongToken :: Char -> Tokenizer ()
addToLongToken c =
  modify
    ( \TokenState {word = _word, ..} ->
        TokenState {word = _word ++ [c], ..}
    )

startLongToken :: Char -> Tokenizer ()
startLongToken c = do
  clearLongToken
  addToLongToken c

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
  point <- peek
  digit <- peekNext
  if point == '.' && isDigit digit
    then do
      addToLongToken '.'
      _ <- advance
      integer
      addToken $ NUMBER 0.5
    else
      addToken $ NUMBER 1.0

identifier :: Tokenizer ()
identifier = do
  next <- peek
  if isAlphaNum next || next == '_'
    then do
      addToLongToken next
      _ <- advance
      identifier
    else do
      ts <- get
      let name = word ts
          kws = keywords ts
          maybeToken = kws !? name
      case maybeToken of
        Just token -> addToken token
        Nothing -> addToken $ IDENTIFIER name

string :: Tokenizer ()
string = do
  end <- isAtEnd
  next <- peek
  if next /= '"' && not end
    then do
      addToLongToken next
      _ <- advance
      if next == '\n'
        then do
          newline
          string
        else
          string
    else do
      if end
        then return ()
        else do
          ts <- get
          _ <- advance
          addToken $ STRING $ word ts

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
        '"' -> do
          -- We don't want to have the '"' in the string itself
          clearLongToken
          string
        -- Comments
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
          -- Numbers
          if isDigit c
            then do
              startLongToken c
              number
            else
              -- Identifiers and keywords
              if isAlpha c
                then do
                  startLongToken c
                  identifier
                else
                  unrecognizedSymbol c
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
            keywords = loxKeywords,
            word = "",
            line = 0,
            pos = 0,
            syntaxErrors = []
          }

loxKeywords :: Map String Token
loxKeywords =
  fromList
    [ ("and", AND),
      ("class", CLASS),
      ("else", ELSE),
      ("false", FALSE),
      ("fun", FUN),
      ("for", FOR),
      ("if", IF),
      ("nil", NIL),
      ("or", OR),
      ("print", PRINT),
      ("return", RETURN),
      ("super", SUPER),
      ("this", THIS),
      ("true", TRUE),
      ("var", VAR),
      ("while", WHILE)
    ]
