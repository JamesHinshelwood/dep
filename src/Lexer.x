{
module Lexer (
    scanTokens,
    Token(..)
) where
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]

tokens :-

    $white+                         ;
    "--".                           ;
    \*                              { \s -> TStar }
    "[]"                            { \s -> TBox }
    \:                              { \s -> TColon }
    \\                              { \s -> TLambda }
    \.                              { \s -> TDot }
    \(                              { \s -> TLParen }
    \)                              { \s -> TRParen }
    "->"                            { \s -> TArrow }
    \=                              { \s -> TEquals }
    \,                              { \s -> TComma }
    "×"                             { \s -> TProduct }
    ".1"                            { \s -> TFirst }
    ".2"                            { \s -> TSecond }
    "inl"                           { \s -> TInL }
    "inr"                           { \s -> TInR }
    \+                              { \s -> TSum }
    \|                              { \s -> TBar }
    "case"                          { \s -> TCase }
    "of"                            { \s -> TOf }
    "let"                           { \s -> TLet }
    "in"                            { \s -> TIn }
    "fold"                          { \s -> TFold }
    "unfold"                        { \s -> TUnfold }
    \[                              { \s -> TLBracket }
    \]                              { \s -> TRBracket }
    "~"                             { \s -> TTilde }
    $alpha [$alpha $digit \_ \']*   { \s -> TVar s }

{
data Token = TStar
           | TBox
           | TColon
           | TLambda
           | TDot
           | TLParen
           | TRParen
           | TArrow
           | TEquals
           | TComma
           | TProduct
           | TFirst
           | TSecond
           | TInL
           | TInR
           | TSum
           | TBar
           | TCase
           | TOf
           | TLet
           | TIn
           | TFold
           | TUnfold
           | TLBracket
           | TRBracket
           | TTilde
           | TVar String
           deriving (Eq, Show)

scanTokens = alexScanTokens
}