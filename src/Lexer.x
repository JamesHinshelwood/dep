{
module Lexer (
    scanTokens,
    Token(..)
) where

import Syntax
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
    "let"                           { \s -> TLet }
    "in"                            { \s -> TIn }
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
           | TLet
           | TIn
           | TVar String
           deriving (Eq, Show)

scanTokens = alexScanTokens
}