{
module Lexer
    ( Token (..)
    , alexScanTokens
    ) where
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]

@decimal = $digit+
@ident = $alpha [$alpha $digit \_ \']*

tokens :-

$white+                     ;
\+                          { \_ -> Tplus }
\-                          { \_ -> Tminus }
\*                          { \_ -> Ttimes }
\(                          { \_ -> Tlparen }
\)                          { \_ -> Trparen }
Z                           { \_ -> TZ }
S                           { \_ -> TS }
evalto                      { \_ -> Tevalto }
@ident                      { Ident }
@decimal                    { Number . read }

{
data Token
    = Tplus
    | Tminus
    | Ttimes
    | Tequal
    | Tlparen
    | Trparen
    | TZ
    | TS
    | Tevalto
    | Tby
    | Ident String
    | Number Integer
    deriving (Eq, Show)
}