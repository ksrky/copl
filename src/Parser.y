{
module Parser where

import Lexer
import Syntax
}

%name parse
%tokentype { Token }
%error { parseError }

%token

'+'             { Tplus }
'-'             { Tminus }   
'*'             { Ttimes }
'('             { Tlparen }
')'             { Trparen }
'Z'             { TZ }
'S'             { TS }
'evalto'        { Tevalto }

ident           { Ident $$ }
num             { Number $$ }

%left '+' '-'
%left '*'

%%

Query :: { (Exp, Nat) }
    : Exp 'evalto' Nat                      { ($1, $3) }

Exp :: { Exp }
    : Exp '+' Exp                           { Add $1 $3 }
    | Exp '*' Exp                           { Mul $1 $3 }
    | Nat                                   { N $1 }
    | '(' Exp ')'                           { $2 }

Nat :: { Nat }
    : 'Z'                                   { Z }
    | 'S' '(' Nat ')'                       { S $3 }
{
parseError :: [Token] -> a
parseError ts = error $ "Parse error: " ++ show ts
}