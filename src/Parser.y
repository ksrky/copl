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
'<'             { Tlt }
'('             { Tlparen }
')'             { Trparen }
'if'            { Tif }
'then'          { Tthen }
'else'          { Telse }
'true'          { Ttrue }
'false'         { Tfalse }

ident           { Ident $$ }
num             { Number $$ }

%nonassoc '<'
%left '+' '-'
%left '*'

%%

Exp :: { Exp }
    : Exp '+' Exp                           { Add $1 $3 }
    | Exp '-' Exp                           { Sub $1 $3 }
    | Exp '*' Exp                           { Mul $1 $3 }
    | Exp '<' Exp                           { Less $1 $3 }
    | num                                   { I $1 }
    | 'if' Exp 'then' Exp 'else' Exp        { Ite $2 $4 $6 }
    | 'true'                                { B True }
    | 'false'                               { B False }
    | '(' Exp ')'                           { $2 }
{
parseError :: [Token] -> a
parseError ts = error $ "Parse error: " ++ show ts
}