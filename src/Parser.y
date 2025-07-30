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
'='             { Tequal }
'('             { Tlparen }
')'             { Trparen }
'['             { Tlbracket }
']'             { Trbracket }
','             { Tcomma }
'.'             { Tdot }
'|-'            { Tvdash }
'->'            { Tarrow }
'#'             { Tsharp }
'if'            { Tif }
'then'          { Tthen }
'else'          { Telse }
'true'          { Ttrue }
'false'         { Tfalse }
'let'           { Tlet }
'in'            { Tin }
'rec'           { Trec }
'fun'           { Tfun }

ident           { Ident $$ }
num             { Number $$ }

%nonassoc '<'
%left '+' '-'
%left '*'

%%

Query :: { (Env, Exp) }
    : Env '|-' Exp                          { ($1, $3) }

Env :: { Env }
    : {- empty -}                           { Empty }
    | Env1                                  { $1 }

Env1 :: { Env }
    : Val                         { Snoc Empty $1 }
    | Env1 ',' Val                { Snoc $1 $3 }

Val :: { Val }
    : num                                   { VInt $1 }
    | 'true'                                { VBool True }
    | 'false'                               { VBool False }
    | '(' Env ')' '[' 'fun' '.' '=' Exp ']'
                                            { VClos $2 $8 }
    | '(' Env ')' '[' 'rec' '.' '=' 'fun' '.' '=' Exp ']'
                                            { VFix $2 $11 }

Exp :: { Exp }
    : Exp '+' Exp                           { Add $1 $3 }
    | Exp '-' Exp                           { Sub $1 $3 }
    | Exp '*' Exp                           { Mul $1 $3 }
    | Exp '<' Exp                           { Less $1 $3 }
    | 'if' Exp 'then' Exp 'else' Exp        { Ite $2 $4 $6 }
    | 'let' '.' '=' Exp 'in' Exp          { Let $4 $6 }
    | 'fun' '.' '->' Exp                  { Fun $4 }
    | 'let' 'rec' '.' '=' 'fun' '.' '->' Exp 'in' Exp
                                            { Letrec $8 $10 }
    | Exp2                                  { $1 }

Exp2 :: { Exp }
    : Exp2 Exp1                             { App $1 $2 }
    | Exp1                                  { $1 }

Exp1 :: { Exp }
    : num                                   { I $1 }
    | 'true'                                { B True }
    | 'false'                               { B False }
    | '#' num                                 { V $2 }
    | '(' Exp ')'                           { $2 }
{
parseError :: [Token] -> a
parseError ts = error $ "Parse error: " ++ show ts
}