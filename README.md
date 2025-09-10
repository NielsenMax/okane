# Okane
## AST

N ::= n% (numeros entre 1 y 100)
    | n/n (n numeros enteros)

C ::= [ STRING ] (STRING es una cadena de caracteres en mayusculas)

S' ::= @x | { S }

Sx ::= S | remaining from S'

S ::= @x
    | N from S'
    | max n from S' (n es un numero entero)
    | S, Sx

D' ::= @x | { D }
 
Dx ::= D | remaining to D'

D ::= @x
    | N to D'
    | D, Dx

A' ::= \epsilon | A

A ::= A A'
    | C
    | S'
    | D'

Ty ::= Src | Dst | Coin

Px ::= \epsilon 
     | string: Ty

P' ::= Px
     | ( P )

P ::= string: Ty
    | P, Px

T ::= send S' C D' 
    | fun string P' { TS } 
    | trx { TS }
    | run string A'

TS ::= T
     | T; TS