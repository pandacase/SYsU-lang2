lexer grammar SYsULexer;

Int : 'int';
Void : 'void';
Float : 'float';
Double : 'double';
Char : 'char';

Return : 'return';
Const: 'const';
If: 'if';
Else: 'else';
While: 'while';
Break: 'break';
Continue: 'continue';
Switch: 'switch';

LeftParen : '(';
RightParen : ')';
LeftBracket : '[';
RightBracket : ']';
LeftBrace : '{';
RightBrace : '}';

Semi : ';';
Comma : ',';

Equal : '=';

Plus : '+';
Plusequal : '+=';
Minus : '-';
Minusequal : '-=';
Star : '*';
Starequal : '*=';
Slash : '/';
Slashequal : '/=';
Percent : '%';
Percentequal : '%=';

Greater : '>';
Greaterequal : '>=';
Less : '<';
Lessequal : '<=';

Exclaim : '!';
Equalequal : '==';
Exclaimequal : '!=';

Pipe : '|';
Pipepipe : '||';
Amp : '&';
Ampamp : '&&';

Identifier
    :   IdentifierNondigit
        (   IdentifierNondigit
        |   Digit
        )*
    ;

fragment
IdentifierNondigit
    :   Nondigit
    ;

fragment
Nondigit
    :   [a-zA-Z_]
    ;

fragment
Digit
    :   [0-9]
    ;

Constant
    :   IntegerConstant
    ;

fragment
IntegerConstant
    :   DecimalConstant
    |   OctalConstant
    |   HexConstant
    ;

fragment
DecimalConstant
    :   NonzeroDigit Digit*
    ;

fragment
OctalConstant
    :   '0' OctalDigit*
    ;

fragment
HexConstant
    :   '0x' HexDigit*
    ;

fragment
NonzeroDigit
    :   [1-9]
    ;

fragment
OctalDigit
    :   [0-7]
    ;

fragment
HexDigit
    :   [0-9a-f]
    ;

// HIDDEN channel:

LineAfterPreprocessing
    :   '#' Whitespace* ~[\r\n]*
        -> channel(HIDDEN)
    ;

Whitespace
    :   [ \t]+
        -> channel(HIDDEN)
    ;

Newline
    :   (   '\r' '\n'?
        |   '\n'
        )
        -> channel(HIDDEN)
    ;

