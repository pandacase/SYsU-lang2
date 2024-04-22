parser grammar SYsUParser;

options {
  tokenVocab=SYsULexer;
}

//////////////////////////////////////////////////////////////////////////////
//! Expression
//////////////////////////////////////////////////////////////////////////////

// ctx
primaryExpression
    :   Identifier
    |   Constant
    |   LeftParen expression RightParen
    ;

// ctx
postfixExpression
    :   primaryExpression  
    //  array subscript expr
    |   postfixExpression LeftBracket expression RightBracket
    // function call expr
    |   postfixExpression LeftParen (expression)? RightParen
    ;

// ctx
unaryExpression
    :   postfixExpression
    |   unaryOperator unaryExpression
    ;

unaryOperator
    :   Plus
    |   Minus
    |   Exclaim
    ;

// ctx
multiplicativeExpression
    :   unaryExpression ((Star | Slash | Percent) unaryExpression)*
    ;

// ctx
additiveExpression
    :   multiplicativeExpression ((Plus | Minus) multiplicativeExpression)*
    ;

// ctx
relationalExpression
    :   additiveExpression ((Less | Greater | Lessequal | Greaterequal) additiveExpression)*
    ;

// ctx
equalityExpression
    :   relationalExpression ((Equalequal | Exclaimequal) relationalExpression)*
    ;

// ctx
logicalAndExpression
    :   equalityExpression (Ampamp equalityExpression)*
    ;

// ctx
logicalOrExpression
    :   logicalAndExpression (Pipepipe logicalAndExpression)*
    ;


// ctx
assignmentExpression
    :   logicalOrExpression
    |   unaryExpression Equal assignmentExpression
    ;

// ctx
expression
    :   assignmentExpression (Comma assignmentExpression)*
    ;


//////////////////////////////////////////////////////////////////////////////
//! Declaration
//////////////////////////////////////////////////////////////////////////////

// ctx
declaration
    :   declarationSpecifiers initDeclaratorList? Semi
    ;

// ctx
declarationSpecifiers
    :   declarationSpecifier+
    ;

declarationSpecifier
    :   typeSpecifier
    |   typeQualifier
    ;

typeSpecifier
    :   Void
    |   Char
    |   Int
    |   Long
    ;

typeQualifier
    :   Const
    ;

initDeclaratorList
    :   initDeclarator (Comma initDeclarator)*
    ;

// ctx
initDeclarator
    :   declarator (Equal initializer)?
    ;

// ctx
declarator
    :   directDeclarator
    ;

// ctx
directDeclarator
    :   Identifier
    // array decl
    |   directDeclarator LeftBracket assignmentExpression? RightBracket
    // function decl
    |   directDeclarator LeftParen parameterList? RightParen
    ;


parameterList
    :   parameterDeclaration (Comma parameterDeclaration)*
    ;

// ctx
parameterDeclaration
    :   declarationSpecifiers directDeclarator
    ;

identifierList
    :   Identifier (Comma Identifier)*
    ;

// ctx
initializer
    :   assignmentExpression
    |   LeftBrace initializerList? Comma? RightBrace
    ;

initializerList
    // :   designation? initializer (Comma designation? initializer)*
    :   initializer (Comma initializer)*
    ;


//////////////////////////////////////////////////////////////////////////////
//! Statement
//////////////////////////////////////////////////////////////////////////////

// ctx
statement
    :   compoundStatement
    |   expressionStatement
    |   ifStatement
    |   iterationStatement
    |   jumpStatement
    ;

// ctx
compoundStatement
    :   LeftBrace blockItemList? RightBrace
    ;

blockItemList
    :   blockItem+
    ;

blockItem
    :   statement
    |   declaration
    ;

// ctx
expressionStatement
    :   expression? Semi
    ;


// ctx
jumpStatement
    :
    (   Break
    |   Continue
    |   Return expression?
    )
    Semi
    ;


// ctx
ifStatement
    :   If LeftParen expression RightParen statement (Else statement)?
    ;

// ctx
iterationStatement
    :   While LeftParen expression RightParen statement
    ;

//////////////////////////////////////////////////////////////////////////////
//! Top
//////////////////////////////////////////////////////////////////////////////

compilationUnit
    :   translationUnit? EOF
    ;

// ctx
translationUnit
    :   externalDeclaration+
    ;

externalDeclaration
    :   functionDefinition
    |   declaration
    ;

// ctx
functionDefinition
    :   declarationSpecifiers initDeclarator compoundStatement
    ;
