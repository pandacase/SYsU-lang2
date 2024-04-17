parser grammar SYsUParser;

options {
  tokenVocab=SYsULexer;
}

// ctx
primaryExpression
    :   Identifier
    |   Constant
    ;

// ctx
postfixExpression
    :   primaryExpression  
    ;

// ctx
unaryExpression
    :
    (   postfixExpression
    |   unaryOperator unaryExpression
    )
    ;

unaryOperator
    :   Plus | Minus
    ;

// additiveExpression
//     :   unaryExpression ((Plus|Minus) unaryExpression)*
//     ;

// ctx
multiplicativeExpression
    :   unaryExpression ((Star | Slash | Percent) unaryExpression)*
    ;

// ctx
additiveExpression
    :   multiplicativeExpression ((Plus | Minus) multiplicativeExpression)*
    ;

// ctx
assignmentExpression
    :   additiveExpression
    |   unaryExpression Equal assignmentExpression
    ;

// ctx
expression
    :   assignmentExpression (Comma assignmentExpression)*
    ;

// ctx
declaration
    :   declarationSpecQuals 
        initDeclaratorList? Semi
    ;

// declarationQualifiers
//     :   declarationQualifier*
//     ;


// ctx
declarationSpecQuals
    :   declarationQualifier*
        declarationSpecifier+
    ;

declarationSpecifier
    :   typeSpecifier
    ;

declarationQualifier
    :   typeQualifier
    ;


initDeclaratorList
    :   initDeclarator (Comma initDeclarator)*
    ;

// ctx
initDeclarator
    :   declarator (Equal initializer)?
    ;


typeSpecifier
    :   Int
    ;

typeQualifier
    :   Const
    ;

// ctx
declarator
    :   directDeclarator
    ;

// ctx
directDeclarator
    :   Identifier
    |   directDeclarator LeftBracket assignmentExpression? RightBracket
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

// ctx
statement
    :   compoundStatement
    |   expressionStatement
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
    :   (Return expression?)
    Semi
    ;

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
    : declarationSpecQuals directDeclarator LeftParen RightParen compoundStatement
    ;

