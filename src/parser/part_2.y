%code {
    #include <stdio.h>
    #include <stdlib.h>

    // Declarations from lexer (flex)
    extern int yylex();
    extern FILE* yyin;
    extern FILE* yyout;

    // Declarations
    int yyerror(const char* s);
}

// Constant tokens
%token T_CONSTANT_CHAR         // char
%token T_CONSTANT_INT           // int
%token T_CONSTANT_FLOAT       // float
%token T_CONSTANT_STRING     // string: char[] str = "T_CONSTANT_STRING"

// Identifier token
%token T_IDENTIFIER

// Keyword tokens
%token T_KEYWORD_BREAK      // break
%token T_KEYWORD_CASE       // case
%token T_KEYWORD_CHAR       // char
%token T_KEYWORD_CONTINUE   // continue
%token T_KEYWORD_DEFAULT    // default
%token T_KEYWORD_ELSE       // else
%token T_KEYWORD_FLOAT      // float
%token T_KEYWORD_FOR        // for
%token T_KEYWORD_IF         // if
%token T_KEYWORD_INT        // int
%token T_KEYWORD_PRINTF     // printf
%token T_KEYWORD_RETURN     // return
%token T_KEYWORD_SWITCH     // switch
%token T_KEYWORD_WHILE      // while

// Operators tokens
%token T_OP_DIV             // /
%token T_OP_DOUBLE_AND      // &&
%token T_OP_DOUBLE_EQ       // ==
%token T_OP_DOUBLE_OR       // ||
%token T_OP_EQ              // =
%token T_OP_EXCLAMATION     // !
%token T_OP_GEQ             // >=
%token T_OP_GT              // >
%token T_OP_LEQ             // <=
%token T_OP_LT              // <
%token T_OP_MINUS           // -
%token T_OP_MOD             // %
%token T_OP_MUL             // *
%token T_OP_NEQ             // !=
%token T_OP_PLUS            // +

// Separators tokens
%token T_COLON              // ;
%token T_COMMA              // ,
%token T_LBRACE             // {
%token T_LBRACKET           // [
%token T_LPARAN             // (
%token T_RPARAN             // )
%token T_RBRACKET           // ]
%token T_RBRACE             // }
%token T_SEMICOLON          // ;

%nonassoc THEN
%nonassoc T_KEYWORD_ELSE

%start translation_unit

/*
    Our Grammar for C is ADAPTED from:
    https://cs.wmich.edu/~gupta/teaching/cs4850/sumII06/The%20syntax%20of%20C%20in%20Backus-Naur%20form.htm
*/

%%

translation_unit:
    external_declaration_star
    {
        printf("valid code\n");
        return 0;
    }
;

external_declaration_star:
    external_declaration_star external_declaration
    {
    }
|
    %empty
    {
    }
;

external_declaration:
    function_definition
    {
    }
|
    declaration
    {
    }
;

/*
    remove declaration_star from function_definition -> type_specifier declarator declaration_star compound_statement
*/

function_definition:
    type_specifier declarator
    {
    }
    compound_statement
    {
    }
;

type_specifier:
    T_KEYWORD_CHAR
    {
    }
|
    T_KEYWORD_INT 
    {
    }
|
    T_KEYWORD_FLOAT
    {
    }
;

/*
    Remove declarator -> ( declarator )
    Remove declarator -> declarator ( identifier_star )
    Remove identifier_star
    Change from constant_expression to expression
    Remove initializer_list productions
    Change declarator for arrays to be only constant integer expressions
*/

declarator:
    T_IDENTIFIER 
    {
    }
|
    T_IDENTIFIER T_LBRACKET T_CONSTANT_INT T_RBRACKET 
    {
    }
|
    T_IDENTIFIER T_LBRACKET T_CONSTANT_INT T_RBRACKET T_LBRACKET T_CONSTANT_INT T_RBRACKET 
    {
    }
|
    T_IDENTIFIER T_LPARAN T_RPARAN
    {
    }
|
    T_IDENTIFIER T_LPARAN
    {
    }
    parameter_list T_RPARAN
    {
    }
;

/* 
    Bypass conditional_expression to logical_or_expression as no ternary expressions allowed 
    Remove constant_expression entirely
*/

logical_or_expression:
    logical_and_expression
    {
    }
|
    logical_or_expression T_OP_DOUBLE_OR logical_and_expression
    {
    }
;

/*
    inclusive_or_expression -> exclusive_or_expression
    exclusive_or_expression -> and_expression
    and_expression -> equality_expression
    shift_expression -> additive_expression
    unary_expression -> remove ++, --
    unary_operator -> remove &, *, ~
    postfix_expression -> postfix_expression . <identifier>
    postfix_expression -> postfix_expression -> <identifier>
    not implemented as no structs in our language
    postfix_expression -> remove ++, --
    assignment_operator -> remove *=, /=, %=, +=, -=, <<=, >>=, &=, ^=, |=
    assignment_operator -> reduce to only = [TOKEN]
*/

logical_and_expression:
    equality_expression
    {
    }
|
    logical_and_expression T_OP_DOUBLE_AND equality_expression
    {
    }
;

equality_expression:
    relational_expression
    {
    }
|
    equality_expression T_OP_DOUBLE_EQ relational_expression
    {
    }
|
    equality_expression T_OP_NEQ relational_expression
    {
    }
;

relational_expression:
    additive_expression
    {
    }
|
    relational_expression T_OP_LT additive_expression
    {
    }
|
    relational_expression T_OP_GT additive_expression
    {
    }
|
    relational_expression T_OP_LEQ additive_expression
    {
    }
|
    relational_expression T_OP_GEQ additive_expression
    {
    }
;

additive_expression:
    multiplicative_expression
    {
    }
|
    additive_expression T_OP_PLUS multiplicative_expression
    {
    }
|
    additive_expression T_OP_MINUS multiplicative_expression
    {
    }
;

multiplicative_expression:
    unary_expression
    {
    }
|
    multiplicative_expression T_OP_MUL unary_expression
    {
    }
|
    multiplicative_expression T_OP_DIV unary_expression
    {
    }
|
    multiplicative_expression T_OP_MOD unary_expression
    {
    }
;

unary_expression:
    postfix_expression
    {
    }
|
    unary_operator unary_expression
    {
    }
;

/*
    Remove assignment_expression_star
    Introduce argument_list instead of assignment_expression_star
*/

postfix_expression:
    primary_expression
    {
    }
|
    primary_expression T_LBRACKET expression T_RBRACKET
    {
    }
|
    primary_expression T_LBRACKET expression T_RBRACKET T_LBRACKET expression T_RBRACKET
    {
    }
|
    primary_expression T_LPARAN argument_list_optional T_RPARAN
    {
    }
;

argument_list_optional:
    argument_list
    {
    }
|
    %empty
    {
    }
;

argument_list:
    argument_list T_COMMA assignment_expression
    {
    }
|
    assignment_expression
    {
    }
;

/*
    Integrate <constant> into primary_expression
*/

primary_expression:
    T_IDENTIFIER
    {
    }
|
    T_CONSTANT_CHAR
    {
    }
|
    T_CONSTANT_INT
    {
    }
|
    T_CONSTANT_FLOAT
    {
    }
|
    T_CONSTANT_STRING
    {
    }
|
    T_LPARAN expression T_RPARAN
    {
    }
;

expression:
    assignment_expression
    {
    }
|
    expression T_COMMA assignment_expression
    {
    }
;

assignment_expression:
    logical_or_expression
    {
    }
|
    postfix_expression T_OP_EQ assignment_expression
    {
    }
;

unary_operator:
    T_OP_PLUS
    {
    }
|
    T_OP_MINUS
    {
    }
|
    T_OP_EXCLAMATION
    {
    }
;

parameter_list:
    parameter_declaration
    {
    }
|
    parameter_list T_COMMA parameter_declaration
    {
    }
;

/*
    Remove parameter_declaration -> type_specifier abstract_declarator
    Remove parameter_declaration -> type_specifier
    Remove abstract_declarator
*/
parameter_declaration:
    type_specifier declarator
    {
    }
;

declaration:
    type_specifier init_declarator_plus T_SEMICOLON
    {
    }
;

init_declarator_plus:
    init_declarator_plus T_COMMA init_declarator
    {
    }
|
    init_declarator
    {
    }
;

init_declarator:
    declarator
    {
    }
|
    declarator T_OP_EQ assignment_expression
    {
    }
;

/*
    Remove initializer completely
*/

compound_statement:
    T_LBRACE
    {
    }
    declaration_or_statement_star T_RBRACE
    {
    }
;

declaration_or_statement_star:
    declaration_or_statement_star declaration
    {
    }
|
    declaration_or_statement_star statement
    {
    }
|
    %empty
    {
    }
;

statement:
    labeled_statement
    {
    }
|
    expression_statement
    {
    }
|
    compound_statement
    {
    }
|
    selection_statement
    {
    }
|
    iteration_statement
    {
    }
|
    jump_statement
    {
    }
|
    printf_statement
    {
    }
;

labeled_statement:
    T_KEYWORD_CASE expression T_COLON statement
    {
    }
|
    T_KEYWORD_DEFAULT T_COLON statement
    {
    }
;

expression_statement:
    expression T_SEMICOLON
    {
    }
|
    T_SEMICOLON
    {
    }
;

selection_statement:
    T_KEYWORD_IF T_LPARAN expression T_RPARAN statement %prec THEN
    {
    }
|
    T_KEYWORD_IF T_LPARAN expression T_RPARAN statement T_KEYWORD_ELSE statement
    {
    }
|
    T_KEYWORD_SWITCH T_LPARAN expression T_RPARAN statement
    {
    }
;

iteration_statement:
    T_KEYWORD_WHILE T_LPARAN expression T_RPARAN statement
    {
    }
|
    T_KEYWORD_FOR T_LPARAN
    {
    }
    for_loop_body T_RPARAN statement
    {
    }
;

for_loop_body:
    expression_statement expression_statement
    {
    }
|
    expression_statement expression_statement expression
    {
    }
|
    declaration expression_statement
    {
    }
|
    declaration expression_statement expression
    {
    }
;

jump_statement:
    T_KEYWORD_CONTINUE T_SEMICOLON
    {
    }
|
    T_KEYWORD_BREAK T_SEMICOLON
    {
    }
|
    T_KEYWORD_RETURN expression T_SEMICOLON
    {
    }
;

printf_statement:
    T_KEYWORD_PRINTF T_LPARAN T_CONSTANT_STRING T_RPARAN T_SEMICOLON
    {
    }
|
    T_KEYWORD_PRINTF T_LPARAN T_CONSTANT_STRING T_COMMA argument_list T_RPARAN T_SEMICOLON
    {
    }
;

%%

int yyerror(const char* s) {
    printf("%s\n", s);
    exit(-1);
}

int main(int argc, char** argv) {
    if(argc != 2){
        printf("Usage: ./parser <filename>\n");
        exit(-1);
    }

    yyin = fopen(argv[1], "r");
    if(yyin == NULL) {
        printf("Cannot open file: %s\n", argv[1]);
        exit(-1);
    }

    yyout = fopen("flex.out", "w");
    
    yyparse();
}
