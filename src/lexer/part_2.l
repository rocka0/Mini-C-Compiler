%option noyywrap
%option yylineno

%{
    #include "y.tab.h"
%}

lowercase [a-z]
uppercase [A-Z]
digit [0-9]
digits {digit}+
letter {lowercase}|{uppercase}
alphanumeric {letter}|{digit}
whitespace [ \t\n\v\b\r\f\a]
whitespaces {whitespace}*

%x MULTI_LINE_COMMENT

%%

{whitespaces} {
    // Ignore whitespaces
}

[#]include.* {
    // Ignore preprocessor includes
    fprintf(yyout, "%d\t\t\t%s\n", yylineno, "[PREPROCESSOR DIRECTIVE]");
}

"//".* {
    // Ignore single line comments
    fprintf(yyout, "%d\t\t\t%s\n", yylineno, "[SINGLE LINE COMMENT]");
}

<INITIAL>"/*" {
    // Begin ignore multi line comments
    fprintf(yyout, "%d\t\t\t%s\n", yylineno, "[MULTI LINE COMMENT START]");
    BEGIN(MULTI_LINE_COMMENT);
}

<MULTI_LINE_COMMENT>"*/" {
    // End ignore multi line comments
    fprintf(yyout, "%d\t\t\t%s\n", yylineno, "[MULTI LINE COMMENT END]");
    BEGIN(INITIAL);
}

<MULTI_LINE_COMMENT>[^*\n]+ {
    // eat comment in chunks
}

<MULTI_LINE_COMMENT>"*" {
    // eat the lone star
}

<MULTI_LINE_COMMENT>\n {}

[']([a-zA-Z0-9$&+,:;=?@#|"<>.^*()%!-]|(\\[ntvbrfa\?'"]))['] {
    // Character constants
    fprintf(yyout, "%d\t\t\t%s\t\t\t%s\n", yylineno, yytext, "[T_CONSTANT_CHAR]");
    return T_CONSTANT_CHAR;
}

(("."{digits})|({digits}".")|({digits}"."{digits}))([eE][+-]?{digits})? {
    // Floating constant with a .
    fprintf(yyout, "%d\t\t\t%s\t\t\t%s\n", yylineno, yytext, "[T_CONSTANT_FLOAT]");
    return T_CONSTANT_FLOAT;
}

{digits}([eE][+-]?{digits}) {
    // Floating constant without a .
    fprintf(yyout, "%d\t\t\t%s\t\t\t%s\n", yylineno, yytext, "[T_CONSTANT_FLOAT]");
    return T_CONSTANT_FLOAT;
}

{digits} {
    // Integer constants
    fprintf(yyout, "%d\t\t\t%s\t\t\t%s\n", yylineno, yytext, "[T_CONSTANT_INT]");
    return T_CONSTANT_INT;
}

["].*["] {
    // String constants
    fprintf(yyout, "%d\t\t\t%s\t\t\t%s\n", yylineno, yytext, "[T_CONSTANT_STRING]");
    return T_CONSTANT_STRING;
}

break {
    fprintf(yyout, "%d\t\t\t%s\t\t\t%s\n", yylineno, yytext, "[T_KEYWORD_BREAK]");
    return T_KEYWORD_BREAK;
}

case {
    fprintf(yyout, "%d\t\t\t%s\t\t\t%s\n", yylineno, yytext, "[T_KEYWORD_CASE]");
    return T_KEYWORD_CASE;
}

char {
    fprintf(yyout, "%d\t\t\t%s\t\t\t%s\n", yylineno, yytext, "[T_KEYWORD_CHAR]");
    return T_KEYWORD_CHAR;
}

continue {
    fprintf(yyout, "%d\t\t\t%s\t\t\t%s\n", yylineno, yytext, "[T_KEYWORD_CONTINUE]");
    return T_KEYWORD_CONTINUE;
}

default {
    fprintf(yyout, "%d\t\t\t%s\t\t\t%s\n", yylineno, yytext, "[T_KEYWORD_DEFAULT]");
    return T_KEYWORD_DEFAULT;
}

else {
    fprintf(yyout, "%d\t\t\t%s\t\t\t%s\n", yylineno, yytext, "[T_KEYWORD_ELSE]");
    return T_KEYWORD_ELSE;
}

float {
    fprintf(yyout, "%d\t\t\t%s\t\t\t%s\n", yylineno, yytext, "[T_KEYWORD_FLOAT]");
    return T_KEYWORD_FLOAT;
}

for {
    fprintf(yyout, "%d\t\t\t%s\t\t\t%s\n", yylineno, yytext, "[T_KEYWORD_FOR]");
    return T_KEYWORD_FOR;
}

if {
    fprintf(yyout, "%d\t\t\t%s\t\t\t%s\n", yylineno, yytext, "[T_KEYWORD_IF]");
    return T_KEYWORD_IF;
}

int {
    fprintf(yyout, "%d\t\t\t%s\t\t\t%s\n", yylineno, yytext, "[T_KEYWORD_INT]");
    return T_KEYWORD_INT;
}

printf {
    fprintf(yyout, "%d\t\t\t%s\t\t\t%s\n", yylineno, yytext, "[T_KEYWORD_PRINTF]");
    return T_KEYWORD_PRINTF;
}

return {
    fprintf(yyout, "%d\t\t\t%s\t\t\t%s\n", yylineno, yytext, "[T_KEYWORD_RETURN]");
    return T_KEYWORD_RETURN;
}

switch {
    fprintf(yyout, "%d\t\t\t%s\t\t\t%s\n", yylineno, yytext, "[T_KEYWORD_SWITCH]");
    return T_KEYWORD_SWITCH;
}

while {
    fprintf(yyout, "%d\t\t\t%s\t\t\t%s\n", yylineno, yytext, "[T_KEYWORD_WHILE]");
    return T_KEYWORD_WHILE;
}

(_|{letter})(_|{alphanumeric})* {
    // Returns identifiers
    fprintf(yyout, "%d\t\t\t%s\t\t\t%s\n", yylineno, yytext, "[T_IDENTIFIER]");
    return T_IDENTIFIER;
}

"/" {
    fprintf(yyout, "%d\t\t\t%s\t\t\t%s\n", yylineno, yytext, "[T_OP_DIV]");
    return T_OP_DIV;
}

"&&" {
    fprintf(yyout, "%d\t\t\t%s\t\t\t%s\n", yylineno, yytext, "[T_OP_DOUBLE_AND]");
    return T_OP_DOUBLE_AND;
}

"==" {
    fprintf(yyout, "%d\t\t\t%s\t\t\t%s\n", yylineno, yytext, "[T_OP_DOUBLE_EQ]");
    return T_OP_DOUBLE_EQ;
}

"||" {
    fprintf(yyout, "%d\t\t\t%s\t\t\t%s\n", yylineno, yytext, "[T_OP_DOUBLE_OR]");
    return T_OP_DOUBLE_OR;
}

"=" {
    fprintf(yyout, "%d\t\t\t%s\t\t\t%s\n", yylineno, yytext, "[T_OP_EQ]");
    return T_OP_EQ;
}

"!" {
    fprintf(yyout, "%d\t\t\t%s\t\t\t%s\n", yylineno, yytext, "[T_OP_EXCLAMATION]");
    return T_OP_EXCLAMATION;
}

">=" {
    fprintf(yyout, "%d\t\t\t%s\t\t\t%s\n", yylineno, yytext, "[T_OP_GEQ]");
    return T_OP_GEQ;
}

">" {
    fprintf(yyout, "%d\t\t\t%s\t\t\t%s\n", yylineno, yytext, "[T_OP_GT]");
    return T_OP_GT;
}

"<=" {
    fprintf(yyout, "%d\t\t\t%s\t\t\t%s\n", yylineno, yytext, "[T_OP_LEQ]");
    return T_OP_LEQ;
}

"<" {
    fprintf(yyout, "%d\t\t\t%s\t\t\t%s\n", yylineno, yytext, "[T_OP_LT]");
    return T_OP_LT;
}

"-" {
    fprintf(yyout, "%d\t\t\t%s\t\t\t%s\n", yylineno, yytext, "[T_OP_MINUS]");
    return T_OP_MINUS;
}

"%" {
    fprintf(yyout, "%d\t\t\t%s\t\t\t%s\n", yylineno, yytext, "[T_OP_MOD]");
    return T_OP_MOD;
}

"*" {
    fprintf(yyout, "%d\t\t\t%s\t\t\t%s\n", yylineno, yytext, "[T_OP_MUL]");
    return T_OP_MUL;
}

"!=" {
    fprintf(yyout, "%d\t\t\t%s\t\t\t%s\n", yylineno, yytext, "[T_OP_NEQ]");
    return T_OP_NEQ;
}

"+" {
    fprintf(yyout, "%d\t\t\t%s\t\t\t%s\n", yylineno, yytext, "[T_OP_PLUS]");
    return T_OP_PLUS;
}

":" {
    fprintf(yyout, "%d\t\t\t%s\t\t\t%s\n", yylineno, yytext, "[T_COLON]");
    return T_COLON;
}

"," {
    fprintf(yyout, "%d\t\t\t%s\t\t\t%s\n", yylineno, yytext, "[T_COMMA]");
    return T_COMMA;
}

"{" {
    fprintf(yyout, "%d\t\t\t%s\t\t\t%s\n", yylineno, yytext, "[T_LBRACE]");
    return T_LBRACE;
}

"[" {
    fprintf(yyout, "%d\t\t\t%s\t\t\t%s\n", yylineno, yytext, "[T_LBRACKET]");
    return T_LBRACKET;
}

"(" {
    fprintf(yyout, "%d\t\t\t%s\t\t\t%s\n", yylineno, yytext, "[T_LPARAN]");
    return T_LPARAN;
}

"}" {
    fprintf(yyout, "%d\t\t\t%s\t\t\t%s\n", yylineno, yytext, "[T_RBRACE]");
    return T_RBRACE;
}

"]" {
    fprintf(yyout, "%d\t\t\t%s\t\t\t%s\n", yylineno, yytext, "[T_RBRACKET]");
    return T_RBRACKET;
}

")" {
    fprintf(yyout, "%d\t\t\t%s\t\t\t%s\n", yylineno, yytext, "[T_RPARAN]");
    return T_RPARAN;
}

";" {
    fprintf(yyout, "%d\t\t\t%s\t\t\t%s\n", yylineno, yytext, "[T_SEMICOLON]");
    return T_SEMICOLON;
}

. {
    // Caught invalid token
    fprintf(yyout, "%d\t\t\t%s\t\t\t%s\n", yylineno, yytext, "[BAD_TOKEN]");
    printf("[BAD_TOKEN] -> %s on Line: %d\n", yytext, yylineno);
    exit(-1);
}

%%
