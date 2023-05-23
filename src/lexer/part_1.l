%option noyywrap
%option yylineno

%{
    #include <stdio.h>
    #include <stdlib.h>
    #include <string.h>
    char formatString[1000];
    int args = 0;
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
%x PRINTF_EXPECT_OPENING_BRACKET
%x PRINTF_EXPECT_STRING
%x PRINTF_SEEN_STRING
%x PRINTF_EXPECT_ARGS
%x PRINTF_EXPECT_CLOSING_BRACKET
%x PRINTF_END
%x PRINTF_INVALID

%%

{whitespaces} {
    // Ignore whitespaces
}

[#]include.* {
    // Ignore preprocessor includes
    fprintf(yyout, "%d\t\t\t\t\t\t%s\n", yylineno, "[PREPROCESSOR DIRECTIVE]");
}

"//".* {
    // Ignore single line comments
    fprintf(yyout, "%d\t\t\t\t\t\t%s\n", yylineno, "[SINGLE LINE COMMENT]");
}

<INITIAL>"/*" {
    // Begin ignore multi line comments
    fprintf(yyout, "%d\t\t\t\t\t\t%s\n", yylineno, "[MULTI LINE COMMENT START]");
    BEGIN(MULTI_LINE_COMMENT);
}

<MULTI_LINE_COMMENT>"*/" {
    // End ignore multi line comments
    fprintf(yyout, "%d\t\t\t\t\t\t%s\n", yylineno, "[MULTI LINE COMMENT END]");
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
}

(("."{digits})|({digits}".")|({digits}"."{digits}))([eE][+-]?{digits})? {
    // Floating constant with a .
    fprintf(yyout, "%d\t\t\t%s\t\t\t%s\n", yylineno, yytext, "[T_CONSTANT_FLOAT]");
}

{digits}([eE][+-]?{digits}) {
    // Floating constant without a .
    fprintf(yyout, "%d\t\t\t%s\t\t\t%s\n", yylineno, yytext, "[T_CONSTANT_FLOAT]");
}

{digits} {
    // Integer constants
    fprintf(yyout, "%d\t\t\t%s\t\t\t%s\n", yylineno, yytext, "[T_CONSTANT_INT]");
}

["].*["] {
    // String constants
    fprintf(yyout, "%d\t\t\t%s\t\t\t%s\n", yylineno, yytext, "[T_CONSTANT_STRING]");
}

break {
    fprintf(yyout, "%d\t\t\t%s\t\t\t%s\n", yylineno, yytext, "[T_KEYWORD_BREAK]");
}

case {
    fprintf(yyout, "%d\t\t\t%s\t\t\t%s\n", yylineno, yytext, "[T_KEYWORD_CASE]");
}

char {
    fprintf(yyout, "%d\t\t\t%s\t\t\t%s\n", yylineno, yytext, "[T_KEYWORD_CHAR]");
}

continue {
    fprintf(yyout, "%d\t\t\t%s\t\t\t%s\n", yylineno, yytext, "[T_KEYWORD_CONTINUE]");
}

default {
    fprintf(yyout, "%d\t\t\t%s\t\t\t%s\n", yylineno, yytext, "[T_KEYWORD_DEFAULT]");
}

else {
    fprintf(yyout, "%d\t\t\t%s\t\t\t%s\n", yylineno, yytext, "[T_KEYWORD_ELSE]");
}

float {
    fprintf(yyout, "%d\t\t\t%s\t\t\t%s\n", yylineno, yytext, "[T_KEYWORD_FLOAT]");
}

for {
    fprintf(yyout, "%d\t\t\t%s\t\t\t%s\n", yylineno, yytext, "[T_KEYWORD_FOR]");
}

if {
    fprintf(yyout, "%d\t\t\t%s\t\t\t%s\n", yylineno, yytext, "[T_KEYWORD_IF]");
}

int {
    fprintf(yyout, "%d\t\t\t%s\t\t\t%s\n", yylineno, yytext, "[T_KEYWORD_INT]");
}

printf {
    args = 0;
    BEGIN(PRINTF_EXPECT_OPENING_BRACKET);
}

<PRINTF_EXPECT_OPENING_BRACKET>{whitespaces}[(] {
    BEGIN(PRINTF_EXPECT_STRING);
}

<PRINTF_EXPECT_OPENING_BRACKET>. {
    BEGIN(PRINTF_INVALID);
}

<PRINTF_EXPECT_STRING>{whitespaces}["].*["] {
    strcpy(formatString, yytext);
    BEGIN(PRINTF_SEEN_STRING);
}

<PRINTF_EXPECT_STRING>. {
    BEGIN(PRINTF_INVALID);
}

<PRINTF_SEEN_STRING>{whitespaces}[)] {
    BEGIN(PRINTF_END);
}

<PRINTF_SEEN_STRING>{whitespaces}[,] {
    BEGIN(PRINTF_EXPECT_ARGS);
}

<PRINTF_SEEN_STRING>. {
    BEGIN(PRINTF_INVALID);
}

<PRINTF_EXPECT_ARGS>{whitespaces}(_|{letter})(_|{alphanumeric})*[,] {
    ++args;
}

<PRINTF_EXPECT_ARGS>{whitespaces}(_|{letter})(_|{alphanumeric})* {
    ++args;
    BEGIN(PRINTF_EXPECT_CLOSING_BRACKET);
}

<PRINTF_EXPECT_ARGS>. {
    BEGIN(PRINTF_INVALID);
}

<PRINTF_EXPECT_CLOSING_BRACKET>{whitespaces}[)] {
    BEGIN(PRINTF_END);
}

<PRINTF_EXPECT_CLOSING_BRACKET>. {
    BEGIN(PRINTF_INVALID);
}

<PRINTF_END>{whitespaces}[;] {
    int n = strlen(formatString);

    // Check if number of % matches number of arguments
    int seen = 0;

    for(int i = 0; i < n; ++i) {
        if(formatString[i] == '%') {
            ++seen;
        }
    }

    if(seen == args) {
        fprintf(yyout, "%d\t\t\t\t\t\t%s\n", yylineno, "[VALID PRINTF STATEMENT]");
    }
    else {
        fprintf(yyout, "%d\t\t\t\t\t\t%s\n", yylineno, "[INVALID PRINTF STATEMENT]");
    }

    BEGIN(INITIAL);
}

<PRINTF_END>. {
    BEGIN(PRINTF_INVALID);
}

<PRINTF_INVALID>. {
    // Printf is invalid, eat characters until newline
}

<PRINTF_INVALID>\n {
    // End of invalid printf
    fprintf(yyout, "%d\t\t\t\t\t\t%s\n", yylineno-1, "[INVALID PRINTF STATEMENT]");
    BEGIN(INITIAL);
}

return {
    fprintf(yyout, "%d\t\t\t%s\t\t\t%s\n", yylineno, yytext, "[T_KEYWORD_RETURN]");
}

switch {
    fprintf(yyout, "%d\t\t\t%s\t\t\t%s\n", yylineno, yytext, "[T_KEYWORD_SWITCH]");
}

while {
    fprintf(yyout, "%d\t\t\t%s\t\t\t%s\n", yylineno, yytext, "[T_KEYWORD_WHILE]");
}

(_|{letter})(_|{alphanumeric})* {
    fprintf(yyout, "%d\t\t\t%s\t\t\t%s\n", yylineno, yytext, "[T_IDENTIFIER]");
}

"/" {
    fprintf(yyout, "%d\t\t\t%s\t\t\t%s\n", yylineno, yytext, "[T_OP_DIV]");
}

"&&" {
    fprintf(yyout, "%d\t\t\t%s\t\t\t%s\n", yylineno, yytext, "[T_OP_DOUBLE_AND]");
}

"==" {
    fprintf(yyout, "%d\t\t\t%s\t\t\t%s\n", yylineno, yytext, "[T_OP_DOUBLE_EQ]");
}

"||" {
    fprintf(yyout, "%d\t\t\t%s\t\t\t%s\n", yylineno, yytext, "[T_OP_DOUBLE_OR]");
}

"=" {
    fprintf(yyout, "%d\t\t\t%s\t\t\t%s\n", yylineno, yytext, "[T_OP_EQ]");
}

"!" {
    fprintf(yyout, "%d\t\t\t%s\t\t\t%s\n", yylineno, yytext, "[T_OP_EXCLAMATION]");
}

">=" {
    fprintf(yyout, "%d\t\t\t%s\t\t\t%s\n", yylineno, yytext, "[T_OP_GEQ]");
}

">" {
    fprintf(yyout, "%d\t\t\t%s\t\t\t%s\n", yylineno, yytext, "[T_OP_GT]");
}

"<=" {
    fprintf(yyout, "%d\t\t\t%s\t\t\t%s\n", yylineno, yytext, "[T_OP_LEQ]");
}

"<" {
    fprintf(yyout, "%d\t\t\t%s\t\t\t%s\n", yylineno, yytext, "[T_OP_LT]");
}

"-" {
    fprintf(yyout, "%d\t\t\t%s\t\t\t%s\n", yylineno, yytext, "[T_OP_MINUS]");
}

"%" {
    fprintf(yyout, "%d\t\t\t%s\t\t\t%s\n", yylineno, yytext, "[T_OP_MOD]");
}

"*" {
    fprintf(yyout, "%d\t\t\t%s\t\t\t%s\n", yylineno, yytext, "[T_OP_MUL]");
}

"!=" {
    fprintf(yyout, "%d\t\t\t%s\t\t\t%s\n", yylineno, yytext, "[T_OP_NEQ]");
}

"+" {
    fprintf(yyout, "%d\t\t\t%s\t\t\t%s\n", yylineno, yytext, "[T_OP_PLUS]");
}

":" {
    fprintf(yyout, "%d\t\t\t%s\t\t\t%s\n", yylineno, yytext, "[T_COLON]");
}

"," {
    fprintf(yyout, "%d\t\t\t%s\t\t\t%s\n", yylineno, yytext, "[T_COMMA]");
}

"{" {
    fprintf(yyout, "%d\t\t\t%s\t\t\t%s\n", yylineno, yytext, "[T_LBRACE]");
}

"[" {
    fprintf(yyout, "%d\t\t\t%s\t\t\t%s\n", yylineno, yytext, "[T_LBRACKET]");
}

"(" {
    fprintf(yyout, "%d\t\t\t%s\t\t\t%s\n", yylineno, yytext, "[T_LPARAN]");
}

"}" {
    fprintf(yyout, "%d\t\t\t%s\t\t\t%s\n", yylineno, yytext, "[T_RBRACE]");
}

"]" {
    fprintf(yyout, "%d\t\t\t%s\t\t\t%s\n", yylineno, yytext, "[T_RBRACKET]");
}

")" {
    fprintf(yyout, "%d\t\t\t%s\t\t\t%s\n", yylineno, yytext, "[T_RPARAN]");
}

";" {
    fprintf(yyout, "%d\t\t\t%s\t\t\t%s\n", yylineno, yytext, "[T_SEMICOLON]");
}

. {
    // Caught invalid token
    fprintf(yyout, "%d\t\t\t%s\t\t\t%s\n", yylineno, yytext, "[BAD_CHAR]");
    printf("[BAD_CHAR] -> %s on Line: %d\n", yytext, yylineno);
    exit(-1);
}

%%

int main(int argc, char** argv) {
    if (argc != 2) {
        printf("Usage: ./lexer <input_file>\n");
        exit(-1);
    }

    yyin = fopen(argv[1], "r");
    if(yyin == NULL) {
        printf("Cannot open file: %s\n", argv[1]);
        exit(-1);
    }

    yyout = fopen("flex.out", "w");

    yylex();

    fclose(yyin);
    fclose(yyout);
}
