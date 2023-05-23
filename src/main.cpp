#include <ast/ast.hpp>
#include <fstream>
#include <iostream>

// Global variables that act as compiler flags
constexpr bool genThreeAddressCode = false;
constexpr bool executeCode = false;

// Declarations from lexer (flex)
extern FILE* yyin;
extern FILE* yyout;

// Declarations from parser (bison)
extern std::ofstream bisonout;
extern int yyparse();

// Definitions
SymbolTable* globalST;     // Global Symbol Table
SymbolTable* currentST;    // Current Symbol Table
TranslationUnit* root;     // Root of AST

int main(int argc, char** argv) {
    std::string filename;

    if (argc != 2) {
        std::cout << "Usage: ./mcc <C-file>" << std::endl;
        return -1;
    } else {
        filename = argv[1];
    }

    yyin = fopen(filename.c_str(), "r");
    if (yyin == NULL) {
        std::cout << "Cannot open file: " << filename << std::endl;
        return -1;
    }
    yyout = fopen("flex.out", "w");
    bisonout = std::ofstream("bison.out");

    globalST = new SymbolTable("global");
    currentST = globalST;

    yyparse();

    fclose(yyin);
    fclose(yyout);
    bisonout.close();

    if (genThreeAddressCode) {
        QuadArray tac = root->genCode();
        tac.backPatch();
        tac.print();
    } else if (executeCode) {
        root->execute();
    } else {
        std::cout << "Code is semantically correct." << std::endl << std::endl;
        std::string ast;
        root->print(ast);
        std::cout << ast << std::endl;
    }
}
