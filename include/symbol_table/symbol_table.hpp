#ifndef _SYMBOL_TABLE_H
#define _SYMBOL_TABLE_H

#include <list>
#include <string>

// Forward declaration of SymbolTable
struct SymbolTable;

// Define a symbol struct to represent a row in the symbol table
struct Symbol {
    // Define an enum to represent the different types of symbols
    enum Type {
        UNDEFINED,            // No datatype assigned yet but will become one of CHAR, INT, FLOAT, FUNCTION_CHAR, FUNCTION_INT, FUNCTION_FLOAT
        CHAR,                 // A character
        INT,                  // An integer
        FLOAT,                // A floating point number
        ARR_UNDEFINED,        // No datatype assigned yet but will become one of ARR_CHAR, ARR_INT, ARR_FLOAT
        ARR_CHAR,             // An array of characters
        ARR_INT,              // An array of integers
        ARR_FLOAT,            // An array of floating point numbers
        ARR_ARR_UNDEFINED,    // No datatype assigned yet but will become one of ARR_ARR_CHAR, ARR_ARR_INT, ARR_ARR_FLOAT
        ARR_ARR_CHAR,         // A two-dimensional array of characters
        ARR_ARR_INT,          // A two-dimensional array of integers
        ARR_ARR_FLOAT,        // A two-dimensional array of floating point numbers
        FUNCTION_CHAR,        // A function that returns a character
        FUNCTION_INT,         // A function that returns an integer
        FUNCTION_FLOAT,       // A function that returns a floating point number
    };

    Symbol(std::string name, Type type, SymbolTable* nestedTable = nullptr);

    std::string name;            // The name of the symbol
    Type type;                   // The type of the symbol
    int firstDimension;          // The size of the first dimension of the symbol (for arrays)
    int secondDimension;         // The size of the second dimension of the symbol (for two-dimensional arrays)
    std::string val;             // The value of the symbol
    SymbolTable* nestedTable;    // A pointer to a nested symbol table (if this symbol represents a function)
};

// Function to convert the symbol type to its corresponding string representation
std::string symbolTypeToStr(Symbol::Type);

// Define a symbol table struct to hold multiple symbols
struct SymbolTable {
    SymbolTable(std::string name, SymbolTable* parent = nullptr);
    ~SymbolTable();    // Destructor that frees up all the memory used by the symbol table

    // Returns a pointer to the symbol with the specified name, or nullptr if the symbol is not found
    // If recursive is specified, it will search in parent symbol table if not nullptr
    Symbol* findSymbol(std::string symbolName, bool recursive = false);

    // Adds a symbol to the symbol table
    void addSymbol(Symbol* sym);

    // Prints the contents of the symbol table in a readable format
    void print();

    std::string name;            // The name of the symbol table
    SymbolTable* parent;         // A pointer to the parent symbol table (if this is a nested symbol table)
    std::list<Symbol*> table;    // The list of symbols in the symbol table

    std::list<SymbolTable*> forLoops;              // The list of for loops in the symbol table
    std::list<SymbolTable*> compoundStatements;    // The list of compound statements in the symbol table
};

#endif    // _SYMBOL_TABLE_H
