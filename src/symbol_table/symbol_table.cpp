#include <iomanip>
#include <iostream>
#include <sstream>
#include <symbol_table/symbol_table.hpp>

// Convert symbol type enum to string
std::string symbolTypeToStr(Symbol::Type type) {
    switch (type) {
        case Symbol::CHAR:
            return "char";
        case Symbol::INT:
            return "int";
        case Symbol::FLOAT:
            return "float";
        case Symbol::ARR_CHAR:
            return "char[]";
        case Symbol::ARR_INT:
            return "int[]";
        case Symbol::ARR_FLOAT:
            return "float[]";
        case Symbol::ARR_ARR_CHAR:
            return "char[][]";
        case Symbol::ARR_ARR_INT:
            return "int[][]";
        case Symbol::ARR_ARR_FLOAT:
            return "float[][]";
        case Symbol::FUNCTION_CHAR:
            return "char()";
        case Symbol::FUNCTION_INT:
            return "int()";
        case Symbol::FUNCTION_FLOAT:
            return "float()";
        default:
            return "undefined";
    }
}

// Constructor for Symbol class
Symbol::Symbol(std::string name, Symbol::Type type, SymbolTable* nestedTable) {
    this->name = name;
    this->type = type;
    this->nestedTable = nestedTable;
}

// Constructor for SymbolTable class
SymbolTable::SymbolTable(std::string name, SymbolTable* parent) {
    this->name = name;
    this->parent = parent;
}

// Destructor for SymbolTable class
SymbolTable::~SymbolTable() {
    // Free memory allocated for each symbol in the table
    for (auto& sym : table) {
        if (sym->nestedTable != nullptr) delete sym->nestedTable;
        delete sym;
    }
}

// Find a symbol in the symbol table by name
Symbol* SymbolTable::findSymbol(std::string symbolName, bool recursive) {
    for (auto& sym : table) {
        if (sym->name == symbolName) {
            return sym;
        }
    }

    if (recursive and parent != nullptr)
        return parent->findSymbol(symbolName, true);
    else
        return nullptr;
}

// Add a symbol to the symbol table
void SymbolTable::addSymbol(Symbol* sym) {
    table.push_back(sym);
}

// Print the contents of the symbol table
void SymbolTable::print() {
    // Width of each column in the table
    constexpr int WIDTH = 35;

    // Lambda function to print table separator line
    auto separator = [WIDTH] {
        const std::string line(WIDTH + 2, '-');
        for (int i = 0; i < 5; ++i) std::cout << '+' << line;
        std::cout << '+' << std::endl;
    };

    // Lambda function to check if a character is an escape character
    auto isEscapeChar = [](char c) {
        return c == '\n' or c == '\t' or c == '\r' or c == '\v' or c == '\b' or c == '\a' or c == '\f' or c == '\\' or c == '\'' or c == '\"';
    };

    // String to print the name of the symbol table
    const std::string nameBanner = "+" + std::string(WIDTH + 2, '-') + "+";
    const std::string nameLine = "| " + std::string(WIDTH + 1, ' ') + "|";

    if (table.empty()) {
        std::cout << nameBanner << std::endl;
        std::cout << "| " << std::setw(WIDTH + 1) << std::left << name << "|" << std::endl;
        std::cout << nameBanner << std::endl;
        std::cout << std::endl;
    } else {
        std::cout << nameBanner << std::endl;
        std::cout << "| " << std::setw(WIDTH + 1) << std::left << name << "|" << std::endl;

        // Print header
        separator();
        std::cout << "| " << std::setw(WIDTH + 1) << std::left << "Name"
                  << "| " << std::setw(WIDTH + 1) << std::left << "Type"
                  << "| " << std::setw(WIDTH + 1) << std::left << "Dimension(s)"
                  << "| " << std::setw(WIDTH + 1) << std::left << "Value"
                  << "| " << std::setw(WIDTH + 1) << std::left << "Nested Symbol Table"
                  << "|" << std::endl;
        separator();

        // Print each symbol
        for (auto sym : table) {
            std::cout << "| " << std::left << std::setw(WIDTH + 1) << sym->name << "| ";
            std::cout << std::left << std::setw(WIDTH + 1) << symbolTypeToStr(sym->type) << "| ";

            // Handle dimensions
            if (sym->type == Symbol::ARR_CHAR or sym->type == Symbol::ARR_INT or sym->type == Symbol::ARR_FLOAT) {
                std::cout << std::setw(WIDTH + 1) << sym->firstDimension << "| ";
            } else if (sym->type == Symbol::ARR_ARR_CHAR or sym->type == Symbol::ARR_ARR_INT or sym->type == Symbol::ARR_ARR_FLOAT) {
                const std::string dimensions = std::to_string(sym->firstDimension) + " x " + std::to_string(sym->secondDimension);
                std::cout << std::setw(WIDTH + 1) << dimensions << "| ";
            } else {
                std::cout << std::setw(WIDTH + 1) << "-"
                          << "| ";
            }

            // Print value
            std::string val;
            if (sym->val.empty()) {
                val = "-";
            } else if (sym->type == Symbol::ARR_CHAR or sym->type == Symbol::ARR_ARR_CHAR or sym->type == Symbol::ARR_INT or sym->type == Symbol::ARR_ARR_INT or sym->type == Symbol::ARR_FLOAT or sym->type == Symbol::ARR_ARR_FLOAT) {
                uint64_t pointer = std::stoull(sym->val);
                std::stringstream ss;
                ss << "0x" << std::hex << pointer;
                val = ss.str();
            } else if (sym->type == Symbol::CHAR) {
                char c = sym->val[0];
                if (!isEscapeChar(c)) {
                    val += '\'';
                    val += c;
                    val += "' (";
                    val += std::to_string(static_cast<int>(c));
                    val += ')';
                } else {
                    val += "<SPECIAL_CHAR> (";
                    val += sym->val;
                    val += ')';
                }
            } else {
                val = sym->val;
            }
            std::cout << std::left << std::setw(WIDTH + 1) << val << "| ";

            // Print nested table name
            if (sym->nestedTable) {
                std::cout << std::left << std::setw(WIDTH + 1) << sym->nestedTable->name << "| ";
            } else {
                std::cout << std::left << std::setw(WIDTH + 1) << "-"
                          << "| ";
            }
            std::cout << std::endl;
        }

        // Print footer
        separator();
        std::cout << std::endl;
    }

    // Print (functions) nested tables in current table
    for (auto& symbol : table) {
        if (symbol->nestedTable) {
            symbol->nestedTable->print();
        }
    }

    // Print for loops in current table
    for (auto& fl : forLoops) {
        fl->print();
    }

    // Print compound statements in current table
    for (auto& cs : compoundStatements) {
        cs->print();
    }
}
