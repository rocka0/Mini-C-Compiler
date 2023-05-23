#ifndef _QUAD_H
#define _QUAD_H

#include <list>
#include <string>

// Struct representing a Quad in three address code
struct Quad {
    enum Operator {
        UNCONDITIONAL_JUMP,    // goto arg_1
        CONDITIONAL_JUMP,      // if arg_1 goto result
        FUNCTION_BEGIN,        // func begin <identifier>
        FUNCTION_END,          // func end
        RETURN,                // return <expression>
        CALL,                  // calls function <result> with <arg_1> arguments
        PARAM,                 // places param <arg_1> in the stack
        REFPARAM,              // places refparam <arg_1> in the stack
        ASSIGNMENT,
        ARRAY_ASSIGNMENT,    // array_assignment: result[arg_2] = arg_1
        ARRAY_ACCESS,        // array_access: result = arg_1[arg_2]
        LOGICAL_OR,
        LOGICAL_AND,
        LOGICAL_NOT,
        DOUBLE_EQUAL,
        NOT_EQUAL,
        LESS,
        LESS_EQUAL,
        GREATER,
        GREATER_EQUAL,
        PLUS,
        MINUS,
        MUL,
        DIV,
        MOD,
    };

    Quad(Operator op);
    Quad(Operator op, std::string arg_1);
    Quad(Operator op, std::string arg_1, std::string result);
    Quad(Operator op, std::string arg_1, std::string arg_2, std::string result);
    void print();

    Operator _op;
    std::string _arg1;
    std::string _arg2;
    std::string _result;
};

// Struct representing a list of Quads
struct QuadArray {
    void append(const Quad& quad);
    void append(const QuadArray& quads);
    void backPatch();
    void print();

    std::list<Quad> _quads;
};

#endif
