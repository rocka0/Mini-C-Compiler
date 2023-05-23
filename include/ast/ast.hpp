#ifndef _AST_H
#define _AST_H

#include <quad/quad.hpp>
#include <symbol_table/symbol_table.hpp>

struct TranslationUnit;
struct ExternalDeclaration;
struct FunctionDefinition;

struct Declarator;
struct InitDeclarator;
struct Declaration;
struct ParameterDeclaration;
struct ParameterList;
struct ArgumentList;

struct MultiplicativeExpression;
struct AdditiveExpression;
struct RelationalExpression;
struct EqualityExpression;
struct LogicalANDExpression;
struct LogicalORExpression;
struct PrimaryExpression;
struct PostfixExpression;
struct UnaryExpression;
struct AssignmentExpression;
struct Expression;

struct Statement;
struct LabelStatement;
struct ExpressionStatement;
struct CompoundStatement;
struct SelectionStatement;
struct IterationStatement;
struct ForLoopBody;
struct JumpStatement;
struct PrintfStatement;

struct ExpressionResult {
    enum Type {
        CHAR,
        INT,
        FLOAT,
        STRING,
    };

    union Value {
        char charVal;
        int intVal;
        float floatVal;
        char* strVal;
    };

    Value _val;
    Type _type;
};

const char* expressionResultTypeToStr(ExpressionResult::Type type);

struct TranslationUnit {
    void print(std::string& s);
    QuadArray genCode();
    void execute();

    std::list<ExternalDeclaration*> _external_declarations;
};

struct ExternalDeclaration {
    ExternalDeclaration(FunctionDefinition* function_definition);
    ExternalDeclaration(Declaration* declaration);
    void print(std::string& s);
    QuadArray genCode();
    void execute();

    FunctionDefinition* _function_definition;
    Declaration* _declaration;
};

struct FunctionDefinition {
    enum ReturnType {
        CHAR,
        INT,
        FLOAT,
    };

    FunctionDefinition(ReturnType return_type, Declarator* declarator, CompoundStatement* compound_stmt);
    void print(std::string& s);
    QuadArray genCode();
    ExpressionResult execute();

    ReturnType _return_type;
    Declarator* _declarator;
    CompoundStatement* _compound_stmt;
};

struct ParameterDeclaration {
    enum Type {
        CHAR,
        INT,
        FLOAT,
    };

    ParameterDeclaration(Type type, Declarator* declarator);

    Type _type;
    Declarator* _declarator;
};

struct ParameterList {
    std::list<ParameterDeclaration*> _parameter_decls;
};

struct MultiplicativeExpression {
    enum Operator {
        MUL,
        DIV,
        MOD,
    };

    MultiplicativeExpression(UnaryExpression* unary_expr);
    MultiplicativeExpression(MultiplicativeExpression* multiplicative_expr, Operator multiplicative_op, UnaryExpression* unary_expr);
    void print(std::string& s);
    ExpressionResult::Type type();
    QuadArray genCode();
    ExpressionResult eval();

    Operator _multiplicative_op;
    MultiplicativeExpression* _multiplicative_expr;
    UnaryExpression* _unary_expr;
    std::string _var;
};

struct AdditiveExpression {
    enum Operator {
        PLUS,
        MINUS,
    };

    AdditiveExpression(MultiplicativeExpression* multiplicative_expr);
    AdditiveExpression(AdditiveExpression* additive_expr, Operator additive_op, MultiplicativeExpression* multiplicative_expr);
    void print(std::string& s);
    ExpressionResult::Type type();
    QuadArray genCode();
    ExpressionResult eval();

    Operator _additive_op;
    AdditiveExpression* _additive_expr;
    MultiplicativeExpression* _multiplicative_expr;
    std::string _var;
};

struct RelationalExpression {
    enum Operator {
        LESS,
        GREATER,
        LESS_EQUAL,
        GREATER_EQUAL,
    };

    RelationalExpression(AdditiveExpression* additive_expr);
    RelationalExpression(RelationalExpression* relational_expr, Operator relational_op, AdditiveExpression* additive_expr);
    void print(std::string& s);
    ExpressionResult::Type type();
    QuadArray genCode();
    ExpressionResult eval();

    Operator _relational_op;
    RelationalExpression* _relational_expr;
    AdditiveExpression* _additive_expr;
    std::string _var;
};

struct EqualityExpression {
    enum Operator {
        EQUAL,
        NOT_EQUAL,
    };

    EqualityExpression(RelationalExpression* relational_expr);
    EqualityExpression(EqualityExpression* equality_expr, Operator equality_op, RelationalExpression* relational_expr);
    void print(std::string& s);
    ExpressionResult::Type type();
    QuadArray genCode();
    ExpressionResult eval();

    Operator _equality_op;
    EqualityExpression* _equality_expr;
    RelationalExpression* _relational_expr;
    std::string _var;
};

struct LogicalANDExpression {
    LogicalANDExpression(EqualityExpression* equality_expr);
    LogicalANDExpression(LogicalANDExpression* logical_and_expr, EqualityExpression* equality_expr);
    void print(std::string& s);
    ExpressionResult::Type type();
    QuadArray genCode();
    ExpressionResult eval();

    EqualityExpression* _equality_expr;
    LogicalANDExpression* _logical_and_expr;
    std::string _var;
};

struct LogicalORExpression {
    LogicalORExpression(LogicalANDExpression* logical_and_expr);
    LogicalORExpression(LogicalORExpression* logical_or_expr, LogicalANDExpression* logical_and_expr);
    void print(std::string& s);
    ExpressionResult::Type type();
    QuadArray genCode();
    ExpressionResult eval();

    LogicalANDExpression* _logical_and_expr;
    LogicalORExpression* _logical_or_expr;
    std::string _var;
};

struct PrimaryExpression {
    enum Type {
        IDENTIFIER,
        CONSTANT_CHAR,
        CONSTANT_INT,
        CONSTANT_FLOAT,
        CONSTANT_STRING,
        EXPRESSION,
    };

    PrimaryExpression(Symbol* identifier);
    PrimaryExpression(char char_val);
    PrimaryExpression(int int_val);
    PrimaryExpression(float float_val);
    PrimaryExpression(char* str_val);
    PrimaryExpression(Expression* expr);
    void print(std::string& s);
    ExpressionResult::Type type();
    QuadArray genCode();
    ExpressionResult eval();

    Type _type;
    Symbol* _identifier;
    char _char_val;
    int _int_val;
    float _float_val;
    char* _str_val;
    Expression* _expr;
    std::string _var;
};

struct ArgumentList {
    std::list<AssignmentExpression*> _arguments;
};

struct PostfixExpression {
    enum Type {
        PRIMARY_EXPRESSION,
        ONE_D_ARRAY_ACCESS,
        TWO_D_ARRAY_ACCESS,
        FUNCTION_CALL,
    };

    PostfixExpression(PrimaryExpression* primary_expr);
    PostfixExpression(PrimaryExpression* primary_expr, Expression* expr_1);
    PostfixExpression(PrimaryExpression* primary_expr, Expression* expr_1, Expression* expr_2);
    PostfixExpression(PrimaryExpression* primary_expr, ArgumentList* argument_list);
    void print(std::string& s);
    ExpressionResult::Type type();
    QuadArray genCode();
    ExpressionResult eval();

    Type _type;
    PrimaryExpression* _primary_expr;
    Expression* _expr_1;
    Expression* _expr_2;
    ArgumentList* _argument_list;
    std::string _var;
};

struct UnaryExpression {
    enum Operator {
        PLUS,
        MINUS,
        EXCLAMATION,
    };

    UnaryExpression(PostfixExpression* postfix_expr);
    UnaryExpression(Operator unary_op, UnaryExpression* unary_expr);
    void print(std::string& s);
    ExpressionResult::Type type();
    QuadArray genCode();
    ExpressionResult eval();

    PostfixExpression* _postfix_expr;
    Operator _unary_op;
    UnaryExpression* _unary_expr;
    std::string _var;
};

struct AssignmentExpression {
    AssignmentExpression(LogicalORExpression* logical_or_expr);
    AssignmentExpression(PostfixExpression* postfix_expr, AssignmentExpression* assignment_expr);
    void print(std::string& s);
    ExpressionResult::Type type();
    QuadArray genCode();
    ExpressionResult eval();

    PostfixExpression* _postfix_expr;
    AssignmentExpression* _assignment_expr;
    LogicalORExpression* _logical_or_expr;
    std::string _var;
};

struct Expression {
    Expression(AssignmentExpression* assignment_expr);
    Expression(Expression* expr, AssignmentExpression* assignment_expr);
    void print(std::string& s);
    ExpressionResult::Type type();
    QuadArray genCode();
    ExpressionResult eval();

    Expression* _expr;
    AssignmentExpression* _assignment_expr;
    std::string _var;
};

struct Declarator {
    enum Type {
        IDENTIFIER,
        ONE_D_ARRAY,
        TWO_D_ARRAY,
        FUNCTION,
    };

    Declarator(Symbol* identifier);
    Declarator(Symbol* identifier, int first_dimension);
    Declarator(Symbol* identifier, int first_dimension, int second_dimension);
    Declarator(Symbol* identifier, ParameterList* parameter_list);
    void print(std::string& s);

    Type _type;
    Symbol* _identifier;
    int _first_dimension;
    int _second_dimension;
    ParameterList* _parameter_list;
};

struct InitDeclarator {
    InitDeclarator(Declarator* declarator);
    InitDeclarator(Declarator* declarator, AssignmentExpression* assignment_expr);
    void print(std::string& s);
    QuadArray genCode();
    void execute();

    Declarator* _declarator;
    AssignmentExpression* _assignment_expr;
};

struct Declaration {
    enum Type {
        CHAR,
        INT,
        FLOAT,
    };

    void print(std::string& s);
    QuadArray genCode();
    void execute();

    Type _type;
    std::list<InitDeclarator*> _init_declarators;
};

struct Statement {
    Statement(LabelStatement* label_stmt);
    Statement(ExpressionStatement* expr_stmt);
    Statement(CompoundStatement* compound_stmt);
    Statement(SelectionStatement* selection_stmt);
    Statement(IterationStatement* iteration_stmt);
    Statement(JumpStatement* jump_stmt);
    Statement(PrintfStatement* printf_stmt);
    void print(std::string& s);
    QuadArray genCode();
    void execute();

    LabelStatement* _label_stmt;
    ExpressionStatement* _expr_stmt;
    CompoundStatement* _compound_stmt;
    SelectionStatement* _selection_stmt;
    IterationStatement* _iteration_stmt;
    JumpStatement* _jump_stmt;
    PrintfStatement* _printf_stmt;
};

struct LabelStatement {
    enum Type {
        CASE,
        DEFAULT,
    };

    LabelStatement(Expression* expr, Statement* stmt);
    LabelStatement(Statement* stmt);
    void print(std::string& s);
    void execute();

    Type _type;
    Statement* _stmt;
    Expression* _expr;
};

struct ExpressionStatement {
    ExpressionStatement(Expression* expr);
    ExpressionStatement();
    void print(std::string& s);
    QuadArray genCode();
    ExpressionResult execute();

    Expression* _expr;
    std::string _var;
};

struct CompoundStatement {
    enum Type {
        DECLARATION,
        STATEMENT,
    };
    typedef std::pair<Type, void*> decl_or_stmt;

    void addDeclaration(Declaration* decl);
    void addStatement(Statement* stmt);
    void print(std::string& s);
    QuadArray genCode();
    void execute();

    std::list<decl_or_stmt> _body;
    SymbolTable* _scope;
};

struct SelectionStatement {
    enum Type {
        IF,
        IF_ELSE,
        SWITCH,
    };

    SelectionStatement(Type type, Expression* expr, Statement* stmt_1);
    SelectionStatement(Expression* expr, Statement* stmt_1, Statement* stmt_2);
    void print(std::string& s);
    QuadArray genCode();
    void execute();

    Type _type;
    Expression* _expr;
    Statement* _stmt_1;
    Statement* _stmt_2;
};

struct ForLoopBody {
    enum Type {
        ES1_ES2,
        ES1_ES2_EXPR,
        DECL_ES1,
        DECL_ES1_EXPR,
    };

    ForLoopBody(ExpressionStatement* expr_stmt_1, ExpressionStatement* expr_stmt_2);
    ForLoopBody(ExpressionStatement* expr_stmt_1, ExpressionStatement* expr_stmt_2, Expression* expr);
    ForLoopBody(Declaration* decl, ExpressionStatement* expr_stmt_1);
    ForLoopBody(Declaration* decl, ExpressionStatement* expr_stmt_1, Expression* expr);
    void print(std::string& s);

    Type _type;
    Declaration* _decl;
    ExpressionStatement* _expr_stmt_1;
    ExpressionStatement* _expr_stmt_2;
    Expression* _expr;
    SymbolTable* _scope;
};

struct IterationStatement {
    enum Type { WHILE, FOR };

    IterationStatement(Expression* expr, Statement* stmt);
    IterationStatement(ForLoopBody* for_loop_body, Statement* stmt);
    void print(std::string& s);
    QuadArray genCode();
    void execute();

    Type _type;
    ForLoopBody* _for_loop_body;
    Expression* _expr;
    Statement* _stmt;
};

struct JumpStatement {
    enum Type {
        BREAK,
        CONTINUE,
        RETURN,
    };

    JumpStatement(Type type);
    JumpStatement(Expression* expr);
    void print(std::string& s);
    void execute();

    Type _type;
    Expression* _expr;
};

struct PrintfStatement {
    PrintfStatement(std::string);
    PrintfStatement(std::string, ArgumentList* argument_list);
    void print(std::string& s);
    QuadArray genCode();
    void execute();

    std::string _format_string;
    ArgumentList* _argument_list;
};

#endif
