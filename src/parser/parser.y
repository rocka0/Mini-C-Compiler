%code requires {
    #include <ast/ast.hpp>
}

%code {
    #include <iostream>
    #include <fstream>
    #include <string>
    #include <cstdarg>
    #include <cstring>
    #include <symbol_table/symbol_table.hpp>

    // Declarations from lexer (flex)
    extern int yylex();
    extern int yylineno;

    // Declarations from main
    extern SymbolTable* currentST;
    extern TranslationUnit* root;

    // Declarations
    int yyerror(const char* s);
    int error(const char* err, ...);

    // Definitions
    std::ofstream bisonout;
}

%union {
    char charVal;
    int intVal;
    float floatVal;
    char* stringVal;

    TranslationUnit* translation_unit;
    ExternalDeclaration* external_declaration;
    FunctionDefinition* function_definition;

    Declarator* declarator;
    InitDeclarator* init_declarator;
    Declaration* declaration;
    ParameterDeclaration* parameter_declaration;
    ParameterList* parameter_list;
    ArgumentList* argument_list;
    
    MultiplicativeExpression* multiplicative_expression;
    AdditiveExpression* additive_expression;
    RelationalExpression* relational_expression;
    EqualityExpression* equality_expression;
    LogicalANDExpression* logical_and_expression;
    LogicalORExpression* logical_or_expression;
    PrimaryExpression* primary_expression;
    PostfixExpression* postfix_expression;
    UnaryExpression* unary_expression;
    AssignmentExpression* assignment_expression;
    Expression* expression;

    Statement* statement;
    LabelStatement* labeled_statement;
    ExpressionStatement* expression_statement;
    CompoundStatement* compound_statement;
    SelectionStatement* selection_statement;
    ForLoopBody* for_loop_body;
    IterationStatement* iteration_statement;
    JumpStatement* jump_statement;
    PrintfStatement* printf_statement;
}

// Constant tokens
%token<charVal> T_CONSTANT_CHAR         // char
%token<intVal> T_CONSTANT_INT           // int
%token<floatVal> T_CONSTANT_FLOAT       // float
%token<stringVal> T_CONSTANT_STRING     // string: char[] str = "T_CONSTANT_STRING"

// Identifier token
%token<stringVal> T_IDENTIFIER

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

%type <translation_unit> translation_unit
%type <external_declaration> external_declaration
%type <function_definition> function_definition

%type <declarator> declarator
%type <init_declarator> init_declarator
%type <declaration> declaration
%type <parameter_declaration> parameter_declaration
%type <parameter_list> parameter_list
%type <argument_list> argument_list_optional
%type <argument_list> argument_list

%type <multiplicative_expression> multiplicative_expression
%type <additive_expression> additive_expression
%type <relational_expression> relational_expression
%type <equality_expression> equality_expression
%type <logical_and_expression> logical_and_expression
%type <logical_or_expression> logical_or_expression
%type <primary_expression> primary_expression
%type <postfix_expression> postfix_expression
%type <unary_expression> unary_expression
%type <assignment_expression> assignment_expression
%type <expression> expression

%type <statement> statement
%type <labeled_statement> labeled_statement
%type <expression_statement> expression_statement
%type <compound_statement> compound_statement
%type <selection_statement> selection_statement
%type <for_loop_body> for_loop_body
%type <iteration_statement> iteration_statement
%type <jump_statement> jump_statement
%type <printf_statement> printf_statement

%type <stringVal> type_specifier
%type <charVal> unary_operator
%type <declaration> init_declarator_plus
%type <compound_statement> declaration_or_statement_star
%type <translation_unit> external_declaration_star

%start translation_unit

/*
    Our Grammar for C is ADAPTED from:
    https://cs.wmich.edu/~gupta/teaching/cs4850/sumII06/The%20syntax%20of%20C%20in%20Backus-Naur%20form.htm
*/

%%

translation_unit:
    external_declaration_star
    {
        bisonout << "translation_unit -> external_declaration_star" << std::endl;
        
        // Copy translation unit from external_declaration_star to translation_unit
        $$ = $1;

        // Set root of AST
        root = $$;
    }
;

external_declaration_star:
    external_declaration_star external_declaration
    {
        bisonout << "external_declaration_star -> external_declaration_star external_declaration" << std::endl;
        
        // Append external declaration to translation unit
        $$ = $1;
        $$->_external_declarations.push_back($2);
    }
|
    %empty
    {
        bisonout << "external_declaration_star -> ϵ" << std::endl;

        // Create new TranslationUnit with no external declarations
        $$ = new TranslationUnit();
    }
;

external_declaration:
    function_definition
    {
        bisonout << "external_declaration -> function_definition" << std::endl;

        // Create new ExternalDeclaration with function definition
        $$ = new ExternalDeclaration($1);
    }
|
    declaration
    {
        bisonout << "external_declaration -> declaration" << std::endl;

        // Create new ExternalDeclaration with declaration
        $$ = new ExternalDeclaration($1);
    }
;

/*
    remove declaration_star from function_definition -> type_specifier declarator declaration_star compound_statement
*/

function_definition:
    type_specifier declarator
    {
        // Get the type specifier
        std::string functionType($1);

        // Check if declarator type is function
        if($2->_type != Declarator::Type::FUNCTION) {
            error("Invalid function definition @ line %d\n", yylineno);
        }

        // Get symbol corresponding to function declarator
        Symbol* function = $2->_identifier;

        // Set the return type of the function
        if(functionType == "char"){
            function->type = Symbol::Type::FUNCTION_CHAR;
        }
        else if(functionType == "int"){
            function->type = Symbol::Type::FUNCTION_INT;
        }
        else {
            function->type = Symbol::Type::FUNCTION_FLOAT;
        }

        // Change scope to scope of the function
        currentST = function->nestedTable;
    }
    compound_statement
    {
        bisonout << "function_definition -> type_specifier declarator compound_statement" << std::endl;
        
        // Restore scope to be the parent scope
        currentST = currentST->parent;

        // Get the name of the function
        std::string functionName = $2->_identifier->name;
        
        // Get the type specifier
        std::string functionType($1);

        // Free memory allocated for type specifier
        free($1);

        // Check if all return statements in function have the same type as the function return type
        for(auto &[type, statement]: $4->_body) {
            if(type == CompoundStatement::Type::STATEMENT) {
                Statement* stmt = static_cast<Statement*>(statement);
                if(stmt->_jump_stmt != nullptr) {
                    if(stmt->_jump_stmt->_type == JumpStatement::Type::RETURN) {
                        ExpressionResult::Type retType = stmt->_jump_stmt->_expr->type();
                        // Check that retType matches functionType
                        if(retType == ExpressionResult::Type::CHAR and functionType == "char") {
                            // Do nothing
                        }
                        else if(retType == ExpressionResult::Type::INT and functionType == "int") {
                            // Do nothing
                        }
                        else if(functionType == "float") {
                            if(retType == ExpressionResult::Type::INT or retType == ExpressionResult::Type::FLOAT) {
                                // Do nothing
                            }
                            else {
                                error("Invalid return type (%s) for function \"%s\"\n", expressionResultTypeToStr(retType), functionName.c_str()); 
                            }
                        }
                        else {
                            error("Invalid return type (%s) for function \"%s\"\n", expressionResultTypeToStr(retType), functionName.c_str()); 
                        }
                    }
                }
            }
        }
        
        // Create FunctionDefinition from type_specifier declarator compound_statement_without_scope
        if(functionType == "char") {
           $$ = new FunctionDefinition(FunctionDefinition::ReturnType::CHAR, $2, $4);
        }
        else if(functionType == "int") {
            $$ = new FunctionDefinition(FunctionDefinition::ReturnType::INT, $2, $4);
        }
        else {
            $$ = new FunctionDefinition(FunctionDefinition::ReturnType::FLOAT, $2, $4);
        }
    }
;

type_specifier:
    T_KEYWORD_CHAR
    {
        bisonout << "type_specifier -> char" << std::endl;

        // Create type specifier from char
        $$ = (char*) malloc(5*sizeof(char));
        memset($$, 0, 5*sizeof(char));
        strcpy($$, "char");
    }
|
    T_KEYWORD_INT 
    {
        bisonout << "type_specifier -> int" << std::endl;

        // Create type specifier from int
        $$ = (char*) malloc(4*sizeof(char));
        memset($$, 0, 4*sizeof(char));
        strcpy($$, "int");
    }
|
    T_KEYWORD_FLOAT
    {
        bisonout << "type_specifier -> float" << std::endl;

        // Create type specifier from float
        $$ = (char*) malloc(6*sizeof(char));
        memset($$, 0, 6*sizeof(char));
        strcpy($$, "float");
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
        std::string variableName = std::string($1);
        bisonout << "declarator -> " << variableName << std::endl;

        // Find symbol with name = variableName in current symbol table
        Symbol* symbol = currentST->findSymbol(variableName);

        // If symbol is not nullptr, then it is a re-definition so throw error
        if(symbol != nullptr)
        {
            error("Re-definition of \"%s\" @ line %d\n", variableName.c_str(), yylineno);
        }

        // Create a new symbol for the variable
        symbol = new Symbol(variableName, Symbol::Type::UNDEFINED);

        // Add symbol to the current symbol table
        currentST->addSymbol(symbol);

        // Create Declarator from variable symbol
        $$ = new Declarator(symbol);
    }
|
    T_IDENTIFIER T_LBRACKET T_CONSTANT_INT T_RBRACKET 
    {
        std::string arrayName($1);
        bisonout << "declarator -> " << arrayName << "[" << $3 << "]" << std::endl;

        // First find the arrayName in the current symbol table
        Symbol* symbol = currentST->findSymbol(arrayName);

        // If the symbol is not nullptr, this is a re-definition
        if (symbol != nullptr) {
            error("Re-definition of \"%s\" @ line %d\n", arrayName.c_str(), yylineno);
        }

        // Create a new symbol for the 1-D array
        symbol = new Symbol(arrayName, Symbol::Type::ARR_UNDEFINED);

        // Set the first(only) dimension of the array
        symbol->firstDimension = $3;

        // Add symbol to the current symbol table
        currentST->addSymbol(symbol);

        // Create Declarator from array symbol
        $$ = new Declarator(symbol, $3);
    }
|
    T_IDENTIFIER T_LBRACKET T_CONSTANT_INT T_RBRACKET T_LBRACKET T_CONSTANT_INT T_RBRACKET 
    {
        std::string arrayName($1);
        bisonout << "declarator -> " << arrayName << "[" << $3 << "][" << $6 << "]" << std::endl;

        // First find the arrayName in the current symbol table
        Symbol* symbol = currentST->findSymbol(arrayName);

        // If the symbol is not nullptr, this is a re-definition
        if (symbol != nullptr) {
            error("Re-definition of \"%s\" @ line %d\n", arrayName.c_str(), yylineno);
        }

        // Create a new symbol for the 2-D array
        symbol = new Symbol(arrayName, Symbol::Type::ARR_ARR_UNDEFINED);

        // Set the first dimension of the array
        symbol->firstDimension = $3;

        // Set the second dimension of the array
        symbol->secondDimension = $6;

        // Add symbol to the current symbol table
        currentST->addSymbol(symbol);

        // Create Declarator from array symbol
        $$ = new Declarator(symbol, $3, $6);
    }
|
    T_IDENTIFIER T_LPARAN T_RPARAN
    {
        std::string funcName($1);
        bisonout << "declarator -> " << funcName << "()" << std::endl;

        // First find the funcName in the current symbol table
        Symbol* symbol = currentST->findSymbol(funcName);

        // If the symbol is not nullptr, this is a re-definition
        if (symbol != nullptr) {
            error("Re-definition of \"%s\" @ line %d\n", funcName.c_str(), yylineno);
        }

        // Create a nested symbol table for the function
        std::string funcSTName = currentST->name + "." + funcName;
        SymbolTable* funcST = new SymbolTable(funcSTName, currentST);

        // Create a new symbol for the function
        symbol = new Symbol(funcName, Symbol::Type::UNDEFINED, funcST);

        // Add symbol to the current symbol table
        currentST->addSymbol(symbol);

        // Create Declarator from function symbol
        $$ = new Declarator(symbol, nullptr);
    }
|
    T_IDENTIFIER T_LPARAN
    {
        std::string funcName($1);

        // First find the funcName in the current symbol table
        Symbol* symbol = currentST->findSymbol(funcName);

        // If the symbol is not nullptr, this is a re-definition
        if (symbol != nullptr) {
            error("Re-definition of \"%s\" @ line %d\n", funcName.c_str(), yylineno);
        }

        // Create a nested symbol table for the function
        std::string funcSTName = currentST->name + "." + funcName;
        SymbolTable* funcST = new SymbolTable(funcSTName, currentST);

        // Create a new symbol for the function
        symbol = new Symbol(funcName, Symbol::Type::UNDEFINED, funcST);

        // Add symbol to the current symbol table
        currentST->addSymbol(symbol);
        
        // Set the current scope to be the function scope
        currentST = funcST;
    }
    parameter_list T_RPARAN
    {
        // Get the function name
        std::string funcName($1);

        bisonout << "declarator -> " << funcName << "(parameter_list)" << std::endl;

        // Restore scope to be the parent scope
        currentST = currentST->parent;

        // Find the function symbol in the parent symbol table
        Symbol* symbol = currentST->findSymbol(funcName);

        // Create Declarator from function symbol
        $$ = new Declarator(symbol, $4);
    }
;

/* 
    Bypass conditional_expression to logical_or_expression as no ternary expressions allowed 
    Remove constant_expression entirely
*/

logical_or_expression:
    logical_and_expression
    {
        bisonout << "logical_or_expression -> logical_and_expression" << std::endl;
        
        // Create LogicalORExpression from LogicalANDExpression
        $$ = new LogicalORExpression($1);
    }
|
    logical_or_expression T_OP_DOUBLE_OR logical_and_expression
    {
        bisonout << "logical_or_expression -> logical_or_expression || logical_and_expression" << std::endl;

        // Create LogicalORExpression from LogicalORExpression and LogicalANDExpression
        $$ = new LogicalORExpression($1, $3);
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
        bisonout << "logical_and_expression -> equality_expression" << std::endl;

        // Create LogicalANDExpression from EqualityExpression
        $$ = new LogicalANDExpression($1);
    }
|
    logical_and_expression T_OP_DOUBLE_AND equality_expression
    {
        bisonout << "logical_and_expression -> logical_and_expression && equality_expression" << std::endl;

        // Create LogicalANDExpression from LogicalANDExpression and EqualityExpression
        $$ = new LogicalANDExpression($1, $3);
    }
;

equality_expression:
    relational_expression
    {
        bisonout << "equality_expression -> relational_expression" << std::endl;

        // Create EqualityExpression from RelationalExpression
        $$ = new EqualityExpression($1);
    }
|
    equality_expression T_OP_DOUBLE_EQ relational_expression
    {
        bisonout << "equality_expression -> equality_expression == relational_expression" << std::endl;

        // Create EqualityExpression from EqualityExpression and RelationalExpression
        $$ = new EqualityExpression($1, EqualityExpression::Operator::EQUAL, $3);
    }
|
    equality_expression T_OP_NEQ relational_expression
    {
        bisonout << "equality_expression -> equality_expression != relational_expression" << std::endl;

        // Create EqualityExpression from EqualityExpression and RelationalExpression
        $$ = new EqualityExpression($1, EqualityExpression::Operator::NOT_EQUAL, $3);
    }
;

relational_expression:
    additive_expression
    {
        bisonout << "relational_expression -> additive_expression" << std::endl;

        // Create RelationalExpression from AdditiveExpression
        $$ = new RelationalExpression($1);
    }
|
    relational_expression T_OP_LT additive_expression
    {
        bisonout << "relational_expression -> relational_expression < additive_expression" << std::endl;
        
        // Get type of relational_expression
        ExpressionResult::Type relational_expr_type = $1->type();

        // Get type of additive_expression
        ExpressionResult::Type additive_expr_type = $3->type();
        
        // Type Check relational_expression and additive_expression
        if (relational_expr_type != additive_expr_type) {
            error("Type mismatch in relational expression (%s vs %s) @ line %d\n", expressionResultTypeToStr(relational_expr_type), expressionResultTypeToStr(additive_expr_type), yylineno);
        }

        // Create RelationalExpression from RelationalExpression and AdditiveExpression
        $$ = new RelationalExpression($1, RelationalExpression::Operator::LESS, $3);
    }
|
    relational_expression T_OP_GT additive_expression
    {
        bisonout << "relational_expression -> relational_expression > additive_expression" << std::endl;
        
        // Get type of relational_expression
        ExpressionResult::Type relational_expr_type = $1->type();

        // Get type of additive_expression
        ExpressionResult::Type additive_expr_type = $3->type();
        
        // Type Check relational_expression and additive_expression
        if (relational_expr_type != additive_expr_type) {
            error("Type mismatch in relational expression (%s vs %s) @ line %d\n", expressionResultTypeToStr(relational_expr_type), expressionResultTypeToStr(additive_expr_type), yylineno);
        }

        // Create RelationalExpression from RelationalExpression and AdditiveExpression
        $$ = new RelationalExpression($1, RelationalExpression::Operator::GREATER, $3);
    }
|
    relational_expression T_OP_LEQ additive_expression
    {
        bisonout << "relational_expression -> relational_expression <= additive_expression" << std::endl;
        
        // Get type of relational_expression
        ExpressionResult::Type relational_expr_type = $1->type();

        // Get type of additive_expression
        ExpressionResult::Type additive_expr_type = $3->type();
        
        // Type Check relational_expression and additive_expression
        if (relational_expr_type != additive_expr_type) {
            error("Type mismatch in relational expression (%s vs %s) @ line %d\n", expressionResultTypeToStr(relational_expr_type), expressionResultTypeToStr(additive_expr_type), yylineno);
        }

        // Create RelationalExpression from RelationalExpression and AdditiveExpression
        $$ = new RelationalExpression($1, RelationalExpression::Operator::LESS_EQUAL, $3);
    }
|
    relational_expression T_OP_GEQ additive_expression
    {
        bisonout << "relational_expression -> relational_expression >= additive_expression" << std::endl;
        
        // Get type of relational_expression
        ExpressionResult::Type relational_expr_type = $1->type();

        // Get type of additive_expression
        ExpressionResult::Type additive_expr_type = $3->type();
        
        // Type Check relational_expression and additive_expression
        if (relational_expr_type != additive_expr_type) {
            error("Type mismatch in relational expression (%s vs %s) @ line %d\n", expressionResultTypeToStr(relational_expr_type), expressionResultTypeToStr(additive_expr_type), yylineno);
        }

        // Create RelationalExpression from RelationalExpression and AdditiveExpression
        $$ = new RelationalExpression($1, RelationalExpression::Operator::GREATER_EQUAL, $3);
    }
;

additive_expression:
    multiplicative_expression
    {
        bisonout << "additive_expression -> multiplicative_expression" << std::endl;

        // Create AdditiveExpression from MultiplicativeExpression
        $$ = new AdditiveExpression($1);
    }
|
    additive_expression T_OP_PLUS multiplicative_expression
    {
        bisonout << "additive_expression -> additive_expression + multiplicative_expression" << std::endl;

        // Create AdditiveExpression from AdditiveExpression and MultiplicativeExpression
        $$ = new AdditiveExpression($1, AdditiveExpression::Operator::PLUS, $3);
    }
|
    additive_expression T_OP_MINUS multiplicative_expression
    {
        bisonout << "additive_expression -> additive_expression - multiplicative_expression" << std::endl;

        // Create AdditiveExpression from AdditiveExpression and MultiplicativeExpression
        $$ = new AdditiveExpression($1, AdditiveExpression::Operator::MINUS, $3);
    }
;

multiplicative_expression:
    unary_expression
    {
        bisonout << "multiplicative_expression -> unary_expression" << std::endl;

        // Create MultiplicativeExpression from UnaryExpression
        $$ = new MultiplicativeExpression($1);
    }
|
    multiplicative_expression T_OP_MUL unary_expression
    {
        bisonout << "multiplicative_expression -> multiplicative_expression * unary_expression" << std::endl;

        // Create MultiplicativeExpression from MultiplicativeExpression and UnaryExpression
        $$ = new MultiplicativeExpression($1, MultiplicativeExpression::Operator::MUL, $3);
    }
|
    multiplicative_expression T_OP_DIV unary_expression
    {
        bisonout << "multiplicative_expression -> multiplicative_expression / unary_expression" << std::endl;

        // Create MultiplicativeExpression from MultiplicativeExpression and UnaryExpression
        $$ = new MultiplicativeExpression($1, MultiplicativeExpression::Operator::DIV, $3);
    }
|
    multiplicative_expression T_OP_MOD unary_expression
    {
        bisonout << "multiplicative_expression -> multiplicative_expression % unary_expression" << std::endl;

        // Create MultiplicativeExpression from MultiplicativeExpression and UnaryExpression
        $$ = new MultiplicativeExpression($1, MultiplicativeExpression::Operator::MOD, $3);
    }
;

unary_expression:
    postfix_expression
    {
        bisonout << "unary_expression -> postfix_expression" << std::endl;

        // Create UnaryExpression from PostfixExpression
        $$ = new UnaryExpression($1);
    }
|
    unary_operator unary_expression
    {
        bisonout << "unary_expression -> unary_operator unary_expression" << std::endl;

        // Create UnaryExpression from UnaryOperator and UnaryExpression
        if($1 == '+'){
            $$ = new UnaryExpression(UnaryExpression::Operator::PLUS, $2);
        }
        else if($1 == '-') {
            $$ = new UnaryExpression(UnaryExpression::Operator::MINUS, $2);
        }
        else {
            $$ = new UnaryExpression(UnaryExpression::Operator::EXCLAMATION, $2);
        }
    }
;

/*
    Remove assignment_expression_star
    Introduce argument_list instead of assignment_expression_star
*/

postfix_expression:
    primary_expression
    {
        // If primary_expression is an identifier, it must be a primitive type
        if($1->_type == PrimaryExpression::IDENTIFIER) {
            switch($1->_identifier->type) {
                case Symbol::Type::UNDEFINED: {
                    error("Uninitialized identifier %s @ line %d\n", $1->_identifier->name.c_str(), yylineno);
                    break;
                }
                case Symbol::Type::CHAR:
                case Symbol::Type::INT:
                case Symbol::Type::FLOAT: {
                    break;
                }
                default: {
                    error("Got array or function in postfix_expression @ line %d\n", yylineno);
                }
            }
        }

        bisonout << "postfix_expression -> primary_expression" << std::endl;
        
        // Create PostfixExpression from PrimaryExpression
        $$ = new PostfixExpression($1);
    }
|
    primary_expression T_LBRACKET expression T_RBRACKET
    {
        // Ensure that primary_expression is an identifier
        if($1->_type != PrimaryExpression::IDENTIFIER) {
            error("Expected identifier in array access @ line %d\n", yylineno);
        }

        // Ensure that the identifier is of type 1-D array
        if($1->_identifier->type == Symbol::Type::ARR_CHAR or $1->_identifier->type == Symbol::Type::ARR_INT or $1->_identifier->type == Symbol::Type::ARR_FLOAT) {
            // Do nothing
        }
        else {
            error("Expected 1-D array in array access @ line %d\n", yylineno);
        }

        bisonout << "postfix_expression -> identifier[expression]" << std::endl;

        // Create PostfixExpression from Identifier and Expression
        $$ = new PostfixExpression($1, $3); 
    }
|
    primary_expression T_LBRACKET expression T_RBRACKET T_LBRACKET expression T_RBRACKET
    {
        bisonout << "postfix_expression -> identifier[expression][expression]" << std::endl;
        
        // Ensure that primary_expression is an identifier
        if($1->_type != PrimaryExpression::IDENTIFIER) {
            error("Expected identifier in array access @ line %d\n", yylineno);
        }
        
        // Ensure that the identifier is of type 2-D array
        if($1->_identifier->type == Symbol::Type::ARR_ARR_CHAR or $1->_identifier->type == Symbol::Type::ARR_ARR_INT or $1->_identifier->type == Symbol::Type::ARR_ARR_FLOAT) {
            // Do nothing
        }
        else {
            error("Expected 2-D array in array access @ line %d\n", yylineno);
        }
        
        // Create PostfixExpression from Identifier and Expression
        $$ = new PostfixExpression($1, $3, $6);
    }
|
    primary_expression T_LPARAN argument_list_optional T_RPARAN
    {
        bisonout << "postfix_expression -> identifier(argument_list_optional)" << std::endl;

        // Ensure that primary_expression is an identifier
        if($1->_type != PrimaryExpression::IDENTIFIER) {
            error("Expected identifier in function call @ line %d\n", yylineno);
        }

        // Ensure that the identifier is of type function
        if($1->_identifier->type == Symbol::Type::FUNCTION_CHAR or $1->_identifier->type == Symbol::Type::FUNCTION_INT or $1->_identifier->type == Symbol::Type::FUNCTION_FLOAT) {
            // Get symbol table of function
            SymbolTable* functionSymbolTable = $1->_identifier->nestedTable;

            // No arguments were passed
            if($3 == nullptr) {
                // Get the expected number of arguments
                int expected_arguments = functionSymbolTable->table.size();

                // Ensure that the function has no arguments
                if(functionSymbolTable->table.size() > 0) {
                    error("Expected %d argument(s) in function call @ line %d\n", expected_arguments, yylineno);
                }
            }
            // Some arguments were passed
            else {
                std::list<AssignmentExpression*> passed_arguments = $3->_arguments;
                
                // Ensure that passed arguments count matches function arguments count
                if(functionSymbolTable->table.size() != passed_arguments.size()) {
                    error("Expected %d arguments in function call @ line %d\n", functionSymbolTable->table.size(), yylineno);
                }

                auto it = passed_arguments.begin();

                for(auto &function_arg: functionSymbolTable->table){
                    ExpressionResult::Type passed_argument_type = (*it)->type();
                    if(function_arg->type == Symbol::Type::CHAR) {
                        if(passed_argument_type != ExpressionResult::Type::CHAR) {
                            error("Expected char, got %s in function call @ line %d\n", expressionResultTypeToStr(passed_argument_type), yylineno);
                        }
                    }
                    else if(function_arg->type == Symbol::Type::INT) {
                        if(passed_argument_type != ExpressionResult::Type::INT) {
                            error("Expected int, got %s in function call @ line %d\n", expressionResultTypeToStr(passed_argument_type), yylineno);
                        }
                    }
                    else {
                        if(passed_argument_type != ExpressionResult::Type::FLOAT and passed_argument_type != ExpressionResult::Type::INT) {
                            error("Expected float, got %s in function call @ line %d\n", expressionResultTypeToStr(passed_argument_type), yylineno);
                        }
                    }
                    ++it;
                }
            }
        }
        else {
            error("Expected function for function call @ line %d\n", yylineno);
        }

        // Create PostfixExpression from Identifier and ArgumentList
        $$ = new PostfixExpression($1, $3);
    }
;

argument_list_optional:
    argument_list
    {
        bisonout << "argument_list_optional -> argument_list" << std::endl;
        
        // Copy ArgumentList from argument_list to argument_list_optional
        $$ = $1;
    }
|
    %empty
    {
        bisonout << "argument_list_optional -> ϵ" << std::endl;

        // Create empty ArgumentList
        $$ = nullptr;
    }
;

argument_list:
    argument_list T_COMMA assignment_expression
    {
        bisonout << "argument_list -> argument_list, assignment_expression" << std::endl;
        
        // Append argument_expression to argument_list
        $$ = $1;
        $$->_arguments.push_back($3);
    }
|
    assignment_expression
    {
        bisonout << "argument_list -> assignment_expression" << std::endl;
        
        // Create first argument_list from assignment_expression
        $$ = new ArgumentList();
        $$->_arguments.push_back($1);
    }
;

/*
    Integrate <constant> into primary_expression
*/

primary_expression:
    T_IDENTIFIER
    {
        bisonout << "primary_expression -> " << $1 << std::endl;
        
        // Get varName
        std::string varName($1);

        // Search for identifier in current symbol table recursively upto global scope
        Symbol* identifier = currentST->findSymbol(varName, true);

        // If identifier is nullptr, symbol does not exist
        if(identifier == nullptr) {
            error("Identifier %s not declared @ line %d\n", varName.c_str(), yylineno);
        }

        // Create PrimaryExpression from T_IDENTIFIER
        $$ = new PrimaryExpression(identifier);
    }
|
    T_CONSTANT_CHAR
    {
        bisonout << "primary_expression -> T_CONSTANT_CHAR" << std::endl;

        // Create PrimaryExpression from T_CONSTANT_CHAR
        $$ = new PrimaryExpression($1);
    }
|
    T_CONSTANT_INT
    {
        bisonout << "primary_expression -> T_CONSTANT_INT" << std::endl;

        // Create PrimaryExpression from T_CONSTANT_INT
        $$ = new PrimaryExpression($1);
    }
|
    T_CONSTANT_FLOAT
    {
        bisonout << "primary_expression -> T_CONSTANT_FLOAT" << std::endl;

        // Create PrimaryExpression from T_CONSTANT_FLOAT
        $$ = new PrimaryExpression($1);
    }
|
    T_CONSTANT_STRING
    {
        bisonout << "primary_expression -> T_CONSTANT_STRING" << std::endl;

        // Create PrimaryExpression from T_CONSTANT_STRING
        $$ = new PrimaryExpression($1);
    }
|
    T_LPARAN expression T_RPARAN
    {
        bisonout << "primary_expression -> (expression)" << std::endl;

        // Create PrimaryExpression from Expression
        $$ = new PrimaryExpression($2);
    }
;

expression:
    assignment_expression
    {
        bisonout << "expression -> assignment_expression" << std::endl;

        // Create Expression from AssignmentExpression
        $$ = new Expression($1);
    }
|
    expression T_COMMA assignment_expression
    {
        bisonout << "expression -> expression, assignment_expression" << std::endl;

        // Create Expression from Expression and AssignmentExpression
        $$ = new Expression($1, $3);
    }
;

assignment_expression:
    logical_or_expression
    {
        bisonout << "assignment_expression -> logical_or_expression" << std::endl;

        // Create AssignmentExpression from LogicalOrExpression
        $$ = new AssignmentExpression($1);
    }
|
    postfix_expression T_OP_EQ assignment_expression
    {
        bisonout << "assignment_expression -> postfix_expression = assignment_expression" << std::endl;
        
        // Check if postfix_expression is a function call, if so, error
        if($1->_type == PostfixExpression::FUNCTION_CALL) {
            error("Cannot assign value to function call @ line %d\n", yylineno);
        }

        // Check if postfix_expression is a primary_expression, if so, it must be an identifier
        if($1->_type == PostfixExpression::PRIMARY_EXPRESSION) {
            if($1->_primary_expr->_type != PrimaryExpression::Type::IDENTIFIER) {
                error("Cannot assign value to non-variable @ line %d\n", yylineno);
            }
        }

        // Make sure that the type of LHS (postfix_expression) and RHS (assignment_expression) are same
        ExpressionResult::Type lhs = $1->type();
        ExpressionResult::Type rhs = $3->type();

        if(lhs == ExpressionResult::Type::CHAR) {
            if(rhs != ExpressionResult::Type::CHAR) {
                error("Type mismatch (char vs %s) in assignment @ line %d\n", expressionResultTypeToStr(rhs), yylineno);
            }
        }
        else if (lhs == ExpressionResult::Type::INT) {
            if(rhs != ExpressionResult::Type::INT) {
                error("Type mismatch (int vs %s) in assignment @ line %d\n", expressionResultTypeToStr(rhs), yylineno);
            }
        }
        else if (lhs == ExpressionResult::Type::FLOAT) {
            if(rhs != ExpressionResult::Type::FLOAT and rhs != ExpressionResult::Type::INT) {
                error("Type mismatch (float vs %s) in assignment @ line %d\n", expressionResultTypeToStr(rhs), yylineno);
            }
        }
        else {
            error("Cannot assign value to char* @ line %d\n", yylineno);
        }

        // Create AssignmentExpression from PostfixExpression and AssignmentExpression
        $$ = new AssignmentExpression($1, $3);
    }
;

unary_operator:
    T_OP_PLUS
    {
        bisonout << "unary_operator -> +" << std::endl;

        // Create UnaryOperator from T_OP_PLUS
        $$ = '+';
    }
|
    T_OP_MINUS
    {
        bisonout << "unary_operator -> -" << std::endl;

        // Create UnaryOperator from T_OP_MINUS
        $$ = '-';
    }
|
    T_OP_EXCLAMATION
    {
        bisonout << "unary_operator -> !" << std::endl;

        // Create UnaryOperator from T_OP_EXCLAMATION
        $$ = '!';
    }
;

parameter_list:
    parameter_declaration
    {
        bisonout << "parameter_list -> parameter_declaration" << std::endl;

        // Create ParameterList from parameter_declaration
        $$ = new ParameterList();
        $$->_parameter_decls.push_back($1);
    }
|
    parameter_list T_COMMA parameter_declaration
    {
        bisonout << "parameter_list -> parameter_list, parameter_declaration" << std::endl;

        // Append parameter_declaration to ParameterList
        $$ = $1;
        $$->_parameter_decls.push_back($3);
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
        bisonout << "parameter_declaration -> type_specifier declarator" << std::endl;

        // Check if declarator is a primitive datatype identifier
        if($2->_type != Declarator::Type::IDENTIFIER){
            error("Parameter to function must not be an array or function @ line %d\n", yylineno);
        }

        // Get type_specifier
        std::string type($1);

        // Set type of declarator and create ParameterDeclaration
        if(type == "char"){
            $2->_identifier->type = Symbol::Type::CHAR;
            $$ = new ParameterDeclaration(ParameterDeclaration::Type::CHAR, $2);
        }
        else if(type == "int"){
            $2->_identifier->type = Symbol::Type::INT;
            $$ = new ParameterDeclaration(ParameterDeclaration::Type::INT, $2);
        }
        else {
            $2->_identifier->type = Symbol::Type::FLOAT;
            $$ = new ParameterDeclaration(ParameterDeclaration::Type::FLOAT, $2);
        }
    }
;

declaration:
    type_specifier init_declarator_plus T_SEMICOLON
    {
        bisonout << "declaration -> type_specifier init_declarator_plus ;" << std::endl;

        // Copy Declaration from init_declarator_plus to declaration
        $$ = $2;

        // Get type_specifier
        std::string type($1);

        // Free memory of type_specifier
        free($1);

        // Set type of Declaration
        if(type == "char") {
            $$->_type = Declaration::Type::CHAR;
        }
        else if(type == "int") {
            $$->_type = Declaration::Type::INT;
        }
        else {
            $$->_type = Declaration::Type::FLOAT;
        }
        
        for(auto &init_decl: $$->_init_declarators){
            if(init_decl->_assignment_expr == nullptr) {
                Symbol* identifier = init_decl->_declarator->_identifier;

                if(init_decl->_declarator->_type == Declarator::IDENTIFIER) {
                    if(type == "char") {
                        identifier->type = Symbol::Type::CHAR;
                    }
                    else if(type == "int") {
                        identifier->type = Symbol::Type::INT;
                    }
                    else {
                        identifier->type = Symbol::Type::FLOAT;
                    }
                }
                else if(init_decl->_declarator->_type == Declarator::ONE_D_ARRAY) {
                    int dim = identifier->firstDimension;

                    if(type == "char") {
                        identifier->type = Symbol::Type::ARR_CHAR;

                        char* array = (char*) malloc(dim * sizeof(char));
                        identifier->val = std::to_string((uint64_t) array);
                    }
                    else if(type == "int") {
                        identifier->type = Symbol::Type::ARR_INT;
                        
                        int* array = (int*) malloc(dim * sizeof(int));
                        identifier->val = std::to_string((uint64_t) array);
                    }
                    else {
                        identifier->type = Symbol::Type::ARR_FLOAT;

                        float* array = (float*) malloc(dim * sizeof(float));
                        identifier->val = std::to_string((uint64_t) array);
                    }
                }
                else {
                    int dim1 = identifier->firstDimension;
                    int dim2 = identifier->secondDimension;

                    if(type == "char") {
                        identifier->type = Symbol::Type::ARR_ARR_CHAR;

                        char** array = (char**) malloc(dim1 * sizeof(char*));
                        for(int i = 0; i < dim1; ++i) {
                            array[i] = (char*) malloc(dim2 * sizeof(char));
                        }
                        identifier->val = std::to_string((uint64_t) array);
                    }
                    else if(type == "int") {
                        identifier->type = Symbol::Type::ARR_ARR_INT;
                        
                        int** array = (int**) malloc(dim1 * sizeof(int*));
                        for(int i = 0; i < dim1; ++i) {
                            array[i] = (int*) malloc(dim2 * sizeof(int));
                        }
                        identifier->val = std::to_string((uint64_t) array);
                    }
                    else {
                        identifier->type = Symbol::Type::ARR_ARR_FLOAT;
                        
                        float** array = (float**) malloc(dim1 * sizeof(float*));
                        for(int i = 0; i < dim1; ++i) {
                            array[i] = (float*) malloc(dim2 * sizeof(float));
                        }
                        identifier->val = std::to_string((uint64_t) array);
                    }
                }
            }
            else {
                // In here, we have type_specifier declarator = assignment_expression;
                // We need to check if type_specifier and assignment_expression are compatible

                // Get the symbol corresponding to the declarator
                Symbol* identifier = init_decl->_declarator->_identifier;

                // Type of assignment_expression
                ExpressionResult::Type rhs = init_decl->_assignment_expr->type();
                
                if(type == "char" and rhs == ExpressionResult::Type::CHAR) {
                    // Assign char to declarator
                    identifier->type = Symbol::Type::CHAR;
                }
                else if(type == "int" and rhs == ExpressionResult::Type::INT) {
                    // Assign int to declarator
                    identifier->type = Symbol::Type::INT;
                }
                else if(type == "float") {
                    // Assign float to declarator
                    if(rhs == ExpressionResult::Type::INT or rhs == ExpressionResult::Type::FLOAT) {
                        identifier->type = Symbol::Type::FLOAT;
                    }
                    else {
                        error("Type mismatch in variable initialization (float vs char) @ line %d\n", yylineno);
                    }
                }
                else {
                    error("Type mismatch in variable initialization (%s vs %s) @ line %d\n", type.c_str(), expressionResultTypeToStr(rhs), yylineno);
                }
            }
        }
    }
;

init_declarator_plus:
    init_declarator_plus T_COMMA init_declarator
    {
        bisonout << "init_declarator_plus -> init_declarator_plus, init_declarator" << std::endl;

        // Append init_declarator to Declaration
        $$ = $1;
        $$->_init_declarators.push_back($3);
    }
|
    init_declarator
    {
        bisonout << "init_declarator_plus -> init_declarator" << std::endl;

        // Create Declaration from init_declarator
        $$ = new Declaration();
        $$->_init_declarators.push_back($1);
    }
;

init_declarator:
    declarator
    {
        bisonout << "init_declarator -> declarator" << std::endl;
        
        // Check if declarator is a function
        if($1->_type == Declarator::Type::FUNCTION){
            error("Cannot have function in declaration @ line %d\n", yylineno);
        }
        
        // Create InitDeclarator from declarator
        $$ = new InitDeclarator($1);
    }
|
    declarator T_OP_EQ assignment_expression
    {
        bisonout << "init_declarator -> declarator = assignment_expression" << std::endl;
        
        // Check if declarator is a primitive datatype identifier
        if($1->_type != Declarator::Type::IDENTIFIER){
            error("Cannot assign value to array or function @ line %d\n", yylineno);
        }

        // Create InitDeclarator from declarator = assignment_expression
        $$ = new InitDeclarator($1, $3);
    }
;

/*
    Remove initializer completely
*/

compound_statement:
    T_LBRACE
    {
        // Get scope name for compound_statement scope
        std::string scopeName = currentST->name + ".cs" + std::to_string(currentST->compoundStatements.size());

        // Create new symbol table for compound_statement
        SymbolTable* newST = new SymbolTable(scopeName, currentST);

        // Insert new scope symbol table into current symbol table
        currentST->compoundStatements.push_back(newST);

        // Set the current scope to the new scope
        currentST = newST;
    }
    declaration_or_statement_star T_RBRACE
    {
        bisonout << "compound_statement -> { declaration_or_statement_star }" << std::endl;
        
        // Set the symbol table of the CompoundStatement to the current symbol table
        // which is the symbol table of the compound_statement scope
        $3->_scope = currentST;

        // Restore scope to parent scope
        currentST = currentST->parent;
        
        // Copy CompoundStatement from declaration_or_statement_star to compound_statement
        $$ = $3;
    }
;

declaration_or_statement_star:
    declaration_or_statement_star declaration
    {
        bisonout << "declaration_or_statement_star -> declaration_or_statement_star declaration" << std::endl;

        // Append declaration to CompoundStatement
        $$ = $1;
        $$->addDeclaration($2);
    }
|
    declaration_or_statement_star statement
    {
        bisonout << "declaration_or_statement_star -> declaration_or_statement_star declaration" << std::endl;

        // Append statement to CompoundStatement
        $$ = $1;
        $$->addStatement($2);
    }
|
    %empty
    {
        bisonout << "declaration_or_statement_star -> ϵ" << std::endl;
        
        // Create CompoundStatement with no declarations or statements
        $$ = new CompoundStatement();
    }
;

statement:
    labeled_statement
    {
        bisonout << "statement -> labeled_statement" << std::endl;

        // Create Statement from labeled_statement
        $$ = new Statement($1);
    }
|
    expression_statement
    {
        bisonout << "statement -> expression_statement" << std::endl;

        // Create Statement from expression_statement
        $$ = new Statement($1);
    }
|
    compound_statement
    {
        bisonout << "statement -> compound_statement" << std::endl;

        // Create Statement from compound_statement
        $$ = new Statement($1);
    }
|
    selection_statement
    {
        bisonout << "statement -> selection_statement" << std::endl;

        // Create Statement from selection_statement
        $$ = new Statement($1);
    }
|
    iteration_statement
    {
        bisonout << "statement -> iteration_statement" << std::endl;

        // Create Statement from iteration_statement
        $$ = new Statement($1);
    }
|
    jump_statement
    {
        bisonout << "statement -> jump_statement" << std::endl;

        // Create Statement from jump_statement
        $$ = new Statement($1);
    }
|
    printf_statement
    {
        bisonout << "statement -> printf_statement" << std::endl;

        // Create Statement from printf_statement
        $$ = new Statement($1);
    }
;

labeled_statement:
    T_KEYWORD_CASE expression T_COLON statement
    {
        bisonout << "labeled_statement -> case expression : statement" << std::endl;

        // Create LabelStatement from case expression : statement
        $$ = new LabelStatement($2, $4);
    }
|
    T_KEYWORD_DEFAULT T_COLON statement
    {
        bisonout << "labeled_statement -> default : statement" << std::endl;

        // Create LabelStatement from default : statement
        $$ = new LabelStatement($3);
    }
;

expression_statement:
    expression T_SEMICOLON
    {
        bisonout << "expression_statement -> expression ;" << std::endl;

        // Create ExpressionStatement from expression
        $$ = new ExpressionStatement($1);
    }
|
    T_SEMICOLON
    {
        bisonout << "expression_statement -> ;" << std::endl;

        // Create ExpressionStatement with no expression
        $$ = new ExpressionStatement();
    }
;

selection_statement:
    T_KEYWORD_IF T_LPARAN expression T_RPARAN statement %prec THEN
    {
        bisonout << "selection_statement -> if ( expression ) statement" << std::endl;

        // Create SelectionStatement from if ( expression ) statement
        $$ = new SelectionStatement(SelectionStatement::Type::IF, $3, $5);
    }
|
    T_KEYWORD_IF T_LPARAN expression T_RPARAN statement T_KEYWORD_ELSE statement
    {
        bisonout << "selection_statement -> if ( expression ) statement else statement" << std::endl;

        // Create SelectionStatement from if ( expression ) statement else statement
        $$ = new SelectionStatement($3, $5, $7);
    }
|
    T_KEYWORD_SWITCH T_LPARAN expression T_RPARAN
    {
        // Get return type of expression
        ExpressionResult::Type expr_type = $3->type();

        // Check if expression is of type float
        if (expr_type == ExpressionResult::Type::FLOAT) {
            error("Switch expression cannot be of type float @ line %d\n", yylineno);
        }
    }
    statement
    {
        bisonout << "selection_statement -> switch ( expression ) statement" << std::endl;

        // Get the type of the switch expression
        ExpressionResult::Type expr_type = $3->type();

        // Make sure that _stmt_1 is a compound statement
        if($6->_compound_stmt != nullptr){
            // Make sure that any case expressions are of the same type as the switch expression and also non float
            for(auto &[type, statement]: $6->_compound_stmt->_body){
                if(type == CompoundStatement::Type::STATEMENT){
                    Statement* stmt = static_cast<Statement*>(statement);
                    if(stmt->_label_stmt != nullptr and stmt->_label_stmt->_type == LabelStatement::Type::CASE) {
                        // Get the type of the case expression
                        ExpressionResult::Type case_expr_type = stmt->_label_stmt->_expr->type();

                        // Check if case expression is of type float
                        if (case_expr_type == ExpressionResult::Type::FLOAT) {
                            error("Case expression cannot be of type float\n");
                        }

                        // Check if case expression is of the same type as the switch expression
                        if (case_expr_type != expr_type) {
                            error("Case expression must be of the same type as the switch expression\n");
                        }
                    }
                }
            }
        }

        // Create SelectionStatement from switch ( expression ) statement
        $$ = new SelectionStatement(SelectionStatement::Type::SWITCH, $3, $6);
    }
;

iteration_statement:
    T_KEYWORD_WHILE T_LPARAN expression T_RPARAN statement
    {
        bisonout << "iteration_statement -> while ( expression ) statement" << std::endl;

        // Create IterationStatement from while ( expression ) statement
        $$ = new IterationStatement($3, $5);
    }
|
    T_KEYWORD_FOR T_LPARAN
    {
        // Get scope name for for_loop scope
        std::string scopeName = currentST->name + ".for" + std::to_string(currentST->forLoops.size());

        // Create new symbol table for compound_statement
        SymbolTable* newST = new SymbolTable(scopeName, currentST);

        // Insert new scope symbol table into current symbol table
        currentST->forLoops.push_back(newST);

        // Set the current scope to the new scope
        currentST = newST;
    }
    for_loop_body T_RPARAN statement
    {
        bisonout << "iteration_statement -> for ( for_loop_body ) statement" << std::endl;
        
        // Set the for loop scope to the current symbol table scope which happens to be the for loop scope
        $4->_scope = currentST;

        // Restore scope to parent scope
        currentST = currentST->parent;
        
        // Create IterationStatement from for ( for_loop_body ) statement
        $$ = new IterationStatement($4, $6);
    }
;

for_loop_body:
    expression_statement expression_statement
    {
        bisonout << "for_loop_body -> expression_statement expression_statement" << std::endl;

        // Create ForLoopBody from expression_statement expression_statement
        $$ = new ForLoopBody($1, $2);
    }
|
    expression_statement expression_statement expression
    {
        bisonout << "for_loop_body -> expression_statement expression_statement expression" << std::endl;

        // Create ForLoopBody from expression_statement expression_statement expression
        $$ = new ForLoopBody($1, $2, $3);
    }
|
    declaration expression_statement
    {
        bisonout << "for_loop_body -> declaration expression_statement" << std::endl;

        // Create ForLoopBody from declaration expression_statement
        $$ = new ForLoopBody($1, $2);
    }
|
    declaration expression_statement expression
    {
        bisonout << "for_loop_body -> declaration expression_statement expression" << std::endl;

        // Create ForLoopBody from declaration expression_statement expression
        $$ = new ForLoopBody($1, $2, $3);
    }
;

jump_statement:
    T_KEYWORD_CONTINUE T_SEMICOLON
    {
        bisonout << "jump_statement -> continue ;" << std::endl;

        // Create JumpStatement from continue ;
        $$ = new JumpStatement(JumpStatement::Type::CONTINUE);
    }
|
    T_KEYWORD_BREAK T_SEMICOLON
    {
        bisonout << "jump_statement -> break ;" << std::endl;

        // Create JumpStatement from break ;
        $$ = new JumpStatement(JumpStatement::Type::BREAK);
    }
|
    T_KEYWORD_RETURN expression T_SEMICOLON
    {
        bisonout << "jump_statement -> return expression ;" << std::endl;

        // Create JumpStatement from return expression ;
        $$ = new JumpStatement($2);
    }
;

printf_statement:
    T_KEYWORD_PRINTF T_LPARAN T_CONSTANT_STRING T_RPARAN T_SEMICOLON
    {
        bisonout << "printf_statement -> printf ( T_CONSTANT_STRING ) ;" << std::endl;
        
        // Get printf format string
        std::string formatString($3);

        // Since there is no argument list, there should be no %c, %d or %f in the format string
        if (formatString.find("%c") != std::string::npos or formatString.find("%d") != std::string::npos or formatString.find("%f") != std::string::npos) {
            error("printf format string contains %%c, %%d or %%f but no argument list was provided @ line %d\n", yylineno);  
        }
        
        // Create PrintfStatement from printf ( T_CONSTANT_STRING ) ;
        $$ = new PrintfStatement(formatString);
    }
|
    T_KEYWORD_PRINTF T_LPARAN T_CONSTANT_STRING T_COMMA argument_list T_RPARAN T_SEMICOLON
    {
        bisonout << "printf_statement -> printf ( T_CONSTANT_STRING, argument_list ) ;" << std::endl;
        
        // Get printf format string
        std::string formatString($3);

        // Stores the total number of occurences of %c, %d and %f in the format string
        int tot = 0;

        // Count the number of occurences of "%c" in the format string
        std::string::size_type pos = 0;
        std::string target = "%c";
        while ((pos = formatString.find(target, pos)) != std::string::npos) {
            ++tot;
            pos += target.length();
        }

        // Count the number of occurences of "%d" in the format string
        pos = 0;
        target = "%d";
        while ((pos = formatString.find(target, pos)) != std::string::npos) {
            ++tot;
            pos += target.length();
        }

        // Count the number of occurences of "%f" in the format string
        pos = 0;
        target = "%f";
        while ((pos = formatString.find(target, pos)) != std::string::npos) {
            ++tot;
            pos += target.length();
        }

        // Get the arugment_list in std::list form
        std::list<AssignmentExpression*> arguments = $5->_arguments;

        // Check if tot != size of argument list
        if (tot != arguments.size()) {
            error("printf format string contains %d %%c, %%d or %%f but %d argument(s) were provided @ line %d\n", tot, arguments.size(), yylineno);
        }

        // Check if each format specifier is compatible with the type of the corresponding argument
        pos = 0;
        int argCount = 1;

        for(auto &arg: arguments){
            while(formatString[pos] != '%'){
                ++pos;
            }
            ++pos;

            ExpressionResult::Type argType = arg->type();

            if(formatString[pos] == 'c'){
                if(argType != ExpressionResult::Type::CHAR){
                    error("printf format string argument %d is not a char @ line %d\n", argCount, yylineno);
                }
            }
            else if(formatString[pos] == 'd'){
                if(argType != ExpressionResult::Type::INT){
                    error("printf format string argument %d is not an int @ line %d\n", argCount, yylineno);
                }
            }
            else if(formatString[pos] == 'f'){
                if(argType != ExpressionResult::Type::FLOAT){
                    error("printf format string argument %d is not a float @ line %d\n", argCount, yylineno);
                }
            }
            else {
                error("printf format string argument %d is not a %%c, %%d or %%f @ line %d\n", argCount, yylineno);
            }

            ++argCount;
        }
        
        // Create PrintfStatement from printf ( T_CONSTANT_STRING, argument_list ) ;
        $$ = new PrintfStatement(formatString, $5);
    }
;

%%

int yyerror(const char* s) {
    std::cout << s << std::endl;
    exit(-1);
}

int error(const char* err, ...) {
    std::cout << "Error: ";
    va_list args;
    va_start(args, err);
    vfprintf(stdout, err, args);
    va_end(args);
    exit(-1);
}
