#include <ast/ast.hpp>
#include <iostream>

// Declarations from the lexer
extern int yylineno;

// Declarations from the parser
int error(const char* err, ...);

// Declarations from main
extern SymbolTable* currentST;

// Variable to store all the external declarations (used during execution to lookup AST node for function call)
std::list<FunctionDefinition*> function_table;

// Function to get pointer to AST node corresponding to functionName
FunctionDefinition* getFunctionNode(std::string functionName) {
    for (auto& func : function_table) {
        if (func->_declarator->_identifier->name == functionName) return func;
    }
    return nullptr;
}

// Variable to store count of temporary variables in a function scope
int temp_count = 0;

// Function to return a new temporary variable
std::string genTemp() {
    return "t" + std::to_string(temp_count++);
}

// ExpressionResult
bool operator==(const ExpressionResult& lhs, const ExpressionResult& rhs) {
    if (lhs._type == ExpressionResult::CHAR) {
        return lhs._val.charVal == rhs._val.charVal;
    } else if (lhs._type == ExpressionResult::INT) {
        return lhs._val.intVal == rhs._val.intVal;
    } else if (lhs._type == ExpressionResult::FLOAT) {
        return lhs._val.floatVal == rhs._val.floatVal;
    } else {
        error("Cannot compare char* @ line %d\n", yylineno);
        return lhs._val.strVal == rhs._val.strVal;    // dummy comparison to stop warnings
    }
}

bool operator!=(const ExpressionResult& lhs, const ExpressionResult& rhs) {
    return !operator==(lhs, rhs);
}

bool operator<(const ExpressionResult& lhs, const ExpressionResult& rhs) {
    if (lhs._type == ExpressionResult::CHAR) {
        return lhs._val.charVal < rhs._val.charVal;
    } else if (lhs._type == ExpressionResult::INT) {
        return lhs._val.intVal < rhs._val.intVal;
    } else if (lhs._type == ExpressionResult::FLOAT) {
        return lhs._val.floatVal < rhs._val.floatVal;
    } else {
        error("Cannot compare char* @ line %d\n", yylineno);
        return lhs._val.strVal < rhs._val.strVal;    // dummy comparison to stop warnings
    }
}

bool operator>(const ExpressionResult& lhs, const ExpressionResult& rhs) {
    return operator<(rhs, lhs);
}

bool operator<=(const ExpressionResult& lhs, const ExpressionResult& rhs) {
    return !operator>(lhs, rhs);
}

bool operator>=(const ExpressionResult& lhs, const ExpressionResult& rhs) {
    return !operator<(lhs, rhs);
}

ExpressionResult operator+(ExpressionResult lhs, const ExpressionResult& rhs) {
    ExpressionResult ret;
    if (lhs._type == ExpressionResult::CHAR) {
        ret._type = ExpressionResult::CHAR;
        ret._val.charVal = lhs._val.charVal + rhs._val.charVal;
    } else if (lhs._type == ExpressionResult::INT) {
        if (rhs._type == ExpressionResult::INT) {
            ret._type = ExpressionResult::INT;
            ret._val.intVal = lhs._val.intVal + rhs._val.intVal;
        } else {
            ret._type = ExpressionResult::FLOAT;
            ret._val.floatVal = lhs._val.intVal + rhs._val.floatVal;
        }
    } else {
        ret._type = ExpressionResult::FLOAT;
        if (rhs._type == ExpressionResult::INT) {
            ret._val.floatVal = lhs._val.floatVal + rhs._val.intVal;
        } else {
            ret._val.floatVal = lhs._val.floatVal + rhs._val.floatVal;
        }
    }
    return ret;
}

ExpressionResult operator-(ExpressionResult lhs, const ExpressionResult& rhs) {
    ExpressionResult ret;
    if (lhs._type == ExpressionResult::CHAR) {
        ret._type = ExpressionResult::CHAR;
        ret._val.charVal = lhs._val.charVal - rhs._val.charVal;
    } else if (lhs._type == ExpressionResult::INT) {
        if (rhs._type == ExpressionResult::INT) {
            ret._type = ExpressionResult::INT;
            ret._val.intVal = lhs._val.intVal - rhs._val.intVal;
        } else {
            ret._type = ExpressionResult::FLOAT;
            ret._val.floatVal = lhs._val.intVal - rhs._val.floatVal;
        }
    } else {
        ret._type = ExpressionResult::FLOAT;
        if (rhs._type == ExpressionResult::INT) {
            ret._val.floatVal = lhs._val.floatVal - rhs._val.intVal;
        } else {
            ret._val.floatVal = lhs._val.floatVal - rhs._val.floatVal;
        }
    }
    return ret;
}

ExpressionResult operator*(ExpressionResult lhs, const ExpressionResult& rhs) {
    ExpressionResult ret;
    if (lhs._type == ExpressionResult::CHAR) {
        ret._type = ExpressionResult::CHAR;
        ret._val.charVal = lhs._val.charVal * rhs._val.charVal;
    } else if (lhs._type == ExpressionResult::INT) {
        if (rhs._type == ExpressionResult::INT) {
            ret._type = ExpressionResult::INT;
            ret._val.intVal = lhs._val.intVal * rhs._val.intVal;
        } else {
            ret._type = ExpressionResult::FLOAT;
            ret._val.floatVal = lhs._val.intVal * rhs._val.floatVal;
        }
    } else {
        ret._type = ExpressionResult::FLOAT;
        if (rhs._type == ExpressionResult::INT) {
            ret._val.floatVal = lhs._val.floatVal * rhs._val.intVal;
        } else {
            ret._val.floatVal = lhs._val.floatVal * rhs._val.floatVal;
        }
    }
    return ret;
}

ExpressionResult operator/(ExpressionResult lhs, const ExpressionResult& rhs) {
    ExpressionResult ret;
    if (lhs._type == ExpressionResult::CHAR) {
        ret._type = ExpressionResult::CHAR;
        ret._val.charVal = lhs._val.charVal / rhs._val.charVal;
    } else if (lhs._type == ExpressionResult::INT) {
        if (rhs._type == ExpressionResult::INT) {
            ret._type = ExpressionResult::INT;
            ret._val.intVal = lhs._val.intVal / rhs._val.intVal;
        } else {
            ret._type = ExpressionResult::FLOAT;
            ret._val.floatVal = lhs._val.intVal / rhs._val.floatVal;
        }
    } else {
        ret._type = ExpressionResult::FLOAT;
        if (rhs._type == ExpressionResult::INT) {
            ret._val.floatVal = lhs._val.floatVal / rhs._val.intVal;
        } else {
            ret._val.floatVal = lhs._val.floatVal / rhs._val.floatVal;
        }
    }
    return ret;
}

ExpressionResult operator%(ExpressionResult lhs, const ExpressionResult& rhs) {
    ExpressionResult ret;
    if (lhs._type == ExpressionResult::CHAR) {
        ret._type = ExpressionResult::CHAR;
        ret._val.charVal = lhs._val.charVal % rhs._val.charVal;
    } else {
        ret._type = ExpressionResult::INT;
        ret._val.intVal = lhs._val.intVal % rhs._val.intVal;
    }
    return ret;
}

const char* expressionResultTypeToStr(ExpressionResult::Type type) {
    switch (type) {
        case ExpressionResult::CHAR: {
            return "char";
        }
        case ExpressionResult::INT: {
            return "int";
        }
        case ExpressionResult::FLOAT: {
            return "float";
        }
        case ExpressionResult::STRING: {
            return "char*";
        }
    }
    return "undefined";
}

// TranslationUnit
void TranslationUnit::print(std::string& s) {
    s += "(tu ";
    for (auto& external_declaration : _external_declarations) {
        external_declaration->print(s);
    }
    s += ')';
}

QuadArray TranslationUnit::genCode() {
    QuadArray ret;
    for (auto& external_declaration : _external_declarations) {
        ret.append(external_declaration->genCode());
    }
    return ret;
}

void TranslationUnit::execute() {
    // Build the function table
    for (auto& external_declaration : _external_declarations) {
        if (external_declaration->_function_definition != nullptr) {
            function_table.push_back(external_declaration->_function_definition);
        }
    }

    // Execute all external declarations
    for (auto& external_declaration : _external_declarations) {
        external_declaration->execute();
    }
}

// ExternalDeclaration
ExternalDeclaration::ExternalDeclaration(FunctionDefinition* function_definition) {
    _function_definition = function_definition;
    _declaration = nullptr;
}

ExternalDeclaration::ExternalDeclaration(Declaration* declaration) {
    _declaration = declaration;
    _function_definition = nullptr;
}

void ExternalDeclaration::print(std::string& s) {
    s += "(ed ";
    if (_function_definition != nullptr) {
        _function_definition->print(s);
    } else {
        _declaration->print(s);
    }
    s += ')';
}

QuadArray ExternalDeclaration::genCode() {
    if (_function_definition != nullptr) {
        return _function_definition->genCode();
    } else {
        return _declaration->genCode();
    }
}

void ExternalDeclaration::execute() {
    if (_function_definition != nullptr) {
        if (_function_definition->_declarator->_identifier->name == "main") {
            _function_definition->execute();
        }
    } else {
        _declaration->execute();
    }
}

// FunctionDefinition
FunctionDefinition::FunctionDefinition(ReturnType return_type, Declarator* declarator, CompoundStatement* compound_statement) {
    _return_type = return_type;
    _declarator = declarator;
    _compound_stmt = compound_statement;
}

void FunctionDefinition::print(std::string& s) {
    s += "(fd ";
    if (_return_type == ReturnType::CHAR) {
        s += "char ";
    } else if (_return_type == ReturnType::INT) {
        s += "int ";
    } else {
        s += "float ";
    }
    _declarator->print(s);
    _compound_stmt->print(s);
    s += ')';
}

QuadArray FunctionDefinition::genCode() {
    // Store the old value of temp_count
    int old_temp_count = temp_count;

    // Set new value of temp_count as 0
    temp_count = 0;

    QuadArray ret;
    ret.append(Quad(Quad::Operator::FUNCTION_BEGIN, _declarator->_identifier->name));
    for (auto& [type, statement] : _compound_stmt->_body) {
        if (type == CompoundStatement::DECLARATION) {
            ret.append((static_cast<Declaration*>(statement))->genCode());
        } else {
            Statement* stmt = static_cast<Statement*>(statement);
            if (stmt->_jump_stmt != nullptr and stmt->_jump_stmt->_type == JumpStatement::RETURN) {
                ret.append(stmt->_jump_stmt->_expr->genCode());
                ret.append(Quad(Quad::Operator::RETURN, stmt->_jump_stmt->_expr->_var));
            } else {
                ret.append(stmt->genCode());
            }
        }
    }
    ret.append(Quad(Quad::Operator::FUNCTION_END));

    // Restore the old value of temp_count
    temp_count = old_temp_count;

    return ret;
}

ExpressionResult FunctionDefinition::execute() {
    // Store the old scope for restoration later
    SymbolTable* oldST = currentST;

    // Set the default return of the function based on the return type
    ExpressionResult ret;
    if (_return_type == CHAR) {
        ret._type = ExpressionResult::CHAR;
        ret._val.charVal = 0;
    } else if (_return_type == INT) {
        ret._type = ExpressionResult::INT;
        ret._val.intVal = 0;
    } else {
        ret._type = ExpressionResult::FLOAT;
        ret._val.floatVal = 0;
    }

    // Manually switch scope to that of the compound statement
    currentST = _compound_stmt->_scope;

    for (auto& [statementType, statement] : _compound_stmt->_body) {
        if (statementType == CompoundStatement::DECLARATION) {
            static_cast<Declaration*>(statement)->execute();
        } else {
            Statement* stmt = static_cast<Statement*>(statement);
            if (stmt->_jump_stmt != nullptr and stmt->_jump_stmt->_type == JumpStatement::RETURN) {
                ret = stmt->_jump_stmt->_expr->eval();
                if (ret._type == ExpressionResult::CHAR and _return_type == CHAR) {
                    break;
                } else if (ret._type == ExpressionResult::INT and _return_type == INT) {
                    break;
                } else if (_return_type == FLOAT) {
                    if (ret._type == ExpressionResult::INT or ret._type == ExpressionResult::FLOAT) {
                        break;
                    } else {
                        error("Return type of function \"%s\" is float, but return value is %s", _declarator->_identifier->name.c_str(), expressionResultTypeToStr(ret._type));
                    }
                } else {
                    error("Return type of function \"%s\" is wrong, got %s\n", _declarator->_identifier->name.c_str(), expressionResultTypeToStr(ret._type));
                }
            } else {
                stmt->execute();
            }
        }
    }

    // Restore scope to scope before function definition
    currentST = oldST;

    // Return the result of the function
    return ret;
}

// Declaration
void Declaration::print(std::string& s) {
    s += "(dec ";
    s += "(ts ";
    if (_type == CHAR) {
        s += "char";
    } else if (_type == INT) {
        s += "int";
    } else {
        s += "float";
    }
    s += ')';
    for (auto& init_declarator : _init_declarators) {
        init_declarator->print(s);
    }
    s += ')';
}

QuadArray Declaration::genCode() {
    QuadArray ret;
    for (auto& init_declarator : _init_declarators) {
        ret.append(init_declarator->genCode());
    }
    return ret;
}

void Declaration::execute() {
    for (auto& init_declarator : _init_declarators) {
        init_declarator->execute();
    }
}

// Declarator
Declarator::Declarator(Symbol* identifier) {
    _type = IDENTIFIER;
    _identifier = identifier;
    _parameter_list = nullptr;
}

Declarator::Declarator(Symbol* identifier, int first_dimension) {
    _type = ONE_D_ARRAY;
    _identifier = identifier;
    _first_dimension = first_dimension;
    _parameter_list = nullptr;
}

Declarator::Declarator(Symbol* identifier, int first_dimension, int second_dimension) {
    _type = TWO_D_ARRAY;
    _identifier = identifier;
    _first_dimension = first_dimension;
    _second_dimension = second_dimension;
    _parameter_list = nullptr;
}

Declarator::Declarator(Symbol* identifier, ParameterList* parameter_list) {
    _type = FUNCTION;
    _identifier = identifier;
    _parameter_list = parameter_list;
}

void Declarator::print(std::string& s) {
    s += "(decl ";
    if (_type == IDENTIFIER) {
        s += "IDENTIFIER ";
    } else if (_type == ONE_D_ARRAY) {
        s += "1D_ARR ";
    } else if (_type == TWO_D_ARRAY) {
        s += "2D_ARR ";
    } else {
        s += "FUNCTION ";
    }
    s += _identifier->name;
    s += ')';
}

// CompoundStatement
void CompoundStatement::addDeclaration(Declaration* declaration) {
    _body.push_back(std::make_pair(Type::DECLARATION, declaration));
}

void CompoundStatement::addStatement(Statement* statement) {
    _body.push_back(std::make_pair(Type::STATEMENT, statement));
}

void CompoundStatement::print(std::string& s) {
    s += "(cs ";
    for (auto& [statementType, statement] : _body) {
        if (statementType == DECLARATION) {
            static_cast<Declaration*>(statement)->print(s);
        } else {
            static_cast<Statement*>(statement)->print(s);
        }
    }
    s += ')';
}

QuadArray CompoundStatement::genCode() {
    QuadArray ret;
    for (auto& [statementType, statement] : _body) {
        if (statementType == DECLARATION) {
            ret.append((static_cast<Declaration*>(statement))->genCode());
        } else {
            ret.append((static_cast<Statement*>(statement))->genCode());
        }
    }
    return ret;
}

void CompoundStatement::execute() {
    // Switch scope to compound statement scope
    SymbolTable* oldST = currentST;
    currentST = _scope;

    // Iterate over all statements in the compound statement and execute them
    for (auto& [statementType, statement] : _body) {
        if (statementType == DECLARATION) {
            static_cast<Declaration*>(statement)->execute();
        } else {
            static_cast<Statement*>(statement)->execute();
        }
    }

    // Restore the scope to the scope before the compound statement
    currentST = oldST;
}

// ParameterDeclaration
ParameterDeclaration::ParameterDeclaration(Type type, Declarator* declarator) {
    _type = type;
    _declarator = declarator;
}

// InitDeclarator
InitDeclarator::InitDeclarator(Declarator* declarator) {
    _declarator = declarator;
    _assignment_expr = nullptr;
}

InitDeclarator::InitDeclarator(Declarator* declarator, AssignmentExpression* assignment_expr) {
    _declarator = declarator;
    _assignment_expr = assignment_expr;
}

void InitDeclarator::print(std::string& s) {
    s += "(i_decl ";
    _declarator->print(s);
    if (_assignment_expr != nullptr) {
        _assignment_expr->print(s);
    }
    s += ')';
}

QuadArray InitDeclarator::genCode() {
    QuadArray ret;
    if (_assignment_expr != nullptr) {
        std::string var = _declarator->_identifier->name;
        ret.append(_assignment_expr->genCode());
        ret.append(Quad(Quad::Operator::ASSIGNMENT, _assignment_expr->_var, var));
    }
    return ret;
}

void InitDeclarator::execute() {
    if (_assignment_expr == nullptr) {
        // This is a declaration without an assignment, bison already handled assigning the type to the identifier
        return;
    }

    // We have already made sure that _declarator is a primitive datatype identifier
    // and we have type checked the _assignment_expr with the datatype from parent Declaration for this InitDeclarator

    // Therefore, all we need to do is set the value of the identifier from _assignment_expr

    ExpressionResult assignment_expr_result = _assignment_expr->eval();

    if (assignment_expr_result._type == ExpressionResult::CHAR) {
        _declarator->_identifier->val = std::string(1, assignment_expr_result._val.charVal);
    } else if (assignment_expr_result._type == ExpressionResult::INT) {
        _declarator->_identifier->val = std::to_string(assignment_expr_result._val.intVal);
    } else {
        if (assignment_expr_result._type == ExpressionResult::INT) {
            _declarator->_identifier->val = std::to_string(assignment_expr_result._val.intVal);
        } else {
            _declarator->_identifier->val = std::to_string(assignment_expr_result._val.floatVal);
        }
    }
}

// AssignmentExpression
AssignmentExpression::AssignmentExpression(LogicalORExpression* logical_or_expr) {
    _logical_or_expr = logical_or_expr;
    _postfix_expr = nullptr;
    _assignment_expr = nullptr;
}

AssignmentExpression::AssignmentExpression(PostfixExpression* postfix_expr, AssignmentExpression* assignment_expr) {
    _postfix_expr = postfix_expr;
    _assignment_expr = assignment_expr;
    _logical_or_expr = nullptr;
}

void AssignmentExpression::print(std::string& s) {
    s += "(ae ";
    if (_logical_or_expr != nullptr) {
        _logical_or_expr->print(s);
    } else {
        _postfix_expr->print(s);
        _assignment_expr->print(s);
    }
    s += ')';
}

ExpressionResult::Type AssignmentExpression::type() {
    if (_logical_or_expr != nullptr) {
        return _logical_or_expr->type();
    } else {
        // Check if _postfix_expr type is compatible with _assignment_expr type
        ExpressionResult::Type postfix_expr_type = _postfix_expr->type();
        ExpressionResult::Type assignment_expr_type = _assignment_expr->type();

        if (postfix_expr_type == ExpressionResult::CHAR) {
            if (assignment_expr_type != ExpressionResult::CHAR) {
                error("Type mismatch in assignment expression (char vs %s) @ line %d\n", expressionResultTypeToStr(assignment_expr_type), yylineno);
            }
        } else if (postfix_expr_type == ExpressionResult::INT) {
            if (assignment_expr_type != ExpressionResult::INT) {
                error("Type mismatch in assignment expression (int vs %s) @ line %d\n", expressionResultTypeToStr(assignment_expr_type), yylineno);
            }
        } else if (postfix_expr_type == ExpressionResult::FLOAT) {
            if (assignment_expr_type != ExpressionResult::FLOAT and assignment_expr_type != ExpressionResult::INT) {
                error("Type mismatch in assignment expression (float vs %s) @ line %d\n", expressionResultTypeToStr(assignment_expr_type), yylineno);
            }
        } else {
            error("Cannot assign value to char* @ line %d\n", yylineno);
        }

        return _postfix_expr->type();
    }
}

QuadArray AssignmentExpression::genCode() {
    QuadArray ret;
    if (_logical_or_expr != nullptr) {
        ret = _logical_or_expr->genCode();
        _var = _logical_or_expr->_var;
    } else {
        ret = _assignment_expr->genCode();
        _var = _assignment_expr->_var;
        std::string identifier = _postfix_expr->_primary_expr->_identifier->name;
        switch (_postfix_expr->_type) {
            case PostfixExpression::PRIMARY_EXPRESSION: {
                ret.append(Quad(Quad::Operator::ASSIGNMENT, _assignment_expr->_var, identifier));
                break;
            }
            case PostfixExpression::ONE_D_ARRAY_ACCESS: {
                ret.append(_postfix_expr->_expr_1->genCode());
                std::string dim = _postfix_expr->_expr_1->_var;
                int dataSize;
                if (_postfix_expr->_primary_expr->_identifier->type == Symbol::ARR_CHAR) {
                    dataSize = 1;
                } else {
                    dataSize = 4;
                }
                std::string offset = genTemp();
                ret.append(Quad(Quad::Operator::MUL, dim, std::to_string(dataSize), offset));
                ret.append(Quad(Quad::Operator::ARRAY_ASSIGNMENT, _assignment_expr->_var, offset, identifier));
                break;
            }
            case PostfixExpression::TWO_D_ARRAY_ACCESS: {
                ret.append(_postfix_expr->_expr_1->genCode());
                ret.append(_postfix_expr->_expr_2->genCode());
                std::string dim1 = _postfix_expr->_expr_1->_var;
                std::string dim2 = _postfix_expr->_expr_2->_var;
                int dataSize;
                if (_postfix_expr->_primary_expr->_identifier->type == Symbol::ARR_ARR_CHAR) {
                    dataSize = 1;
                } else {
                    dataSize = 4;
                }
                // Calculate offset of 2D array in row major order
                std::string offset = genTemp();
                ret.append(Quad(Quad::Operator::MUL, dim1, std::to_string(_postfix_expr->_primary_expr->_identifier->secondDimension), offset));
                ret.append(Quad(Quad::Operator::PLUS, offset, dim2, offset));
                ret.append(Quad(Quad::Operator::MUL, offset, std::to_string(dataSize), offset));
                ret.append(Quad(Quad::Operator::ARRAY_ASSIGNMENT, _assignment_expr->_var, offset, identifier));
                break;
            }
            default: {
                break;
            }
        }
    }
    return ret;
}

ExpressionResult AssignmentExpression::eval() {
    if (_logical_or_expr != nullptr) {
        return _logical_or_expr->eval();
    } else {
        // Variable to store final return from this function
        ExpressionResult ret;

        // First we evaluate _assignment_expr
        ExpressionResult assignment_expr_result = _assignment_expr->eval();

        // We know that _postfix_expr must be of the following types after semantic analysis:
        // 1. PRIMARY_EXPRESSION which is an identifier
        // 2. ONE_D_ARRAY_ACCESS
        // 3. TWO_D_ARRAY_ACCESS

        if (_postfix_expr->_type == PostfixExpression::PRIMARY_EXPRESSION) {
            // Get symbol of identifier associated with _postfix_expr
            Symbol* identifier = _postfix_expr->_primary_expr->_identifier;

            // Set the value of the identifier to the value of _assignment_expr
            if (identifier->type == Symbol::CHAR) {
                identifier->val = std::string(1, assignment_expr_result._val.charVal);
                ret = assignment_expr_result;
            } else if (identifier->type == Symbol::INT) {
                identifier->val = std::to_string(assignment_expr_result._val.intVal);
                ret = assignment_expr_result;
            } else {
                if (assignment_expr_result._type == ExpressionResult::INT) {
                    identifier->val = std::to_string(assignment_expr_result._val.intVal);
                } else {
                    identifier->val = std::to_string(assignment_expr_result._val.floatVal);
                }
                ret._type = ExpressionResult::FLOAT;
                ret._val.floatVal = std::stof(identifier->val);
            }
        } else if (_postfix_expr->_type == PostfixExpression::ONE_D_ARRAY_ACCESS) {
            // Get symbol of identifier associated with _postfix_expr
            Symbol* identifier = _postfix_expr->_primary_expr->_identifier;

            // Get the value of array access index denoted by _postfix_expr->_expr_1
            ExpressionResult dim = _postfix_expr->_expr_1->eval();

            // Check if dim is an integer
            if (dim._type != ExpressionResult::INT) {
                error("Array \"%s\" access index must be an integer\n", identifier->name.c_str());
            }

            // Make sure dim is within bounds of the array
            if (dim._val.intVal < 0 or dim._val.intVal >= identifier->firstDimension) {
                error("Array \"%s\" access index out of bounds\n", identifier->name.c_str());
            }

            // Get pointer of array associated with identifier
            uint64_t pointer = std::stoull(identifier->val);

            // Set the value of the array at index dim to the value of _assignment_expr
            if (identifier->type == Symbol::ARR_CHAR) {
                ((char*) pointer)[dim._val.intVal] = assignment_expr_result._val.charVal;
                ret = assignment_expr_result;
            } else if (identifier->type == Symbol::ARR_INT) {
                ((int*) pointer)[dim._val.intVal] = assignment_expr_result._val.intVal;
                ret = assignment_expr_result;
            } else {
                if (assignment_expr_result._type == ExpressionResult::INT) {
                    ((float*) pointer)[dim._val.intVal] = assignment_expr_result._val.intVal;
                } else {
                    ((float*) pointer)[dim._val.intVal] = assignment_expr_result._val.floatVal;
                }
                ret._type = ExpressionResult::FLOAT;
                ret._val.floatVal = ((float*) pointer)[dim._val.intVal];
            }
        } else {
            // This is a TWO_D_ARRAY_ACCESS

            // Get symbol of identifier associated with _postfix_expr
            Symbol* identifier = _postfix_expr->_primary_expr->_identifier;

            // Get the value of array access indices denoted by _postfix_expr->_expr_1
            ExpressionResult dim1 = _postfix_expr->_expr_1->eval();

            // Check if dim1 is an integer
            if (dim1._type != ExpressionResult::INT) {
                error("Array \"%s\" access index must be an integer\n", identifier->name.c_str());
            }

            // Make sure dim1 is within bounds of the array
            if (dim1._val.intVal < 0 or dim1._val.intVal >= identifier->firstDimension) {
                error("Array \"%s\" access index out of bounds\n", identifier->name.c_str());
            }

            // Get the value of array access indices denoted by _postfix_expr->_expr_2
            ExpressionResult dim2 = _postfix_expr->_expr_2->eval();

            // Check if dim2 is an integer
            if (dim2._type != ExpressionResult::INT) {
                error("Array \"%s\" access index must be an integer\n", identifier->name.c_str());
            }

            // Make sure dim2 is within bounds of the array
            if (dim2._val.intVal < 0 or dim2._val.intVal >= identifier->secondDimension) {
                error("Array \"%s\" access index out of bounds\n", identifier->name.c_str());
            }

            // Get pointer of array associated with identifier
            uint64_t pointer = std::stoull(identifier->val);

            // Set the value of the array at index dim to the value of _assignment_expr
            if (identifier->type == Symbol::ARR_ARR_CHAR) {
                ((char**) pointer)[dim1._val.intVal][dim2._val.intVal] = assignment_expr_result._val.charVal;
                ret = assignment_expr_result;
            } else if (identifier->type == Symbol::ARR_ARR_INT) {
                ((int**) pointer)[dim1._val.intVal][dim2._val.intVal] = assignment_expr_result._val.intVal;
                ret = assignment_expr_result;
            } else {
                if (assignment_expr_result._type == ExpressionResult::INT) {
                    ((float**) pointer)[dim1._val.intVal][dim2._val.intVal] = assignment_expr_result._val.intVal;
                } else {
                    ((float**) pointer)[dim1._val.intVal][dim2._val.intVal] = assignment_expr_result._val.floatVal;
                }
                ret._type = ExpressionResult::FLOAT;
                ret._val.floatVal = ((float**) pointer)[dim1._val.intVal][dim2._val.intVal];
            }
        }

        return ret;
    }
}

// LogicalORExpression
LogicalORExpression::LogicalORExpression(LogicalANDExpression* logical_and_expr) {
    _logical_and_expr = logical_and_expr;
    _logical_or_expr = nullptr;
}

LogicalORExpression::LogicalORExpression(LogicalORExpression* logical_or_expr, LogicalANDExpression* logical_and_expr) {
    _logical_or_expr = logical_or_expr;
    _logical_and_expr = logical_and_expr;
}

void LogicalORExpression::print(std::string& s) {
    s += "(loe ";
    if (_logical_or_expr != nullptr) {
        _logical_or_expr->print(s);
        s += "(||)";
        _logical_and_expr->print(s);
    } else {
        _logical_and_expr->print(s);
    }
    s += ')';
}

ExpressionResult::Type LogicalORExpression::type() {
    if (_logical_or_expr == nullptr) {
        return _logical_and_expr->type();
    } else {
        // Check if _logical_or_expr and _logical_and_expr types match INT
        if (_logical_or_expr->type() != ExpressionResult::INT or _logical_and_expr->type() != ExpressionResult::INT) {
            error("Type mismatch in || expression (%s vs %s) @ line %d\n", expressionResultTypeToStr(_logical_or_expr->type()), expressionResultTypeToStr(_logical_and_expr->type()), yylineno);
        }
        return ExpressionResult::INT;
    }
}

QuadArray LogicalORExpression::genCode() {
    QuadArray ret;
    if (_logical_or_expr == nullptr) {
        ret = _logical_and_expr->genCode();
        _var = _logical_and_expr->_var;
    } else {
        _var = genTemp();
        ret.append(_logical_or_expr->genCode());
        ret.append(_logical_and_expr->genCode());
        ret.append(Quad(Quad::Operator::LOGICAL_OR, _logical_or_expr->_var, _logical_and_expr->_var, _var));
    }
    return ret;
}

ExpressionResult LogicalORExpression::eval() {
    if (_logical_or_expr == nullptr) {
        return _logical_and_expr->eval();
    }

    ExpressionResult ret;
    ret._type = ExpressionResult::INT;
    ret._val.intVal = _logical_or_expr->eval()._val.intVal || _logical_and_expr->eval()._val.intVal;
    return ret;
}

// LogicalANDExpression
LogicalANDExpression::LogicalANDExpression(EqualityExpression* equality_expr) {
    _equality_expr = equality_expr;
    _logical_and_expr = nullptr;
}

LogicalANDExpression::LogicalANDExpression(LogicalANDExpression* logical_and_expr, EqualityExpression* equality_expr) {
    _logical_and_expr = logical_and_expr;
    _equality_expr = equality_expr;
}

void LogicalANDExpression::print(std::string& s) {
    s += "(lae ";
    if (_logical_and_expr != nullptr) {
        _logical_and_expr->print(s);
        s += "(&&)";
        _equality_expr->print(s);
    } else {
        _equality_expr->print(s);
    }
    s += ')';
}

ExpressionResult::Type LogicalANDExpression::type() {
    if (_logical_and_expr == nullptr) {
        return _equality_expr->type();
    } else {
        // Check if _logical_and_expr and _equality_expr types match INT
        if (_logical_and_expr->type() != ExpressionResult::INT or _equality_expr->type() != ExpressionResult::INT) {
            error("Type mismatch in && expression (%s vs %s) @ line %d\n", expressionResultTypeToStr(_logical_and_expr->type()), expressionResultTypeToStr(_equality_expr->type()), yylineno);
        }
        return ExpressionResult::INT;
    }
}

QuadArray LogicalANDExpression::genCode() {
    QuadArray ret;
    if (_logical_and_expr == nullptr) {
        ret = _equality_expr->genCode();
        _var = _equality_expr->_var;
    } else {
        _var = genTemp();
        ret.append(_logical_and_expr->genCode());
        ret.append(_equality_expr->genCode());
        ret.append(Quad(Quad::Operator::LOGICAL_AND, _logical_and_expr->_var, _equality_expr->_var, _var));
    }
    return ret;
}

ExpressionResult LogicalANDExpression::eval() {
    if (_logical_and_expr == nullptr) {
        return _equality_expr->eval();
    }

    ExpressionResult ret;
    ret._type = ExpressionResult::INT;
    ret._val.intVal = _logical_and_expr->eval()._val.intVal && _equality_expr->eval()._val.intVal;
    return ret;
}

// EqualityExpression
EqualityExpression::EqualityExpression(RelationalExpression* relational_expr) {
    _relational_expr = relational_expr;
    _equality_expr = nullptr;
}

EqualityExpression::EqualityExpression(EqualityExpression* equality_expr, Operator equality_op, RelationalExpression* relational_expr) {
    _equality_expr = equality_expr;
    _equality_op = equality_op;
    _relational_expr = relational_expr;
}

void EqualityExpression::print(std::string& s) {
    s += "(eqe ";
    if (_equality_expr != nullptr) {
        _equality_expr->print(s);
        if (_equality_op == EqualityExpression::Operator::EQUAL) {
            s += "(==)";
        } else {
            s += "(!=)";
        }
        _relational_expr->print(s);
    } else {
        _relational_expr->print(s);
    }
    s += ')';
}

ExpressionResult::Type EqualityExpression::type() {
    if (_equality_expr == nullptr) {
        return _relational_expr->type();
    } else {
        // Check if _equality_expr and _relational_expr types match
        if (_equality_expr->type() == _relational_expr->type()) {
            return ExpressionResult::INT;
        } else {
            error("Type mismatch in equality-expression (%s vs %s) @ line %d\n", expressionResultTypeToStr(_equality_expr->type()), expressionResultTypeToStr(_relational_expr->type()), yylineno);
            return ExpressionResult::STRING;    // dummy return to supress warnings, should never execute
        }
    }
}

QuadArray EqualityExpression::genCode() {
    QuadArray ret;
    if (_equality_expr == nullptr) {
        ret = _relational_expr->genCode();
        _var = _relational_expr->_var;
    } else {
        _var = genTemp();
        ret.append(_equality_expr->genCode());
        ret.append(_relational_expr->genCode());
        if (_equality_op == EQUAL) {
            ret.append(Quad(Quad::Operator::DOUBLE_EQUAL, _equality_expr->_var, _relational_expr->_var, _var));
        } else {
            ret.append(Quad(Quad::Operator::NOT_EQUAL, _equality_expr->_var, _relational_expr->_var, _var));
        }
    }
    return ret;
}

ExpressionResult EqualityExpression::eval() {
    if (_equality_expr == nullptr) {
        return _relational_expr->eval();
    }

    ExpressionResult ret;
    ret._type = ExpressionResult::INT;
    switch (_equality_op) {
        case EQUAL: {
            ret._val.intVal = _equality_expr->eval()._val.intVal == _relational_expr->eval()._val.intVal;
            break;
        }
        case NOT_EQUAL: {
            ret._val.intVal = _equality_expr->eval()._val.intVal != _relational_expr->eval()._val.intVal;
            break;
        }
    }
    return ret;
}

// RelationalExpression
RelationalExpression::RelationalExpression(AdditiveExpression* additive_expr) {
    _additive_expr = additive_expr;
    _relational_expr = nullptr;
}

RelationalExpression::RelationalExpression(RelationalExpression* relational_expr, Operator relational_op, AdditiveExpression* additive_expr) {
    _relational_expr = relational_expr;
    _relational_op = relational_op;
    _additive_expr = additive_expr;
}

void RelationalExpression::print(std::string& s) {
    s += "(re ";
    if (_relational_expr != nullptr) {
        _relational_expr->print(s);
        if (_relational_op == RelationalExpression::Operator::LESS) {
            s += "(<)";
        } else if (_relational_op == RelationalExpression::Operator::GREATER) {
            s += "(>)";
        } else if (_relational_op == RelationalExpression::Operator::LESS_EQUAL) {
            s += "(<=)";
        } else {
            s += "(>=)";
        }
        _additive_expr->print(s);
    } else {
        _additive_expr->print(s);
    }
    s += ')';
}

ExpressionResult::Type RelationalExpression::type() {
    if (_relational_expr == nullptr) {
        return _additive_expr->type();
    } else {
        // Check if _relational_expr and _additive_expr types match
        if (_relational_expr->type() == _additive_expr->type()) {
            return ExpressionResult::INT;
        } else {
            error("Type mismatch in relational expression (%s vs %s) @ line %d\n", expressionResultTypeToStr(_relational_expr->type()), expressionResultTypeToStr(_additive_expr->type()), yylineno);
            return ExpressionResult::STRING;    // dummy return to supress warnings, should never execute
        }
    }
}

QuadArray RelationalExpression::genCode() {
    QuadArray ret;
    if (_relational_expr == nullptr) {
        ret = _additive_expr->genCode();
        _var = _additive_expr->_var;
    } else {
        _var = genTemp();
        ret.append(_relational_expr->genCode());
        ret.append(_additive_expr->genCode());
        switch (_relational_op) {
            case LESS: {
                ret.append(Quad(Quad::Operator::LESS, _relational_expr->_var, _additive_expr->_var, _var));
                break;
            }
            case LESS_EQUAL: {
                ret.append(Quad(Quad::Operator::LESS_EQUAL, _relational_expr->_var, _additive_expr->_var, _var));
                break;
            }
            case GREATER: {
                ret.append(Quad(Quad::Operator::GREATER, _relational_expr->_var, _additive_expr->_var, _var));
                break;
            }
            case GREATER_EQUAL: {
                ret.append(Quad(Quad::Operator::GREATER_EQUAL, _relational_expr->_var, _additive_expr->_var, _var));
                break;
            }
        }
    }
    return ret;
}

ExpressionResult RelationalExpression::eval() {
    if (_relational_expr == nullptr) {
        return _additive_expr->eval();
    }

    ExpressionResult ret;
    ret._type = ExpressionResult::INT;
    switch (_relational_op) {
        case LESS: {
            ret._val.intVal = _relational_expr->eval() < _additive_expr->eval();
            break;
        }
        case LESS_EQUAL: {
            ret._val.intVal = _relational_expr->eval() <= _additive_expr->eval();
            break;
        }
        case GREATER: {
            ret._val.intVal = _relational_expr->eval() > _additive_expr->eval();
            break;
        }
        case GREATER_EQUAL: {
            ret._val.intVal = _relational_expr->eval() >= _additive_expr->eval();
            break;
        }
    }
    return ret;
}

// AdditiveExpression
AdditiveExpression::AdditiveExpression(MultiplicativeExpression* multiplicative_expr) {
    _multiplicative_expr = multiplicative_expr;
    _additive_expr = nullptr;
}

AdditiveExpression::AdditiveExpression(AdditiveExpression* additive_expr, Operator additive_op, MultiplicativeExpression* multiplicative_expr) {
    _additive_expr = additive_expr;
    _additive_op = additive_op;
    _multiplicative_expr = multiplicative_expr;
}

void AdditiveExpression::print(std::string& s) {
    s += "(adde ";
    if (_additive_expr != nullptr) {
        _additive_expr->print(s);
        if (_additive_op == AdditiveExpression::Operator::PLUS) {
            s += "(+)";
        } else {
            s += "(-)";
        }
        _multiplicative_expr->print(s);
    } else {
        _multiplicative_expr->print(s);
    }
    s += ')';
}

ExpressionResult::Type AdditiveExpression::type() {
    if (_additive_expr == nullptr) {
        return _multiplicative_expr->type();
    } else {
        // Check if _additive_expr type is compatible with _multiplicative_expr type
        ExpressionResult::Type additive_type = _additive_expr->type();
        ExpressionResult::Type multiplicative_type = _multiplicative_expr->type();

        if (additive_type == ExpressionResult::CHAR) {
            if (multiplicative_type != ExpressionResult::CHAR) {
                error("Type mismatch in additive expression (char vs %s) @ line %d\n", expressionResultTypeToStr(multiplicative_type), yylineno);
            } else {
                return ExpressionResult::CHAR;
            }
        } else if (additive_type == ExpressionResult::INT) {
            if (multiplicative_type == ExpressionResult::CHAR) {
                error("Type mismatch in additive expression (int vs char) @ line %d\n", yylineno);
            } else if (multiplicative_type == ExpressionResult::INT) {
                return ExpressionResult::INT;
            } else if (multiplicative_type == ExpressionResult::FLOAT) {
                return ExpressionResult::FLOAT;
            } else {
                error("Cannot use char* in additive expression @ line %d\n", yylineno);
            }
        } else if (additive_type == ExpressionResult::FLOAT) {
            if (multiplicative_type == ExpressionResult::CHAR) {
                error("Type mismatch in additive expression (float vs char) @ line %d\n", yylineno);
            } else if (multiplicative_type == ExpressionResult::INT) {
                return ExpressionResult::FLOAT;
            } else if (multiplicative_type == ExpressionResult::FLOAT) {
                return ExpressionResult::FLOAT;
            } else {
                error("Cannot use char* in additive expression @ line %d\n", yylineno);
            }
        } else {
            error("Cannot use char* in additive expression @ line %d\n", yylineno);
        }
        return ExpressionResult::STRING;    // dummy return to supress warnings, should never execute
    }
}

QuadArray AdditiveExpression::genCode() {
    QuadArray ret;
    if (_additive_expr == nullptr) {
        ret = _multiplicative_expr->genCode();
        _var = _multiplicative_expr->_var;
    } else {
        _var = genTemp();
        ret.append(_additive_expr->genCode());
        ret.append(_multiplicative_expr->genCode());
        switch (_additive_op) {
            case PLUS: {
                ret.append(Quad(Quad::Operator::PLUS, _additive_expr->_var, _multiplicative_expr->_var, _var));
                break;
            }
            case MINUS: {
                ret.append(Quad(Quad::Operator::MINUS, _additive_expr->_var, _multiplicative_expr->_var, _var));
                break;
            }
        }
    }
    return ret;
}

ExpressionResult AdditiveExpression::eval() {
    if (_additive_expr == nullptr) {
        return _multiplicative_expr->eval();
    } else {
        ExpressionResult additive_expr_result = _additive_expr->eval();
        ExpressionResult multiplicative_expr_result = _multiplicative_expr->eval();
        ExpressionResult ret;
        switch (_additive_op) {
            case PLUS: {
                ret = additive_expr_result + multiplicative_expr_result;
                break;
            }
            case MINUS: {
                ret = additive_expr_result - multiplicative_expr_result;
                break;
            }
        }
        return ret;
    }
}

// MultiplicativeExpression
MultiplicativeExpression::MultiplicativeExpression(UnaryExpression* unary_expr) {
    _unary_expr = unary_expr;
    _multiplicative_expr = nullptr;
}

MultiplicativeExpression::MultiplicativeExpression(MultiplicativeExpression* multiplicative_expr, Operator multiplicative_op, UnaryExpression* unary_expr) {
    _multiplicative_expr = multiplicative_expr;
    _multiplicative_op = multiplicative_op;
    _unary_expr = unary_expr;
}

void MultiplicativeExpression::print(std::string& s) {
    s += "(me ";
    if (_multiplicative_expr != nullptr) {
        _multiplicative_expr->print(s);
        if (_multiplicative_op == MultiplicativeExpression::Operator::MUL) {
            s += "(*)";
        } else if (_multiplicative_op == MultiplicativeExpression::Operator::DIV) {
            s += "(/)";
        } else {
            s += "(%)";
        }
        _unary_expr->print(s);
    } else {
        _unary_expr->print(s);
    }
    s += ')';
}

ExpressionResult::Type MultiplicativeExpression::type() {
    if (_multiplicative_expr == nullptr) {
        return _unary_expr->type();
    } else {
        ExpressionResult::Type multiplicative_type = _multiplicative_expr->type();
        ExpressionResult::Type unary_type = _unary_expr->type();

        if (_multiplicative_op == MOD) {
            if (multiplicative_type == ExpressionResult::CHAR) {
                if (unary_type == ExpressionResult::CHAR) {
                    return ExpressionResult::CHAR;
                } else {
                    error("Type mismatch in multiplicative expression (char vs %s) @ line %d\n", expressionResultTypeToStr(unary_type), yylineno);
                }
            } else if (multiplicative_type == ExpressionResult::INT) {
                if (unary_type == ExpressionResult::INT) {
                    return ExpressionResult::INT;
                } else {
                    error("Type mismatch in multiplicative expression (int vs %s) @ line %d\n", expressionResultTypeToStr(unary_type), yylineno);
                }
            } else {
                error("Type mismatch for %% operator, got %s @ line %d\n", expressionResultTypeToStr(multiplicative_type), yylineno);
            }
        } else {
            if (multiplicative_type == ExpressionResult::CHAR) {
                if (unary_type == ExpressionResult::CHAR) {
                    return ExpressionResult::CHAR;
                } else {
                    error("Type mismatch in multiplicative expression (char vs %s) @ line %d\n", expressionResultTypeToStr(unary_type), yylineno);
                }
            } else if (multiplicative_type == ExpressionResult::INT) {
                if (unary_type == ExpressionResult::INT) {
                    return ExpressionResult::INT;
                } else if (unary_type == ExpressionResult::FLOAT) {
                    return ExpressionResult::FLOAT;
                } else {
                    error("Type mismatch in multiplicative expression (int vs %s) @ line %d\n", expressionResultTypeToStr(unary_type), yylineno);
                }
            } else if (multiplicative_type == ExpressionResult::FLOAT) {
                if (unary_type == ExpressionResult::INT) {
                    return ExpressionResult::FLOAT;
                } else if (unary_type == ExpressionResult::FLOAT) {
                    return ExpressionResult::FLOAT;
                } else {
                    error("Type mismatch in multiplicative expression (int vs %s) @ line %d\n", expressionResultTypeToStr(unary_type), yylineno);
                }
            } else {
                error("Cannot use char* in multiplicative expression @ line %d\n", yylineno);
            }
        }
        return ExpressionResult::STRING;    // dummy return to supress warnings, should never execute
    }
}

QuadArray MultiplicativeExpression::genCode() {
    QuadArray ret;
    if (_multiplicative_expr == nullptr) {
        ret = _unary_expr->genCode();
        _var = _unary_expr->_var;
    } else {
        _var = genTemp();
        ret.append(_multiplicative_expr->genCode());
        ret.append(_unary_expr->genCode());
        switch (_multiplicative_op) {
            case MUL: {
                ret.append(Quad(Quad::Operator::MUL, _multiplicative_expr->_var, _unary_expr->_var, _var));
                break;
            }
            case DIV: {
                ret.append(Quad(Quad::Operator::DIV, _multiplicative_expr->_var, _unary_expr->_var, _var));
                break;
            }
            case MOD: {
                ret.append(Quad(Quad::Operator::MOD, _multiplicative_expr->_var, _unary_expr->_var, _var));
                break;
            }
        }
    }
    return ret;
}

ExpressionResult MultiplicativeExpression::eval() {
    if (_multiplicative_expr == nullptr) {
        return _unary_expr->eval();
    } else {
        ExpressionResult multiplicative_expr_result = _multiplicative_expr->eval();
        ExpressionResult unary_expr_result = _unary_expr->eval();
        ExpressionResult ret;

        switch (_multiplicative_op) {
            case MUL: {
                ret = multiplicative_expr_result * unary_expr_result;
                break;
            }
            case DIV: {
                ret = multiplicative_expr_result / unary_expr_result;
                break;
            }
            case MOD: {
                ret = multiplicative_expr_result % unary_expr_result;
                break;
            }
        }

        return ret;
    }
}

// UnaryExpression
UnaryExpression::UnaryExpression(PostfixExpression* postfix_expr) {
    _postfix_expr = postfix_expr;
    _unary_expr = nullptr;
}

UnaryExpression::UnaryExpression(Operator unary_op, UnaryExpression* unary_expr) {
    _unary_op = unary_op;
    _unary_expr = unary_expr;
    _postfix_expr = nullptr;
}

void UnaryExpression::print(std::string& s) {
    s += "(ue ";
    if (_postfix_expr != nullptr) {
        _postfix_expr->print(s);
    } else {
        if (_unary_op == UnaryExpression::Operator::PLUS) {
            s += "(+)";
        } else if (_unary_op == UnaryExpression::Operator::MINUS) {
            s += "(-)";
        } else {
            s += "(!)";
        }
        _unary_expr->print(s);
    }
    s += ')';
}

ExpressionResult::Type UnaryExpression::type() {
    if (_postfix_expr != nullptr) {
        return _postfix_expr->type();
    } else {
        if (_unary_op == EXCLAMATION) {
            // Check if _unary_expr type matches INT
            if (_unary_expr->type() == ExpressionResult::INT) {
                return ExpressionResult::INT;
            } else {
                error("Type mismatch in unary expression (%s) @ line %d\n", expressionResultTypeToStr(_unary_expr->type()), yylineno);
            }
        } else {
            return _unary_expr->type();
        }
        return ExpressionResult::STRING;    // dummy return to supress warnings, should never execute
    }
}

QuadArray UnaryExpression::genCode() {
    QuadArray ret;
    if (_postfix_expr != nullptr) {
        ret = _postfix_expr->genCode();
        _var = _postfix_expr->_var;
    } else {
        ret = _unary_expr->genCode();
        switch (_unary_op) {
            case PLUS: {
                _var = _unary_expr->_var;
                break;
            }
            case MINUS: {
                _var = genTemp();
                ret.append(Quad(Quad::Operator::MINUS, _unary_expr->_var, _var));
                break;
            }
            case EXCLAMATION: {
                _var = genTemp();
                ret.append(Quad(Quad::Operator::LOGICAL_NOT, _unary_expr->_var, _var));
                break;
            }
        }
    }
    return ret;
}

ExpressionResult UnaryExpression::eval() {
    if (_postfix_expr != nullptr) {
        return _postfix_expr->eval();
    } else {
        ExpressionResult unary_expr_result = _unary_expr->eval();
        switch (_unary_op) {
            case PLUS: {
                break;
            }
            case MINUS: {
                if (unary_expr_result._type == ExpressionResult::CHAR) {
                    unary_expr_result._val.charVal *= -1;
                } else if (unary_expr_result._type == ExpressionResult::INT) {
                    unary_expr_result._val.intVal *= -1;
                } else {
                    unary_expr_result._val.floatVal *= -1;
                }
                break;
            }
            case EXCLAMATION: {
                unary_expr_result._val.intVal = !unary_expr_result._val.intVal;
                break;
            }
        }
        return unary_expr_result;
    }
}

// PostfixExpression
PostfixExpression::PostfixExpression(PrimaryExpression* primary_expr) {
    _type = Type::PRIMARY_EXPRESSION;
    _primary_expr = primary_expr;
    _expr_1 = nullptr;
    _expr_2 = nullptr;
    _argument_list = nullptr;
}

PostfixExpression::PostfixExpression(PrimaryExpression* primary_expr, Expression* expr_1) {
    _type = Type::ONE_D_ARRAY_ACCESS;
    _primary_expr = primary_expr;
    _expr_1 = expr_1;
    _expr_2 = nullptr;
    _argument_list = nullptr;
}

PostfixExpression::PostfixExpression(PrimaryExpression* primary_expr, Expression* expr_1, Expression* expr_2) {
    _type = Type::TWO_D_ARRAY_ACCESS;
    _primary_expr = primary_expr;
    _expr_1 = expr_1;
    _expr_2 = expr_2;
    _argument_list = nullptr;
}

PostfixExpression::PostfixExpression(PrimaryExpression* primary_expr, ArgumentList* argument_list) {
    _type = Type::FUNCTION_CALL;
    _primary_expr = primary_expr;
    _argument_list = argument_list;
    _expr_1 = nullptr;
    _expr_2 = nullptr;
}

void PostfixExpression::print(std::string& s) {
    s += "(pofe ";
    switch (_type) {
        case PRIMARY_EXPRESSION: {
            s += "primary_expr ";
            break;
        }
        case ONE_D_ARRAY_ACCESS: {
            s += "1D_ARR_ACCESS ";
            break;
        }
        case TWO_D_ARRAY_ACCESS: {
            s += "2D_ARR_ACCESS ";
            break;
        }
        case FUNCTION_CALL: {
            s += "FUNC_CALL ";
            break;
        }
    }
    _primary_expr->print(s);
    s += ')';
}

ExpressionResult::Type PostfixExpression::type() {
    return _primary_expr->type();
}

QuadArray PostfixExpression::genCode() {
    QuadArray ret;
    switch (_type) {
        case PRIMARY_EXPRESSION: {
            ret = _primary_expr->genCode();
            _var = _primary_expr->_var;
            break;
        }
        case ONE_D_ARRAY_ACCESS: {
            _var = genTemp();
            std::string offset = genTemp();

            // Get identifier name corresponding to array
            Symbol* array = _primary_expr->_identifier;

            // Generate code for array access
            ret = _primary_expr->genCode();
            ret.append(_expr_1->genCode());

            std::string dim = _expr_1->_var;

            int dataSize;
            if (array->type == Symbol::Type::ARR_CHAR) {
                dataSize = 1;
            } else {
                dataSize = 4;
            }

            ret.append(Quad(Quad::Operator::MUL, dim, std::to_string(dataSize), offset));
            ret.append(Quad(Quad::Operator::ARRAY_ACCESS, array->name, offset, _var));

            break;
        }
        case TWO_D_ARRAY_ACCESS: {
            _var = genTemp();
            std::string offset = genTemp();

            // Get identifier name corresponding to array
            Symbol* array = _primary_expr->_identifier;

            // Generate code for array access
            ret = _primary_expr->genCode();
            ret.append(_expr_1->genCode());
            ret.append(_expr_2->genCode());

            std::string dim1 = _expr_1->_var;
            std::string dim2 = _expr_2->_var;

            int dataSize;
            if (array->type == Symbol::Type::ARR_ARR_CHAR) {
                dataSize = 1;
            } else {
                dataSize = 4;
            }

            ret.append(Quad(Quad::Operator::MUL, dim1, std::to_string(array->secondDimension), offset));
            ret.append(Quad(Quad::Operator::PLUS, offset, dim2, offset));
            ret.append(Quad(Quad::Operator::MUL, offset, std::to_string(dataSize), offset));
            ret.append(Quad(Quad::Operator::ARRAY_ACCESS, array->name, offset, _var));

            break;
        }
        case FUNCTION_CALL: {
            std::string funcName = _primary_expr->_identifier->name;
            if (_argument_list == nullptr) {
                // Directly call function
                ret.append(Quad(Quad::Operator::REFPARAM, "result"));
                ret.append(Quad(Quad::Operator::CALL, "1", funcName));
            } else {
                for (auto& arg : _argument_list->_arguments) {
                    ret.append(arg->genCode());
                }
                for (auto& arg : _argument_list->_arguments) {
                    ret.append(Quad(Quad::Operator::PARAM, arg->_var));
                }
                ret.append(Quad(Quad::Operator::REFPARAM, "result"));
                int argc = _argument_list->_arguments.size() + 1;
                ret.append(Quad(Quad::Operator::CALL, std::to_string(argc), funcName));
            }

            // Store the return from result into a temp variable
            _var = genTemp();
            ret.append(Quad(Quad::Operator::ASSIGNMENT, "result", _var));

            break;
        }
    }
    return ret;
}

ExpressionResult PostfixExpression::eval() {
    ExpressionResult ret;
    switch (_type) {
        case PRIMARY_EXPRESSION: {
            ret = _primary_expr->eval();
            break;
        }
        case ONE_D_ARRAY_ACCESS: {
            // Get identifier corresponding to array
            Symbol* array = _primary_expr->_identifier;

            // Get index of array access
            ExpressionResult dim = _expr_1->eval();

            // Check if dim is an integer
            if (dim._type != ExpressionResult::INT) {
                error("Array \"%s\" index must be an integer\n", array->name.c_str());
            }

            // Check if dim is within bounds of array
            if (dim._val.intVal < 0 or dim._val.intVal >= array->firstDimension) {
                error("Array \"%s\" index out of bounds\n", array->name.c_str());
            }

            // Get pointer corresponding to array
            uint64_t pointer = std::stoull(array->val);

            // Set the type and value of ret depending on array symbol
            if (array->type == Symbol::ARR_CHAR) {
                ret._type = ExpressionResult::CHAR;
                ret._val.charVal = ((char*) pointer)[dim._val.intVal];
            } else if (array->type == Symbol::ARR_INT) {
                ret._type = ExpressionResult::INT;
                ret._val.intVal = ((int*) pointer)[dim._val.intVal];
            } else {
                ret._type = ExpressionResult::FLOAT;
                ret._val.floatVal = ((float*) pointer)[dim._val.intVal];
            }

            break;
        }
        case TWO_D_ARRAY_ACCESS: {
            // Get identifier corresponding to array
            Symbol* array = _primary_expr->_identifier;

            // Get first index of array access
            ExpressionResult dim_1 = _expr_1->eval();

            // Check if dim_1 is an integer
            if (dim_1._type != ExpressionResult::INT) {
                error("Array \"%s\" index must be an integer\n", array->name.c_str());
            }

            // Make sure dim_1 is within bounds of array
            if (dim_1._val.intVal < 0 or dim_1._val.intVal >= array->firstDimension) {
                error("Array \"%s\" index out of bounds\n", array->name.c_str());
            }

            // Get second index of array access
            ExpressionResult dim_2 = _expr_2->eval();

            // Check if dim_2 is an integer
            if (dim_2._type != ExpressionResult::INT) {
                error("Array \"%s\" index must be an integer\n", array->name.c_str());
            }

            // Make sure dim_2 is within bounds of array
            if (dim_2._val.intVal < 0 or dim_2._val.intVal >= array->secondDimension) {
                error("Array \"%s\" index out of bounds\n", array->name.c_str());
            }

            // Get pointer corresponding to array
            uint64_t pointer = std::stoull(array->val);

            // Set the type and value of ret depending on array symbol
            if (array->type == Symbol::ARR_ARR_CHAR) {
                ret._type = ExpressionResult::CHAR;
                ret._val.charVal = ((char**) pointer)[dim_1._val.intVal][dim_2._val.intVal];
            } else if (array->type == Symbol::ARR_ARR_INT) {
                ret._type = ExpressionResult::INT;
                ret._val.intVal = ((int**) pointer)[dim_1._val.intVal][dim_2._val.intVal];
            } else {
                ret._type = ExpressionResult::FLOAT;
                ret._val.floatVal = ((float**) pointer)[dim_1._val.intVal][dim_2._val.intVal];
            }

            break;
        }
        case FUNCTION_CALL: {
            // We know that the semantic checking has made sure that all arguments are of the correct type
            // and that the count matches function parameter count

            // Get function AST node
            std::string functionName = _primary_expr->_identifier->name;
            FunctionDefinition* function = getFunctionNode(functionName);

            // Check if function has no arguments
            if (_argument_list == nullptr) {
                ret = function->execute();
                break;
            }

            // Convert arguments into a list of ExpressionResults
            std::list<ExpressionResult> args;

            for (auto& arg : _argument_list->_arguments) {
                args.push_back(arg->eval());
            }

            // Place the values of the arguments into the function's symbol table
            SymbolTable* functionSymbolTable = function->_declarator->_identifier->nestedTable;

            auto it = args.begin();

            for (auto& symbol : functionSymbolTable->table) {
                ExpressionResult arg = *it;
                if (arg._type == ExpressionResult::CHAR) {
                    symbol->val = std::string(1, arg._val.charVal);
                } else if (arg._type == ExpressionResult::INT) {
                    symbol->val = std::to_string(arg._val.intVal);
                } else {
                    symbol->val = std::to_string(arg._val.floatVal);
                }
                ++it;
            }

            // Execute the function
            ret = function->execute();
            break;
        }
    }
    return ret;
}

// PrimaryExpression
PrimaryExpression::PrimaryExpression(Symbol* identifier) {
    _type = IDENTIFIER;
    _identifier = identifier;
    _expr = nullptr;
}

PrimaryExpression::PrimaryExpression(char char_val) {
    _type = CONSTANT_CHAR;
    _char_val = char_val;
    _identifier = nullptr;
    _expr = nullptr;
}

PrimaryExpression::PrimaryExpression(int int_val) {
    _type = CONSTANT_INT;
    _int_val = int_val;
    _identifier = nullptr;
    _expr = nullptr;
}

PrimaryExpression::PrimaryExpression(float float_val) {
    _type = CONSTANT_FLOAT;
    _float_val = float_val;
    _identifier = nullptr;
    _expr = nullptr;
}

PrimaryExpression::PrimaryExpression(char* str_val) {
    _type = CONSTANT_STRING;
    _str_val = str_val;
    _identifier = nullptr;
    _expr = nullptr;
}

PrimaryExpression::PrimaryExpression(Expression* expr) {
    _type = EXPRESSION;
    _expr = expr;
    _identifier = nullptr;
}

void PrimaryExpression::print(std::string& s) {
    s += "(pme ";
    switch (_type) {
        case IDENTIFIER: {
            s += _identifier->name;
            break;
        }
        case CONSTANT_CHAR: {
            s += std::string(1, _char_val);
            break;
        }
        case CONSTANT_INT: {
            s += std::to_string(_int_val);
            break;
        }
        case CONSTANT_FLOAT: {
            s += std::to_string(_float_val);
            break;
        }
        case CONSTANT_STRING: {
            s += _str_val;
            break;
        }
        case EXPRESSION: {
            s += "(";
            _expr->print(s);
            s += ")";
            break;
        }
    }
    s += ')';
}

ExpressionResult::Type PrimaryExpression::type() {
    switch (_type) {
        case IDENTIFIER: {
            // Symbol of array/function is stored in _identifier
            switch (_identifier->type) {
                case Symbol::Type::CHAR:
                case Symbol::Type::ARR_CHAR:
                case Symbol::Type::ARR_ARR_CHAR:
                case Symbol::Type::FUNCTION_CHAR: {
                    return ExpressionResult::CHAR;
                }
                case Symbol::Type::INT:
                case Symbol::Type::ARR_INT:
                case Symbol::Type::ARR_ARR_INT:
                case Symbol::Type::FUNCTION_INT: {
                    return ExpressionResult::INT;
                }
                case Symbol::Type::FLOAT:
                case Symbol::Type::ARR_FLOAT:
                case Symbol::Type::ARR_ARR_FLOAT:
                case Symbol::Type::FUNCTION_FLOAT: {
                    return ExpressionResult::FLOAT;
                }
                default: {
                    error("Undefined datatype for identifier %s @ line %d\n", _identifier->name.c_str(), yylineno);
                    return ExpressionResult::STRING;    // dummy return to supress warnings, should never execute
                }
            }
        }
        case CONSTANT_CHAR: {
            return ExpressionResult::CHAR;
        }
        case CONSTANT_INT: {
            return ExpressionResult::INT;
        }
        case CONSTANT_FLOAT: {
            return ExpressionResult::FLOAT;
        }
        case CONSTANT_STRING: {
            return ExpressionResult::STRING;
        }
        case EXPRESSION: {
            return _expr->type();
        }
        default: {
            error("Undefined datatype for primary expression @ line %d\n", yylineno);
            return ExpressionResult::STRING;    // dummy return to supress warnings, should never execute
        }
    }
}

QuadArray PrimaryExpression::genCode() {
    QuadArray ret;
    switch (_type) {
        case IDENTIFIER: {
            _var = _identifier->name;
            break;
        }
        case CONSTANT_CHAR: {
            _var = "\'" + std::string{_char_val} + "\'";
            break;
        }
        case CONSTANT_INT: {
            _var = std::to_string(_int_val);
            break;
        }
        case CONSTANT_FLOAT: {
            _var = std::to_string(_float_val);
            break;
        }
        case CONSTANT_STRING: {
            error("Cannot use string constant as primary expression\n", yylineno);
            break;
        }
        case EXPRESSION: {
            ret = _expr->genCode();
            _var = _expr->_var;
            break;
        }
    }
    return ret;
}

ExpressionResult PrimaryExpression::eval() {
    ExpressionResult ret;
    switch (_type) {
        case IDENTIFIER: {
            // Get identifier's value
            std::string val = _identifier->val;

            // Check if identifier actually has a value, if not throw uninitialized error
            if (val.empty()) {
                error("Identifier \"%s\" is uninitialized\n", _identifier->name.c_str());
            }

            switch (_identifier->type) {
                case Symbol::Type::CHAR: {
                    ret._type = ExpressionResult::CHAR;
                    ret._val.charVal = val[0];
                    break;
                }
                case Symbol::Type::INT: {
                    ret._type = ExpressionResult::INT;
                    ret._val.intVal = std::stoi(val);
                    break;
                }
                case Symbol::Type::FLOAT: {
                    ret._type = ExpressionResult::FLOAT;
                    ret._val.floatVal = std::stof(val);
                    break;
                }
                default: {
                    error("Cannot use non-primitive type \"%s\" as an expression\n", _identifier->name.c_str());
                }
            }
            break;
        }
        case CONSTANT_CHAR: {
            ret._type = ExpressionResult::CHAR;
            ret._val.charVal = _char_val;
            break;
        }
        case CONSTANT_INT: {
            ret._type = ExpressionResult::INT;
            ret._val.intVal = _int_val;
            break;
        }
        case CONSTANT_FLOAT: {
            ret._type = ExpressionResult::FLOAT;
            ret._val.floatVal = _float_val;
            break;
        }
        case CONSTANT_STRING: {
            ret._type = ExpressionResult::STRING;
            ret._val.strVal = _str_val;
            break;
        }
        case EXPRESSION: {
            ret = _expr->eval();
            break;
        }
    }
    return ret;
}

// Expression
Expression::Expression(AssignmentExpression* assignment_expr) {
    _assignment_expr = assignment_expr;
    _expr = nullptr;
}

Expression::Expression(Expression* expr, AssignmentExpression* assignment_expr) {
    _expr = expr;
    _assignment_expr = assignment_expr;
}

void Expression::print(std::string& s) {
    s += "(exp ";
    if (_expr != nullptr) {
        _expr->print(s);
    }
    _assignment_expr->print(s);
    s += ')';
}

ExpressionResult::Type Expression::type() {
    return _assignment_expr->type();
}

QuadArray Expression::genCode() {
    QuadArray ret;
    if (_expr != nullptr) {
        ret.append(_expr->genCode());
    }
    ret.append(_assignment_expr->genCode());
    _var = _assignment_expr->_var;
    return ret;
}

ExpressionResult Expression::eval() {
    if (_expr == nullptr) {
        return _assignment_expr->eval();
    } else {
        // we have expression -> expression, assignment_expression
        // so we first evalute expression and discard its value
        _expr->eval();

        // then we evalute assignement_expression and return its value
        return _assignment_expr->eval();
    }
}

// IterationStatement
IterationStatement::IterationStatement(ForLoopBody* for_loop_body, Statement* stmt) {
    _type = FOR;
    _for_loop_body = for_loop_body;
    _stmt = stmt;
    _expr = nullptr;
}

void IterationStatement::print(std::string& s) {
    s += "(is ";
    if (_for_loop_body == nullptr) {
        s += "WHILE ";
        _expr->print(s);
    } else {
        s += "FOR ";
        _for_loop_body->print(s);
    }
    _stmt->print(s);
    s += ')';
}

QuadArray IterationStatement::genCode() {
    QuadArray ret;
    if (_for_loop_body == nullptr) {
        ret = _expr->genCode();
        std::string notExpr = genTemp();
        ret.append(Quad(Quad::LOGICAL_NOT, _expr->_var, notExpr));
        QuadArray stmtCode = _stmt->genCode();
        ret.append(Quad(Quad::CONDITIONAL_JUMP, notExpr, std::to_string(stmtCode._quads.size() + 2)));
        ret.append(stmtCode);
        ret.append(Quad(Quad::UNCONDITIONAL_JUMP, std::to_string(-ret._quads.size())));
    } else {
        switch (_for_loop_body->_type) {
            case ForLoopBody::ES1_ES2: {
                QuadArray expr_stmt_1_code = _for_loop_body->_expr_stmt_1->genCode();
                QuadArray expr_stmt_2_code = _for_loop_body->_expr_stmt_2->genCode();
                QuadArray stmt_code = _stmt->genCode();

                ret = expr_stmt_1_code;
                ret.append(expr_stmt_2_code);

                std::string notExpr = genTemp();
                ret.append(Quad(Quad::LOGICAL_NOT, _for_loop_body->_expr_stmt_2->_var, notExpr));
                ret.append(Quad(Quad::CONDITIONAL_JUMP, notExpr, std::to_string(stmt_code._quads.size() + 2)));
                ret.append(stmt_code);
                ret.append(Quad(Quad::UNCONDITIONAL_JUMP, std::to_string(-(stmt_code._quads.size() + expr_stmt_2_code._quads.size() + 2))));

                break;
            }
            case ForLoopBody::ES1_ES2_EXPR: {
                QuadArray expr_stmt_1_code = _for_loop_body->_expr_stmt_1->genCode();
                QuadArray expr_stmt_2_code = _for_loop_body->_expr_stmt_2->genCode();
                QuadArray expr_code = _for_loop_body->_expr->genCode();
                QuadArray stmt_code = _stmt->genCode();

                ret = expr_stmt_1_code;
                ret.append(expr_stmt_2_code);

                std::string notExpr = genTemp();
                ret.append(Quad(Quad::LOGICAL_NOT, _for_loop_body->_expr_stmt_2->_var, notExpr));
                ret.append(Quad(Quad::CONDITIONAL_JUMP, notExpr, std::to_string(stmt_code._quads.size() + expr_code._quads.size() + 2)));
                ret.append(stmt_code);
                ret.append(expr_code);
                ret.append(Quad(Quad::UNCONDITIONAL_JUMP, std::to_string(-(stmt_code._quads.size() + expr_code._quads.size() + expr_stmt_2_code._quads.size() + 2))));

                break;
            }
            case ForLoopBody::DECL_ES1: {
                QuadArray decl_code = _for_loop_body->_decl->genCode();
                QuadArray expr_stmt_1_code = _for_loop_body->_expr_stmt_1->genCode();
                QuadArray stmt_code = _stmt->genCode();

                ret = decl_code;
                ret.append(expr_stmt_1_code);

                std::string notExpr = genTemp();
                ret.append(Quad(Quad::LOGICAL_NOT, _for_loop_body->_expr_stmt_1->_var, notExpr));
                ret.append(Quad(Quad::CONDITIONAL_JUMP, notExpr, std::to_string(stmt_code._quads.size() + 2)));
                ret.append(stmt_code);
                ret.append(Quad(Quad::UNCONDITIONAL_JUMP, std::to_string(-(stmt_code._quads.size() + expr_stmt_1_code._quads.size() + 2))));

                break;
            }
            case ForLoopBody::DECL_ES1_EXPR: {
                QuadArray decl_code = _for_loop_body->_decl->genCode();
                QuadArray expr_stmt_1_code = _for_loop_body->_expr_stmt_1->genCode();
                QuadArray expr_code = _for_loop_body->_expr->genCode();
                QuadArray stmt_code = _stmt->genCode();

                ret = decl_code;
                ret.append(expr_stmt_1_code);

                std::string notExpr = genTemp();
                ret.append(Quad(Quad::LOGICAL_NOT, _for_loop_body->_expr_stmt_1->_var, notExpr));
                ret.append(Quad(Quad::CONDITIONAL_JUMP, notExpr, std::to_string(stmt_code._quads.size() + expr_code._quads.size() + 2)));
                ret.append(stmt_code);
                ret.append(expr_code);
                ret.append(Quad(Quad::UNCONDITIONAL_JUMP, std::to_string(-(stmt_code._quads.size() + expr_code._quads.size() + expr_stmt_1_code._quads.size() + 2))));

                break;
            }
        }
    }
    return ret;
}

void IterationStatement::execute() {
    if (_type == WHILE) {
        if (_expr->type() == ExpressionResult::CHAR) {
            while (_expr->eval()._val.charVal) {
                _stmt->execute();
            }
        } else if (_expr->type() == ExpressionResult::INT) {
            while (_expr->eval()._val.intVal) {
                _stmt->execute();
            }
        } else {
            while (_expr->eval()._val.floatVal) {
                _stmt->execute();
            }
        }
    } else {
        // Switch scope into the for loop body
        SymbolTable* oldST = currentST;
        currentST = _for_loop_body->_scope;

        switch (_for_loop_body->_type) {
            case ForLoopBody::Type::ES1_ES2: {
                ExpressionStatement* expr_stmt_1 = _for_loop_body->_expr_stmt_1;
                ExpressionStatement* expr_stmt_2 = _for_loop_body->_expr_stmt_2;

                if (expr_stmt_1->_expr == nullptr) {
                    if (expr_stmt_2->_expr == nullptr) {
                        for (;;) {
                            _stmt->execute();
                        }
                    } else {
                        if (expr_stmt_2->_expr->type() == ExpressionResult::CHAR) {
                            for (; expr_stmt_2->execute()._val.charVal;) {
                                _stmt->execute();
                            }
                        } else if (expr_stmt_2->_expr->type() == ExpressionResult::INT) {
                            for (; expr_stmt_2->execute()._val.intVal;) {
                                _stmt->execute();
                            }
                        } else {
                            for (; expr_stmt_2->execute()._val.floatVal;) {
                                _stmt->execute();
                            }
                        }
                    }
                } else {
                    if (expr_stmt_2->_expr == nullptr) {
                        for (expr_stmt_1->execute();;) {
                            _stmt->execute();
                        }
                    } else {
                        if (expr_stmt_2->_expr->type() == ExpressionResult::CHAR) {
                            for (expr_stmt_1->execute(); expr_stmt_2->execute()._val.charVal;) {
                                _stmt->execute();
                            }
                        } else if (expr_stmt_2->_expr->type() == ExpressionResult::INT) {
                            for (expr_stmt_1->execute(); expr_stmt_2->execute()._val.intVal;) {
                                _stmt->execute();
                            }
                        } else {
                            for (expr_stmt_1->execute(); expr_stmt_2->execute()._val.floatVal;) {
                                _stmt->execute();
                            }
                        }
                    }
                }

                break;
            }
            case ForLoopBody::Type::ES1_ES2_EXPR: {
                ExpressionStatement* expr_stmt_1 = _for_loop_body->_expr_stmt_1;
                ExpressionStatement* expr_stmt_2 = _for_loop_body->_expr_stmt_2;
                Expression* expr = _for_loop_body->_expr;

                if (expr_stmt_1->_expr == nullptr) {
                    if (expr_stmt_2->_expr == nullptr) {
                        for (;; expr->eval()) {
                            _stmt->execute();
                        }
                    } else {
                        if (expr_stmt_2->_expr->type() == ExpressionResult::CHAR) {
                            for (; expr_stmt_2->execute()._val.charVal; expr->eval()) {
                                _stmt->execute();
                            }
                        } else if (expr_stmt_2->_expr->type() == ExpressionResult::INT) {
                            for (; expr_stmt_2->execute()._val.intVal; expr->eval()) {
                                _stmt->execute();
                            }
                        } else {
                            for (; expr_stmt_2->execute()._val.floatVal; expr->eval()) {
                                _stmt->execute();
                            }
                        }
                    }
                } else {
                    if (expr_stmt_2->_expr == nullptr) {
                        for (expr_stmt_1->execute();; expr->eval()) {
                            _stmt->execute();
                        }
                    } else {
                        if (expr_stmt_2->_expr->type() == ExpressionResult::CHAR) {
                            for (expr_stmt_1->execute(); expr_stmt_2->execute()._val.charVal; expr->eval()) {
                                _stmt->execute();
                            }
                        } else if (expr_stmt_2->_expr->type() == ExpressionResult::INT) {
                            for (expr_stmt_1->execute(); expr_stmt_2->execute()._val.intVal; expr->eval()) {
                                _stmt->execute();
                            }
                        } else {
                            for (expr_stmt_1->execute(); expr_stmt_2->execute()._val.floatVal; expr->eval()) {
                                _stmt->execute();
                            }
                        }
                    }
                }

                break;
            }
            case ForLoopBody::Type::DECL_ES1: {
                Declaration* decl = _for_loop_body->_decl;
                ExpressionStatement* expr_stmt_1 = _for_loop_body->_expr_stmt_1;

                if (expr_stmt_1 == nullptr) {
                    for (decl->execute();;) {
                        _stmt->execute();
                    }
                } else {
                    if (expr_stmt_1->_expr->type() == ExpressionResult::CHAR) {
                        for (decl->execute(); expr_stmt_1->execute()._val.charVal;) {
                            _stmt->execute();
                        }
                    } else if (expr_stmt_1->_expr->type() == ExpressionResult::INT) {
                        for (decl->execute(); expr_stmt_1->execute()._val.intVal;) {
                            _stmt->execute();
                        }
                    } else {
                        for (decl->execute(); expr_stmt_1->execute()._val.floatVal;) {
                            _stmt->execute();
                        }
                    }
                }

                break;
            }
            case ForLoopBody::Type::DECL_ES1_EXPR: {
                Declaration* decl = _for_loop_body->_decl;
                ExpressionStatement* expr_stmt_1 = _for_loop_body->_expr_stmt_1;
                Expression* expr = _for_loop_body->_expr;

                if (expr_stmt_1 == nullptr) {
                    for (decl->execute();; expr->eval()) {
                        _stmt->execute();
                    }
                } else {
                    if (expr_stmt_1->_expr->type() == ExpressionResult::CHAR) {
                        for (decl->execute(); expr_stmt_1->execute()._val.charVal; expr->eval()) {
                            _stmt->execute();
                        }
                    } else if (expr_stmt_1->_expr->type() == ExpressionResult::INT) {
                        for (decl->execute(); expr_stmt_1->execute()._val.intVal; expr->eval()) {
                            _stmt->execute();
                        }
                    } else {
                        for (decl->execute(); expr_stmt_1->execute()._val.floatVal; expr->eval()) {
                            _stmt->execute();
                        }
                    }
                }

                break;
            }
        }

        // Restore scope back to the parent
        currentST = oldST;
    }
}

IterationStatement::IterationStatement(Expression* expr, Statement* stmt) {
    _type = WHILE;
    _expr = expr;
    _stmt = stmt;
    _for_loop_body = nullptr;
}

// ForLoopBody
ForLoopBody::ForLoopBody(ExpressionStatement* expr_stmt_1, ExpressionStatement* expr_stmt_2) {
    _type = ES1_ES2;
    _expr_stmt_1 = expr_stmt_1;
    _expr_stmt_2 = expr_stmt_2;
    _decl = nullptr;
    _expr = nullptr;
}

ForLoopBody::ForLoopBody(ExpressionStatement* expr_stmt_1, ExpressionStatement* expr_stmt_2, Expression* expr) {
    _type = ES1_ES2_EXPR;
    _expr_stmt_1 = expr_stmt_1;
    _expr_stmt_2 = expr_stmt_2;
    _expr = expr;
    _decl = nullptr;
}

ForLoopBody::ForLoopBody(Declaration* decl, ExpressionStatement* expr_stmt_1) {
    _type = DECL_ES1;
    _decl = decl;
    _expr_stmt_1 = expr_stmt_1;
    _expr_stmt_2 = nullptr;
    _expr = nullptr;
}

ForLoopBody::ForLoopBody(Declaration* decl, ExpressionStatement* expr_stmt_1, Expression* expr) {
    _type = DECL_ES1_EXPR;
    _decl = decl;
    _expr_stmt_1 = expr_stmt_1;
    _expr = expr;
    _expr_stmt_2 = nullptr;
}

void ForLoopBody::print(std::string& s) {
    s += "(flb ";
    switch (_type) {
        case Type::ES1_ES2: {
            s += "ES1_ES2 ";
            _expr_stmt_1->print(s);
            _expr_stmt_2->print(s);
            break;
        }
        case Type::ES1_ES2_EXPR: {
            s += "ES1_ES2_EXPR ";
            _expr_stmt_1->print(s);
            _expr_stmt_2->print(s);
            _expr->print(s);
            break;
        }
        case Type::DECL_ES1: {
            s += "DECL_ES1 ";
            _decl->print(s);
            _expr_stmt_1->print(s);
            break;
        }
        case Type::DECL_ES1_EXPR: {
            s += "DECL_ES1_EXPR ";
            _decl->print(s);
            _expr_stmt_1->print(s);
            _expr->print(s);
            break;
        }
    }
    s += ')';
}

// Statement
Statement::Statement(LabelStatement* label_stmt) {
    _label_stmt = label_stmt;
    _expr_stmt = nullptr;
    _compound_stmt = nullptr;
    _selection_stmt = nullptr;
    _iteration_stmt = nullptr;
    _jump_stmt = nullptr;
    _printf_stmt = nullptr;
}

Statement::Statement(ExpressionStatement* expr_stmt) {
    _expr_stmt = expr_stmt;
    _label_stmt = nullptr;
    _compound_stmt = nullptr;
    _selection_stmt = nullptr;
    _iteration_stmt = nullptr;
    _jump_stmt = nullptr;
    _printf_stmt = nullptr;
}

Statement::Statement(CompoundStatement* compound_stmt) {
    _compound_stmt = compound_stmt;
    _label_stmt = nullptr;
    _expr_stmt = nullptr;
    _selection_stmt = nullptr;
    _iteration_stmt = nullptr;
    _jump_stmt = nullptr;
    _printf_stmt = nullptr;
}

Statement::Statement(SelectionStatement* selection_stmt) {
    _selection_stmt = selection_stmt;
    _label_stmt = nullptr;
    _expr_stmt = nullptr;
    _compound_stmt = nullptr;
    _iteration_stmt = nullptr;
    _jump_stmt = nullptr;
    _printf_stmt = nullptr;
}

Statement::Statement(IterationStatement* iteration_stmt) {
    _iteration_stmt = iteration_stmt;
    _label_stmt = nullptr;
    _expr_stmt = nullptr;
    _compound_stmt = nullptr;
    _selection_stmt = nullptr;
    _jump_stmt = nullptr;
    _printf_stmt = nullptr;
}

Statement::Statement(JumpStatement* jump_stmt) {
    _jump_stmt = jump_stmt;
    _label_stmt = nullptr;
    _expr_stmt = nullptr;
    _compound_stmt = nullptr;
    _selection_stmt = nullptr;
    _iteration_stmt = nullptr;
    _printf_stmt = nullptr;
}

Statement::Statement(PrintfStatement* printf_stmt) {
    _printf_stmt = printf_stmt;
    _label_stmt = nullptr;
    _expr_stmt = nullptr;
    _compound_stmt = nullptr;
    _selection_stmt = nullptr;
    _iteration_stmt = nullptr;
    _jump_stmt = nullptr;
}

void Statement::print(std::string& s) {
    s += "(st ";
    if (_label_stmt != nullptr) {
        _label_stmt->print(s);
    } else if (_expr_stmt != nullptr) {
        _expr_stmt->print(s);
    } else if (_compound_stmt != nullptr) {
        _compound_stmt->print(s);
    } else if (_selection_stmt != nullptr) {
        _selection_stmt->print(s);
    } else if (_iteration_stmt != nullptr) {
        _iteration_stmt->print(s);
    } else if (_jump_stmt != nullptr) {
        _jump_stmt->print(s);
    } else if (_printf_stmt != nullptr) {
        _printf_stmt->print(s);
    }
    s += ')';
}

QuadArray Statement::genCode() {
    QuadArray ret;
    if (_label_stmt != nullptr) {
    } else if (_expr_stmt != nullptr and _expr_stmt->_expr != nullptr) {
        return _expr_stmt->genCode();
    } else if (_compound_stmt != nullptr) {
        return _compound_stmt->genCode();
    } else if (_selection_stmt != nullptr) {
        return _selection_stmt->genCode();
    } else if (_iteration_stmt != nullptr) {
        return _iteration_stmt->genCode();
    } else if (_jump_stmt != nullptr) {
    } else if (_printf_stmt != nullptr) {
        return _printf_stmt->genCode();
    }
    return ret;
}

void Statement::execute() {
    if (_label_stmt != nullptr) {
        _label_stmt->execute();
    } else if (_expr_stmt != nullptr and _expr_stmt->_expr != nullptr) {
        _expr_stmt->execute();
    } else if (_compound_stmt != nullptr) {
        _compound_stmt->execute();
    } else if (_selection_stmt != nullptr) {
        _selection_stmt->execute();
    } else if (_iteration_stmt != nullptr) {
        _iteration_stmt->execute();
    } else if (_jump_stmt != nullptr) {
        _jump_stmt->execute();
    } else if (_printf_stmt != nullptr) {
        _printf_stmt->execute();
    }
}

// LabelStatement
LabelStatement::LabelStatement(Expression* expr, Statement* stmt) {
    _type = CASE;
    _expr = expr;
    _stmt = stmt;
}

LabelStatement::LabelStatement(Statement* stmt) {
    _type = DEFAULT;
    _stmt = stmt;
    _expr = nullptr;
}

void LabelStatement::print(std::string& s) {
    s += "(ls ";
    if (_type == CASE) {
        s += "case";
    } else {
        s += "default";
    }
    s += ')';
}

void LabelStatement::execute() {
    if (_type == CASE) {
        error("Case statements not supported\n");
    } else {
        error("Default statements not supported\n");
    }
}

// ExpressionStatement
ExpressionStatement::ExpressionStatement(Expression* expr) {
    _expr = expr;
}

ExpressionStatement::ExpressionStatement() {
    _expr = nullptr;
}

void ExpressionStatement::print(std::string& s) {
    s += "(est ";
    if (_expr != nullptr) {
        _expr->print(s);
    }
    s += ')';
}

QuadArray ExpressionStatement::genCode() {
    QuadArray ret;
    if (_expr != nullptr) {
        ret = _expr->genCode();
        _var = _expr->_var;
    }
    return ret;
}

ExpressionResult ExpressionStatement::execute() {
    if (_expr == nullptr) {
        error("ExpressionStatement::execute() called with null expression\n");
    }
    return _expr->eval();
}

// SelectionStatement
SelectionStatement::SelectionStatement(Type type, Expression* expr, Statement* stmt_1) {
    _type = type;
    _expr = expr;
    _stmt_1 = stmt_1;
    _stmt_2 = nullptr;
}

SelectionStatement::SelectionStatement(Expression* expr, Statement* stmt_1, Statement* stmt_2) {
    _type = IF_ELSE;
    _expr = expr;
    _stmt_1 = stmt_1;
    _stmt_2 = stmt_2;
}

void SelectionStatement::print(std::string& s) {
    s += "(ss ";
    if (_type == IF) {
        s += "if";
    } else if (_type == IF_ELSE) {
        s += "if-else";
    } else {
        s += "switch";
    }
    if (_expr != nullptr) {
        _expr->print(s);
    }
    if (_stmt_1 != nullptr) {
        _stmt_1->print(s);
    }
    if (_stmt_2 != nullptr) {
        _stmt_2->print(s);
    }
    s += ')';
}

QuadArray SelectionStatement::genCode() {
    QuadArray ret;
    if (_type == IF) {
        ret = _expr->genCode();
        std::string notExpr = genTemp();
        ret.append(Quad(Quad::Operator::LOGICAL_NOT, _expr->_var, notExpr));

        QuadArray stmt_1 = _stmt_1->genCode();
        ret.append(Quad(Quad::Operator::CONDITIONAL_JUMP, notExpr, std::to_string(stmt_1._quads.size() + 1)));
        ret.append(stmt_1);
    } else {
        ret = _expr->genCode();
        std::string notExpr = genTemp();
        ret.append(Quad(Quad::Operator::LOGICAL_NOT, _expr->_var, notExpr));

        QuadArray stmt_1 = _stmt_1->genCode();
        QuadArray stmt_2 = _stmt_2->genCode();
        ret.append(Quad(Quad::Operator::CONDITIONAL_JUMP, notExpr, std::to_string(stmt_1._quads.size() + 2)));
        ret.append(stmt_1);
        ret.append(Quad(Quad::Operator::UNCONDITIONAL_JUMP, std::to_string(stmt_2._quads.size() + 1)));
        ret.append(stmt_2);
    }
    return ret;
}

void SelectionStatement::execute() {
    switch (_type) {
        case SWITCH: {
            error("Switch-case statements are not supported\n");
            break;
        }
        case IF: {
            bool condition = _expr->eval()._val.intVal;
            if (condition) {
                _stmt_1->execute();
            }
            break;
        }
        case IF_ELSE: {
            bool condition = _expr->eval()._val.intVal;
            if (condition) {
                _stmt_1->execute();
            } else {
                _stmt_2->execute();
            }
            break;
        }
    }
}

// JumpStatement
JumpStatement::JumpStatement(Type type) {
    _type = type;
    _expr = nullptr;
}

JumpStatement::JumpStatement(Expression* expr) {
    _type = RETURN;
    _expr = expr;
}

void JumpStatement::print(std::string& s) {
    s += "(js ";
    if (_type == BREAK) {
        s += "break";
    } else if (_type == CONTINUE) {
        s += "continue";
    } else {
        s += "return";
        if (_expr != nullptr) {
            _expr->print(s);
        }
    }
    s += ')';
}

void JumpStatement::execute() {
    if (_type == BREAK) {
        error("Stray break statement encountered\n");
    } else if (_type == CONTINUE) {
        error("Stray continue statement encountered\n");
    } else {
        error("return statement not at end of function\n");
    }
}

// PrintfStatement
PrintfStatement::PrintfStatement(std::string format_string) {
    _format_string = format_string;
    _argument_list = nullptr;
}

PrintfStatement::PrintfStatement(std::string format_string, ArgumentList* argument_list) {
    _format_string = format_string;
    _argument_list = argument_list;
}

void PrintfStatement::print(std::string& s) {
    s += "(pfs format_str";
    if (_argument_list != nullptr) {
        s += " arg_list";
    }
    s += ')';
}

QuadArray PrintfStatement::genCode() {
    QuadArray ret;

    std::string format_string = genTemp();
    ret.append(Quad(Quad::Operator::ASSIGNMENT, "\"" + _format_string + "\"", format_string));

    // Check if there are any arguments
    if (_argument_list == nullptr) {
        ret.append(Quad(Quad::Operator::PARAM, format_string));
        ret.append(Quad(Quad::Operator::CALL, "1", "printf"));
        return ret;
    }

    for (auto& arg : _argument_list->_arguments) {
        ret.append(arg->genCode());
    }
    ret.append(Quad(Quad::Operator::PARAM, format_string));
    for (auto& arg : _argument_list->_arguments) {
        ret.append(Quad(Quad::Operator::PARAM, arg->_var));
    }
    ret.append(Quad(Quad::Operator::CALL, std::to_string(1 + _argument_list->_arguments.size()), "printf"));

    return ret;
}

void PrintfStatement::execute() {
    // We know that this printf is semantically correct because of bison

    // Lambda function to "fix" a format_string. What was observed was that \n appeared as \ and n in the string,
    // two separate characters instead of one escape sequence. So this function takes a string and returns a string
    // in which the \ and n are combined into one \n
    auto fix = [](std::string s) -> std::string {
        std::string ret;
        for (size_t i = 0; i < s.size(); ++i) {
            if (i + 1 == s.size()) {
                ret += s[i];
                continue;
            }
            if (s[i] == '\\') {
                char c = s[i + 1];
                switch (c) {
                    case 'n': {
                        ret += '\n';
                        ++i;
                        break;
                    }
                    case 't': {
                        ret += '\t';
                        ++i;
                        break;
                    }
                    case 'v': {
                        ret += '\v';
                        ++i;
                        break;
                    }
                    case 'b': {
                        ret += '\b';
                        ++i;
                        break;
                    }
                    case 'r': {
                        ret += '\r';
                        ++i;
                        break;
                    }
                    case 'f': {
                        ret += '\f';
                        ++i;
                        break;
                    }
                    case 'a': {
                        ret += '\a';
                        ++i;
                        break;
                    }
                    case '\\': {
                        ret += '\\';
                        ++i;
                        break;
                    }
                    case '?': {
                        ret += '\?';
                        ++i;
                        break;
                    }
                    case '\'': {
                        ret += '\'';
                        ++i;
                        break;
                    }
                    case '\"': {
                        ret += '\"';
                        ++i;
                        break;
                    }
                    default: {
                        ret += s[i];
                        break;
                    }
                }
            } else {
                ret += s[i];
            }
        }
        return ret;
    };

    // Get the format string
    std::string format_string = _format_string;

    // Fix the format string
    format_string = fix(format_string);

    // Check if there is no argument list
    if (_argument_list == nullptr) {
        // Print the format string
        std::cout << format_string;
        return;
    }

    // Get the argument list
    std::list<AssignmentExpression*> argument_list = _argument_list->_arguments;

    // Variable to store the output string
    std::string output_string;

    // Iterator to store the current argument
    std::list<AssignmentExpression*>::iterator it = argument_list.begin();

    // Iterate through the format string
    for (size_t i = 0; i < format_string.size(); ++i) {
        if (i == format_string.size() - 1) {
            output_string += format_string[i];
            continue;
        }
        char c = format_string[i];
        char cc = format_string[i + 1];
        if (c == '%') {
            if (cc == 'c') {
                output_string += (*it)->eval()._val.charVal;
            } else if (cc == 'd') {
                output_string += std::to_string((*it)->eval()._val.intVal);
            } else if (cc == 'f') {
                output_string += std::to_string((*it)->eval()._val.floatVal);
            } else {
                error("Invalid format string argument (%%%c)\n", cc);
            }
            ++i;
            ++it;
        } else {
            output_string += c;
        }
    }

    // Print the output string
    std::cout << output_string;
}
