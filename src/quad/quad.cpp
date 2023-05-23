#include <iostream>
#include <quad/quad.hpp>

Quad::Quad(Operator op) {
    this->_op = op;
}

Quad::Quad(Operator op, std::string arg_1) {
    this->_op = op;
    this->_arg1 = arg_1;
}

Quad::Quad(Operator op, std::string arg_1, std::string result) {
    this->_op = op;
    this->_arg1 = arg_1;
    this->_result = result;
}

Quad::Quad(Operator op, std::string arg_1, std::string arg_2, std::string result) {
    this->_op = op;
    this->_arg1 = arg_1;
    this->_arg2 = arg_2;
    this->_result = result;
}

void Quad::print() {
    switch (_op) {
        case FUNCTION_BEGIN: {
            std::cout << "func begin " << _arg1 << std::endl;
            break;
        }
        case FUNCTION_END: {
            std::cout << "func end" << std::endl;
            break;
        }
        case ASSIGNMENT: {
            std::cout << _result << " = " << _arg1 << std::endl;
            break;
        }
        case ARRAY_ASSIGNMENT: {
            std::cout << _result << "[" << _arg2 << "] = " << _arg1 << std::endl;
            break;
        }
        case ARRAY_ACCESS: {
            std::cout << _result << " = " << _arg1 << "[" << _arg2 << "]" << std::endl;
            break;
        }
        case LOGICAL_OR: {
            std::cout << _result << " = " << _arg1 << " || " << _arg2 << std::endl;
            break;
        }
        case LOGICAL_AND: {
            std::cout << _result << " = " << _arg1 << " && " << _arg2 << std::endl;
            break;
        }
        case LOGICAL_NOT: {
            std::cout << _result << " = !" << _arg1 << std::endl;
            break;
        }
        case DOUBLE_EQUAL: {
            std::cout << _result << " = " << _arg1 << " == " << _arg2 << std::endl;
            break;
        }
        case NOT_EQUAL: {
            std::cout << _result << " = " << _arg1 << " != " << _arg2 << std::endl;
            break;
        }
        case LESS: {
            std::cout << _result << " = " << _arg1 << " < " << _arg2 << std::endl;
            break;
        }
        case LESS_EQUAL: {
            std::cout << _result << " = " << _arg1 << " <= " << _arg2 << std::endl;
            break;
        }
        case GREATER: {
            std::cout << _result << " = " << _arg1 << " > " << _arg2 << std::endl;
            break;
        }
        case GREATER_EQUAL: {
            std::cout << _result << " = " << _arg1 << " >= " << _arg2 << std::endl;
            break;
        }
        case PLUS: {
            std::cout << _result << " = " << _arg1 << " + " << _arg2 << std::endl;
            break;
        }
        case MINUS: {
            if (_arg2.empty()) {
                std::cout << _result << " = -" << _arg1 << std::endl;
            } else {
                std::cout << _result << " = " << _arg1 << " - " << _arg2 << std::endl;
            }
            break;
        }
        case MUL: {
            std::cout << _result << " = " << _arg1 << " * " << _arg2 << std::endl;
            break;
        }
        case DIV: {
            std::cout << _result << " = " << _arg1 << " / " << _arg2 << std::endl;
            break;
        }
        case MOD: {
            std::cout << _result << " = " << _arg1 << " % " << _arg2 << std::endl;
            break;
        }
        case RETURN: {
            std::cout << "return " << _arg1 << std::endl;
            break;
        }
        case CALL: {
            std::cout << "call " << _result << ", " << _arg1 << std::endl;
            break;
        }
        case PARAM: {
            std::cout << "param " << _arg1 << std::endl;
            break;
        }
        case REFPARAM: {
            std::cout << "refparam " << _arg1 << std::endl;
            break;
        }
        case CONDITIONAL_JUMP: {
            std::cout << "if " << _arg1 << " goto " << _result << std::endl;
            break;
        }
        case UNCONDITIONAL_JUMP: {
            std::cout << "goto " << _arg1 << std::endl;
            break;
        }
        default: {
            break;
        }
    }
}

void QuadArray::append(const Quad &quad) {
    _quads.push_back(quad);
}

void QuadArray::append(const QuadArray &quads) {
    for (auto &quad : quads._quads) {
        append(quad);
    }
}

// Function to convert an int to string including negative integers
int strToInt(const std::string &str) {
    int result = 0;
    int sign = 1;
    int i = 0;
    if (str[0] == '-') {
        sign = -1;
        i = 1;
    }
    for (; i < str.length(); ++i) {
        result = result * 10 + (str[i] - '0');
    }
    return sign * result;
}

void QuadArray::backPatch() {
    int i = 0;
    for (auto &quad : _quads) {
        if (quad._op == Quad::Operator::CONDITIONAL_JUMP) {
            int target = strToInt(quad._result);
            target += i;
            quad._result = std::to_string(target);
        } else if (quad._op == Quad::Operator::UNCONDITIONAL_JUMP) {
            int target = strToInt(quad._arg1);
            target += i;
            quad._arg1 = std::to_string(target);
        }
        ++i;
    }
}

void QuadArray::print() {
    int i = 0;
    for (auto &quad : _quads) {
        std::cout << i++ << ": ";
        quad.print();
    }
}
