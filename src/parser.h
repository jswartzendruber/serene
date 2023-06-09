#ifndef PARSER_H
#define PARSER_H

#include <cassert>
#include <exception>
#include <iostream>
#include <memory>
#include <optional>
#include <string>
#include <unordered_map>
#include <variant>
#include <vector>

#include "token.h"

class Parser;
class ParseException;

class Function;
class FunctionCall;
class TypedValue;

class Expression;
class BaseExpression;
class ValueExpression;
class BinaryExpression;

class Statement;
class IfStatement;
class LetStatement;
class BaseStatement;
class ReturnStatement;

class ASTVisitor;
class TypeCheckVisitor;

enum class ValueExpressionType {
  i64,
  f64,
  Call,
  Ident,
  String,
};

std::string_view valueExpressionTypeToString(ValueExpressionType type);
std::string debugPrintExpr(std::unique_ptr<Expression> expr);

void walkFunction(ASTVisitor *visitor, Function *function);
void walkExpression(ASTVisitor *visitor, Expression *expr);
void walkValueExpression(ASTVisitor *visitor, ValueExpression *expr);
void walkCallExpression(ASTVisitor *visitor, FunctionCall *call);
void walkBinaryExpression(ASTVisitor *visitor, BinaryExpression *expr);
void walkStatement(ASTVisitor *visitor, Statement *stmt);
void walkLetStatement(ASTVisitor *visitor, LetStatement *stmt);
void walkIfStatement(ASTVisitor *visitor, IfStatement *stmt);
void walkReturnStatement(ASTVisitor *visitor, ReturnStatement *stmt);

class ASTVisitor {
 public:
  virtual void visitFunction(Function *function);
  virtual void visitExpression(Expression *expr);
  virtual void visitValueExpression(ValueExpression *expr);
  virtual void visitCallExpression(FunctionCall *call);
  virtual void visitBinaryExpression(BinaryExpression *expr);
  virtual void visitStatement(Statement *stmt);
  virtual void visitLetStatement(LetStatement *stmt);
  virtual void visitIfStatement(IfStatement *stmt);
  virtual void visitReturnStatement(ReturnStatement *stmt);
};

class Parser {
 public:
  Parser(std::vector<Token> tokens);

  std::vector<std::shared_ptr<Function>> parse();

  std::unique_ptr<std::unordered_map<std::string_view, std::string_view>>
      m_currEnv;
  std::unordered_map<std::string_view, std::shared_ptr<Function>> m_symbolTable;

 private:
  std::vector<Token> m_tokens;
  int m_idx;

  Token peek();
  Token peek(int n);
  bool at(TokenType type);
  bool atIdentifier(std::string expected);
  Token expect(TokenType type);
  std::string_view expectIdentifier();
  std::string_view expectIdentifier(std::string expected);
  std::shared_ptr<Function> parseFunction();
  Statement parseStatement();
  std::unique_ptr<Expression> parseExpression();
  std::unique_ptr<Expression> parseExpressionBP(int bp);
  TypedValue parseFunctionArgument();
};

class ParseException : public std::exception {
 public:
  explicit ParseException(const std::string &msg) : m_msg(msg) {}
  const char *what() const noexcept override { return m_msg.c_str(); }

 private:
  std::string m_msg;
};

class TypedValue {
 public:
  TypedValue(std::string_view name, std::string_view type);

  std::string_view m_name;
  std::string_view m_type;
};

class FunctionCall {
 public:
  FunctionCall(std::string_view name,
               std::vector<std::unique_ptr<Expression>> args);

  std::string_view m_name;
  std::vector<std::unique_ptr<Expression>> m_args;
};

class BaseExpression {
 public:
  virtual ~BaseExpression() = default;
};

class Expression {
 public:
  enum Type {
    Value,
    BinOp,
  };

  Expression(Expression &&) = default;
  Expression(Type type, BaseExpression *expression);

  Type m_type;
  std::unique_ptr<BaseExpression> m_expression;
};

class BinaryExpression : public BaseExpression {
 public:
  enum Type {
    Add,
    Sub,
    Mul,
    Div,
    Compare,
    LogicalOr,
  };

  BinaryExpression(Type type, std::unique_ptr<Expression> left,
                   std::unique_ptr<Expression> right);

  static int infixBP(Type op);

  Type m_type;
  std::unique_ptr<Expression> m_left;
  std::unique_ptr<Expression> m_right;
};

using ValueExpressionValue =
    std::variant<long, double, std::string_view, FunctionCall>;

class ValueExpression : public BaseExpression {
 public:
  ValueExpression(ValueExpressionValue value, ValueExpressionType type);
  std::string valueString();

  ValueExpressionValue m_value;
  ValueExpressionType m_type;
};

class BaseStatement {
 public:
  virtual ~BaseStatement() = default;
};

class Statement {
 public:
  enum Type {
    If,
    Let,
    Return,
  };

  Statement(Statement &&) = default;
  Statement(Type type, BaseStatement *statement);

  Type m_type;
  std::unique_ptr<BaseStatement> m_statement;
};

class LetStatement : public BaseStatement {
 public:
  LetStatement(std::string_view name, std::string_view type,
               std::unique_ptr<Expression> initialValue);

  std::string_view m_name;
  std::string_view m_type;
  std::unique_ptr<Expression> m_initialValue;
};

class IfStatement : public BaseStatement {
 public:
  IfStatement(std::unique_ptr<Expression> condition,
              std::vector<Statement> bodyIfTrue,
              std::optional<std::vector<Statement>> bodyIfFalse);

  std::unique_ptr<Expression> m_condition;
  std::vector<Statement> m_bodyIfTrue;
  std::optional<std::vector<Statement>> m_bodyIfFalse;
};

class ReturnStatement : public BaseStatement {
 public:
  ReturnStatement(std::unique_ptr<Expression> value);

  std::unique_ptr<Expression> m_value;
};

class Function {
 public:
  Function(
      std::string_view name,
      std::unique_ptr<std::unordered_map<std::string_view, std::string_view>>
          env,
      std::vector<TypedValue> args, std::string_view returnType,
      std::vector<Statement> statements);
  Function(Function &&) = default;

  std::string_view m_name;
  std::unique_ptr<std::unordered_map<std::string_view, std::string_view>> m_env;
  std::vector<TypedValue> m_args;
  std::string_view m_returnType;
  std::vector<Statement> m_statements;
};

#endif