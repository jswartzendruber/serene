#include "parser.h"

std::string debugPrintExpr(Expression expr) {
  std::string s;

  if (expr.m_type == Expression::Type::BinOp) {
    BinaryExpression *opExpr =
        static_cast<BinaryExpression *>(expr.m_expression);
    BinaryExpression::Type op = opExpr->m_type;
    std::string opstr;

    if (op == BinaryExpression::Add) {
      opstr = " + ";
    } else if (op == BinaryExpression::Sub) {
      opstr = " - ";
    } else if (op == BinaryExpression::Mul) {
      opstr = " * ";
    } else if (op == BinaryExpression::Div) {
      opstr = " / ";
    } else if (op == BinaryExpression::Compare) {
      opstr = " == ";
    } else {
      opstr = " ? ";
    }

    s += "( " + debugPrintExpr(opExpr->m_left) + opstr +
         debugPrintExpr(opExpr->m_right) + " )";

  } else if (expr.m_type == Expression::Type::Value) {
    ValueExpression *vExpr = static_cast<ValueExpression *>(expr.m_expression);
    s += vExpr->valueString();
  }

  return s;
}

std::string_view valueExpressionTypeToString(ValueExpressionType type) {
  const char *s;
#define PROCESS_VAL(p) \
  case (p):            \
    s = #p;            \
    break;
  switch (type) {
    PROCESS_VAL(ValueExpressionType::i64);
    PROCESS_VAL(ValueExpressionType::f64);
    PROCESS_VAL(ValueExpressionType::Call);
    PROCESS_VAL(ValueExpressionType::Ident);
    PROCESS_VAL(ValueExpressionType::String);

    default:
      s = "ValueExpressionType::UNKNOWN";
      break;
  }
#undef PROCESS_VAL

  return s + 21;  // hacky way to remove ValueExpressionType::
}

void ASTVisitor::visitFunction(Function *function) {
  walkFunction(this, function);
};
void ASTVisitor::visitExpression(Expression *expr) {
  walkExpression(this, expr);
};
void ASTVisitor::visitValueExpression(ValueExpression *expr) {
  walkValueExpression(this, expr);
};
void ASTVisitor::visitBinaryExpression(BinaryExpression *expr) {
  walkBinaryExpression(this, expr);
};
void ASTVisitor::visitStatement(Statement *stmt) { walkStatement(this, stmt); };
void ASTVisitor::visitLetStatement(LetStatement *stmt) {
  walkLetStatement(this, stmt);
};
void ASTVisitor::visitIfStatement(IfStatement *stmt) {
  walkIfStatement(this, stmt);
};
void ASTVisitor::visitReturnStatement(ReturnStatement *stmt) {
  walkReturnStatement(this, stmt);
};

Parser::Parser(std::vector<Token> tokens) : m_tokens(tokens) { m_idx = 0; }
Parser::~Parser() {}

Token Parser::peek() {
  if (m_idx < m_tokens.size()) {
    return m_tokens[m_idx];
  } else {
    return m_tokens[m_idx];
  }
}

Token Parser::peek(int n) {
  if (m_idx + n < m_tokens.size()) {
    return m_tokens[m_idx + n];
  } else {
    return m_tokens[m_idx + n];
  }
}

Token Parser::expect(TokenType type) {
  Token t = m_tokens[m_idx];
  if (m_idx < m_tokens.size() && t.m_type == type) {
    m_idx++;
    return t;
  } else {
    throw ParseException("Expected " + tokenTypeToString(type) + ", got " +
                         tokenTypeToString(t.m_type) + " on line " +
                         std::to_string(t.m_line) + ".");
  }
}

std::string_view Parser::expectIdentifier() {
  return expect(TokenType::Identifier).m_src;
}

std::string_view Parser::expectIdentifier(std::string expected) {
  Token token = expect(TokenType::Identifier);

  if (token.m_src.compare(expected) == 0) {
    return token.m_src;
  } else {
    throw ParseException("Expected identifier '" + expected + "', got '" +
                         std::string(token.m_src) + "' on line " +
                         std::to_string(token.m_line) + ".");
  }
}

bool Parser::at(TokenType type) {
  return m_idx < m_tokens.size() && m_tokens[m_idx].m_type == type;
}

bool Parser::atIdentifier(std::string expected) {
  if (at(TokenType::Identifier)) {
    return peek().m_src.compare(expected) == 0;
  } else {
    return false;
  }
}

std::vector<Function> Parser::parse() {
  std::vector<Function> fns;
  while (!at(TokenType::Eof)) {
    fns.push_back(parseFunction());
  }
  return fns;
}

TypedValue Parser::parseFunctionArgument() {
  std::string_view argName = expectIdentifier();
  expect(TokenType::Colon);
  std::string_view argType = expectIdentifier();
  if (peek().m_type != TokenType::RParen) {
    expect(TokenType::Comma);
  }
  return TypedValue(argName, argType);
}

Statement Parser::parseStatement() {
  if (atIdentifier("return")) {
    expectIdentifier("return");
    Expression expr = parseExpression();
    expect(TokenType::Semicolon);

    return Statement(Statement::Type::Return, new ReturnStatement(expr));
  } else if (atIdentifier("if")) {
    expectIdentifier("if");
    Expression condition = parseExpression();

    expect(TokenType::LCurly);
    std::vector<Statement> bodyIfTrue;
    while (!at(TokenType::RCurly)) {
      bodyIfTrue.push_back(parseStatement());
    }
    expect(TokenType::RCurly);

    std::optional<std::vector<Statement>> bodyIfFalse = std::nullopt;
    if (atIdentifier("else")) {
      expectIdentifier("else");

      expect(TokenType::LCurly);
      std::vector<Statement> tmpBody;
      while (!at(TokenType::RCurly)) {
        tmpBody.push_back(parseStatement());
      }
      expect(TokenType::RCurly);

      bodyIfFalse = tmpBody;
    }

    return Statement(Statement::Type::If,
                     new IfStatement(condition, bodyIfTrue, bodyIfFalse));
  } else if (atIdentifier("let")) {
    expectIdentifier("let");
    std::string_view name = expect(TokenType::Identifier).m_src;
    expect(TokenType::Colon);
    std::string_view type = expect(TokenType::Identifier).m_src;
    expect(TokenType::Eq);
    Expression initialValue = parseExpression();
    expect(TokenType::Semicolon);

    (*m_currEnv)[name] = type;
    return Statement(Statement::Type::Let,
                     new LetStatement(name, type, initialValue));
  } else {
    Token curr = peek();
    throw ParseException("Expected statement, got " + std::string(curr.m_src) +
                         " on line " + std::to_string(curr.m_line));
  }
}

Function Parser::parseFunction() {
  m_currEnv = new std::unordered_map<std::string_view, std::string_view>();

  expectIdentifier("fn");
  std::string_view functionName = expectIdentifier();

  expect(TokenType::LParen);
  std::vector<TypedValue> args;
  while (!at(TokenType::RParen)) {
    args.push_back(parseFunctionArgument());
  }
  expect(TokenType::RParen);

  // TODO: optional return type
  expect(TokenType::Arrow);
  std::string_view returnType = expectIdentifier();
  m_symbolTable[functionName] = returnType;

  expect(TokenType::LCurly);
  std::vector<Statement> statements;
  while (!at(TokenType::RCurly)) {
    statements.push_back(parseStatement());
  }
  expect(TokenType::RCurly);

  return Function(functionName, m_currEnv, args, returnType, statements);
}

Expression Parser::parseExpression() { return parseExpressionBP(0); }

Expression Parser::parseExpressionBP(int minBP) {
  Expression lhs;

  if (at(TokenType::Integer)) {
    Token t = expect(TokenType::Integer);
    long val = std::stol(std::string(t.m_src));
    lhs = Expression(Expression::Type::Value,
                     new ValueExpression(val, ValueExpressionType::i64));
  } else if (at(TokenType::Float)) {
    Token t = expect(TokenType::Float);
    double val = std::stod(std::string(t.m_src));
    lhs = Expression(Expression::Type::Value,
                     new ValueExpression(val, ValueExpressionType::f64));
  } else if (at(TokenType::String)) {
    Token t = expect(TokenType::String);
    lhs = Expression(Expression::Type::Value,
                     new ValueExpression(t.m_src, ValueExpressionType::String));
  } else if (at(TokenType::Identifier)) {
    if (peek(1).m_type == TokenType::LParen) {
      std::string_view name = expect(TokenType::Identifier).m_src;
      std::vector<Expression> args;
      expect(TokenType::LParen);
      while (!at(TokenType::RParen)) {
        args.push_back(parseExpression());
      }
      expect(TokenType::RParen);
      lhs = Expression(Expression::Type::Value,
                       new ValueExpression(FunctionCall(name, args),
                                           ValueExpressionType::Call));
    } else {
      Token t = expect(TokenType::Identifier);
      lhs =
          Expression(Expression::Type::Value,
                     new ValueExpression(t.m_src, ValueExpressionType::Ident));
    }
  } else if (at(TokenType::LParen)) {
    expect(TokenType::LParen);
    lhs = parseExpression();
    expect(TokenType::RParen);
  } else {
    throw ParseException("Expected part of expression, got '" +
                         std::string(peek().m_src) + "'.");
  }

  while (true) {
    BinaryExpression::Type op;
    Token curr = peek();
    if (curr.m_type == TokenType::Plus) {
      op = BinaryExpression::Type::Add;
    } else if (curr.m_type == TokenType::Minus) {
      op = BinaryExpression::Type::Sub;
    } else if (curr.m_type == TokenType::Star) {
      op = BinaryExpression::Type::Mul;
    } else if (curr.m_type == TokenType::Slash) {
      op = BinaryExpression::Type::Div;
    } else if (curr.m_type == TokenType::EqEq) {
      op = BinaryExpression::Type::Compare;
    } else if (curr.m_type == TokenType::PipePipe) {
      op = BinaryExpression::Type::Compare;
    } else {
      break;
    }

    int lbp = BinaryExpression::infixBP(op);

    if (lbp > 0) {
      if (lbp < minBP) {
        break;
      }

      m_idx++;  // Skip over op
      Expression rhs = parseExpressionBP(lbp + 1);
      lhs = Expression(Expression::Type::BinOp,
                       new BinaryExpression(op, lhs, rhs));
      continue;
    }
  }

  return lhs;
}

TypedValue::TypedValue(std::string_view name, std::string_view type)
    : m_name(name), m_type(type) {}
TypedValue::~TypedValue() {}

Expression::Expression() {}
Expression::~Expression() {}
Expression::Expression(Type type, BaseExpression *expression)
    : m_type(type), m_expression(expression) {}

BinaryExpression::BinaryExpression(Type type, Expression left, Expression right)
    : m_type(type), m_left(left), m_right(right) {}
BinaryExpression::~BinaryExpression() {}

int BinaryExpression::infixBP(Type op) {
  switch (op) {
    case Compare:
      return 10;

    case Add:
    case Sub:
      return 20;

    case Mul:
    case Div:
      return 30;

    default:
      return 0;
  }
}

ValueExpression::ValueExpression(ValueExpressionValue value,
                                 ValueExpressionType type)
    : m_value(value), m_type(type) {}
ValueExpression::~ValueExpression() {}
std::string ValueExpression::valueString() {
  if (m_type == ValueExpressionType::i64) {
    return std::to_string(std::get<long>(m_value));
  } else if (m_type == ValueExpressionType::f64) {
    return std::to_string(std::get<double>(m_value));
  } else if (m_type == ValueExpressionType::String) {
    return '"' + std::string(std::get<std::string_view>(m_value)) + '"';
  } else if (m_type == ValueExpressionType::Ident) {
    return std::string(std::get<std::string_view>(m_value));
  } else if (m_type == ValueExpressionType::Call) {
    FunctionCall fn = std::get<FunctionCall>(m_value);
    std::string args;
    for (int i = 0; i < fn.m_args.size(); i++) {
      Expression arg = fn.m_args[i];
      if (i == fn.m_args.size() - 1) {
        args += debugPrintExpr(arg);
      } else {
        args += debugPrintExpr(arg) + ", ";
      }
    }
    return std::string(fn.m_name) + "(" + args + ")";
  } else {
    assert(!"Unknown type in value expression");
  }
}

Statement::Statement(Type type, BaseStatement *statement)
    : m_type(type), m_statement(statement) {}
Statement::~Statement() {}

LetStatement::LetStatement(std::string_view name, std::string_view type,
                           Expression initialValue)
    : m_name(name), m_type(type), m_initialValue(initialValue) {}
LetStatement::~LetStatement() {}

IfStatement::IfStatement(Expression condition,
                         std::vector<Statement> bodyIfTrue,
                         std::optional<std::vector<Statement>> bodyIfFalse)
    : m_condition(condition),
      m_bodyIfTrue(bodyIfTrue),
      m_bodyIfFalse(bodyIfFalse) {}
IfStatement::~IfStatement() {}

ReturnStatement::ReturnStatement(Expression value) : m_value(value) {}
ReturnStatement::~ReturnStatement() {}

Function::Function(std::string_view name,
                   std::unordered_map<std::string_view, std::string_view> *env,
                   std::vector<TypedValue> args, std::string_view returnType,
                   std::vector<Statement> statements)
    : m_name(name),
      m_env(env),
      m_args(args),
      m_returnType(returnType),
      m_statements(statements) {}
Function::~Function() {}

FunctionCall::FunctionCall(std::string_view name, std::vector<Expression> args)
    : m_name(name), m_args(args) {}
FunctionCall::~FunctionCall() {}

void walkExpression(ASTVisitor *visitor, Expression *expr) {
  Expression::Type type = expr->m_type;
  if (type == Expression::Type::BinOp) {
    visitor->visitBinaryExpression(
        static_cast<BinaryExpression *>(expr->m_expression));
  } else if (type == Expression::Type::Value) {
    visitor->visitValueExpression(
        static_cast<ValueExpression *>(expr->m_expression));
  } else {
    assert(!"Unknown expression type");
  }
}

void walkValueExpression(ASTVisitor *visitor, ValueExpression *expr) {
  // what do here
}

void walkBinaryExpression(ASTVisitor *visitor, BinaryExpression *expr) {
  visitor->visitExpression(&expr->m_left);
  visitor->visitExpression(&expr->m_right);
}

void walkLetStatement(ASTVisitor *visitor, LetStatement *stmt) {
  visitor->visitExpression(&stmt->m_initialValue);
}

void walkIfStatement(ASTVisitor *visitor, IfStatement *stmt) {
  visitor->visitExpression(&stmt->m_condition);

  for (Statement s : stmt->m_bodyIfTrue) {
    visitor->visitStatement(&s);
  }

  if (stmt->m_bodyIfFalse.has_value()) {
    for (Statement s : stmt->m_bodyIfFalse.value()) {
      visitor->visitStatement(&s);
    }
  }
}

void walkReturnStatement(ASTVisitor *visitor, ReturnStatement *stmt) {
  visitor->visitExpression(&stmt->m_value);
}

void walkStatement(ASTVisitor *visitor, Statement *statement) {
  Statement::Type type = statement->m_type;
  if (type == Statement::Type::If) {
    visitor->visitIfStatement(
        static_cast<IfStatement *>(statement->m_statement));
  } else if (type == Statement::Type::Return) {
    visitor->visitReturnStatement(
        static_cast<ReturnStatement *>(statement->m_statement));
  } else if (type == Statement::Type::Let) {
    visitor->visitLetStatement(
        static_cast<LetStatement *>(statement->m_statement));
  } else {
    assert(!"Unknown statement type");
  }
}

void walkFunction(ASTVisitor *visitor, Function *function) {
  for (Statement s : function->m_statements) {
    visitor->visitStatement(&s);
  }
}