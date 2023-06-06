#ifndef TOKEN_H
#define TOKEN_H

#include <string>
#include <string_view>

enum class TokenType {
  Identifier,
  Semicolon,
  Integer,
  String,
  LParen,
  RParen,
  LCurly,
  RCurly,
  Minus,
  Slash,
  Arrow,
  Colon,
  Comma,
  Plus,
  Star,
  EqEq,
  Eof,
  Eq,
};

class Token {
 public:
  Token(TokenType type, std::string_view src, int line);
  ~Token(){};

  void debug_display();

  std::string_view m_src;
  TokenType m_type;
  int m_line;
};

std::string tokenTypeToString(TokenType type);

#endif