#include "token.h"

Token::Token(TokenType type, std::string_view src, int line) : m_src(src) {
  m_type = type;
  m_line = line;
}
Token::~Token() {}

std::string tokenTypeToString(TokenType type) {
  const char *s;
#define PROCESS_VAL(p) \
  case (p):            \
    s = #p;            \
    break;
  switch (type) {
    PROCESS_VAL(TokenType::Identifier);
    PROCESS_VAL(TokenType::Semicolon);
    PROCESS_VAL(TokenType::PipePipe);
    PROCESS_VAL(TokenType::Integer);
    PROCESS_VAL(TokenType::String);
    PROCESS_VAL(TokenType::LParen);
    PROCESS_VAL(TokenType::RParen);
    PROCESS_VAL(TokenType::LCurly);
    PROCESS_VAL(TokenType::RCurly);
    PROCESS_VAL(TokenType::Minus);
    PROCESS_VAL(TokenType::Slash);
    PROCESS_VAL(TokenType::Arrow);
    PROCESS_VAL(TokenType::Colon);
    PROCESS_VAL(TokenType::Comma);
    PROCESS_VAL(TokenType::Float);
    PROCESS_VAL(TokenType::Plus);
    PROCESS_VAL(TokenType::Star);
    PROCESS_VAL(TokenType::EqEq);
    PROCESS_VAL(TokenType::Eof);
    PROCESS_VAL(TokenType::Eq);
    PROCESS_VAL(TokenType::Or);

    default:
      s = "TokenType::UNKNOWN";
      break;
  }
#undef PROCESS_VAL

  return s + 11;  // hacky way to remove TokenType::
}

std::string pad_right(std::string const &str, size_t s) {
  if (str.size() < s)
    return str + std::string(s - str.size(), ' ');
  else
    return str;
}

std::string pad_left(std::string const &str, size_t s) {
  if (str.size() < s)
    return std::string(s - str.size(), ' ') + str;
  else
    return str;
}

void Token::debug_display() {
  std::cout << pad_right(tokenTypeToString(m_type), 12) << ":  " << m_src
            << "\n";
}
