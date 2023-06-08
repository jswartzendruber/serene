#include "lexer.h"

Lexer::Lexer(std::string_view src) : m_src(src), m_tokens() {
  m_line = 1;
  m_idx = 0;
}
Lexer::~Lexer() {}

inline bool isNum(char c) { return c >= '0' && c <= '9'; }

inline bool isAlpha(char c) {
  return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z');
}

inline bool isAlphanumeric(char c) { return isAlpha(c) || isNum(c); }

std::vector<Token> Lexer::lex() {
  int tempIdx;
  int startIdx;
  while (m_idx < m_src.size()) {
    char c = m_src[m_idx];

    if (isAlpha(c)) {
      startIdx = m_idx;
      tempIdx = m_idx;
      while (tempIdx < m_src.size() && isAlphanumeric(m_src[tempIdx])) {
        tempIdx++;
      }

      addToken(TokenType::Identifier, tempIdx - startIdx);
    } else if (isNum(c)) {
      startIdx = m_idx;
      tempIdx = m_idx;
      bool floating = false;
      while (tempIdx < m_src.size()) {
        if (m_src[tempIdx] == '.') {
          floating = true;
        } else if (!isNum(m_src[tempIdx])) {
          break;
        }
        tempIdx++;
      }

      if (floating) {
        addToken(TokenType::Float, tempIdx - startIdx);
      } else {
        addToken(TokenType::Integer, tempIdx - startIdx);
      }
    } else {
      switch (c) {
        case '\n':
          m_line++;
        case ' ':
        case '\t':
        case '\r':
          m_idx++;
          break;

        case '(':
          addToken(TokenType::LParen, 1);
          break;

        case ')':
          addToken(TokenType::RParen, 1);
          break;

        case '{':
          addToken(TokenType::LCurly, 1);
          break;

        case '}':
          addToken(TokenType::RCurly, 1);
          break;

        case ';':
          addToken(TokenType::Semicolon, 1);
          break;

        case '+':
          addToken(TokenType::Plus, 1);
          break;

        case '-':
          if (m_idx < m_src.size() && m_src[m_idx + 1] == '>') {
            addToken(TokenType::Arrow, 2);
          } else {
            addToken(TokenType::Minus, 1);
          }
          break;

        case '*':
          addToken(TokenType::Star, 1);
          break;

        case '/':
          addToken(TokenType::Slash, 1);
          break;

        case ':':
          addToken(TokenType::Colon, 1);
          break;

        case ',':
          addToken(TokenType::Comma, 1);
          break;

        case '=':
          if (m_idx < m_src.size() && m_src[m_idx + 1] == '=') {
            addToken(TokenType::EqEq, 2);
          } else {
            addToken(TokenType::Eq, 1);
          }
          break;

        case '|':
          if (m_idx < m_src.size() && m_src[m_idx + 1] == '|') {
            addToken(TokenType::PipePipe, 2);
          } else {
            addToken(TokenType::Or, 1);
          }
          break;

        case '"':
          m_idx++;  // Skip opening quote
          startIdx = m_idx;
          tempIdx = m_idx;
          while (tempIdx < m_src.size() && m_src[tempIdx] != '"') {
            tempIdx++;
          }

          addToken(TokenType::String, tempIdx - startIdx);
          m_idx++;  // Skip closing quote
          break;

        default:
          std::cout << "Unknown token " << (int)c << " '" << c << "'\n";
          m_idx++;
          break;
      }
    }
  }

  m_tokens.push_back(Token(TokenType::Eof, "EOF", m_line));
  return m_tokens;
}

void Lexer::addToken(TokenType type, int len) {
  Token token = Token(type, m_src.substr(m_idx, len), m_line);
  m_idx += len;
  m_tokens.push_back(token);
}
