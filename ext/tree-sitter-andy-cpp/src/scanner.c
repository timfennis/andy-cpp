#include "tree_sitter/parser.h"
#include <string.h>
#include <wctype.h>

// External tokens that need more lookahead than tree-sitter's static lexer can
// express:
//
//   NAMED_OP_ASSIGN  the `multiply=` in `x multiply= 10` — an identifier glued
//                    to a single `=` that is NOT part of `==`. The lexer forms
//                    this only when the `=` is not followed by another `=`.
//
//   RAW_STRING       `r"..."`, `r#"..."#`, `r###"..."###`, … The content runs
//                    until a `"` followed by exactly as many `#` as opened the
//                    literal, so the fence can contain embedded quotes. A
//                    balanced fence cannot be matched by a regex token.

enum TokenType {
  NAMED_OP_ASSIGN,
  RAW_STRING,
};

void *tree_sitter_andy_cpp_external_scanner_create(void) { return NULL; }
void tree_sitter_andy_cpp_external_scanner_destroy(void *payload) {}
unsigned tree_sitter_andy_cpp_external_scanner_serialize(void *payload, char *buffer) { return 0; }
void tree_sitter_andy_cpp_external_scanner_deserialize(void *payload, const char *buffer, unsigned length) {}

static bool is_id_start(int32_t c) { return c == '_' || iswalpha(c); }
static bool is_id_continue(int32_t c) { return c == '_' || c == '?' || iswalnum(c); }

// Keywords are never augmentable operator names, so they must not be mistaken
// for the identifier part of a named op-assign (e.g. `in`, `and`).
static bool is_keyword(const char *s) {
  static const char *const kws[] = {
      "let", "and", "or", "not", "while", "if", "else", "fn", "for", "in",
      "true", "false", "return", "break", "continue", "pure", "Inf", "NaN",
  };
  for (unsigned i = 0; i < sizeof(kws) / sizeof(kws[0]); i++) {
    if (strcmp(s, kws[i]) == 0) return true;
  }
  return false;
}

// `=` must follow the identifier and must not be `==`. The caller has already
// consumed the identifier into `name`.
static bool finish_op_assign(TSLexer *lexer, const char *name) {
  if (is_keyword(name)) return false;
  if (lexer->lookahead != '=') return false;
  lexer->advance(lexer, false);
  if (lexer->lookahead == '=') return false; // `==` is equality, not op-assign
  lexer->result_symbol = NAMED_OP_ASSIGN;
  lexer->mark_end(lexer);
  return true;
}

// Entered with the opening `r` already consumed and the next character being
// `#` or `"`. Scans through the matching closing fence.
static bool scan_raw_string(TSLexer *lexer) {
  unsigned hashes = 0;
  while (lexer->lookahead == '#') {
    lexer->advance(lexer, false);
    hashes++;
  }
  if (lexer->lookahead != '"') return false;
  lexer->advance(lexer, false); // opening quote

  for (;;) {
    if (lexer->lookahead == 0) return false; // unterminated
    if (lexer->lookahead == '"') {
      lexer->advance(lexer, false);
      unsigned matched = 0;
      while (matched < hashes && lexer->lookahead == '#') {
        lexer->advance(lexer, false);
        matched++;
      }
      if (matched == hashes) {
        lexer->result_symbol = RAW_STRING;
        lexer->mark_end(lexer);
        return true;
      }
      // A `"` followed by too few `#` is part of the content; keep scanning.
    } else {
      lexer->advance(lexer, false);
    }
  }
}

bool tree_sitter_andy_cpp_external_scanner_scan(void *payload, TSLexer *lexer,
                                                const bool *valid_symbols) {
  while (iswspace(lexer->lookahead)) lexer->advance(lexer, true);

  // A leading `r` may begin a raw string or an identifier (which could form a
  // named op-assign). Disambiguate on the character right after `r`.
  if (lexer->lookahead == 'r') {
    lexer->advance(lexer, false);
    if (lexer->lookahead == '#' || lexer->lookahead == '"') {
      return valid_symbols[RAW_STRING] && scan_raw_string(lexer);
    }
    if (!valid_symbols[NAMED_OP_ASSIGN]) return false;
    char buf[64];
    unsigned len = 0;
    buf[len++] = 'r';
    while (is_id_continue(lexer->lookahead)) {
      if (len < sizeof(buf) - 1) buf[len++] = (char)lexer->lookahead;
      lexer->advance(lexer, false);
    }
    buf[len] = '\0';
    return finish_op_assign(lexer, buf);
  }

  if (valid_symbols[NAMED_OP_ASSIGN] && is_id_start(lexer->lookahead)) {
    char buf[64];
    unsigned len = 0;
    while (is_id_continue(lexer->lookahead)) {
      if (len < sizeof(buf) - 1) buf[len++] = (char)lexer->lookahead;
      lexer->advance(lexer, false);
    }
    buf[len] = '\0';
    return finish_op_assign(lexer, buf);
  }

  return false;
}
