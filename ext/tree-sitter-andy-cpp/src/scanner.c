#include "tree_sitter/parser.h"
#include <string.h>
#include <wctype.h>

// External token for named augmented assignment, e.g. the `multiply=` in
// `x multiply= 10`. The Andy C++ lexer forms this token when an identifier is
// immediately followed by a single `=` that is NOT part of `==`. That
// "not followed by `=`" rule needs one character of lookahead, which a static
// tree-sitter token cannot express (it would greedily eat `foo=` in `foo==bar`),
// so it is recognised here instead.

enum TokenType {
  NAMED_OP_ASSIGN,
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

bool tree_sitter_andy_cpp_external_scanner_scan(void *payload, TSLexer *lexer,
                                                const bool *valid_symbols) {
  if (!valid_symbols[NAMED_OP_ASSIGN]) return false;

  while (iswspace(lexer->lookahead)) lexer->advance(lexer, true);

  if (!is_id_start(lexer->lookahead)) return false;

  char buf[64];
  unsigned len = 0;
  while (is_id_continue(lexer->lookahead)) {
    if (len < sizeof(buf) - 1) buf[len++] = (char)lexer->lookahead;
    lexer->advance(lexer, false);
  }
  buf[len] = '\0';

  if (is_keyword(buf)) return false;

  if (lexer->lookahead != '=') return false;
  lexer->advance(lexer, false);
  if (lexer->lookahead == '=') return false; // `==` is equality, not op-assign

  lexer->result_symbol = NAMED_OP_ASSIGN;
  lexer->mark_end(lexer);
  return true;
}
