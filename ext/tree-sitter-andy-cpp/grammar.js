/**
 * @file Tree-sitter grammar for the Andy C++ (`.ndc`) language.
 * @author Andy C++ contributors
 * @license MIT
 *
 * The precedence ladder below mirrors the recursive-descent parser in
 * `ndc_parser/src/parser.rs` (loosest binding first):
 *
 *   assignment  <  comma/sequence  <  or  <  and  <  not  <  range
 *     <  comparison/in  <  spaceship  <  shift  <  |  <  ~  <  &
 *     <  + - ++ <>  <  * / \ % %%  <  ^ (right assoc)  <  unary ! - ~
 *     <  postfix call/index/member
 */

/* eslint-disable arrow-parens */
/* eslint-disable-next-line spaced-comment */
/// <reference types="tree-sitter-cli/dsl" />
// @ts-check

const PREC = {
  assign: 1,
  sequence: 2,
  or: 3,
  and: 4,
  not: 5,
  range: 6,
  compare: 7,
  spaceship: 8,
  shift: 9,
  bit_or: 10,
  bit_xor: 11,
  bit_and: 12,
  term: 13,
  factor: 14,
  exponent: 15,
  unary: 16,
  call: 17,
};

/** @param {RuleOrLiteral} rule */
function commaSep1(rule) {
  return seq(rule, repeat(seq(',', rule)));
}

/** @param {RuleOrLiteral} rule */
function commaSep(rule) {
  return optional(commaSep1(rule));
}

module.exports = grammar({
  name: 'andy_cpp',

  word: $ => $.identifier,

  externals: $ => [$.named_op_assign, $.raw_string],

  extras: $ => [/\s/, $.comment],

  conflicts: $ => [
    [$._pattern, $._expression],
    [$.if_expression, $.if_guard],
  ],

  rules: {
    source_file: $ => repeat($._statement),

    _statement: $ => choice(
      $.variable_declaration,
      prec.right(seq($._expression_or_sequence, optional(';'))),
      ';',
    ),

    // ------------------------------------------------------------------
    // Declarations
    // ------------------------------------------------------------------

    variable_declaration: $ => prec.right(seq(
      'let',
      field('name', $._let_target),
      optional(seq(':', field('type', $._type))),
      '=',
      field('value', $._expression_or_sequence),
      optional(';'),
    )),

    _let_target: $ => choice(
      $._pattern,
      $.pattern_sequence,
    ),

    pattern_sequence: $ => prec(PREC.sequence, seq(
      $._pattern,
      repeat1(seq(',', $._pattern)),
    )),

    _pattern: $ => choice(
      $.identifier,
      $.tuple,
      $.list,
      $.index,
      $.parenthesized_expression,
    ),

    function_definition: $ => prec.right(seq(
      optional('pure'),
      'fn',
      optional(field('name', $.identifier)),
      field('parameters', $.parameters),
      optional(seq('->', field('return_type', $._type))),
      choice(
        field('body', $.block),
        seq('=>', field('body', $._expression)),
      ),
    )),

    parameters: $ => seq(
      '(',
      commaSep($.parameter),
      optional(','),
      ')',
    ),

    parameter: $ => seq(
      field('name', choice($.identifier, $.tuple, $.list)),
      optional(seq(':', field('type', $._type))),
    ),

    // ------------------------------------------------------------------
    // Type annotations
    // ------------------------------------------------------------------

    _type: $ => choice(
      $.generic_type,
      $.type_identifier,
      $.tuple_type,
    ),

    type_identifier: $ => /[A-Za-z_][A-Za-z0-9_]*/,

    generic_type: $ => seq(
      field('name', $.type_identifier),
      '<',
      commaSep($._type),
      optional(','),
      '>',
    ),

    tuple_type: $ => seq(
      '(',
      commaSep($._type),
      optional(','),
      ')',
    ),

    // ------------------------------------------------------------------
    // Expressions
    // ------------------------------------------------------------------

    _expression_or_sequence: $ => choice(
      $._expression,
      $.tuple_expression,
    ),

    // Bare comma tuple at statement / value position, e.g. `fn(x) => x, 2`
    tuple_expression: $ => prec.left(PREC.sequence, seq(
      $._expression,
      repeat1(seq(',', $._expression)),
    )),

    _expression: $ => choice(
      $.identifier,
      $._literal,
      $.unary_expression,
      $.not_expression,
      $.binary_expression,
      $.range_expression,
      $.assignment,
      $.augmented_assignment,
      $.call,
      $.index,
      $.member_expression,
      $.parenthesized_expression,
      $.tuple,
      $.unit,
      $.list,
      $.list_comprehension,
      $.map,
      $.map_comprehension,
      $.function_definition,
      $.if_expression,
      $.while_expression,
      $.for_expression,
      $.block,
      $.return_expression,
      $.break_expression,
      $.continue_expression,
    ),

    parenthesized_expression: $ => seq('(', $._expression, ')'),

    unit: $ => prec(1, seq('(', ')')),

    tuple: $ => seq(
      '(',
      $._expression,
      ',',
      optional(seq(commaSep1($._expression), optional(','))),
      ')',
    ),

    assignment: $ => prec.right(PREC.assign, seq(
      field('left', $._expression),
      '=',
      field('right', $._expression_or_sequence),
    )),

    augmented_assignment: $ => prec.right(PREC.assign, seq(
      field('left', $._expression),
      field('operator', choice(
        '+=', '-=', '*=', '/=', '\\=', '%=', '%%=', '^=',
        '&=', '|=', '~=', '++=', '<>=', '<<=', '>>=',
        // Named op-assign like `multiply=` (recognised by the external scanner).
        $.named_op_assign,
      )),
      field('right', $._expression_or_sequence),
    )),

    unary_expression: $ => prec(PREC.unary, seq(
      field('operator', choice('!', '-', '~')),
      field('operand', $._expression),
    )),

    not_expression: $ => prec(PREC.not, seq(
      field('operator', 'not'),
      field('operand', $._expression),
    )),

    binary_expression: $ => {
      const table = [
        ['or', PREC.or],
        ['and', PREC.and],
        ['<=>', PREC.spaceship],
        ['>=<', PREC.spaceship],
        ['<<', PREC.shift],
        ['>>', PREC.shift],
        ['|', PREC.bit_or],
        ['~', PREC.bit_xor],
        ['&', PREC.bit_and],
        ['+', PREC.term],
        ['-', PREC.term],
        ['++', PREC.term],
        ['<>', PREC.term],
        ['*', PREC.factor],
        ['/', PREC.factor],
        ['\\', PREC.factor],
        ['%', PREC.factor],
        ['%%', PREC.factor],
      ];

      const left = table.map(([operator, precedence]) =>
        prec.left(precedence, seq(
          field('left', $._expression),
          field('operator', operator),
          field('right', $._expression),
        )),
      );

      // Comparison operators may be preceded by `not`, e.g. `a not in b`,
      // which desugars to `not (a in b)`.
      const compareOps = ['==', '!=', '>', '>=', '<', '<=', 'in'];
      const compare = prec.left(PREC.compare, seq(
        field('left', $._expression),
        optional('not'),
        field('operator', choice(...compareOps)),
        field('right', $._expression),
      ));

      // Exponent is right-associative.
      const right = prec.right(PREC.exponent, seq(
        field('left', $._expression),
        field('operator', '^'),
        field('right', $._expression),
      ));

      return choice(...left, compare, right);
    },

    range_expression: $ => prec.left(PREC.range, choice(
      seq(field('start', $._expression), choice('..', '..='), optional(field('end', $._expression))),
      seq(choice('..', '..='), field('end', $._expression)),
    )),

    call: $ => prec(PREC.call, seq(
      field('function', $._expression),
      field('arguments', $.arguments),
    )),

    arguments: $ => seq(
      '(',
      commaSep($._expression),
      optional(','),
      ')',
    ),

    index: $ => prec(PREC.call, seq(
      field('value', $._expression),
      '[',
      field('index', $._expression),
      ']',
    )),

    member_expression: $ => prec.right(PREC.call, seq(
      field('object', $._expression),
      '.',
      field('method', $.identifier),
      optional(field('arguments', $.arguments)),
    )),

    // ------------------------------------------------------------------
    // Collections
    // ------------------------------------------------------------------

    list: $ => seq(
      '[',
      optional(seq(commaSep1($._expression), optional(','))),
      ']',
    ),

    list_comprehension: $ => seq(
      '[',
      // The yielded value may be a bare comma tuple, e.g. `[x, y for …]`.
      field('body', $._expression_or_sequence),
      $.comprehension_clauses,
      ']',
    ),

    map: $ => seq(
      '%{',
      optional(choice(
        $.map_default,
        seq($.map_default, ',', commaSep1($.map_entry), optional(',')),
        seq(commaSep1($.map_entry), optional(',')),
      )),
      '}',
    ),

    map_comprehension: $ => seq(
      '%{',
      optional(seq($.map_default, ',')),
      field('body', $.map_entry),
      $.comprehension_clauses,
      '}',
    ),

    // Default value for missing keys, e.g. `%{:0}` or `%{:false, ...}`.
    map_default: $ => seq(':', field('value', $._expression)),

    map_entry: $ => choice(
      seq(field('key', $._expression), ':', field('value', $._expression)),
      $._expression,
    ),

    comprehension_clauses: $ => seq(
      'for',
      commaSep1($._comprehension_clause),
      optional(','),
    ),

    _comprehension_clause: $ => choice(
      $.for_binding,
      $.if_guard,
    ),

    // ------------------------------------------------------------------
    // Control flow
    // ------------------------------------------------------------------

    if_expression: $ => prec.right(seq(
      'if',
      field('condition', $._expression),
      field('consequence', $.block),
      optional(seq(
        'else',
        field('alternative', choice($.if_expression, $.block)),
      )),
    )),

    while_expression: $ => seq(
      'while',
      field('condition', $._expression),
      field('body', $.block),
    ),

    for_expression: $ => seq(
      'for',
      commaSep1($._comprehension_clause),
      field('body', $.block),
    ),

    for_binding: $ => seq(
      field('pattern', $._let_target),
      'in',
      field('sequence', $._expression),
    ),

    if_guard: $ => seq('if', field('condition', $._expression)),

    block: $ => seq(
      '{',
      repeat($._statement),
      '}',
    ),

    return_expression: $ => prec.right(seq(
      'return',
      optional($._expression_or_sequence),
    )),

    break_expression: _ => 'break',

    continue_expression: _ => 'continue',

    // ------------------------------------------------------------------
    // Literals
    // ------------------------------------------------------------------

    _literal: $ => choice(
      $.integer,
      $.float,
      $.complex,
      $.boolean,
      $.string,
      $.raw_string,
      $.special_constant,
    ),

    integer: _ => token(choice(
      /0b[01][01_]*/,
      /0o[0-7][0-7_]*/,
      /0x[0-9A-Fa-f][0-9A-Fa-f_]*/,
      /[0-9]+r[0-9A-Za-z][0-9A-Za-z_]*/, // arbitrary radix, e.g. 16rFF
      /[0-9][0-9_]*/,
    )),

    float: _ => token(/[0-9][0-9_]*\.[0-9][0-9_]*/),

    // Imaginary part of a complex literal: `3i`, `2.5j`
    complex: _ => token(/[0-9][0-9_]*(\.[0-9][0-9_]*)?[ij]/),

    boolean: _ => choice('true', 'false'),

    special_constant: _ => choice('Inf', 'NaN'),

    string: $ => seq(
      '"',
      repeat(choice(
        $.escape_sequence,
        token.immediate(prec(1, /[^"\\]+/)),
      )),
      '"',
    ),

    escape_sequence: _ => token.immediate(/\\[nrt0\\"]/),

    // `raw_string` (`r"..."`, `r#"..."#`, `r###"..."###`, …) is recognised by
    // the external scanner so its balanced `#` fence can contain quotes.

    // ------------------------------------------------------------------
    // Identifiers & comments
    // ------------------------------------------------------------------

    // Identifiers may contain and end with `?` (but not start with it).
    identifier: _ => /[A-Za-z_][A-Za-z0-9_]*\??/,

    comment: _ => token(seq(choice('//', '#!'), /[^\n]*/)),
  },
});
