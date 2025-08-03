import { InvalidSyntaxError } from "./errors";

export const TokenTypes = {
  IGNORE: "IGNORE",
  COMMENT: "COMMENT",
  BOOL_LITERAL: "BOOL_LITERAL",
  NUMBER_LITERAL: "NUMBER_LITERAL",
  STRING_LITERAL: "STRING_LITERAL",
  STRING_INTERPOLATED: "STRING_INTERPOLATED",
  NOT: "!",
  EQUALS: "==",
  NOT_EQUALS: "!=",
  EQUAL: "=",
  LTE: "<=",
  GTE: ">=",
  LT: "<",
  GT: ">",
  INCREMENT: "++",
  PLUS_EQUAL: "+",
  PLUS: "+",
  DECREMENT: "--",
  MINUS_EQUAL: "-=",
  MINUS: "-",
  POW_EQUAL: "**=",
  POW: "**",
  MUL_EQUAL: "*=",
  MUL: "*",
  DIV_EQUAL: "/=",
  DIV: "/",
  MOD_EQUAL: "%=",
  MOD: "%",
  TERNARY: "?",
  LPAREN: "(",
  RPAREN: ")",
  DOT: ".",
  COMMA: ",",
  LBRACE: "{",
  RBRACE: "}",
  COLON: ":",
  SEMICOLON: ";",
  STRUCT: "STRUCT",
  FUNCTION: "FUNCTION",
  RETURN: "RETURN",
  IF: "IF",
  ELSE: "ELSE",
  FOR: "FOR",
  IDENTIFIER: "IDENTIFIER",
} as const;

export const TokenTypeNames = Object.fromEntries(
  Object.entries(TokenTypes).map(([key, value]) => [value, key])
) as Record<
  (typeof TokenTypes)[keyof typeof TokenTypes],
  keyof typeof TokenTypes
>;

export type TokenType = (typeof TokenTypes)[keyof typeof TokenTypes];

interface Parser {
  matcher: RegExp;
  type: TokenType;
  nop?: boolean;
  extractor?: (match: string) => string;
}

export interface Token {
  type: TokenType;
  start: number;
  end: number;
  value?: string;
  nop?: boolean;
}

export class Tokenizer {
  #parsers;
  #offset;

  constructor(parsers: Parser[], offset = 0) {
    this.#parsers = parsers;
    this.#offset = offset;
  }

  *tokenize(text: string): Generator<Token, void, unknown> {
    let index = 0;

    while (index < text.length) {
      let matched = false;

      for (const { matcher, type, nop, extractor } of this.#parsers) {
        const current = new RegExp(matcher.source, "y");
        current.lastIndex = index;
        const match = current.exec(text);

        if (match === null) continue;

        const raw = match[0];
        const start = this.#offset + index;
        const end = start + raw.length;

        index += raw.length;

        const token = {
          type,
          start,
          end,
          value: extractor ? extractor(raw) : undefined,
          nop,
        };

        yield token;

        matched = true;
        break;
      }

      if (!matched) {
        throw new InvalidSyntaxError(
          `Unexpected token "${text[this.#offset + index]}" at offset ${
            this.#offset + index
          }`,
          this.#offset + index
        );
      }
    }
  }
}

export function tokenize(
  text: string,
  options?: { offset?: number }
): Generator<Token, void, unknown> {
  const tokenizer = new Tokenizer(
    [
      { matcher: /[ \t\r\n]+/, type: TokenTypes.IGNORE, nop: true },
      {
        matcher: /\/\/(.*?)(?=\r?\n|$)/,
        type: TokenTypes.COMMENT,
        nop: true,
      },
      {
        matcher: /\/\*[\s\S]*?\*\//,
        type: TokenTypes.COMMENT,
        nop: true,
      },
      {
        matcher: /true|false/,
        type: TokenTypes.BOOL_LITERAL,
        extractor: (match) => match,
      },
      {
        matcher: /-?0[xX][0-9a-fA-F]+/,
        type: TokenTypes.NUMBER_LITERAL,
        extractor: (match) => parseInt(match, 16).toString(),
      },
      {
        matcher: /-?[0-9]+\.?[0-9]*/,
        type: TokenTypes.NUMBER_LITERAL,
        extractor: (match) => BigInt(match).toString(),
      },
      {
        matcher: /"[^"\r\n]*"/,
        type: TokenTypes.STRING_LITERAL,
        extractor: (match) => match.slice(1, -1),
      },
      {
        matcher: /\$"[^"\r\n]*"/,
        type: TokenTypes.STRING_INTERPOLATED,
        extractor: (match) => match,
      },
      { matcher: /!/, type: TokenTypes.NOT },
      { matcher: /==/, type: TokenTypes.EQUALS },
      { matcher: /!=/, type: TokenTypes.NOT_EQUALS },
      { matcher: /=/, type: TokenTypes.EQUAL },
      { matcher: /<=/, type: TokenTypes.LTE },
      { matcher: />=/, type: TokenTypes.GTE },
      { matcher: /</, type: TokenTypes.LT },
      { matcher: />/, type: TokenTypes.GT },
      { matcher: /\+\+/, type: TokenTypes.INCREMENT },
      { matcher: /\+=/, type: TokenTypes.PLUS_EQUAL },
      { matcher: /\+/, type: TokenTypes.PLUS },
      { matcher: /--/, type: TokenTypes.DECREMENT },
      { matcher: /-=/, type: TokenTypes.MINUS_EQUAL },
      { matcher: /\-/, type: TokenTypes.MINUS },
      { matcher: /\*\*/, type: TokenTypes.POW },
      { matcher: /\*=/, type: TokenTypes.MUL_EQUAL },
      { matcher: /\*/, type: TokenTypes.MUL },
      { matcher: /%=/, type: TokenTypes.MOD_EQUAL },
      { matcher: /%/, type: TokenTypes.MOD },
      { matcher: /\/=/, type: TokenTypes.DIV_EQUAL },
      { matcher: /\//, type: TokenTypes.DIV },
      { matcher: /\?/, type: TokenTypes.TERNARY },
      { matcher: /\(/, type: TokenTypes.LPAREN },
      { matcher: /\)/, type: TokenTypes.RPAREN },
      { matcher: /\./, type: TokenTypes.DOT },
      { matcher: /,/, type: TokenTypes.COMMA },
      { matcher: /{/, type: TokenTypes.LBRACE },
      { matcher: /}/, type: TokenTypes.RBRACE },
      { matcher: /:/, type: TokenTypes.COLON },
      { matcher: /;/, type: TokenTypes.SEMICOLON },
      { matcher: /struct/, type: TokenTypes.STRUCT },
      { matcher: /fn/, type: TokenTypes.FUNCTION },
      { matcher: /return/, type: TokenTypes.RETURN },
      { matcher: /if/, type: TokenTypes.IF },
      { matcher: /else/, type: TokenTypes.ELSE },
      { matcher: /for/, type: TokenTypes.FOR },
      {
        matcher: /[a-zA-Z$_][a-zA-Z0-9$_]*/,
        type: TokenTypes.IDENTIFIER,
        extractor: (match) => match,
      },
    ],
    options?.offset
  );
  return tokenizer.tokenize(text);
}
