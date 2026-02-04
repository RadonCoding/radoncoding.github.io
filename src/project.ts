
// --- FILE: .\main.ts ---

import "./style.css";

for (const element of document.getElementsByClassName("copy")) {
  if (!(element instanceof HTMLElement)) continue;

  element.addEventListener("click", () => {
    const text = element.getAttribute("data-text");

    if (!text) return;

    navigator.clipboard.writeText(text);
  });
}

// --- FILE: .\vite-env.d.ts ---

/// <reference types="vite/client" />

// --- FILE: .\compiler\errors.ts ---

export class LanguageError extends Error {
  offset: number;

  constructor(message: string, offset: number) {
    super(message);

    this.offset = offset;
  }
}
export class InvalidSyntaxError extends LanguageError {}
export class CompilerError extends LanguageError {}
export class RuntimeError extends LanguageError {}

// --- FILE: .\compiler\index.ts ---

import { Parser } from "./ast/Parser";
import { tokenize } from "./tokens";
import { Compiler } from "./wasm/Compiler";
import { LanguageError, RuntimeError } from "./errors";
import { highlight, showErrorTooltip } from "./syntax";
import example from "../assets/example.rn?raw";

async function compile() {
  const inputElement = document.getElementById("compiler-input")!;
  const outputElement = document.getElementById("compiler-output")!;

  function clearOutput() {
    outputElement.innerText = "";
  }

  function writeToOutput(text: string) {
    outputElement.innerText += text;
    outputElement.innerText += "\n";
  }

  clearOutput();

  const code = inputElement.innerText!;

  try {
    const tokens = tokenize(code);

    const parser = new Parser(tokens);
    const ast = parser.parse();

    const compiler = new Compiler();
    const wasm = await compiler.compile(ast);

    let memory: WebAssembly.Memory | null = null;

    const decoder = new TextDecoder();

    const imports = {
      env: {
        print: (message: bigint) => {
          const ptr = Number(message & 0xffffffffn);
          const len = Number(message >> 32n);
          const bytes = new Uint8Array(memory!.buffer, ptr, len);
          writeToOutput(decoder.decode(bytes));
        },
        error: (message: bigint, index: number) => {
          const ptr = Number(message & 0xffffffffn);
          const len = Number(message >> 32n);
          const bytes = new Uint8Array(memory!.buffer, ptr, len);
          const decodedMessage = decoder.decode(bytes);
          throw new RuntimeError(decodedMessage, index);
        },
      },
    };

    const { instance } = await WebAssembly.instantiate(wasm, imports);

    memory = instance.exports.memory as WebAssembly.Memory;

    if (!(instance.exports.main instanceof Function)) {
      return;
    }

    instance.exports.main();
  } catch (err) {
    console.error(err);

    if (
      err instanceof WebAssembly.CompileError ||
      err instanceof WebAssembly.RuntimeError
    ) {
      showErrorTooltip(inputElement, err.message, code.length - 1);
    }

    if (err instanceof LanguageError) {
      showErrorTooltip(inputElement, err.message, err.offset);
    }
  }
}

document.addEventListener("DOMContentLoaded", () => {
  document.getElementById("compile-button")!.addEventListener("click", compile);

  const inputElement = document.getElementById("compiler-input")!;
  inputElement.innerText = example;

  highlight(inputElement);

  inputElement.addEventListener("input", () => highlight(inputElement));

  let isCollapsed = false;
  const toggle = document.getElementById("toggle-output")!;
  toggle.addEventListener("click", () => {
    const output = document.getElementById("output-section")!;

    isCollapsed = !isCollapsed;

    if (isCollapsed) {
      output.classList.add("collapsed");
      toggle.innerHTML = '<i class="fas fa-chevron-up"></i>';
    } else {
      output.classList.remove("collapsed");
      toggle.innerHTML = '<i class="fas fa-chevron-down"></i>';
    }
  });
});

// --- FILE: .\compiler\syntax.ts ---

import { LanguageError } from "./errors";
import { tokenize, TokenTypeNames, TokenTypes } from "./tokens";

function saveCursor(element: Node) {
  const sel = window.getSelection();

  if (!sel) return null;

  if (sel.rangeCount === 0) return null;

  const range = sel.getRangeAt(0);
  const pre = range.cloneRange();
  pre.selectNodeContents(element);
  pre.setEnd(range.startContainer, range.startOffset);
  const start = pre.toString().length;

  return start;
}

function restoreCursor(element: HTMLElement, saved: number | null) {
  if (saved === null) return;

  const range = document.createRange();
  range.setStart(element, 0);
  range.collapse(true);

  let stop = false;
  let node: Node | undefined;
  let nodes: Node[] = [element];
  let previous = 0;

  while (!stop && (node = nodes.pop())) {
    if (node.nodeType === 3) {
      const length = node.textContent?.length || 0;
      const next = previous + length;

      if (saved <= next) {
        range.setStart(node, saved - previous);
        range.collapse(true);
        stop = true;
      }
      previous = next;
    } else {
      let i = node.childNodes.length;
      while (i--) {
        nodes.push(node.childNodes[i]);
      }
    }
  }

  const sel = window.getSelection();

  if (sel) {
    sel.removeAllRanges();
    sel.addRange(range);
  }
}

let error = {
  element: null as HTMLElement | null,
  node: null as Node | null,
  offset: 0,
};

function moveErrorTooltip() {
  const tooltip = document.getElementById("error-tooltip")!;
  if (tooltip.style.display !== "block") return;

  const { element, node: target, offset } = error;

  if (!element || !target) return;

  const range = document.createRange();
  try {
    range.setStart(target, offset);
    range.setEnd(target, offset + 1);
  } catch {
    return;
  }

  const rect = range.getBoundingClientRect();
  const left = rect.left + rect.width / 2;
  const top = rect.bottom + 8;

  tooltip.style.left = `${left}px`;
  tooltip.style.top = `${top}px`;
}

export function showErrorTooltip(
  element: HTMLElement,
  message: string,
  index: number
) {
  const tooltip = document.getElementById("error-tooltip")!;
  tooltip.style.display = "none";

  const range = document.createRange();

  const walker = document.createTreeWalker(element, NodeFilter.SHOW_ALL);

  let current = 0;
  let offset = 0;

  let node: Node | null;

  while ((node = walker.nextNode())) {
    if (node.nodeType !== Node.TEXT_NODE) {
      if (node.nodeName === "BR") {
        current++;
      }
      continue;
    }

    const length = node.textContent?.length || 0;

    if (index >= current && index < current + length) {
      offset = index - current;
      break;
    }
    current += length;
  }

  if (!node) {
    throw Error("No node found for index");
  }

  range.setStart(node, offset);
  range.setEnd(node, offset + 1);

  const rect = range.getBoundingClientRect();

  const scroller = range.startContainer.parentElement;

  if (scroller) {
    scroller.scrollIntoView({
      behavior: "smooth",
      block: "center",
      inline: "nearest",
    });
  }

  tooltip.innerText = message;
  tooltip.style.display = "block";

  const left = rect.left + rect.width / 2;
  const top = rect.bottom + 8;

  tooltip.style.left = `${left}px`;
  tooltip.style.top = `${top}px`;

  error = { element, node, offset };

  element.addEventListener("scroll", moveErrorTooltip, { passive: true });
}

export function hideErrorTooltip() {
  const tooltip = document.getElementById("error-tooltip")!;
  tooltip.style.display = "none";
}

export function highlight(element: HTMLElement) {
  hideErrorTooltip();

  const code = element.innerText;

  try {
    const tokens = tokenize(code);

    const saved = saveCursor(element);

    let result = "";
    let cursor = 0;

    for (const token of tokens) {
      const raw = code.slice(cursor, token.end);
      const name = TokenTypeNames[token.type!];

      let clazz = `token-${name.toLowerCase().replace(/_/g, "-")}`;

      if (token.type === TokenTypes.IDENTIFIER) {
        clazz += `-${token.value}`;
      }

      result += `<span class="${clazz}">${raw}</span>`;
      cursor = token.end;
    }

    element.innerHTML = result;

    restoreCursor(element, saved);
  } catch (err) {
    console.error(err);

    if (err instanceof LanguageError) {
      showErrorTooltip(element, err.message, err.offset);
    }
  }
}

// --- FILE: .\compiler\tokens.ts ---

import { InvalidSyntaxError } from "./errors";

export const TokenTypes = {
  IGNORE: "IGNORE",
  COMMENT: "COMMENT",
  BOOL_LITERAL: "BOOL_LITERAL",
  NUMBER_LITERAL: "NUMBER_LITERAL",
  STRING_LITERAL: "STRING_LITERAL",
  STRING_INTERPOLATED: "STRING_INTERPOLATED",
  AND: "&&",
  EQUALS: "==",
  NOT_EQUALS: "!=",
  NOT: "!",
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
      { matcher: /&&/, type: TokenTypes.AND },
      { matcher: /==/, type: TokenTypes.EQUALS },
      { matcher: /!=/, type: TokenTypes.NOT_EQUALS },
      { matcher: /!/, type: TokenTypes.NOT },
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
      { matcher: /\*\*=/, type: TokenTypes.POW_EQUAL },
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

// --- FILE: .\compiler\ast\formats.ts ---

export type FormatBase = 10 | 16;
export interface FormatType {
  padder?: string;
  padding?: number;
  right?: boolean;
  base: FormatBase;
  uppercase?: boolean;
}

export const DEFAULT_FORMAT = {
  base: 10,
} as const;

export function parseFormatType(value: string): FormatType | null {
  const pattern = /^(?:(.)([<>]))?(\d+)?([xX])?$/;

  const match = value.match(pattern);

  if (!match) return null;

  const [, padder, alignment, padding, base] = match;

  return {
    padder: padder,
    padding: padding ? parseInt(padding, 10) : undefined,
    right: alignment === "",
    base: base ? 16 : 10,
    uppercase: base === "X",
  };
}

// --- FILE: .\compiler\ast\FunctionTable.ts ---

import { type BuiltInType } from "../ast/types";
import type { FunctionDeclaration } from "./nodes/FunctionDeclaration";

export class FunctionTable {
  #funcs = new Map<string, FunctionDeclaration>();

  add(func: FunctionDeclaration) {
    const key = func.name + func.params.map((param) => param.type).join(",");
    this.#funcs.set(key, func);
  }

  get(name: string, args: BuiltInType[]): FunctionDeclaration | null {
    const key = name + args.join(",");
    return this.#funcs.get(key) ?? null;
  }
}

// --- FILE: .\compiler\ast\operators.ts ---

import { TokenTypes } from "../tokens";

export const BinaryOperators = {
  [TokenTypes.AND]: { precedence: 1, associativity: "left" },
  [TokenTypes.EQUALS]: { precedence: 2, associativity: "left" },
  [TokenTypes.NOT_EQUALS]: { precedence: 2, associativity: "left" },
  [TokenTypes.LT]: { precedence: 3, associativity: "left" },
  [TokenTypes.GT]: { precedence: 3, associativity: "left" },
  [TokenTypes.LTE]: { precedence: 3, associativity: "left" },
  [TokenTypes.GTE]: { precedence: 3, associativity: "left" },
  [TokenTypes.PLUS]: { precedence: 4, associativity: "left" },
  [TokenTypes.MINUS]: { precedence: 4, associativity: "left" },
  [TokenTypes.MUL]: { precedence: 5, associativity: "left" },
  [TokenTypes.DIV]: { precedence: 5, associativity: "left" },
  [TokenTypes.MOD]: { precedence: 5, associativity: "left" },
  [TokenTypes.POW]: { precedence: 6, associativity: "right" },
} as const;

export interface BinaryOperatorInfo {
  precedence: number;
  associativity: "left" | "right";
}

export type BinaryOperator = keyof typeof BinaryOperators;

export function isBinaryOperator(value?: string): value is BinaryOperator {
  return value !== undefined && value in BinaryOperators;
}

export const FixOperators = {
  [TokenTypes.INCREMENT]: true,
  [TokenTypes.DECREMENT]: true,
} as const;

export type FixOperator = keyof typeof FixOperators;

export function isFixOperator(value?: string): value is FixOperator {
  return value !== undefined && value in FixOperators;
}

export const FIX_TO_BINARY: Record<FixOperator, BinaryOperator> = {
  [TokenTypes.INCREMENT]: TokenTypes.PLUS,
  [TokenTypes.DECREMENT]: TokenTypes.MINUS,
} as const;

export const UnaryOperators = {
  [TokenTypes.NOT]: true,
} as const;

export type UnaryOperator = keyof typeof UnaryOperators;

export function isUnaryOperator(value?: string): value is UnaryOperator {
  return value !== undefined && value in UnaryOperators;
}

export const CompoundOperators = {
  [TokenTypes.PLUS_EQUAL]: TokenTypes.PLUS,
  [TokenTypes.MINUS_EQUAL]: TokenTypes.MINUS,
  [TokenTypes.MUL_EQUAL]: TokenTypes.MUL,
  [TokenTypes.DIV_EQUAL]: TokenTypes.DIV,
  [TokenTypes.MOD_EQUAL]: TokenTypes.MOD,
  [TokenTypes.POW_EQUAL]: TokenTypes.POW,
} as const;

export type CompoundOperator = keyof typeof CompoundOperators;

export function isCompoundOperator(value?: string): value is CompoundOperator {
  return value !== undefined && value in CompoundOperators;
}

export const COMPOUND_TO_BINARY: Record<CompoundOperator, BinaryOperator> =
  CompoundOperators;

// --- FILE: .\compiler\ast\Parser.ts ---

import { InvalidSyntaxError } from "../errors";
import { tokenize, TokenTypes, type Token, type TokenType } from "../tokens";
import { parseFormatType } from "./formats";
import type { AnyAstNode } from "./nodes";
import { BinaryExpression } from "./nodes/BinaryExpression";
import { BoolLiteral } from "./nodes/BoolLiteral";
import { CallExpression } from "./nodes/CallExpression";
import { ExpressionStatement } from "./nodes/ExpressionStatement";
import { FieldAssignment } from "./nodes/FieldAssignment";
import { FieldDeclaration } from "./nodes/FieldDeclaration";
import { FieldExpression } from "./nodes/FieldExpression";
import { FormattedExpression } from "./nodes/FormattedExpression";
import { FunctionDeclaration } from "./nodes/FunctionDeclaration";
import { Identifier } from "./nodes/Identifier";
import { IfStatement } from "./nodes/IfStatement";
import { InterpolatedString } from "./nodes/InterpolatedString";
import { LoopStatement } from "./nodes/LoopStatement";
import { NumberLiteral } from "./nodes/NumberLiteral";
import { Parameter } from "./nodes/Parameter";
import { FixExpression } from "./nodes/FixExpression";
import { ReturnStatement } from "./nodes/ReturnStatement";
import { StringLiteral } from "./nodes/StringLiteral";
import { StructDeclaration } from "./nodes/StructDeclaration";
import { TernaryExpression } from "./nodes/TernaryExpression";
import { VariableAssignment as VariableAssignment } from "./nodes/VariableAssignment";
import { VariableDeclaration } from "./nodes/VariableDeclaration";
import {
  isBinaryOperator,
  BinaryOperators,
  isFixOperator,
  FIX_TO_BINARY,
  isCompoundOperator,
  COMPOUND_TO_BINARY,
  isUnaryOperator,
} from "./operators";
import {
  BuiltInTypes,
  isBuiltInType,
  isNumericSuffix,
  NumericSuffixes,
  TypeKinds as TypeKinds,
  type NumericSuffix,
  type Type,
} from "./types";
import { UnaryExpression } from "./nodes/UnaryExpression";

export class Parser {
  #tokens;
  #buffer: Token[] = [];
  #done = false;

  #types: Record<string, Type> = {};

  constructor(tokens: Generator<Token, void, unknown>) {
    this.#tokens = tokens;
  }

  #peek(n = 0): Token | null {
    while (this.#buffer.length <= n && !this.#done) {
      const { value, done } = this.#tokens.next();

      if (done) {
        this.#done = true;
        break;
      }

      if (value.nop) continue;

      this.#buffer.push(value);
    }
    return this.#buffer[n] || null;
  }

  #consume(expected: TokenType) {
    const token = this.#peek(0);

    if (!token || token.type !== expected) {
      throw new InvalidSyntaxError(
        `Expected ${expected} but got ${token ? token.type : "EOF"}`,
        token?.start || 0
      );
    }
    this.#buffer.shift();
    return token;
  }

  #value(expected: TokenType) {
    const token = this.#consume(expected);
    return token.value!;
  }

  #type(): Type {
    const token = this.#consume(TokenTypes.IDENTIFIER);

    const type = token.value!;

    if (isBuiltInType(type)) {
      return BuiltInTypes[type];
    } else if (type in this.#types) {
      return this.#types[type];
    }
    throw new InvalidSyntaxError(`Invalid type "${type}"`, token.start);
  }

  #parseIdentifier() {
    const name = this.#consume(TokenTypes.IDENTIFIER);
    return new Identifier(name.value!, name.start);
  }

  #parseBoolLiteral() {
    const literal = this.#consume(TokenTypes.BOOL_LITERAL);
    return new BoolLiteral(literal.value === "true", literal.start);
  }

  #parseNumberLiteral() {
    const literal = this.#consume(TokenTypes.NUMBER_LITERAL);

    let suffix: NumericSuffix = NumericSuffixes.I32;

    if (this.#peek()?.type === TokenTypes.IDENTIFIER) {
      const token = this.#consume(TokenTypes.IDENTIFIER);

      if (token.value) {
        if (!isNumericSuffix(token.value)) {
          throw new InvalidSyntaxError(
            `Invalid suffix "${token.value}"`,
            token.start
          );
        }

        suffix = token.value;
      }
    }
    return new NumberLiteral(BigInt(literal.value!), suffix, literal.start);
  }

  #parseStringLiteral() {
    const literal = this.#consume(TokenTypes.STRING_LITERAL);
    return new StringLiteral(literal.value!, literal.start);
  }

  #parseStringInterpolated(): InterpolatedString {
    const token = this.#consume(TokenTypes.STRING_INTERPOLATED);
    const text = token.value!.slice(2, -1);
    const parts = [];

    const start = token.start + 2;
    let index = 0;

    while (index < text.length) {
      const char = text[index];

      if (char === "{") {
        let brace = index;
        let count = 0;

        while (text[index] === "{") {
          count++;
          index++;
        }

        if (count % 2 === 0) {
          parts.push(
            new StringLiteral("{".repeat(count / 2), start + brace + 1)
          );
          continue;
        }

        const interpolation = index;
        let depth = 1;

        while (index < text.length && depth > 0) {
          if (text[index] === "{") depth++;
          else if (text[index] === "}") depth--;
          index++;
        }

        if (depth !== 0) {
          throw new InvalidSyntaxError("Unclosed interpolation", start + brace);
        }

        const interpolated = text.slice(interpolation, index - 1).trim();

        if (interpolated.length === 0) {
          throw new InvalidSyntaxError("Empty interpolation", start + brace);
        }

        const specifier = interpolated.indexOf(":");
        const expression =
          specifier === -1
            ? interpolated
            : interpolated.slice(0, specifier).trim();
        const value =
          specifier === -1 ? null : interpolated.slice(specifier + 1).trim();

        const tokens = tokenize(expression, {
          offset: start + interpolation,
        });
        const parser = new Parser(tokens);
        const part = parser.parseExpression();

        if (value !== null) {
          const format = parseFormatType(value);

          if (!format) {
            throw new InvalidSyntaxError(
              `Invalid format "${value}"`,
              start + interpolation + specifier + 1
            );
          }

          parts.push(
            new FormattedExpression(part, format, start + interpolation)
          );
        } else {
          parts.push(part);
        }

        continue;
      }

      if (char === "}") {
        let brace = index;
        let count = 0;
        while (text[index] === "}") {
          count++;
          index++;
        }

        if (count % 2 === 0) {
          parts.push(
            new StringLiteral("}".repeat(count / 2), token.start + brace + 1)
          );
          continue;
        }

        throw new InvalidSyntaxError(
          "Unmatched closing brace",
          token.start + brace + 1
        );
      }

      const literal = index;

      while (
        index < text.length &&
        text[index] !== "{" &&
        text[index] !== "}"
      ) {
        index++;
      }
      if (index > literal) {
        parts.push(
          new StringLiteral(text.slice(literal, index), start + literal + 1)
        );
      }
    }

    return new InterpolatedString(parts, token.start);
  }

  #parseParameter() {
    const type = this.#type();
    const name = this.#consume(TokenTypes.IDENTIFIER);
    return new Parameter(type, name.value!, name.start);
  }

  #parseParameters() {
    this.#consume(TokenTypes.LPAREN);

    const params = [];

    if (this.#peek()?.type !== TokenTypes.RPAREN) {
      params.push(this.#parseParameter());

      while (this.#peek()?.type === TokenTypes.COMMA) {
        this.#consume(TokenTypes.COMMA);
        params.push(this.#parseParameter());
      }
    }
    this.#consume(TokenTypes.RPAREN);
    return params;
  }

  #parseCallExpression() {
    const callee = this.#parseIdentifier();
    this.#consume(TokenTypes.LPAREN);

    const args = [];

    if (this.#peek()?.type !== TokenTypes.RPAREN) {
      args.push(this.parseExpression());

      while (this.#peek()?.type === TokenTypes.COMMA) {
        this.#consume(TokenTypes.COMMA);
        args.push(this.parseExpression());
      }
    }
    this.#consume(TokenTypes.RPAREN);
    return new CallExpression(callee.name, args, callee.offset);
  }

  #parseFieldExpression() {
    const object = this.#parseIdentifier();
    this.#consume(TokenTypes.DOT);
    const member = this.#parseIdentifier();
    return new FieldExpression(object.name, member.name, member.offset);
  }

  #parsePrimaryExpression(): AnyAstNode {
    const next = this.#peek();

    if (!next) throw new Error("Unexpected end of input");

    if (next.type === TokenTypes.LPAREN) {
      this.#consume(TokenTypes.LPAREN);
      const expr = this.parseExpression();
      this.#consume(TokenTypes.RPAREN);
      return expr;
    }

    if (next.type === TokenTypes.BOOL_LITERAL) {
      return this.#parseBoolLiteral();
    }

    if (next.type === TokenTypes.NUMBER_LITERAL) {
      return this.#parseNumberLiteral();
    }

    if (next.type === TokenTypes.STRING_LITERAL) {
      return this.#parseStringLiteral();
    }

    if (next.type === TokenTypes.STRING_INTERPOLATED) {
      return this.#parseStringInterpolated();
    }

    if (next.type === TokenTypes.IDENTIFIER) {
      if (this.#peek(1)?.type === TokenTypes.LPAREN) {
        return this.#parseCallExpression();
      }

      if (this.#peek(1)?.type === TokenTypes.DOT) {
        return this.#parseFieldExpression();
      }

      return this.#parseIdentifier();
    }

    throw new InvalidSyntaxError(
      `Unexpected token ${next.type} at offset ${next.start}`,
      next.start
    );
  }

  #parsePrefixExpression(): AnyAstNode {
    const next = this.#peek();

    if (next && isFixOperator(next.type)) {
      this.#consume(next.type);
      const expression = this.#parsePrimaryExpression();
      return new FixExpression(
        FIX_TO_BINARY[next.type],
        expression,
        next.start,
        true
      );
    }
    return this.#parsePostfixExpression();
  }

  #parsePostfixExpression(): AnyAstNode {
    let expression = this.#parsePrimaryExpression();

    const next = this.#peek();

    if (next && isFixOperator(next.type)) {
      this.#consume(next.type);
      expression = new FixExpression(
        FIX_TO_BINARY[next.type],
        expression,
        next.start,
        false
      );
    }
    return expression;
  }

  #parseUnaryExpression(): AnyAstNode {
    const next = this.#peek();

    if (next && isUnaryOperator(next.type)) {
      this.#consume(next.type);
      const operand = this.#parseUnaryExpression();
      return new UnaryExpression(next.type, operand, next.start);
    }
    return this.#parsePrefixExpression();
  }

  #parseBinaryExpression(precedence = 0) {
    let left = this.#parseUnaryExpression();

    let next = this.#peek();

    while (
      next &&
      isBinaryOperator(next.type) &&
      BinaryOperators[next.type].precedence >= precedence
    ) {
      const info = BinaryOperators[next.type];
      this.#consume(next.type);
      const right = this.#parseBinaryExpression(
        info.associativity === "left" ? info.precedence + 1 : info.precedence
      );
      left = new BinaryExpression(next.type, left, right, next.start);
      next = this.#peek();
    }
    return left;
  }

  #parseTernaryExpression() {
    let condition = this.#parseBinaryExpression();

    const next = this.#peek();

    if (next && next.type === TokenTypes.TERNARY) {
      const token = this.#consume(TokenTypes.TERNARY);
      const consequent = this.parseExpression();
      this.#consume(TokenTypes.COLON);
      const alternate = this.parseExpression();
      return new TernaryExpression(
        condition,
        consequent,
        alternate,
        token.start
      );
    }
    return condition;
  }

  parseExpression(): AnyAstNode {
    return this.#parseTernaryExpression();
  }

  #parseFieldDeclaration() {
    const type = this.#type();
    const name = this.#consume(TokenTypes.IDENTIFIER);
    this.#consume(TokenTypes.SEMICOLON);
    return new FieldDeclaration(type, name.value!, name.start);
  }

  #parseFieldAssignment() {
    const parent = this.#consume(TokenTypes.IDENTIFIER);
    this.#consume(TokenTypes.DOT);
    const member = this.#consume(TokenTypes.IDENTIFIER);
    this.#consume(TokenTypes.EQUAL);
    const expression = this.parseExpression();
    this.#consume(TokenTypes.SEMICOLON);
    return new FieldAssignment(
      parent.value!,
      member.value!,
      expression,
      member.start
    );
  }

  #parseVariableDeclaration() {
    const type = this.#type();
    const name = this.#consume(TokenTypes.IDENTIFIER);

    let expression = null;

    if (this.#peek()?.type === TokenTypes.EQUAL) {
      this.#consume(TokenTypes.EQUAL);
      expression = this.parseExpression();
    }

    this.#consume(TokenTypes.SEMICOLON);

    return new VariableDeclaration(type, name.value!, expression, name.start);
  }

  #parseVariableAssignment() {
    const name = this.#consume(TokenTypes.IDENTIFIER);
    this.#consume(TokenTypes.EQUAL);
    const expression = this.parseExpression();
    this.#consume(TokenTypes.SEMICOLON);
    return new VariableAssignment(name.value!, expression, name.start);
  }

  #parseCompoundVariableAssignment(): AnyAstNode {
    const left = this.#parseIdentifier();
    const token = this.#peek();

    if (!token || !isCompoundOperator(token.type)) {
      throw new InvalidSyntaxError(
        "Expected compound assignment operator",
        token?.start || left.offset
      );
    }

    this.#consume(token.type);
    const right = this.parseExpression();
    this.#consume(TokenTypes.SEMICOLON);

    const expression = new BinaryExpression(
      COMPOUND_TO_BINARY[token.type],
      left,
      right,
      token.start
    );
    return new VariableAssignment(left.name, expression, left.offset);
  }

  #parseFieldCompoundAssignment(): AnyAstNode {
    const left = this.#parseFieldExpression();
    const token = this.#peek();

    if (!token || !isCompoundOperator(token.type)) {
      throw new InvalidSyntaxError(
        "Expected compound assignment operator",
        token?.start || left.offset
      );
    }

    this.#consume(token.type);
    const right = this.parseExpression();
    this.#consume(TokenTypes.SEMICOLON);

    const expression = new BinaryExpression(
      COMPOUND_TO_BINARY[token.type],
      left,
      right,
      token.start
    );
    return new FieldAssignment(
      left.object,
      left.member,
      expression,
      left.offset
    );
  }

  #parseExpressionAsStatement() {
    const expression = this.parseExpression();
    return new ExpressionStatement(expression, expression.offset);
  }

  #parseExpressionStatement() {
    const statement = this.#parseExpressionAsStatement();
    this.#consume(TokenTypes.SEMICOLON);
    return statement;
  }

  #parseBlock() {
    this.#consume(TokenTypes.LBRACE);

    const statements = [];

    while (this.#peek()?.type !== TokenTypes.RBRACE) {
      statements.push(this.#parseStatement());
    }
    this.#consume(TokenTypes.RBRACE);

    return statements;
  }

  #parseStatementOrBlock() {
    if (this.#peek()?.type === TokenTypes.LBRACE) {
      return this.#parseBlock();
    } else {
      return [this.#parseStatement()];
    }
  }

  #parseFields() {
    this.#consume(TokenTypes.LBRACE);

    const fields = [];

    while (this.#peek()?.type !== TokenTypes.RBRACE) {
      fields.push(this.#parseFieldDeclaration());
    }
    this.#consume(TokenTypes.RBRACE);

    return fields;
  }

  #parseStruct() {
    const struct = this.#consume(TokenTypes.STRUCT);
    const name = this.#value(TokenTypes.IDENTIFIER);
    const fields = this.#parseFields();
    this.#types[name] = { kind: TypeKinds.STRUCT, name: name };
    return new StructDeclaration(name, fields, struct.start);
  }

  #parseFunction() {
    const func = this.#consume(TokenTypes.FUNCTION);
    const name = this.#value(TokenTypes.IDENTIFIER);
    const params = this.#parseParameters();

    let result = null;

    const next = this.#peek();

    if (next?.type === TokenTypes.COLON) {
      this.#consume(TokenTypes.COLON);
      result = this.#type();
    }

    const body = this.#parseBlock();
    return new FunctionDeclaration(
      result || BuiltInTypes.void,
      name,
      params,
      body,
      func.start
    );
  }

  #parseReturnStatement() {
    const token = this.#consume(TokenTypes.RETURN);

    let value: AnyAstNode | null = null;

    if (this.#peek()?.type !== TokenTypes.SEMICOLON) {
      value = this.parseExpression();
    }

    this.#consume(TokenTypes.SEMICOLON);

    return new ReturnStatement(value, token.start);
  }

  #parseIfStatement(): IfStatement {
    const token = this.#consume(TokenTypes.IF);
    const condition = this.parseExpression();
    const consequent = this.#parseStatementOrBlock();

    let alternate = null;

    if (this.#peek()?.type === TokenTypes.ELSE) {
      this.#consume(TokenTypes.ELSE);

      if (this.#peek()?.type === TokenTypes.IF) {
        alternate = this.#parseIfStatement();
      } else {
        alternate = this.#parseStatementOrBlock();
      }
    }
    return new IfStatement(condition, consequent, alternate, token.start);
  }

  #parseForLoop() {
    const token = this.#consume(TokenTypes.FOR);
    this.#consume(TokenTypes.LPAREN);

    let declaration: AnyAstNode | null = null;
    let condition: AnyAstNode | null = null;
    let step: AnyAstNode | null = null;

    const next = this.#peek();

    if (next && next.type !== TokenTypes.SEMICOLON) {
      declaration = this.#parseStatement();
    }

    if (this.#peek()?.type !== TokenTypes.SEMICOLON) {
      condition = this.parseExpression();
    }
    this.#consume(TokenTypes.SEMICOLON);

    if (this.#peek()?.type !== TokenTypes.RPAREN) {
      step = this.#parseExpressionAsStatement();
    }
    this.#consume(TokenTypes.RPAREN);

    const body = this.#parseStatementOrBlock();

    return new LoopStatement(declaration, condition, step, body, token.start);
  }

  #parseStatement(): AnyAstNode {
    const next = this.#peek();

    if (!next) throw new Error("Unexpected end of input");

    switch (next.type) {
      case TokenTypes.STRUCT:
        return this.#parseStruct();
      case TokenTypes.FUNCTION:
        return this.#parseFunction();
      case TokenTypes.RETURN:
        return this.#parseReturnStatement();
      case TokenTypes.IF:
        return this.#parseIfStatement();
      case TokenTypes.FOR:
        return this.#parseForLoop();
    }

    if (next.type === TokenTypes.IDENTIFIER) {
      if (this.#peek(1)?.type === TokenTypes.IDENTIFIER) {
        return this.#parseVariableDeclaration();
      }

      if (
        this.#peek(1)?.type === TokenTypes.DOT &&
        this.#peek(2)?.type === TokenTypes.IDENTIFIER &&
        this.#peek(3)?.type === TokenTypes.EQUAL
      ) {
        return this.#parseFieldAssignment();
      }

      if (this.#peek(1)?.type === TokenTypes.EQUAL) {
        return this.#parseVariableAssignment();
      }

      if (isCompoundOperator(this.#peek(1)?.type)) {
        return this.#parseCompoundVariableAssignment();
      }

      if (
        this.#peek(1)?.type === TokenTypes.DOT &&
        this.#peek(2)?.type === TokenTypes.IDENTIFIER &&
        isCompoundOperator(this.#peek(3)?.type)
      ) {
        return this.#parseFieldCompoundAssignment();
      }
    }

    return this.#parseExpressionStatement();
  }

  parse() {
    const ast = [];

    while (true) {
      const next = this.#peek();

      if (!next) break;

      ast.push(this.#parseStatement());
    }
    return ast;
  }
}

// --- FILE: .\compiler\ast\types.ts ---

export const TypeKinds = {
  VOID: "void",
  BOOL: "bool",
  INT: "int",
  LONG: "long",
  STRING: "string",
  STRUCT: "struct",
} as const;

export interface BuiltInType {
  readonly kind: Exclude<TypeKind, typeof TypeKinds.STRUCT>;
}
export interface StructType {
  readonly kind: typeof TypeKinds.STRUCT;
  readonly name: string;
}

export const NumericTypes = {
  [TypeKinds.INT]: { kind: TypeKinds.INT },
  [TypeKinds.LONG]: { kind: TypeKinds.LONG },
};
export const BuiltInTypes: Record<
  Exclude<TypeKind, typeof TypeKinds.STRUCT>,
  BuiltInType
> = {
  [TypeKinds.VOID]: { kind: TypeKinds.VOID },
  [TypeKinds.BOOL]: { kind: TypeKinds.BOOL },
  [TypeKinds.INT]: NumericTypes.int,
  [TypeKinds.LONG]: NumericTypes.long,
  [TypeKinds.STRING]: { kind: TypeKinds.STRING },
};

export type TypeKind = (typeof TypeKinds)[keyof typeof TypeKinds];
export type BuiltInTypeKind = keyof typeof BuiltInTypes;
export type ValidTypeKind = Exclude<TypeKind, typeof TypeKinds.VOID>;
export type NumericTypeKind = keyof typeof NumericTypes;

export type Type = BuiltInType | StructType;
export type SizedType = Type & { kind: ValidTypeKind };
export type NumericType = BuiltInType & { kind: NumericTypeKind };

export function isBuiltInType(value: string): value is BuiltInTypeKind {
  return value in BuiltInTypes;
}
export function isValidType(type: Type): type is SizedType {
  return type.kind !== TypeKinds.VOID;
}
export function isNumericType(type: Type): type is NumericType {
  return type.kind in NumericTypes;
}

export const NumericSuffixes = {
  I32: "i32",
  I64: "i64",
} as const;

export type NumericSuffix =
  (typeof NumericSuffixes)[keyof typeof NumericSuffixes];

export const NumericSuffixTypes: Record<NumericSuffix, NumericType> = {
  [NumericSuffixes.I32]: NumericTypes.int,
  [NumericSuffixes.I64]: NumericTypes.long,
};

export function isNumericSuffix(value: string): value is NumericSuffix {
  return value in NumericSuffixTypes;
}

// --- FILE: .\compiler\ast\nodes\AstNode.ts ---

import type { AnyAstNode, NodeKind } from ".";

export abstract class AstNode {
  abstract readonly kind: NodeKind;
  readonly offset: number;

  constructor(offset: number) {
    this.offset = offset;
  }

  children(): AnyAstNode[] {
    return [];
  }

  branches(): AnyAstNode[][] {
    return [];
  }
}

// --- FILE: .\compiler\ast\nodes\BinaryExpression.ts ---

import { AstNode } from "./AstNode";
import { NodeKinds, type AnyAstNode } from ".";
import type { BinaryOperator } from "../operators";

export class BinaryExpression extends AstNode {
  readonly kind = NodeKinds.BINARY_EXPRESSION;
  readonly operator: BinaryOperator;
  readonly left: AnyAstNode;
  readonly right: AnyAstNode;

  constructor(
    operator: BinaryOperator,
    left: AnyAstNode,
    right: AnyAstNode,
    offset: number
  ) {
    super(offset);

    this.operator = operator;
    this.left = left;
    this.right = right;
  }

  children(): AnyAstNode[] {
    return [this.left, this.right];
  }
}

// --- FILE: .\compiler\ast\nodes\BoolLiteral.ts ---

import { AstNode } from "./AstNode";
import { NodeKinds } from ".";

export class BoolLiteral extends AstNode {
  readonly kind = NodeKinds.BOOL_LITERAL;
  readonly value: boolean;

  constructor(value: boolean, offset: number) {
    super(offset);

    this.value = value;
  }
}

// --- FILE: .\compiler\ast\nodes\CallExpression.ts ---

import { AstNode } from "./AstNode";
import { NodeKinds, type AnyAstNode } from ".";

export class CallExpression extends AstNode {
  readonly kind = NodeKinds.CALL_EXPRESSION;
  readonly callee: string;
  readonly args: AnyAstNode[];

  constructor(callee: string, args: AnyAstNode[], offset: number) {
    super(offset);

    this.callee = callee;
    this.args = args;
  }

  children(): AnyAstNode[] {
    return this.args;
  }
}

// --- FILE: .\compiler\ast\nodes\ExpressionStatement.ts ---

import { AstNode } from "./AstNode";
import { NodeKinds, type AnyAstNode } from ".";

export class ExpressionStatement extends AstNode {
  readonly kind = NodeKinds.EXPRESSION_STATEMENT;
  readonly expression: AnyAstNode;

  constructor(expression: AnyAstNode, offset: number) {
    super(offset);

    this.expression = expression;
  }

  children(): AnyAstNode[] {
    return [this.expression];
  }
}

// --- FILE: .\compiler\ast\nodes\FieldAssignment.ts ---

import { NodeKinds, type AnyAstNode } from ".";
import { AstNode } from "./AstNode";

export class FieldAssignment extends AstNode {
  readonly kind = NodeKinds.FIELD_ASSIGNMENT;
  readonly object: string;
  readonly member: string;
  readonly expression: AnyAstNode;

  constructor(
    object: string,
    member: string,
    expression: AnyAstNode,
    offset: number
  ) {
    super(offset);

    this.object = object;
    this.member = member;
    this.expression = expression;
  }

  children(): AnyAstNode[] {
    return [this.expression];
  }
}

// --- FILE: .\compiler\ast\nodes\FieldDeclaration.ts ---

import { NodeKinds } from ".";
import type { Type } from "../types";
import { AstNode } from "./AstNode";

export class FieldDeclaration extends AstNode {
  readonly kind = NodeKinds.FIELD_DECLARATION;
  readonly type: Type;
  readonly name: string;

  constructor(type: Type, name: string, offset: number) {
    super(offset);

    this.type = type;
    this.name = name;
  }
}

// --- FILE: .\compiler\ast\nodes\FieldExpression.ts ---

import { NodeKinds, type AnyAstNode } from ".";
import { AstNode } from "./AstNode";

export class FieldExpression extends AstNode {
  readonly kind = NodeKinds.FIELD_EXPRESSION;
  readonly object: string;
  readonly member: string;

  constructor(object: string, member: string, offset: number) {
    super(offset);

    this.object = object;
    this.member = member;
  }

  children(): AnyAstNode[] {
    return [];
  }
}

// --- FILE: .\compiler\ast\nodes\FixExpression.ts ---

import { NodeKinds, type AnyAstNode } from ".";
import type { BinaryOperator } from "../operators";
import { AstNode } from "./AstNode";

export class FixExpression extends AstNode {
  readonly kind = NodeKinds.FIX_EXPRESSION;
  readonly operator: BinaryOperator;
  readonly expression: AnyAstNode;
  readonly prefix: boolean;

  constructor(
    operator: BinaryOperator,
    expression: AnyAstNode,
    offset: number,
    prefix: boolean
  ) {
    super(offset);

    this.operator = operator;
    this.expression = expression;
    this.prefix = prefix;
  }

  children(): AnyAstNode[] {
    return [this.expression];
  }
}

// --- FILE: .\compiler\ast\nodes\FormattedExpression.ts ---

import { AstNode } from "./AstNode";
import { NodeKinds, type AnyAstNode } from ".";
import type { FormatType } from "../formats";

export class FormattedExpression extends AstNode {
  readonly kind = NodeKinds.FORMATTED_EXPRESSION;
  readonly expression: AnyAstNode;
  readonly format: FormatType;

  constructor(expression: AnyAstNode, format: FormatType, offset: number) {
    super(offset);

    this.expression = expression;
    this.format = format;
  }

  children(): AnyAstNode[] {
    return [this.expression];
  }
}

// --- FILE: .\compiler\ast\nodes\FunctionDeclaration.ts ---

import type { Parameter } from "./Parameter";
import { NodeKinds, type AnyAstNode } from ".";
import { AstNode } from "./AstNode";
import type { Type } from "../types";

export class FunctionDeclaration extends AstNode {
  readonly kind = NodeKinds.FUNCTION_DECLARATION;
  readonly type: Type;
  readonly name: string;
  readonly params: Parameter[];
  readonly body: AnyAstNode[];

  constructor(
    type: Type,
    name: string,
    params: Parameter[],
    body: AnyAstNode[],
    offset: number
  ) {
    super(offset);

    this.type = type;
    this.name = name;
    this.params = params;
    this.body = body;
  }

  children(): AnyAstNode[] {
    return [...this.params, ...this.body];
  }
}

// --- FILE: .\compiler\ast\nodes\Identifier.ts ---

import { AstNode } from "./AstNode";
import { NodeKinds } from ".";

export class Identifier extends AstNode {
  readonly kind = NodeKinds.IDENTIFIER;
  readonly name: string;

  constructor(name: string, offset: number) {
    super(offset);

    this.name = name;
  }
}

// --- FILE: .\compiler\ast\nodes\IfStatement.ts ---

import { AstNode } from "./AstNode";
import { NodeKinds, type AnyAstNode } from ".";

export class IfStatement extends AstNode {
  readonly kind = NodeKinds.IF_STATEMENT;
  readonly condition: AnyAstNode;
  readonly consequent: AnyAstNode[];
  readonly alternate: AnyAstNode[] | IfStatement | null;

  constructor(
    condition: AnyAstNode,
    consequent: AnyAstNode[],
    alternate: AnyAstNode[] | IfStatement | null,
    offset: number
  ) {
    super(offset);

    this.condition = condition;
    this.consequent = consequent;
    this.alternate = alternate;
  }

  children(): AnyAstNode[] {
    const children = [this.condition, ...this.consequent];

    if (this.alternate) {
      if (Array.isArray(this.alternate)) {
        children.push(...this.alternate);
      } else {
        children.push(this.alternate);
      }
    }
    return children;
  }

  branches(): AnyAstNode[][] {
    const branches = [this.consequent];

    if (this.alternate) {
      if (Array.isArray(this.alternate)) {
        branches.push(this.alternate);
      } else {
        branches.push([this.alternate]);
      }
    }
    return branches;
  }
}

// --- FILE: .\compiler\ast\nodes\index.ts ---

import type { FunctionDeclaration } from "./FunctionDeclaration";
import type { Parameter } from "./Parameter";
import type { VariableDeclaration } from "./VariableDeclaration";
import type { ExpressionStatement } from "./ExpressionStatement";
import type { CallExpression } from "./CallExpression";
import type { BinaryExpression } from "./BinaryExpression";
import type { Identifier } from "./Identifier";
import type { NumberLiteral } from "./NumberLiteral";
import type { StringLiteral } from "./StringLiteral";
import type { InterpolatedString } from "./InterpolatedString";
import type { FormattedExpression } from "./FormattedExpression";
import type { ReturnStatement } from "./ReturnStatement";
import type { IfStatement } from "./IfStatement";
import type { VariableAssignment } from "./VariableAssignment";
import type { BoolLiteral } from "./BoolLiteral";
import type { TernaryExpression } from "./TernaryExpression";
import type { StructDeclaration } from "./StructDeclaration";
import type { FieldDeclaration } from "./FieldDeclaration";
import type { FieldAssignment } from "./FieldAssignment";
import type { FieldExpression } from "./FieldExpression";
import type { LoopStatement } from "./LoopStatement";
import type { FixExpression } from "./FixExpression";
import type { UnaryExpression } from "./UnaryExpression";

export const NodeKinds = {
  STRUCT_DECLARATION: "StructDeclaration",
  STRUCT_EXPRESSION: "StructExpression",
  FIELD_DECLARATION: "FieldDeclaration",
  FIELD_ASSIGNMENT: "FieldAssignment",
  FIELD_EXPRESSION: "FieldExpression",
  FUNCTION_DECLARATION: "FunctionDeclaration",
  RETURN_STATEMENT: "ReturnStatement",
  IF_STATEMENT: "IfStatement",
  LOOP_STATEMENT: "LoopStatement",
  PARAMETER: "Parameter",
  VARIABLE_DECLARATION: "VariableDeclaration",
  VARIABLE_ASSIGNMENT: "VariableAssignment",
  EXPRESSION_STATEMENT: "ExpressionStatement",
  CALL_EXPRESSION: "CallExpression",
  BINARY_EXPRESSION: "BinaryExpression",
  UNARY_EXPRESSION: "UnaryExpression",
  FIX_EXPRESSION: "FixExpression",
  TERNARY_EXPRESSION: "TernaryExpression",
  IDENTIFIER: "Identifier",
  BOOL_LITERAL: "BoolLiteral",
  NUMBER_LITERAL: "NumberLiteral",
  STRING_LITERAL: "StringLiteral",
  INTERPOLATED_STRING: "InterpolatedString",
  FORMATTED_EXPRESSION: "FormattedExpression",
} as const;

export type NodeKind = (typeof NodeKinds)[keyof typeof NodeKinds];

export type AnyAstNode =
  | StructDeclaration
  | FieldDeclaration
  | FieldAssignment
  | FieldExpression
  | FunctionDeclaration
  | ReturnStatement
  | IfStatement
  | LoopStatement
  | Parameter
  | VariableDeclaration
  | VariableAssignment
  | ExpressionStatement
  | CallExpression
  | BinaryExpression
  | FixExpression
  | UnaryExpression
  | TernaryExpression
  | Identifier
  | BoolLiteral
  | NumberLiteral
  | StringLiteral
  | InterpolatedString
  | FormattedExpression;

// --- FILE: .\compiler\ast\nodes\InterpolatedString.ts ---

import { AstNode } from "./AstNode";
import { NodeKinds, type AnyAstNode } from ".";

export class InterpolatedString extends AstNode {
  readonly kind = NodeKinds.INTERPOLATED_STRING;
  readonly parts: AnyAstNode[];

  constructor(parts: AnyAstNode[], offset: number) {
    super(offset);

    this.parts = parts;
  }

  children(): AnyAstNode[] {
    return this.parts;
  }
}

// --- FILE: .\compiler\ast\nodes\LoopStatement.ts ---

import { AstNode } from "./AstNode";
import { NodeKinds, type AnyAstNode } from ".";

export class LoopStatement extends AstNode {
  readonly kind = NodeKinds.LOOP_STATEMENT;
  readonly declaration: AnyAstNode | null;
  readonly condition: AnyAstNode | null;
  readonly step: AnyAstNode | null;
  readonly body: AnyAstNode[];

  constructor(
    declaration: AnyAstNode | null,
    condition: AnyAstNode | null,
    step: AnyAstNode | null,
    body: AnyAstNode[],
    offset: number
  ) {
    super(offset);

    this.declaration = declaration;
    this.condition = condition;
    this.step = step;
    this.body = body;
  }

  children(): AnyAstNode[] {
    const children = [];

    if (this.declaration) {
      children.push(this.declaration);
    }

    if (this.step) {
      children.push(this.step);
    }

    children.push(...this.body);

    return children;
  }
}

// --- FILE: .\compiler\ast\nodes\NumberLiteral.ts ---

import { AstNode } from "./AstNode";
import { NodeKinds } from ".";
import type { NumericSuffix } from "../types";

export class NumberLiteral extends AstNode {
  readonly kind = NodeKinds.NUMBER_LITERAL;
  readonly value: bigint;
  readonly suffix: NumericSuffix;

  constructor(value: bigint, suffix: NumericSuffix, offset: number) {
    super(offset);

    this.value = value;
    this.suffix = suffix;
  }
}

// --- FILE: .\compiler\ast\nodes\Parameter.ts ---

import { NodeKinds } from ".";
import type { Type } from "../types";
import { AstNode } from "./AstNode";

export class Parameter extends AstNode {
  readonly kind = NodeKinds.PARAMETER;
  readonly type: Type;
  readonly name: string;

  constructor(type: Type, name: string, offset: number) {
    super(offset);

    this.type = type;
    this.name = name;
  }
}

// --- FILE: .\compiler\ast\nodes\ReturnStatement.ts ---

import { AstNode } from "./AstNode";
import { NodeKinds, type AnyAstNode } from ".";

export class ReturnStatement extends AstNode {
  readonly kind = NodeKinds.RETURN_STATEMENT;
  readonly expression: AnyAstNode | null;

  constructor(expression: AnyAstNode | null, offset: number) {
    super(offset);

    this.expression = expression;
  }

  children(): AnyAstNode[] {
    return this.expression ? [this.expression] : [];
  }
}

// --- FILE: .\compiler\ast\nodes\StringLiteral.ts ---

import { AstNode } from "./AstNode";
import { NodeKinds } from ".";

export class StringLiteral extends AstNode {
  readonly kind = NodeKinds.STRING_LITERAL;
  readonly value: string;

  constructor(value: string, offset: number) {
    super(offset);

    this.value = value;
  }
}

// --- FILE: .\compiler\ast\nodes\StructDeclaration.ts ---

import { NodeKinds, type AnyAstNode } from ".";
import { AstNode } from "./AstNode";
import type { FieldDeclaration } from "./FieldDeclaration";

export class StructDeclaration extends AstNode {
  readonly kind = NodeKinds.STRUCT_DECLARATION;
  readonly name: string;
  readonly fields: FieldDeclaration[];

  constructor(name: string, fields: FieldDeclaration[], offset: number) {
    super(offset);

    this.name = name;
    this.fields = fields;
  }

  children(): AnyAstNode[] {
    return [...this.fields];
  }
}

// --- FILE: .\compiler\ast\nodes\TernaryExpression.ts ---

import { AstNode } from "./AstNode";
import { NodeKinds, type AnyAstNode } from ".";

export class TernaryExpression extends AstNode {
  readonly kind = NodeKinds.TERNARY_EXPRESSION;
  readonly condition: AnyAstNode;
  readonly consequent: AnyAstNode;
  readonly alternate: AnyAstNode;

  constructor(
    condition: AnyAstNode,
    consequent: AnyAstNode,
    alternate: AnyAstNode,
    offset: number
  ) {
    super(offset);

    this.condition = condition;
    this.consequent = consequent;
    this.alternate = alternate;
  }

  children(): AnyAstNode[] {
    return [this.condition, this.consequent, this.alternate];
  }
}

// --- FILE: .\compiler\ast\nodes\UnaryExpression.ts ---

import { NodeKinds, type AnyAstNode } from ".";
import type { UnaryOperator } from "../operators";
import { AstNode } from "./AstNode";

export class UnaryExpression extends AstNode {
  readonly kind = NodeKinds.UNARY_EXPRESSION;
  readonly operator: UnaryOperator;
  readonly expression: AnyAstNode;

  constructor(operator: UnaryOperator, expression: AnyAstNode, offset: number) {
    super(offset);

    this.operator = operator;
    this.expression = expression;
  }

  children(): AnyAstNode[] {
    return [this.expression];
  }
}

// --- FILE: .\compiler\ast\nodes\VariableAssignment.ts ---

import { NodeKinds, type AnyAstNode } from ".";
import { AstNode } from "./AstNode";

export class VariableAssignment extends AstNode {
  readonly kind = NodeKinds.VARIABLE_ASSIGNMENT;
  readonly name: string;
  readonly expression: AnyAstNode;

  constructor(name: string, expression: AnyAstNode, offset: number) {
    super(offset);

    this.name = name;
    this.expression = expression;
  }

  children(): AnyAstNode[] {
    return [this.expression];
  }
}

// --- FILE: .\compiler\ast\nodes\VariableDeclaration.ts ---

import { NodeKinds, type AnyAstNode } from ".";
import type { Type } from "../types";
import { AstNode } from "./AstNode";

export class VariableDeclaration extends AstNode {
  readonly kind = NodeKinds.VARIABLE_DECLARATION;
  readonly type: Type;
  readonly name: string;
  readonly expression: AnyAstNode | null;

  constructor(
    type: Type,
    name: string,
    expression: AnyAstNode | null,
    offset: number
  ) {
    super(offset);

    this.type = type;
    this.name = name;
    this.expression = expression;
  }

  children(): AnyAstNode[] {
    return this.expression ? [this.expression] : [];
  }
}

// --- FILE: .\compiler\wasm\Compiler.ts ---

import {
  BuiltInTypes,
  isNumericType,
  isValidType,
  NumericSuffixTypes,
  TypeKinds,
  type NumericType,
  type Type,
} from "../ast/types";
import { SymbolTable } from "./SymbolTable";
import { TokenTypes } from "../tokens";
import wabt from "wabt";
import { CompilerError } from "../errors";
import type { FunctionDeclaration } from "../ast/nodes/FunctionDeclaration";
import type { ExpressionStatement } from "../ast/nodes/ExpressionStatement";
import type { VariableDeclaration } from "../ast/nodes/VariableDeclaration";
import { NodeKinds, type AnyAstNode } from "../ast/nodes";
import type { CallExpression } from "../ast/nodes/CallExpression";
import type { Identifier } from "../ast/nodes/Identifier";
import type { BinaryExpression } from "../ast/nodes/BinaryExpression";
import { StringLiteral } from "../ast/nodes/StringLiteral";
import type { NumberLiteral } from "../ast/nodes/NumberLiteral";
import runtime from "../../assets/runtime.wat?raw";
import type { InterpolatedString } from "../ast/nodes/InterpolatedString";
import { FormattedExpression } from "../ast/nodes/FormattedExpression";
import { DEFAULT_FORMAT } from "../ast/formats";
import type { ReturnStatement } from "../ast/nodes/ReturnStatement";
import { FunctionTable } from "./FunctionTable";
import type { IfStatement } from "../ast/nodes/IfStatement";
import { LocalTable } from "./LocalTable";
import type { VariableAssignment } from "../ast/nodes/VariableAssignment";
import type { BoolLiteral } from "../ast/nodes/BoolLiteral";
import type { TernaryExpression } from "../ast/nodes/TernaryExpression";
import type { FieldAssignment } from "../ast/nodes/FieldAssignment";
import { StructTable } from "./StructTable";
import {
  WASM_CONSTANT_EMITS,
  WASM_LOAD_EMITS,
  WASM_NUMERIC_FORMATTERS,
  WASM_OPERATION_OPERANDS,
  WASM_BINARY_OPERATIONS,
  WASM_STORE_EMITS,
  WASM_TYPES,
  type OperandSize,
  WASM_UNARY_OPERATIONS,
} from "./types";
import type { FieldExpression } from "../ast/nodes/FieldExpression";
import { ReturnTable } from "./ReturnTable";
import type { LoopStatement } from "../ast/nodes/LoopStatement";
import type { FixExpression } from "../ast/nodes/FixExpression";
import type { BinaryOperator, UnaryOperator } from "../ast/operators";
import type { UnaryExpression } from "../ast/nodes/UnaryExpression";

interface StringLiteralInfo {
  offset: number;
  length: number;
}

const BUILTIN_FUNCTIONS = [
  {
    name: "print",
    params: [
      {
        name: "message",
        type: BuiltInTypes.string,
      },
    ],
    type: BuiltInTypes.void,
  },
  {
    name: "error",
    params: [
      {
        name: "message",
        type: BuiltInTypes.string,
      },
      {
        name: "index",
        type: BuiltInTypes.int,
      },
    ],
    type: BuiltInTypes.void,
  },
];

export class Compiler {
  #wat: string[] = [];
  #offset = 0;
  #strings = new Map<string, StringLiteralInfo>();
  #scope: SymbolTable | null = null;
  #structs = new StructTable();
  #functions = new FunctionTable();
  #locals = new LocalTable();
  #returns = new ReturnTable();

  #spacing = 0;

  constructor() {
    for (const func of BUILTIN_FUNCTIONS) {
      this.#functions.add(func);
    }
  }

  #block(opener: string, body: () => void) {
    this.#write(opener);
    this.#spacing += 2;
    body();
    this.#spacing -= 2;
    this.#write(")");
  }

  async compile(ast: AnyAstNode[]): Promise<Uint8Array> {
    this.#block("(module", () => {
      for (const func of BUILTIN_FUNCTIONS) {
        this.#block(`(import "env" "${func.name}"`, () => {
          this.#block(`(func $${func.name}`, () => {
            for (let i = 0; i < func.params.length; i++) {
              const param = func.params[i];
              this.#write(this.#getParamDeclaration(param.name, param.type)!);
            }
          });
        });
      }

      this.#write('(memory (export "memory") 1)');
      this.#write("(global $__next_free_address (mut i32) (i32.const 0))");

      this.#write(runtime);

      this.#collectStringLiterals(ast);

      this.#strings.forEach((info, value) => {
        const bytes = new TextEncoder().encode(value);
        const hexBytes = Array.from(bytes)
          .map((b) => `\\${b.toString(16).padStart(2, "0")}`)
          .join("");
        this.#write(`(data (i32.const ${info.offset}) "${hexBytes}")`);
      });

      this.#block("(func $__init_heap", () => {
        this.#write(
          `(global.set $__next_free_address (i32.const ${this.#offset}))`
        );
      });

      this.#write("(start $__init_heap)");

      ast.forEach((node) => {
        if (node.kind === NodeKinds.STRUCT_DECLARATION) {
          if (
            !this.#structs.add({
              name: node.name,
              fields: node.fields,
            })
          ) {
            throw new CompilerError(
              `Struct "${node.name}" has already been declared`,
              node.offset
            );
          }
        }
      });

      ast.forEach((node) => {
        if (node.kind === NodeKinds.FUNCTION_DECLARATION) {
          if (
            !this.#functions.add({
              name: node.name,
              params: node.params,
              type: node.type,
            })
          ) {
            throw new CompilerError(
              `Function "${node.name}" has already been declared`,
              node.offset
            );
          }
        }
      });

      ast.forEach((node) => {
        if (node.kind === NodeKinds.FUNCTION_DECLARATION) {
          this.#compileFunctionDeclaration(node);
        }
      });
    });

    const wat = this.#wat.join("\n");

    const module = await wabt();
    const parsed = module.parseWat("module.wat", wat);
    const { buffer } = parsed.toBinary({ log: true });

    return buffer;
  }

  #write(line: string, offset?: number) {
    const indented = " ".repeat(this.#spacing).concat(line);

    if (offset) {
      this.#wat.splice(offset, 0, indented);
    } else {
      this.#wat.push(indented);
    }
  }

  #collectStringLiterals(node: AnyAstNode | AnyAstNode[] | null) {
    if (Array.isArray(node)) {
      node.forEach((n) => this.#collectStringLiterals(n));
      return;
    }

    if (!node) return;

    switch (node.kind) {
      case NodeKinds.INTERPOLATED_STRING:
        for (const part of node.parts) {
          if (!(part instanceof StringLiteral)) continue;

          if (!this.#strings.has(part.value)) {
            const length = new TextEncoder().encode(part.value).length;
            const aligned = Math.ceil(this.#offset / 4) * 4;
            this.#strings.set(part.value, {
              offset: aligned,
              length,
            });
            this.#offset = aligned + length;
          }
        }
        break;
      case NodeKinds.STRING_LITERAL:
        if (!this.#strings.has(node.value)) {
          const length = new TextEncoder().encode(node.value).length;
          const aligned = Math.ceil(this.#offset / 4) * 4;
          this.#strings.set(node.value, {
            offset: aligned,
            length,
          });
          this.#offset = aligned + length;
        }
        break;
      default:
        this.#collectStringLiterals(node.children());
    }
  }

  #getLocalDeclaration(name: string, type: Type) {
    if (isValidType(type)) {
      return [`$${name} ${WASM_TYPES[type.kind]}`];
    }
    return null;
  }

  #getParamDeclaration(name: string, type: Type) {
    if (isValidType(type)) {
      return `(param $${name} ${WASM_TYPES[type.kind]})`;
    }
    return null;
  }

  #getResultDeclaration(type: Type) {
    if (isValidType(type)) {
      return `(result ${WASM_TYPES[type.kind]})`;
    }
    return null;
  }

  #emitConst(type: NumericType, value: bigint) {
    this.#write(`(${WASM_CONSTANT_EMITS[type.kind]} ${value})`);
  }

  #emitLoad(size: OperandSize) {
    this.#write(`(${WASM_LOAD_EMITS[size]})`);
  }

  #emitStore(size: OperandSize) {
    this.#write(`(${WASM_STORE_EMITS[size]})`);
  }

  #emitLoadLocal(name: string) {
    this.#write(`(local.get $${name})`);
  }

  #emitStoreLocal(name: string) {
    this.#write(`(local.set $${name})`);
  }

  #emitBinaryOperation(type: Type, operator: BinaryOperator, offset: number) {
    const operation = WASM_BINARY_OPERATIONS[type.kind]?.[operator];

    if (!operation) {
      throw new CompilerError(
        `Type mismatch for "${operator}" operator: ${type.kind}`,
        offset
      );
    }
    this.#write(`(${operation})`);
  }

  #emitUnaryOperation(type: Type, operator: UnaryOperator, offset: number) {
    const operation = WASM_UNARY_OPERATIONS[type.kind]?.[operator];

    if (!operation) {
      throw new CompilerError(
        `Type mismatch for "${operator}" operator: ${type.kind}`,
        offset
      );
    }
    this.#write(`(${operation})`);
  }

  #compileFunctionDeclaration(node: FunctionDeclaration) {
    this.#scope = new SymbolTable();

    this.#scope.setFunction(this.#functions.get(node.name));

    this.#locals.clear();
    this.#returns.clear();

    const exportPart = node.name === "main" ? [`(export "${node.name}")`] : [];
    const paramsPart = node.params.map((param) => {
      const decl = this.#getParamDeclaration(param.name, param.type);

      if (!decl) {
        throw new CompilerError(
          `Invalid type "${param.type.kind}" for parameter declaration`,
          param.offset
        );
      }

      this.#scope!.add(param.name, param.name, param.type, param.offset);

      return decl;
    });

    this.#collectLocals(node.body);
    this.#collectReturns(node);

    const declaredResult = node.type !== BuiltInTypes.void;

    const hasResult = this.#returns.returns(node);

    if (declaredResult && !hasResult) {
      throw new CompilerError("Missing return statement", node.offset);
    }

    const resultDecl = this.#getResultDeclaration(node.type);
    const resultPart = hasResult && resultDecl ? [resultDecl] : [];

    if (hasResult && !resultPart) {
      throw new CompilerError(
        `Invalid type "${node.type}" for return type`,
        node.offset
      );
    }

    const signature = [
      `(func $${node.name}`,
      ...exportPart,
      ...paramsPart,
      ...resultPart,
    ];

    this.#block(signature.join(" "), () => {
      const start = this.#wat.length;

      node.body.forEach((stmt) => this.#compileNode(stmt));

      for (const [, local] of this.#locals.entries()) {
        const decl = this.#getLocalDeclaration(local.mangled, local.type);

        if (!decl) {
          throw new CompilerError(
            `Invalid type "${local.type}" for variable declaration`,
            local.offset
          );
        }
        this.#write(`(local ${decl})`, start);
      }
    });

    this.#scope = null;
  }

  #collectLocals(body: AnyAstNode[]) {
    body.forEach((node) => {
      if (node.kind === NodeKinds.VARIABLE_DECLARATION) {
        this.#locals.add(node.name, node.type, node.offset);
      }
      this.#collectLocals(node.children());
    });
  }

  #collectReturns(node: AnyAstNode): boolean {
    if (node.kind === NodeKinds.RETURN_STATEMENT) {
      this.#returns.set(node.offset, true);
      return true;
    }

    const branches = node.branches();

    if (branches.length > 0) {
      const all = branches.every(
        (branch) =>
          branch.length > 0 &&
          branch.every((child) => this.#collectReturns(child))
      );
      this.#returns.set(node.offset, all);
      return all;
    } else {
      const children = node.children();

      if (children.length === 0) {
        this.#returns.set(node.offset, false);
        return false;
      }

      const any = children.some((child) => this.#collectReturns(child));
      this.#returns.set(node.offset, any);
      return any;
    }
  }

  #compileNode(node: AnyAstNode) {
    switch (node.kind) {
      case NodeKinds.FIELD_ASSIGNMENT:
        this.#compileFieldAssignment(node);
        break;
      case NodeKinds.RETURN_STATEMENT:
        this.#compileReturnStatement(node);
        break;
      case NodeKinds.IF_STATEMENT:
        this.#compileIfStatement(node);
        break;
      case NodeKinds.LOOP_STATEMENT:
        this.#compileLoopStatement(node);
        break;
      case NodeKinds.EXPRESSION_STATEMENT:
        this.#compileExpressionStatement(node);
        break;
      case NodeKinds.VARIABLE_DECLARATION:
        this.#compileVariableDeclaration(node);
        break;
      case NodeKinds.VARIABLE_ASSIGNMENT:
        this.#compileVariableAssignment(node);
        break;
      default:
        throw new CompilerError(
          `Invalid node type for statement: ${node.kind}`,
          node.offset
        );
    }
  }

  #struct(object: string, member: string, offset: number) {
    if (!this.#scope) {
      throw new CompilerError("No active scope for struct resolution", offset);
    }

    const symbol = this.#scope.resolve(object);

    if (!symbol) {
      throw new CompilerError(`Struct "${object}" not found in scope`, offset);
    }

    if (symbol.type.kind !== TypeKinds.STRUCT) {
      throw new CompilerError(`Variable "${object}" is not a struct`, offset);
    }

    const struct = this.#structs.get(symbol.type.name);

    if (!struct) {
      throw new CompilerError(
        `Struct definition for "${symbol.type.name}" not found`,
        offset
      );
    }

    const field = struct.fields.get(member);

    if (!field) {
      throw new CompilerError(
        `Field "${member}" not found in struct "${struct.name}"`,
        offset
      );
    }

    this.#emitLoadLocal(symbol.mangled);
    this.#write(`(i32.const ${field.offset})`);
    this.#write("(i32.add)");

    return field;
  }

  #compileFieldAssignment(node: FieldAssignment) {
    const field = this.#struct(node.object, node.member, node.offset);
    this.#compileExpression(node.expression);
    this.#emitStore(field.size);
  }

  #compileReturnStatement(node: ReturnStatement) {
    if (!this.#scope) {
      throw new CompilerError(
        "No active scope for return statement compilation",
        node.offset
      );
    }

    const func = this.#scope.getFunction();

    if (!func) {
      throw new CompilerError(
        "Return statement outside of a function",
        node.offset
      );
    }

    const type = node.expression
      ? this.#getExpressionResultType(node.expression)
      : BuiltInTypes.void;

    if (type !== func.type) {
      throw new CompilerError(
        `Return type mismatch in function "${func.name}": expected ${func.type.kind}, got ${type.kind}`,
        node.offset
      );
    }

    if (node.expression) {
      this.#compileExpression(node.expression);
    }
    this.#write("(return)");
  }

  #compileIfStatement(node: IfStatement) {
    if (!this.#scope) {
      throw new CompilerError(
        "No active scope for if statement compilation",
        node.offset
      );
    }

    const func = this.#scope.getFunction();

    if (!func) {
      throw new CompilerError(
        "If statement outside of a function",
        node.offset
      );
    }

    const condition = this.#getExpressionResultType(node.condition);

    if (condition !== BuiltInTypes.bool) {
      throw new CompilerError(
        `Invalid type "${condition}" as conditional expression`,
        node.condition.offset
      );
    }

    const hasAlternate = node.alternate !== null;
    const hasResult = this.#returns.returns(node);
    const resultDecl = this.#getResultDeclaration(func.type);
    const resultPart =
      hasAlternate && hasResult && resultDecl ? [resultDecl] : [];

    const signature = ["(if", ...resultPart];

    this.#compileExpression(node.condition);
    this.#block(signature.join(" "), () => {
      this.#block("(then", () => {
        this.#scope = this.#scope!.enter();
        node.consequent.forEach((stmt) => this.#compileNode(stmt));
        this.#scope = this.#scope!.exit();
      });

      if (node.alternate) {
        this.#block("(else", () => {
          if (Array.isArray(node.alternate)) {
            this.#scope = this.#scope!.enter();
            node.alternate.forEach((stmt) => this.#compileNode(stmt));
            this.#scope = this.#scope!.exit();
          } else {
            this.#compileIfStatement(node.alternate!);
          }
        });
      }
    });
  }

  #compileLoopStatement(node: LoopStatement) {
    if (!this.#scope) {
      throw new CompilerError(
        "No active scope for if statement compilation",
        node.offset
      );
    }

    const func = this.#scope.getFunction();

    if (!func) {
      throw new CompilerError(
        "If statement outside of a function",
        node.offset
      );
    }

    if (node.declaration) {
      this.#compileNode(node.declaration);
    }

    if (node.condition) {
      const condition = this.#getExpressionResultType(node.condition);

      if (condition !== BuiltInTypes.bool) {
        throw new CompilerError(
          `Invalid type "${condition}" as conditional expression`,
          node.condition.offset
        );
      }
    }

    const identifier = `loop_${node.offset}`;
    this.#block(`(loop $${identifier}`, () => {
      this.#scope = this.#scope!.enter();
      node.body.forEach((stmt) => this.#compileNode(stmt));

      if (node.step) {
        this.#compileNode(node.step);
      }

      if (node.condition) {
        this.#compileExpression(node.condition);
        this.#write(`(br_if $${identifier})`);
      } else {
        this.#write(`(br $${identifier})`);
      }
      this.#scope = this.#scope!.exit();
    });
  }

  #compileExpressionStatement(node: ExpressionStatement) {
    this.#compileExpression(node.expression);

    if (this.#isResultExpression(node.expression)) {
      this.#write("(drop)");
    }
  }

  #compileVariableDeclaration(node: VariableDeclaration) {
    if (!this.#scope) {
      throw new CompilerError(
        "No active scope for variable declaration compilation",
        node.offset
      );
    }

    const local = this.#locals.get(node.name, node.offset);

    if (!local) {
      throw new CompilerError(
        `Variable "${node.name}" not found in scope during compilation`,
        node.offset
      );
    }

    this.#scope.add(local.name, local.mangled, local.type, local.offset);

    if (node.expression) {
      const type = this.#getExpressionResultType(node.expression);

      if (type !== node.type) {
        throw new CompilerError(
          `Type mismatch in assignment to "${node.name}": expected ${node.type.kind}, got ${type.kind}`,
          node.offset
        );
      }

      this.#compileExpression(node.expression);

      this.#emitStoreLocal(local.mangled);
    } else if (node.type.kind === TypeKinds.STRUCT) {
      const object = this.#structs.get(node.type.name);

      if (!object) {
        throw new CompilerError(
          `Object "${node.type.name}" not found in scope during compilation`,
          node.offset
        );
      }

      this.#write(`(i32.const ${object.size})`);
      this.#write("(call $__malloc)");

      this.#emitStoreLocal(local.mangled);
    }
  }

  #compileVariableAssignment(node: VariableAssignment) {
    const type = this.#getExpressionResultType(node.expression);

    this.#compileExpression(node.expression);

    if (!this.#scope) {
      throw new CompilerError(
        "No active scope for variable declaration compilation",
        node.offset
      );
    }
    const symbol = this.#scope.resolve(node.name);

    if (!symbol) {
      throw new CompilerError(
        `Variable "${node.name}" not found in scope during compilation`,
        node.offset
      );
    }

    if (type !== symbol.type) {
      throw new CompilerError(
        `Type mismatch in assignment to "${node.name}": expected ${symbol.type.kind}, got ${type.kind}`,
        node.offset
      );
    }

    this.#emitStoreLocal(symbol.mangled);
  }

  #compileExpression(node: AnyAstNode) {
    switch (node.kind) {
      case NodeKinds.FIELD_EXPRESSION:
        this.#compileFieldExpression(node);
        break;
      case NodeKinds.CALL_EXPRESSION:
        this.#compileCallExpression(node);
        break;
      case NodeKinds.BINARY_EXPRESSION:
        this.#compileBinaryExpression(node);
        break;
      case NodeKinds.FIX_EXPRESSION:
        this.#compileFixExpression(node);
        break;
      case NodeKinds.UNARY_EXPRESSION:
        this.#compileUnaryExpression(node);
        break;
      case NodeKinds.TERNARY_EXPRESSION:
        this.#compileTernaryExpression(node);
        break;
      case NodeKinds.IDENTIFIER:
        this.#compileIdentifier(node);
        break;
      case NodeKinds.BOOL_LITERAL:
        this.#compileBoolLiteral(node);
        break;
      case NodeKinds.NUMBER_LITERAL:
        this.#compileNumberLiteral(node);
        break;
      case NodeKinds.STRING_LITERAL:
        this.#compileStringLiteral(node);
        break;
      case NodeKinds.INTERPOLATED_STRING:
        this.#compileInterpolatedString(node);
        break;
      case NodeKinds.FORMATTED_EXPRESSION:
        this.#compileFormattedExpression(node);
        break;
      default:
        throw new CompilerError(
          `Invalid node type for expression: ${node.kind}`,
          node.offset
        );
    }
  }

  #compileFieldExpression(node: FieldExpression) {
    const field = this.#struct(node.object, node.member, node.offset);
    this.#emitLoad(field.size);
  }

  #compileCallExpression(node: CallExpression) {
    const func = this.#functions.get(node.callee);

    if (!func) {
      throw new CompilerError(
        `Call to undeclared function: ${node.callee}`,
        node.offset
      );
    }

    // Special handling of the error function
    if (node.callee === "error" && node.args.length === 1) {
      node.args.forEach((arg, i) => {
        const type = this.#getExpressionResultType(arg);

        const param = func.params[i];

        this.#compileExpression(arg);

        if (type !== param.type) {
          throw new CompilerError(
            `Type mismatch in call to "${node.callee}": expected ${param.type.kind}, got ${type.kind} for argument ${i}`,
            arg.offset
          );
        }
      });
      this.#write(`(i32.const ${node.offset})`);
      this.#write(`(call $${node.callee})`);
      return;
    }

    if (node.args.length !== func.params.length) {
      throw new CompilerError(
        `Wrong number of arguments for function "${node.callee}"`,
        node.offset
      );
    }

    node.args.forEach((arg, i) => {
      const type = this.#getExpressionResultType(arg);

      const param = func.params[i];

      if (type !== param.type) {
        throw new CompilerError(
          `Type mismatch in call to "${node.callee}": expected ${param.type.kind}, got ${type.kind} for argument ${i}`,
          arg.offset
        );
      }

      this.#compileExpression(arg);
    });
    this.#write(`(call $${node.callee})`);
  }

  #compileBinaryExpression(node: BinaryExpression) {
    const left = this.#getExpressionResultType(node.left);
    const right = this.#getExpressionResultType(node.right);

    if (left !== right) {
      throw new CompilerError(
        `Type mismatch for "${node.operator}" operator: ${left.kind} and ${right.kind}`,
        node.offset
      );
    }

    this.#compileExpression(node.left);
    this.#compileExpression(node.right);
    this.#emitBinaryOperation(left, node.operator, node.offset);
  }

  #compileFixExpression(node: FixExpression) {
    const type = this.#getExpressionResultType(node.expression);

    if (!isNumericType(type)) {
      throw new CompilerError(
        `Type mismatch for "${node.operator}" operator: ${type.kind}`,
        node.offset
      );
    }

    switch (node.expression.kind) {
      case NodeKinds.IDENTIFIER: {
        const symbol = this.#scope!.resolve(node.expression.name);

        if (!symbol) {
          throw new CompilerError(
            `Identifier "${node.expression.name}" not found in scope`,
            node.offset
          );
        }

        const value = this.#locals.tmp(symbol.type, node.expression.offset);

        // Store the variable value in a temporary variable
        this.#emitLoadLocal(symbol.mangled);
        this.#emitStoreLocal(value);

        // Increment the variable value
        this.#emitLoadLocal(symbol.mangled);
        this.#emitConst(type, 1n);
        this.#emitBinaryOperation(type, node.operator, node.offset);
        this.#emitStoreLocal(symbol.mangled);

        if (node.prefix) {
          // Push the incremented value
          this.#emitLoadLocal(symbol.mangled);
        } else {
          // Push the stored value
          this.#emitLoadLocal(value);
        }
        break;
      }
      case NodeKinds.FIELD_EXPRESSION: {
        const field = this.#struct(
          node.expression.object,
          node.expression.member,
          node.expression.offset
        );

        const address = this.#locals.tmp(
          BuiltInTypes.int,
          node.expression.offset
        );
        const value = this.#locals.tmp(field.type, node.expression.offset);

        // Store the struct field address
        this.#emitStoreLocal(address);

        // Store the struct field value in a temporary variable
        this.#emitLoadLocal(address);
        this.#emitLoad(field.size);
        this.#emitStoreLocal(value);

        // Push the struct field address
        this.#emitLoadLocal(address);
        // Increment the struct field value
        this.#emitLoadLocal(value);
        this.#emitConst(type, 1n);
        this.#emitBinaryOperation(type, node.operator, node.offset);
        // Store the result in the struct field
        this.#emitStore(field.size);

        if (node.prefix) {
          // Push the incremented value
          this.#emitLoadLocal(address);
          this.#emitLoad(field.size);
        } else {
          // Push the stored value
          this.#emitLoadLocal(value);
        }
        break;
      }
      default:
        throw new CompilerError(
          `Type mismatch for "${node.operator}" operator: ${type.kind}`,
          node.offset
        );
    }
  }

  #compileUnaryExpression(node: UnaryExpression) {
    const condition = this.#getExpressionResultType(node.expression);

    if (condition !== BuiltInTypes.bool) {
      throw new CompilerError(
        `Invalid type "${condition}" as conditional expression`,
        node.expression.offset
      );
    }

    const type = this.#getExpressionResultType(node);

    if (!isValidType(type)) {
      throw new CompilerError(
        `Invalid result type "${type.kind}" for unary expression`,
        node.offset
      );
    }

    this.#compileExpression(node.expression);
    this.#emitUnaryOperation(type, node.operator, node.offset);
  }

  #compileTernaryExpression(node: TernaryExpression) {
    const condition = this.#getExpressionResultType(node.condition);

    if (condition !== BuiltInTypes.bool) {
      throw new CompilerError(
        `Invalid type "${condition}" as conditional expression`,
        node.condition.offset
      );
    }

    const type = this.#getExpressionResultType(node);

    if (!isValidType(type)) {
      throw new CompilerError(
        `Invalid result type "${type.kind}" for ternary expression`,
        node.offset
      );
    }

    this.#compileExpression(node.condition);
    this.#block(`(if (result ${WASM_TYPES[type.kind]})`, () => {
      this.#block("(then", () => {
        this.#compileExpression(node.consequent);
      });
      this.#block("(else", () => {
        this.#compileExpression(node.alternate);
      });
    });
  }

  #compileIdentifier(node: Identifier) {
    if (!this.#scope) {
      throw new CompilerError(
        "No active scope for identifier compilation.",
        node.offset
      );
    }

    const symbol = this.#scope.resolve(node.name);

    if (!symbol) {
      throw new CompilerError(
        `Identifier '${node.name}' not found in scope.`,
        node.offset
      );
    }
    this.#emitLoadLocal(symbol.mangled);
  }

  #compileBoolLiteral(node: BoolLiteral) {
    this.#write(`(i32.const ${+node.value})`);
  }

  #compileNumberLiteral(node: NumberLiteral) {
    const type = NumericSuffixTypes[node.suffix];
    this.#emitConst(type, node.value);
  }

  #compileStringLiteral(node: StringLiteral) {
    const info = this.#strings.get(node.value);

    if (!info) {
      throw new CompilerError(
        `String literal "${node.value}" not found in collected literals`,
        node.offset
      );
    }

    this.#write(`(i32.const ${info.offset})`);
    this.#write(`(i32.const ${info.length})`);
    this.#write(`(call $__strpack)`);
  }

  #compileInterpolatedString(node: InterpolatedString) {
    node.parts.forEach((part, i) => {
      if (part.kind === NodeKinds.STRING_LITERAL) {
        this.#compileStringLiteral(part);
      } else {
        const type = this.#getExpressionResultType(part);

        if (type === BuiltInTypes.string) {
          this.#compileExpression(part);
        } else {
          this.#compileFormattedExpression(
            new FormattedExpression(part, DEFAULT_FORMAT, part.offset)
          );
        }
      }

      if (i > 0) this.#write("(call $__strcat)");
    });
  }

  #compileFormattedExpression(node: FormattedExpression) {
    const type = this.#getExpressionResultType(node.expression);

    if (!isNumericType(type)) {
      throw new CompilerError(
        `Invalid type "${type.kind}" for formatting`,
        node.offset
      );
    }

    this.#compileExpression(node.expression);

    const {
      padder = "0",
      padding,
      right = false,
      base,
      uppercase,
    } = node.format;

    this.#write(`(i32.const ${base})`);
    this.#write(`(call $${WASM_NUMERIC_FORMATTERS[type.kind]})`);

    if (padding) {
      this.#write(`(i32.const ${padder.charCodeAt(0)})`);
      this.#write(`(i32.const ${padding})`);
      this.#write(`(i32.const ${+right})`);
      this.#write("(call $__pad)");
    }

    if (uppercase) {
      this.#write("(call $__to_upper)");
    }
  }

  #isResultExpression(node: AnyAstNode): boolean {
    const type = this.#getExpressionResultType(node);
    return isValidType(type);
  }

  #getExpressionResultType(node: AnyAstNode): Type {
    if (!this.#scope) {
      throw new CompilerError(
        "No active scope for type inference",
        node.offset
      );
    }

    switch (node.kind) {
      case NodeKinds.BOOL_LITERAL:
        return BuiltInTypes.bool;
      case NodeKinds.NUMBER_LITERAL:
        return NumericSuffixTypes[node.suffix];
      case NodeKinds.STRING_LITERAL:
        return BuiltInTypes.string;
      case NodeKinds.INTERPOLATED_STRING:
        return BuiltInTypes.string;
      case NodeKinds.FORMATTED_EXPRESSION:
        return BuiltInTypes.string;
      case NodeKinds.IDENTIFIER: {
        const symbol = this.#scope.resolve(node.name);

        if (!symbol) {
          throw new CompilerError(
            `Identifier "${node.name}" not found for type inference`,
            node.offset
          );
        }
        return symbol.type;
      }
      case NodeKinds.CALL_EXPRESSION:
        const func = this.#functions.get(node.callee);

        if (!func) {
          throw new CompilerError(
            `Cannot infer return type for call to undeclared function "${node.callee}"`,
            node.offset
          );
        }
        return func.type!;
      case NodeKinds.FIELD_EXPRESSION:
        const symbol = this.#scope.resolve(node.object);

        if (!symbol) {
          throw new CompilerError(
            `Field "${node.object}" not found in scope during compilation`,
            node.offset
          );
        }

        if (symbol.type.kind !== TypeKinds.STRUCT) {
          throw new CompilerError(
            `Cannot assign to field "${node.member}" of non-object variable "${node.object}"`,
            node.offset
          );
        }

        const object = this.#structs.get(symbol.type.name);

        if (!object) {
          throw new CompilerError(
            `Object "${symbol.type.name}" not found in scope during compilation`,
            node.offset
          );
        }

        const member = object.fields.get(node.member);

        if (!member) {
          throw new CompilerError(
            `Struct "${node.object}" has no field named "${node.member}"`,
            node.offset
          );
        }
        return member.type;
      case NodeKinds.BINARY_EXPRESSION: {
        const type = this.#getExpressionResultType(node.left);

        switch (node.operator) {
          case TokenTypes.AND:
          case TokenTypes.EQUALS:
          case TokenTypes.NOT_EQUALS:
          case TokenTypes.LTE:
          case TokenTypes.GTE:
          case TokenTypes.LT:
          case TokenTypes.GT:
            return BuiltInTypes.bool;
          default: {
            if (!WASM_OPERATION_OPERANDS[node.operator]?.includes(type)) {
              throw new CompilerError(
                `Invalid type for "${node.operator}" operation: ${type.kind}`,
                node.offset
              );
            }
            return type;
          }
        }
      }
      case NodeKinds.FIX_EXPRESSION: {
        return this.#getExpressionResultType(node.expression);
      }
      case NodeKinds.UNARY_EXPRESSION: {
        return this.#getExpressionResultType(node.expression);
      }
      case NodeKinds.TERNARY_EXPRESSION: {
        const left = this.#getExpressionResultType(node.consequent);
        const right = this.#getExpressionResultType(node.alternate);

        if (left !== right) {
          throw new CompilerError(
            `Type mismatch for ternary operator: ${left.kind} and ${right.kind}`,
            node.offset
          );
        }
        return left;
      }
      default:
        throw new CompilerError(
          `Cannot infer type for node: ${node.kind}`,
          node.offset
        );
    }
  }
}

// --- FILE: .\compiler\wasm\FunctionTable.ts ---

import type { Type } from "../ast/types";

interface FunctionParam {
  name: string;
  type: Type;
}

export interface FunctionSymbol {
  name: string;
  params: FunctionParam[];
  type: Type;
}

export class FunctionTable {
  #functions = new Map<string, FunctionSymbol>();

  add(func: FunctionSymbol) {
    if (this.#functions.has(func.name)) {
      return false;
    }

    this.#functions.set(func.name, func);

    return true;
  }

  get(name: string) {
    return this.#functions.get(name);
  }
}

// --- FILE: .\compiler\wasm\LocalTable.ts ---

import type { Type } from "../ast/types";

interface LocalInfo {
  name: string;
  mangled: string;
  type: Type;
  offset: number;
}

export class LocalTable {
  #locals = new Map<string, LocalInfo>();

  #counter = 0;

  static mangle(name: string, offset: number) {
    return `${name}_${offset}`;
  }

  tmp(type: Type, offset: number) {
    const name = `__${this.#counter++}`;
    const mangled = name;
    this.#locals.set(mangled, { name, mangled, type, offset });
    return name;
  }

  add(name: string, type: Type, offset: number) {
    const mangled = LocalTable.mangle(name, offset);
    this.#locals.set(mangled, { name, mangled, type, offset });
  }

  get(name: string, offset: number) {
    const mangled = LocalTable.mangle(name, offset);
    return this.#locals.get(mangled);
  }

  entries() {
    return this.#locals.entries();
  }

  clear() {
    this.#locals.clear();
  }
}

// --- FILE: .\compiler\wasm\ReturnTable.ts ---

import { type AnyAstNode } from "../ast/nodes";

export class ReturnTable {
  #returns = new Map<number, boolean>();

  set(offset: number, value: boolean) {
    this.#returns.set(offset, value);
  }

  returns(node: AnyAstNode): boolean | undefined {
    return this.#returns.get(node.offset);
  }

  clear() {
    this.#returns.clear();
  }
}

// --- FILE: .\compiler\wasm\StructTable.ts ---

import { isValidType, type SizedType, type Type } from "../ast/types";
import { CompilerError } from "../errors";
import { WASM_TYPE_SIZES, type OperandSize } from "./types";

export interface StructField {
  name: string;
  type: SizedType;
  offset: number;
  size: OperandSize;
}

export interface StructSymbol {
  name: string;
  size: number;
  fields: Map<string, StructField>;
}

export class StructTable {
  #structs = new Map<string, StructSymbol>();

  add(struct: {
    name: string;
    fields: { name: string; type: Type; offset: number }[];
  }) {
    if (this.#structs.has(struct.name)) return false;

    const fields = new Map<string, StructField>();

    let offset = 0;

    for (const field of struct.fields) {
      if (!isValidType(field.type)) {
        throw new CompilerError(
          `Invalid type "${field.type}" for field declaration`,
          field.offset
        );
      }

      const size = WASM_TYPE_SIZES[field.type.kind];
      const aligned = this.#align(offset, size);
      fields.set(field.name, {
        name: field.name,
        type: field.type,
        offset: aligned,
        size,
      });
      offset = aligned + size;
    }

    this.#structs.set(struct.name, {
      name: struct.name,
      size: offset,
      fields,
    });
    return true;
  }

  get(name: string) {
    return this.#structs.get(name);
  }

  #align(offset: number, alignment: number): number {
    return (offset + alignment - 1) & ~(alignment - 1);
  }
}

// --- FILE: .\compiler\wasm\SymbolTable.ts ---

import type { Type } from "../ast/types";
import { CompilerError } from "../errors";
import type { FunctionSymbol } from "./FunctionTable";

export interface SymbolInfo {
  name: string;
  mangled: string;
  type: Type;
  offset: number;
}

export class SymbolTable {
  #parent;
  symbols;
  #func?: FunctionSymbol;

  constructor(parent: SymbolTable | null = null) {
    this.#parent = parent;
    this.symbols = new Map<string, SymbolInfo>();
  }

  enter() {
    return new SymbolTable(this);
  }

  exit() {
    return this.#parent;
  }

  add(name: string, mangled: string, type: Type, offset: number) {
    const resolved = this.resolve(name);

    if (resolved) {
      throw new CompilerError(
        `Variable "${name}" has already been declared in this scope`,
        offset
      );
    }

    const symbol = { name, mangled, type, offset };
    this.symbols.set(name, symbol);
    return symbol;
  }

  remove(name: string) {
    this.symbols.delete(name);
  }

  resolve(name: string): SymbolInfo | null {
    if (this.symbols.has(name)) {
      return this.symbols.get(name) || null;
    }
    return this.#parent?.resolve(name) || null;
  }

  setFunction(func?: FunctionSymbol) {
    this.#func = func;
  }

  getFunction(): FunctionSymbol | null {
    if (this.#func) {
      return this.#func;
    }
    return this.#parent?.getFunction() || null;
  }
}

// --- FILE: .\compiler\wasm\types.ts ---

import type { BinaryOperator, UnaryOperator } from "../ast/operators";
import {
  BuiltInTypes,
  TypeKinds,
  type NumericTypeKind,
  type ValidTypeKind,
  type Type,
  type TypeKind,
} from "../ast/types";
import { TokenTypes } from "../tokens";

export const WASM_TYPES: Record<ValidTypeKind, string> = {
  [TypeKinds.BOOL]: "i32",
  [TypeKinds.INT]: "i32",
  [TypeKinds.LONG]: "i64",
  [TypeKinds.STRING]: "i64",
  [TypeKinds.STRUCT]: "i32",
} as const;

export const WASM_BINARY_OPERATIONS: Partial<
  Record<TypeKind, Partial<Record<BinaryOperator, string>>>
> = {
  [TypeKinds.BOOL]: {
    [TokenTypes.AND]: "i32.and",
  },
  [TypeKinds.INT]: {
    [TokenTypes.EQUALS]: "i32.eq",
    [TokenTypes.NOT_EQUALS]: "i32.ne",
    [TokenTypes.LT]: "i32.lt_s",
    [TokenTypes.GT]: "i32.gt_s",
    [TokenTypes.LTE]: "i32.le_s",
    [TokenTypes.GTE]: "i32.ge_s",
    [TokenTypes.PLUS]: "i32.add",
    [TokenTypes.MINUS]: "i32.sub",
    [TokenTypes.MUL]: "i32.mul",
    [TokenTypes.POW]: "call $__i32_pow",
    [TokenTypes.DIV]: "i32.div_s",
    [TokenTypes.MOD]: "call $__i32_mod",
  },
  [TypeKinds.LONG]: {
    [TokenTypes.EQUALS]: "i64.eq",
    [TokenTypes.NOT_EQUALS]: "i64.ne",
    [TokenTypes.LT]: "i64.lt_s",
    [TokenTypes.GT]: "i64.gt_s",
    [TokenTypes.LTE]: "i64.le_s",
    [TokenTypes.GTE]: "i64.ge_s",
    [TokenTypes.PLUS]: "i64.add",
    [TokenTypes.MINUS]: "i64.sub",
    [TokenTypes.MUL]: "i64.mul",
    [TokenTypes.POW]: "call $__i64_pow",
    [TokenTypes.DIV]: "i64.div_s",
    [TokenTypes.MOD]: "call $__i64_mod",
  },
  [TypeKinds.STRING]: {
    [TokenTypes.EQUALS]: "call $__strcmp",
    [TokenTypes.PLUS]: "call $__strcat",
  },
};

export const WASM_UNARY_OPERATIONS: Partial<
  Record<TypeKind, Partial<Record<UnaryOperator, string>>>
> = {
  [TypeKinds.BOOL]: {
    [TokenTypes.NOT]: "i32.eqz",
  },
};

export const WASM_OPERATION_OPERANDS: Partial<Record<BinaryOperator, Type[]>> =
  {
    [TokenTypes.PLUS]: [
      BuiltInTypes.int,
      BuiltInTypes.long,
      BuiltInTypes.string,
    ],
    [TokenTypes.MINUS]: [BuiltInTypes.int, BuiltInTypes.long],
    [TokenTypes.MUL]: [BuiltInTypes.int, BuiltInTypes.long],
    [TokenTypes.POW]: [BuiltInTypes.int, BuiltInTypes.long],
    [TokenTypes.DIV]: [BuiltInTypes.int, BuiltInTypes.long],
    [TokenTypes.MOD]: [BuiltInTypes.int, BuiltInTypes.long],
  } as const;

export type OperandSize = 1 | 2 | 4 | 8;

export const WASM_LOAD_EMITS: Record<OperandSize, string> = {
  1: "i32.load8_u",
  2: "i32.load16_u",
  4: "i32.load",
  8: "i64.load",
} as const;

export const WASM_STORE_EMITS: Record<OperandSize, string> = {
  1: "i32.store8",
  2: "i32.store16",
  4: "i32.store",
  8: "i64.store",
} as const;

export const WASM_CONSTANT_EMITS: Record<NumericTypeKind, string> = {
  [TypeKinds.INT]: "i32.const",
  [TypeKinds.LONG]: "i64.const",
} as const;

export const WASM_NUMERIC_FORMATTERS: Record<NumericTypeKind, string> = {
  [TypeKinds.INT]: "__i32_to_str",
  [TypeKinds.LONG]: "__i64_to_str",
} as const;

export const WASM_TYPE_SIZES: Record<ValidTypeKind, OperandSize> = {
  [TypeKinds.BOOL]: 4,
  [TypeKinds.INT]: 4,
  [TypeKinds.LONG]: 8,
  [TypeKinds.STRING]: 8,
  [TypeKinds.STRUCT]: 4,
} as const;
