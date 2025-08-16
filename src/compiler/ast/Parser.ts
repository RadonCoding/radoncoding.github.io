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
