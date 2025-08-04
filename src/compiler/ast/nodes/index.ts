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
