import { TokenTypes } from "../tokens";

export const BinaryOperators = {
  [TokenTypes.EQUALS]: { precedence: 1, associativity: "left" },
  [TokenTypes.NOT_EQUALS]: { precedence: 1, associativity: "left" },
  [TokenTypes.LTE]: { precedence: 1, associativity: "left" },
  [TokenTypes.GTE]: { precedence: 1, associativity: "left" },
  [TokenTypes.LT]: { precedence: 1, associativity: "left" },
  [TokenTypes.GT]: { precedence: 1, associativity: "left" },
  [TokenTypes.PLUS]: { precedence: 2, associativity: "left" },
  [TokenTypes.MINUS]: { precedence: 2, associativity: "left" },
  [TokenTypes.POW]: { precedence: 4, associativity: "right" },
  [TokenTypes.MUL]: { precedence: 3, associativity: "left" },
  [TokenTypes.DIV]: { precedence: 3, associativity: "left" },
  [TokenTypes.MOD]: { precedence: 3, associativity: "left" },
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
