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
