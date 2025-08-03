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
