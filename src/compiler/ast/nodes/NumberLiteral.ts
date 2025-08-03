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
