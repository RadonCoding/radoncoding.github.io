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
