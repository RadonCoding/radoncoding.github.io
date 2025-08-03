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
