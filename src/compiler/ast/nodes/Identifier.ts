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
