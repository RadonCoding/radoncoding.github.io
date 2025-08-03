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
