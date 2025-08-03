import { AstNode } from "./AstNode";
import { NodeKinds, type AnyAstNode } from ".";

export class ReturnStatement extends AstNode {
  readonly kind = NodeKinds.RETURN_STATEMENT;
  readonly expression: AnyAstNode | null;

  constructor(expression: AnyAstNode | null, offset: number) {
    super(offset);

    this.expression = expression;
  }

  children(): AnyAstNode[] {
    return this.expression ? [this.expression] : [];
  }
}
