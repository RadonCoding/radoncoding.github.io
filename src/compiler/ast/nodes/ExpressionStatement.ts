import { AstNode } from "./AstNode";
import { NodeKinds, type AnyAstNode } from ".";

export class ExpressionStatement extends AstNode {
  readonly kind = NodeKinds.EXPRESSION_STATEMENT;
  readonly expression: AnyAstNode;

  constructor(expression: AnyAstNode, offset: number) {
    super(offset);

    this.expression = expression;
  }

  children(): AnyAstNode[] {
    return [this.expression];
  }
}
