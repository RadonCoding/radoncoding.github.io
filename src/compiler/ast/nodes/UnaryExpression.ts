import { NodeKinds, type AnyAstNode } from ".";
import type { UnaryOperator } from "../operators";
import { AstNode } from "./AstNode";

export class UnaryExpression extends AstNode {
  readonly kind = NodeKinds.UNARY_EXPRESSION;
  readonly operator: UnaryOperator;
  readonly expression: AnyAstNode;

  constructor(operator: UnaryOperator, expression: AnyAstNode, offset: number) {
    super(offset);

    this.operator = operator;
    this.expression = expression;
  }

  children(): AnyAstNode[] {
    return [this.expression];
  }
}
