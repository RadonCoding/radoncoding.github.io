import { AstNode } from "./AstNode";
import { NodeKinds, type AnyAstNode } from ".";
import type { BinaryOperator } from "../operators";

export class BinaryExpression extends AstNode {
  readonly kind = NodeKinds.BINARY_EXPRESSION;
  readonly operator: BinaryOperator;
  readonly left: AnyAstNode;
  readonly right: AnyAstNode;

  constructor(
    operator: BinaryOperator,
    left: AnyAstNode,
    right: AnyAstNode,
    offset: number
  ) {
    super(offset);

    this.operator = operator;
    this.left = left;
    this.right = right;
  }

  children(): AnyAstNode[] {
    return [this.left, this.right];
  }
}
