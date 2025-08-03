import { NodeKinds, type AnyAstNode } from ".";
import type { BinaryOperator } from "../operators";
import { AstNode } from "./AstNode";

export class FixExpression extends AstNode {
  readonly kind = NodeKinds.FIX_EXPRESSION;
  readonly operator: BinaryOperator;
  readonly expression: AnyAstNode;
  readonly prefix: boolean;

  constructor(
    operator: BinaryOperator,
    expression: AnyAstNode,
    offset: number,
    prefix: boolean
  ) {
    super(offset);

    this.operator = operator;
    this.expression = expression;
    this.prefix = prefix;
  }

  children(): AnyAstNode[] {
    return [this.expression];
  }
}
