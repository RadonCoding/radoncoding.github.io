import { AstNode } from "./AstNode";
import { NodeKinds, type AnyAstNode } from ".";

export class TernaryExpression extends AstNode {
  readonly kind = NodeKinds.TERNARY_EXPRESSION;
  readonly condition: AnyAstNode;
  readonly consequent: AnyAstNode;
  readonly alternate: AnyAstNode;

  constructor(
    condition: AnyAstNode,
    consequent: AnyAstNode,
    alternate: AnyAstNode,
    offset: number
  ) {
    super(offset);

    this.condition = condition;
    this.consequent = consequent;
    this.alternate = alternate;
  }

  children(): AnyAstNode[] {
    return [this.condition, this.consequent, this.alternate];
  }
}
