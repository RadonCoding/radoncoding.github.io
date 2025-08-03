import { AstNode } from "./AstNode";
import { NodeKinds, type AnyAstNode } from ".";

export class CallExpression extends AstNode {
  readonly kind = NodeKinds.CALL_EXPRESSION;
  readonly callee: string;
  readonly args: AnyAstNode[];

  constructor(callee: string, args: AnyAstNode[], offset: number) {
    super(offset);

    this.callee = callee;
    this.args = args;
  }

  children(): AnyAstNode[] {
    return this.args;
  }
}
