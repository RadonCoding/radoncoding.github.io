import { AstNode } from "./AstNode";
import { NodeKinds, type AnyAstNode } from ".";

export class IfStatement extends AstNode {
  readonly kind = NodeKinds.IF_STATEMENT;
  readonly condition: AnyAstNode;
  readonly consequent: AnyAstNode[];
  readonly alternate: AnyAstNode[] | IfStatement | null;

  constructor(
    condition: AnyAstNode,
    consequent: AnyAstNode[],
    alternate: AnyAstNode[] | IfStatement | null,
    offset: number
  ) {
    super(offset);

    this.condition = condition;
    this.consequent = consequent;
    this.alternate = alternate;
  }

  children(): AnyAstNode[] {
    const children = [this.condition, ...this.consequent];

    if (this.alternate) {
      if (Array.isArray(this.alternate)) {
        children.push(...this.alternate);
      } else {
        children.push(this.alternate);
      }
    }
    return children;
  }

  branches(): AnyAstNode[][] {
    const branches = [this.consequent];

    if (this.alternate) {
      if (Array.isArray(this.alternate)) {
        branches.push(this.alternate);
      } else {
        branches.push([this.alternate]);
      }
    }
    return branches;
  }
}
