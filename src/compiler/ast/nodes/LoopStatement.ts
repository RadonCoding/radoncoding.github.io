import { AstNode } from "./AstNode";
import { NodeKinds, type AnyAstNode } from ".";

export class LoopStatement extends AstNode {
  readonly kind = NodeKinds.LOOP_STATEMENT;
  readonly declaration: AnyAstNode | null;
  readonly condition: AnyAstNode | null;
  readonly step: AnyAstNode | null;
  readonly body: AnyAstNode[];

  constructor(
    declaration: AnyAstNode | null,
    condition: AnyAstNode | null,
    step: AnyAstNode | null,
    body: AnyAstNode[],
    offset: number
  ) {
    super(offset);

    this.declaration = declaration;
    this.condition = condition;
    this.step = step;
    this.body = body;
  }

  children(): AnyAstNode[] {
    const children = [];

    if (this.declaration) {
      children.push(this.declaration);
    }

    if (this.step) {
      children.push(this.step);
    }

    children.push(...this.body);

    return children;
  }
}
