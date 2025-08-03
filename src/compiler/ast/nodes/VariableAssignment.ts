import { NodeKinds, type AnyAstNode } from ".";
import { AstNode } from "./AstNode";

export class VariableAssignment extends AstNode {
  readonly kind = NodeKinds.VARIABLE_ASSIGNMENT;
  readonly name: string;
  readonly expression: AnyAstNode;

  constructor(name: string, expression: AnyAstNode, offset: number) {
    super(offset);

    this.name = name;
    this.expression = expression;
  }

  children(): AnyAstNode[] {
    return [this.expression];
  }
}
