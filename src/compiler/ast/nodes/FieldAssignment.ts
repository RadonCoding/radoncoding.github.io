import { NodeKinds, type AnyAstNode } from ".";
import { AstNode } from "./AstNode";

export class FieldAssignment extends AstNode {
  readonly kind = NodeKinds.FIELD_ASSIGNMENT;
  readonly object: string;
  readonly member: string;
  readonly expression: AnyAstNode;

  constructor(
    object: string,
    member: string,
    expression: AnyAstNode,
    offset: number
  ) {
    super(offset);

    this.object = object;
    this.member = member;
    this.expression = expression;
  }

  children(): AnyAstNode[] {
    return [this.expression];
  }
}
