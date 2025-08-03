import { NodeKinds, type AnyAstNode } from ".";
import { AstNode } from "./AstNode";

export class FieldExpression extends AstNode {
  readonly kind = NodeKinds.FIELD_EXPRESSION;
  readonly object: string;
  readonly member: string;

  constructor(object: string, member: string, offset: number) {
    super(offset);

    this.object = object;
    this.member = member;
  }

  children(): AnyAstNode[] {
    return [];
  }
}
