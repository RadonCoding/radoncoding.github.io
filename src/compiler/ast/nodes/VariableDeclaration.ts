import { NodeKinds, type AnyAstNode } from ".";
import type { Type } from "../types";
import { AstNode } from "./AstNode";

export class VariableDeclaration extends AstNode {
  readonly kind = NodeKinds.VARIABLE_DECLARATION;
  readonly type: Type;
  readonly name: string;
  readonly expression: AnyAstNode | null;

  constructor(
    type: Type,
    name: string,
    expression: AnyAstNode | null,
    offset: number
  ) {
    super(offset);

    this.type = type;
    this.name = name;
    this.expression = expression;
  }

  children(): AnyAstNode[] {
    return this.expression ? [this.expression] : [];
  }
}
