import { NodeKinds } from ".";
import type { Type } from "../types";
import { AstNode } from "./AstNode";

export class FieldDeclaration extends AstNode {
  readonly kind = NodeKinds.FIELD_DECLARATION;
  readonly type: Type;
  readonly name: string;

  constructor(type: Type, name: string, offset: number) {
    super(offset);

    this.type = type;
    this.name = name;
  }
}
