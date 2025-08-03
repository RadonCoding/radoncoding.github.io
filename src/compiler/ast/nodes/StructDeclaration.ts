import { NodeKinds, type AnyAstNode } from ".";
import { AstNode } from "./AstNode";
import type { FieldDeclaration } from "./FieldDeclaration";

export class StructDeclaration extends AstNode {
  readonly kind = NodeKinds.STRUCT_DECLARATION;
  readonly name: string;
  readonly fields: FieldDeclaration[];

  constructor(name: string, fields: FieldDeclaration[], offset: number) {
    super(offset);

    this.name = name;
    this.fields = fields;
  }

  children(): AnyAstNode[] {
    return [...this.fields];
  }
}
