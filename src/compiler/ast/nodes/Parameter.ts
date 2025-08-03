import { NodeKinds } from ".";
import type { Type } from "../types";
import { AstNode } from "./AstNode";

export class Parameter extends AstNode {
  readonly kind = NodeKinds.PARAMETER;
  readonly type: Type;
  readonly name: string;

  constructor(type: Type, name: string, offset: number) {
    super(offset);

    this.type = type;
    this.name = name;
  }
}
