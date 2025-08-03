import type { Parameter } from "./Parameter";
import { NodeKinds, type AnyAstNode } from ".";
import { AstNode } from "./AstNode";
import type { Type } from "../types";

export class FunctionDeclaration extends AstNode {
  readonly kind = NodeKinds.FUNCTION_DECLARATION;
  readonly type: Type;
  readonly name: string;
  readonly params: Parameter[];
  readonly body: AnyAstNode[];

  constructor(
    type: Type,
    name: string,
    params: Parameter[],
    body: AnyAstNode[],
    offset: number
  ) {
    super(offset);

    this.type = type;
    this.name = name;
    this.params = params;
    this.body = body;
  }

  children(): AnyAstNode[] {
    return [...this.params, ...this.body];
  }
}
