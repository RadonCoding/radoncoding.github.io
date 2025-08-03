import { AstNode } from "./AstNode";
import { NodeKinds, type AnyAstNode } from ".";
import type { FormatType } from "../formats";

export class FormattedExpression extends AstNode {
  readonly kind = NodeKinds.FORMATTED_EXPRESSION;
  readonly expression: AnyAstNode;
  readonly format: FormatType;

  constructor(expression: AnyAstNode, format: FormatType, offset: number) {
    super(offset);

    this.expression = expression;
    this.format = format;
  }

  children(): AnyAstNode[] {
    return [this.expression];
  }
}
