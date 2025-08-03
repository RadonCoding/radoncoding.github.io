import type { AnyAstNode, NodeKind } from ".";

export abstract class AstNode {
  abstract readonly kind: NodeKind;
  readonly offset: number;

  constructor(offset: number) {
    this.offset = offset;
  }

  children(): AnyAstNode[] {
    return [];
  }

  branches(): AnyAstNode[][] {
    return [];
  }
}
