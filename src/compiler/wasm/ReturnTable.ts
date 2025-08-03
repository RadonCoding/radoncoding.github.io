import { type AnyAstNode } from "../ast/nodes";

export class ReturnTable {
  #returns = new Map<number, boolean>();

  set(offset: number, value: boolean) {
    this.#returns.set(offset, value);
  }

  returns(node: AnyAstNode): boolean | undefined {
    return this.#returns.get(node.offset);
  }

  clear() {
    this.#returns.clear();
  }
}
