import { type BuiltInType } from "../ast/types";
import type { FunctionDeclaration } from "./nodes/FunctionDeclaration";

export class FunctionTable {
  #funcs = new Map<string, FunctionDeclaration>();

  add(func: FunctionDeclaration) {
    const key = func.name + func.params.map((param) => param.type).join(",");
    this.#funcs.set(key, func);
  }

  get(name: string, args: BuiltInType[]): FunctionDeclaration | null {
    const key = name + args.join(",");
    return this.#funcs.get(key) ?? null;
  }
}
