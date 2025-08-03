import type { Type } from "../ast/types";

interface FunctionParam {
  name: string;
  type: Type;
}

export interface FunctionSymbol {
  name: string;
  params: FunctionParam[];
  type: Type;
}

export class FunctionTable {
  #functions = new Map<string, FunctionSymbol>();

  add(func: FunctionSymbol) {
    if (this.#functions.has(func.name)) {
      return false;
    }

    this.#functions.set(func.name, func);

    return true;
  }

  get(name: string) {
    return this.#functions.get(name);
  }
}
