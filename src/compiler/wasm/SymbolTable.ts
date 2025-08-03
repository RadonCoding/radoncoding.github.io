import type { Type } from "../ast/types";
import { CompilerError } from "../errors";
import type { FunctionSymbol } from "./FunctionTable";

export interface SymbolInfo {
  name: string;
  mangled: string;
  type: Type;
  offset: number;
}

export class SymbolTable {
  #parent;
  symbols;
  #func?: FunctionSymbol;

  constructor(parent: SymbolTable | null = null) {
    this.#parent = parent;
    this.symbols = new Map<string, SymbolInfo>();
  }

  enter() {
    return new SymbolTable(this);
  }

  exit() {
    return this.#parent;
  }

  add(name: string, mangled: string, type: Type, offset: number) {
    const resolved = this.resolve(name);

    if (resolved) {
      throw new CompilerError(
        `Variable "${name}" has already been declared in this scope`,
        offset
      );
    }

    const symbol = { name, mangled, type, offset };
    this.symbols.set(name, symbol);
    return symbol;
  }

  remove(name: string) {
    this.symbols.delete(name);
  }

  resolve(name: string): SymbolInfo | null {
    if (this.symbols.has(name)) {
      return this.symbols.get(name) || null;
    }
    return this.#parent?.resolve(name) || null;
  }

  setFunction(func?: FunctionSymbol) {
    this.#func = func;
  }

  getFunction(): FunctionSymbol | null {
    if (this.#func) {
      return this.#func;
    }
    return this.#parent?.getFunction() || null;
  }
}
