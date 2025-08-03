import type { Type } from "../ast/types";

interface LocalInfo {
  name: string;
  mangled: string;
  type: Type;
  offset: number;
}

export class LocalTable {
  #locals = new Map<string, LocalInfo>();

  #counter = 0;

  static mangle(name: string, offset: number) {
    return `${name}_${offset}`;
  }

  tmp(type: Type, offset: number) {
    const name = `__${this.#counter++}`;
    const mangled = name;
    this.#locals.set(mangled, { name, mangled, type, offset });
    return name;
  }

  add(name: string, type: Type, offset: number) {
    const mangled = LocalTable.mangle(name, offset);
    this.#locals.set(mangled, { name, mangled, type, offset });
  }

  get(name: string, offset: number) {
    const mangled = LocalTable.mangle(name, offset);
    return this.#locals.get(mangled);
  }

  entries() {
    return this.#locals.entries();
  }

  clear() {
    this.#locals.clear();
  }
}
