import { isValidType, type SizedType, type Type } from "../ast/types";
import { CompilerError } from "../errors";
import { WASM_TYPE_SIZES, type OperandSize } from "./types";

export interface StructField {
  name: string;
  type: SizedType;
  offset: number;
  size: OperandSize;
}

export interface StructSymbol {
  name: string;
  size: number;
  fields: Map<string, StructField>;
}

export class StructTable {
  #structs = new Map<string, StructSymbol>();

  add(struct: {
    name: string;
    fields: { name: string; type: Type; offset: number }[];
  }) {
    if (this.#structs.has(struct.name)) return false;

    const fields = new Map<string, StructField>();

    let offset = 0;

    for (const field of struct.fields) {
      if (!isValidType(field.type)) {
        throw new CompilerError(
          `Invalid type "${field.type}" for field declaration`,
          field.offset
        );
      }

      const size = WASM_TYPE_SIZES[field.type.kind];
      const aligned = this.#align(offset, size);
      fields.set(field.name, {
        name: field.name,
        type: field.type,
        offset: aligned,
        size,
      });
      offset = aligned + size;
    }

    this.#structs.set(struct.name, {
      name: struct.name,
      size: offset,
      fields,
    });
    return true;
  }

  get(name: string) {
    return this.#structs.get(name);
  }

  #align(offset: number, alignment: number): number {
    return (offset + alignment - 1) & ~(alignment - 1);
  }
}
