export type FormatBase = 10 | 16;
export interface FormatType {
  padder?: string;
  padding?: number;
  right?: boolean;
  base: FormatBase;
  uppercase?: boolean;
}

export const DEFAULT_FORMAT = {
  base: 10,
} as const;

export function parseFormatType(value: string): FormatType | null {
  const pattern = /^(?:(.)([<>]))?(\d+)?([xX])?$/;

  const match = value.match(pattern);

  if (!match) return null;

  const [, padder, alignment, padding, base] = match;

  return {
    padder: padder,
    padding: padding ? parseInt(padding, 10) : undefined,
    right: alignment === "",
    base: base ? 16 : 10,
    uppercase: base === "X",
  };
}
