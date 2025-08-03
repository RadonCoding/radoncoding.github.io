export class LanguageError extends Error {
  offset: number;

  constructor(message: string, offset: number) {
    super(message);

    this.offset = offset;
  }
}
export class InvalidSyntaxError extends LanguageError {}
export class CompilerError extends LanguageError {}
export class RuntimeError extends LanguageError {}
