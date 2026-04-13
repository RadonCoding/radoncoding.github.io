import { Parser } from "./ast/Parser";
import { tokenize } from "./tokens";
import { Compiler } from "./wasm/Compiler";
import { LanguageError, RuntimeError } from "./errors";
import { highlight, showErrorTooltip } from "./syntax";
import example from "../assets/example.rn?raw";

async function compile() {
  const input = document.getElementById("compiler-input")!;
  const output = document.getElementById("compiler-console")!;

  function clearOutput() {
    output.innerText = "";
  }

  function writeToOutput(text: string) {
    output.innerText += text;
    output.innerText += "\n";
  }

  clearOutput();

  const code = input.innerText;

  try {
    const tokens = tokenize(code);

    const parser = new Parser(tokens);
    const ast = parser.parse();

    const compiler = new Compiler();
    const wasm = await compiler.compile(ast);

    let memory: WebAssembly.Memory | null = null;

    const decoder = new TextDecoder();

    const imports = {
      env: {
        print: (message: bigint) => {
          const ptr = Number(message & 0xffffffffn);
          const len = Number(message >> 32n);
          const bytes = new Uint8Array(memory!.buffer, ptr, len);
          writeToOutput(decoder.decode(bytes));
        },
        error: (message: bigint, index: number) => {
          const ptr = Number(message & 0xffffffffn);
          const len = Number(message >> 32n);
          const bytes = new Uint8Array(memory!.buffer, ptr, len);
          const decodedMessage = decoder.decode(bytes);
          throw new RuntimeError(decodedMessage, index);
        },
      },
    };

    const { instance } = await WebAssembly.instantiate(wasm, imports);

    memory = instance.exports.memory as WebAssembly.Memory;

    if (!(instance.exports.main instanceof Function)) {
      return;
    }

    instance.exports.main();
  } catch (err) {
    console.error(err);

    if (
      err instanceof WebAssembly.CompileError ||
      err instanceof WebAssembly.RuntimeError
    ) {
      showErrorTooltip(input, err.message, code.length - 1);
    }

    if (err instanceof LanguageError) {
      showErrorTooltip(input, err.message, err.offset);
    }
  }
}

document.addEventListener("DOMContentLoaded", () => {
  document.getElementById("compile-button")?.addEventListener("click", compile);

  const input = document.getElementById("compiler-input");

  if (!input) return;

  input.innerText = example;

  highlight(input);

  input.addEventListener("input", () => highlight(input));

  const toggle = document.getElementById("toggle-output")!;
  const container = document.getElementById("compiler-output")!;

  toggle.addEventListener("click", () => {
    const collapsed = container.classList.toggle("compiler__output--collapsed");
    toggle.innerHTML = collapsed
      ? '<i class="bi bi-chevron-up"></i>'
      : '<i class="bi bi-chevron-down"></i>';
  });
});
