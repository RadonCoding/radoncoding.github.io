import { Parser } from "./ast/Parser";
import { tokenize } from "./tokens";
import { Compiler } from "./wasm/Compiler";
import { LanguageError, RuntimeError } from "./errors";
import { highlight, showErrorTooltip } from "./syntax";
import example from "../assets/example.rn?raw";

async function compile() {
  const inputElement = document.getElementById("compiler-input")!;
  const outputElement = document.getElementById("compiler-output")!;

  function clearOutput() {
    outputElement.innerText = "";
  }

  function writeToOutput(text: string) {
    outputElement.innerText += text;
    outputElement.innerText += "\n";
  }

  clearOutput();

  const code = inputElement.innerText!;

  try {
    const tokens = tokenize(code);

    const parser = new Parser(tokens);
    const ast = parser.parse();

    console.log(ast);

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
      showErrorTooltip(inputElement, err.message, code.length - 1);
    }

    if (err instanceof LanguageError) {
      showErrorTooltip(inputElement, err.message, err.offset);
    }
  }
}

document.addEventListener("DOMContentLoaded", () => {
  document.getElementById("compile-button")!.addEventListener("click", compile);

  const inputElement = document.getElementById("compiler-input")!;
  inputElement.innerText = example;

  highlight(inputElement);

  inputElement.addEventListener("input", () => highlight(inputElement));

  let isCollapsed = false;
  const toggle = document.getElementById("toggle-output")!;
  toggle.addEventListener("click", () => {
    const output = document.getElementById("output-section")!;

    isCollapsed = !isCollapsed;

    if (isCollapsed) {
      output.classList.add("collapsed");
      toggle.innerHTML = '<i class="fas fa-chevron-up"></i>';
    } else {
      output.classList.remove("collapsed");
      toggle.innerHTML = '<i class="fas fa-chevron-down"></i>';
    }
  });
});
