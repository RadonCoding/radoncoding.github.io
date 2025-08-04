import { LanguageError } from "./errors";
import { tokenize, TokenTypeNames, TokenTypes } from "./tokens";

function saveCursor(element: Node) {
  const sel = window.getSelection();

  if (!sel) return null;

  if (sel.rangeCount === 0) return null;

  const range = sel.getRangeAt(0);
  const pre = range.cloneRange();
  pre.selectNodeContents(element);
  pre.setEnd(range.startContainer, range.startOffset);
  const start = pre.toString().length;

  return start;
}

function restoreCursor(element: HTMLElement, saved: number | null) {
  if (saved === null) return;

  const range = document.createRange();
  range.setStart(element, 0);
  range.collapse(true);

  let stop = false;
  let node: Node | undefined;
  let nodes: Node[] = [element];
  let previous = 0;

  while (!stop && (node = nodes.pop())) {
    if (node.nodeType === 3) {
      const length = node.textContent?.length || 0;
      const next = previous + length;

      if (saved <= next) {
        range.setStart(node, saved - previous);
        range.collapse(true);
        stop = true;
      }
      previous = next;
    } else {
      let i = node.childNodes.length;
      while (i--) {
        nodes.push(node.childNodes[i]);
      }
    }
  }

  const sel = window.getSelection();

  if (sel) {
    sel.removeAllRanges();
    sel.addRange(range);
  }
}

let error = {
  element: null as HTMLElement | null,
  node: null as Node | null,
  offset: 0,
};

function moveErrorTooltip() {
  const tooltip = document.getElementById("error-tooltip")!;
  if (tooltip.style.display !== "block") return;

  const { element, node: target, offset } = error;

  if (!element || !target) return;

  const range = document.createRange();
  try {
    range.setStart(target, offset);
    range.setEnd(target, offset + 1);
  } catch {
    return;
  }

  const rect = range.getBoundingClientRect();
  const left = rect.left + rect.width / 2;
  const top = rect.bottom + 8;

  tooltip.style.left = `${left}px`;
  tooltip.style.top = `${top}px`;
}

export function showErrorTooltip(
  element: HTMLElement,
  message: string,
  index: number
) {
  const tooltip = document.getElementById("error-tooltip")!;
  tooltip.style.display = "none";

  const range = document.createRange();

  const walker = document.createTreeWalker(element, NodeFilter.SHOW_ALL);

  let current = 0;
  let offset = 0;

  let node: Node | null;

  while ((node = walker.nextNode())) {
    if (node.nodeType !== Node.TEXT_NODE) {
      if (node.nodeName === "BR") {
        current++;
      }
      continue;
    }

    const length = node.textContent?.length || 0;

    if (index >= current && index < current + length) {
      offset = index - current;
      break;
    }
    current += length;
  }

  if (!node) {
    throw Error("No node found for index");
  }

  range.setStart(node, offset);
  range.setEnd(node, offset + 1);

  const rect = range.getBoundingClientRect();

  const scroller = range.startContainer.parentElement;

  if (scroller) {
    scroller.scrollIntoView({
      behavior: "smooth",
      block: "center",
      inline: "nearest",
    });
  }

  tooltip.innerText = message;
  tooltip.style.display = "block";

  const left = rect.left + rect.width / 2;
  const top = rect.bottom + 8;

  tooltip.style.left = `${left}px`;
  tooltip.style.top = `${top}px`;

  error = { element, node, offset };

  element.addEventListener("scroll", moveErrorTooltip, { passive: true });
}

export function hideErrorTooltip() {
  const tooltip = document.getElementById("error-tooltip")!;
  tooltip.style.display = "none";
}

export function highlight(element: HTMLElement) {
  hideErrorTooltip();

  const code = element.innerText;

  try {
    const tokens = tokenize(code);

    const saved = saveCursor(element);

    let result = "";
    let cursor = 0;

    for (const token of tokens) {
      const raw = code.slice(cursor, token.end);
      const name = TokenTypeNames[token.type!];

      let clazz = `token-${name.toLowerCase().replace(/_/g, "-")}`;

      if (token.type === TokenTypes.IDENTIFIER) {
        clazz += `-${token.value}`;
      }

      result += `<span class="${clazz}">${raw}</span>`;
      cursor = token.end;
    }

    element.innerHTML = result;

    restoreCursor(element, saved);
  } catch (err) {
    console.error(err);

    if (err instanceof LanguageError) {
      showErrorTooltip(element, err.message, err.offset);
    }
  }
}
