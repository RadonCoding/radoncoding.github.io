import "./style.css";

for (const element of document.getElementsByClassName("copy")) {
  if (!(element instanceof HTMLElement)) continue;

  element.addEventListener("click", () => {
    const content = element.getAttribute("data-copy");

    if (!content) return;

    navigator.clipboard.writeText(content);
  });
}

const tooltip = document.createElement("div");
tooltip.id = "tooltip";
document.body.appendChild(tooltip);

let timeout: ReturnType<typeof setTimeout>;

for (const element of document.querySelectorAll<HTMLElement>(
  "[data-tooltip]",
)) {
  element.addEventListener("pointerenter", () => {
    clearTimeout(timeout);
    const rect = element.getBoundingClientRect();
    tooltip.textContent = element.getAttribute("data-tooltip");
    tooltip.classList.add("tooltip--visible");
    tooltip.style.left = `${rect.left + rect.width / 2}px`;
    tooltip.style.top = `${rect.top}px`;
  });

  element.addEventListener("pointerleave", () => {
    timeout = setTimeout(
      () => tooltip.classList.remove("tooltip--visible"),
      3000,
    );
  });
}

document.addEventListener("pointerdown", (event) => {
  if (!(event.target as HTMLElement).closest("[data-tooltip]")) {
    tooltip.classList.remove("tooltip--visible");
  }
});
