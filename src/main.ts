import "./style.css";

for (const element of document.getElementsByClassName("copy")) {
  if (!(element instanceof HTMLElement)) continue;

  element.addEventListener("click", () => {
    const text = element.getAttribute("data-text");

    if (!text) return;

    navigator.clipboard.writeText(text);
  });
}
