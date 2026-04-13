import "./style.css";

for (const element of document.getElementsByClassName("copy")) {
  if (!(element instanceof HTMLElement)) continue;

  element.addEventListener("click", () => {
    const content = element.getAttribute("data-copy");

    if (!content) return;

    navigator.clipboard.writeText(content);
  });
}
