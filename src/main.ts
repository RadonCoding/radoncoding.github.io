import "./style.css";

const tooltip = document.createElement("div");
tooltip.id = "tooltip";
document.body.appendChild(tooltip);

let tooltipTimeout: ReturnType<typeof setTimeout>;

const update = (el: HTMLElement) => {
  const rect = el.getBoundingClientRect();
  tooltip.style.left = `${rect.left + window.scrollX + rect.width / 2}px`;
  tooltip.style.top = `${rect.top + window.scrollY}px`;
};

for (const element of document.querySelectorAll<HTMLElement>(
  "[data-tooltip]",
)) {
  element.addEventListener("click", (event) => {
    clearTimeout(tooltipTimeout);

    const content = element.getAttribute("data-tooltip");
    const copy = element.getAttribute("data-copy");
    const href = element.getAttribute("href");

    tooltip.textContent = content;
    tooltip.classList.add("tooltip--visible");
    update(element);

    element.setAttribute("data-state", "success");

    setTimeout(() => {
      element.removeAttribute("data-state");
    }, 1000);

    tooltipTimeout = setTimeout(() => {
      tooltip.classList.remove("tooltip--visible");
    }, 1000);

    if (copy) {
      navigator.clipboard.writeText(copy);
    } else if (href) {
      window.open(href, "_blank");
    }

    event.preventDefault();
  });
}

document.addEventListener("pointerdown", (event) => {
  if (!(event.target as HTMLElement).closest("[data-tooltip]")) {
    tooltip.classList.remove("tooltip--visible");
  }
});
