// Mark regions that scroll sideways so CSS can fade the edge that hides more content.
(() => {
  const update = (el) => {
    const more = el.scrollWidth - el.clientWidth > 2;
    const left = el.scrollLeft > 2, right = el.scrollLeft < el.scrollWidth - el.clientWidth - 2;
    if (!more) el.removeAttribute("data-more");
    else el.setAttribute("data-more", left && right ? "both" : left ? "left" : "right");
  };
  const els = document.querySelectorAll(".fig, pre, .table-wrap");
  els.forEach((el) => { update(el); el.addEventListener("scroll", () => update(el), { passive: true }); });
  addEventListener("resize", () => els.forEach(update));
})();
