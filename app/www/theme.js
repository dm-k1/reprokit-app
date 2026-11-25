// Light and dark colour schemes. The button in the title bar switches
// between them; the choice is remembered in this browser.
(function () {
  "use strict";

  var root = document.documentElement;

  var stored = function () {
    try {
      return window.localStorage.getItem("oracle-theme");
    } catch (e) {
      return null;
    }
  };

  var apply = function (theme) {
    root.setAttribute("data-bs-theme", theme);
    var button = document.getElementById("theme_toggle");
    if (button) button.setAttribute("aria-pressed", String(theme === "dark"));
  };

  // Set the scheme before the page is drawn, so it does not flash.
  apply(stored() === "dark" ? "dark" : "light");

  document.addEventListener("DOMContentLoaded", function () {
    apply(root.getAttribute("data-bs-theme"));
  });

  document.addEventListener("click", function (event) {
    if (!event.target.closest("#theme_toggle")) return;
    var next = root.getAttribute("data-bs-theme") === "dark" ? "light" : "dark";
    apply(next);
    try {
      window.localStorage.setItem("oracle-theme", next);
    } catch (e) { /* remembering is optional */ }
  });
})();
