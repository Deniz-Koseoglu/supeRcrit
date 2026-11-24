/*
 * help_perf.js — make the blue "?" help icons open instantly.
 *
 * shinyhelper's client JS (assets/shinyhelper.js) is pathological: its observe_helpers() runs
 * $(".shinyhelper-icon").on("click", ...) with NO .off(), and re-runs on EVERY shiny:value (every output
 * render). So each help icon accumulates another click handler on every render — measured 510+ on a
 * long-lived icon. One click then fires ALL of them (each doing Shiny.setInputValue), which is the real
 * delay. Icons whose renderUI() re-renders (e.g. DOE Time/Response Variable after "Load Example") get a
 * fresh element and reset to a low count → fast; icons that never re-render (Model Order, etc.) stay slow.
 * (Server side is ~2ms; this is purely the client handler pile-up.)
 *
 * We CANNOT neutralise observe_helpers() — it is not exposed as a global, so it keeps re-adding handlers in
 * the background. Instead we kill the cost at click time: a CAPTURE-phase listener runs BEFORE the stacked
 * direct handlers (capture → target → bubble), strips them off the icon so none of them execute, and a
 * single delegated BUBBLE-phase handler opens the modal exactly once. Net: click cost is independent of how
 * many handlers shinyhelper piled on.
 */
(function () {
  function fireModal(iconEl) {
    var d = iconEl.dataset;
    Shiny.setInputValue("shinyhelper-modal_params", {
      size: d.modalSize, type: d.modalType, title: d.modalTitle, content: d.modalContent,
      label: d.modalLabel, easyClose: d.modalEasyclose, fade: d.modalFade, nonce: Math.random()
    }, { priority: "event" });
  }

  function init() {
    // Capture phase (3rd arg true): fires before the icon's stacked direct handlers. Strip them so the
    // 510+ shinyhelper handlers never run; our own delegated handler (below) opens the modal.
    document.addEventListener("click", function (e) {
      var icon = e.target && e.target.closest ? e.target.closest(".shinyhelper-icon") : null;
      if (icon) { try { $(icon).off("click"); } catch (err) {} }
    }, true);

    // Bubble phase: one delegated handler for all current + future icons (never stacks).
    $(document).off("click.superhelp").on("click.superhelp", ".shinyhelper-icon", function () {
      fireModal(this);
    });
  }

  if (typeof $ !== "undefined") { $(document).ready(init); }
  else { document.addEventListener("DOMContentLoaded", init); }
})();
