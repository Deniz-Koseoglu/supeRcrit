/*
 * help_i18n.js — live translation for inline help-icon modals.
 *
 * shiny.i18n (JS mode) translates the page by swapping the TEXT of
 * <span class="i18n"> nodes when the language changes. The help text produced
 * by input_help() lives in data-modal-content / data-modal-title /
 * data-modal-label ATTRIBUTES of a `.input-help-icon` span, which the built-in
 * translator never touches — so those modals used to stay in English.
 *
 * input_help() now stores the English source string (the i18n data-key) in
 * those attributes. This script looks each one up in shiny.i18n's translation
 * table (the global `i18n_translations`, the same table shiny-i18n.js uses) and
 * rewrites the attributes to the active language, both when the language
 * changes and when help icons are inserted dynamically. The English source is
 * snapshotted once into data-i18n-* so repeated language switches keep working.
 */
(function () {
  var PARTS = ["content", "title", "label"];

  // Mirror of shiny-i18n.js translate(): look up an English key for `lang`.
  function translateKey(key, lang) {
    if (!key) return key;
    if (!lang || typeof i18n_translations === "undefined") return key;
    var hit = i18n_translations
      .filter(function (cell) { return cell._row === key; })
      .map(function (row) { return row[lang]; })[0];
    return (hit === undefined || hit === null || hit === "") ? key : hit;
  }

  function currentLang() {
    var st = document.getElementById("i18n-state");
    if (!st || typeof $ === "undefined") return null;
    return $(st).data("lang") || null;
  }

  function translateIcon(el, lang) {
    PARTS.forEach(function (part) {
      var liveAttr = "data-modal-" + part;
      if (!el.hasAttribute(liveAttr)) return;
      var srcAttr = "data-i18n-" + part;
      // Snapshot the original English source the first time we see this icon.
      if (!el.hasAttribute(srcAttr)) {
        el.setAttribute(srcAttr, el.getAttribute(liveAttr) || "");
      }
      var src = el.getAttribute(srcAttr);
      if (src === "") return;
      // Inline content may be several <br>-joined segments; translate each.
      var translated = src.split("<br>").map(function (seg) {
        return translateKey(seg, lang);
      }).join("<br>");
      el.setAttribute(liveAttr, translated);
    });
  }

  function translateAll(lang) {
    var icons = document.querySelectorAll(".input-help-icon");
    for (var i = 0; i < icons.length; i++) translateIcon(icons[i], lang);
  }

  // Native hover tooltips: an element with class "i18n-tooltip" carries its English source string in a
  // data-title-key attribute. The browser `title` attribute is never touched by shiny.i18n's text-node
  // translator, so we rewrite it here to the active language (same lookup table), both on language change and
  // when such elements are inserted — the browser tooltip then shows the translated text live.
  function translateTooltip(el, lang) {
    var key = el.getAttribute("data-title-key");
    if (!key) return;
    el.setAttribute("title", translateKey(key, lang));
  }
  function translateAllTooltips(lang) {
    var els = document.querySelectorAll(".i18n-tooltip");
    for (var i = 0; i < els.length; i++) translateTooltip(els[i], lang);
  }

  // The blue box-header help buttons (create_help_modal -> shinyhelper) load a
  // language-suffixed markdown file (e.g. com_analysis_help_en). The filename is
  // baked at render time and is NOT an .i18n text node, so swap its _en/_ru/_tr
  // suffix to the active language here, both on language change and on insertion.
  function translateHelpFiles(lang) {
    if (lang !== "en" && lang !== "ru" && lang !== "tr") return;
    var els = document.querySelectorAll('.shinyhelper-icon[data-modal-type="markdown"]');
    for (var i = 0; i < els.length; i++) {
      var c = els[i].getAttribute("data-modal-content");
      if (c) els[i].setAttribute("data-modal-content", c.replace(/_(en|ru|tr)$/, "_" + lang));
    }
  }

  function init() {
    // Re-translate every help icon whenever the language changes. shiny.i18n
    // sets #i18n-state's data-lang before triggering 'change', so it is current.
    $(document).on("change", "#i18n-state", function () {
      var lang = $(this).data("lang") || null;
      translateAll(lang);
      translateAllTooltips(lang);
      translateHelpFiles(lang);
    });

    // Translate dynamically inserted help icons as soon as they appear.
    if (typeof MutationObserver !== "undefined") {
      var mo = new MutationObserver(function (mutations) {
        var lang = currentLang();
        if (!lang) return; // English (key language) needs no rewrite
        mutations.forEach(function (m) {
          for (var i = 0; i < m.addedNodes.length; i++) {
            var node = m.addedNodes[i];
            if (node.nodeType !== 1) continue;
            if (node.classList && node.classList.contains("input-help-icon")) {
              translateIcon(node, lang);
            }
            if (node.classList && node.classList.contains("i18n-tooltip")) {
              translateTooltip(node, lang);
            }
            if (node.querySelectorAll) {
              var inner = node.querySelectorAll(".input-help-icon");
              for (var k = 0; k < inner.length; k++) translateIcon(inner[k], lang);
              var tips = node.querySelectorAll(".i18n-tooltip");
              for (var t = 0; t < tips.length; t++) translateTooltip(tips[t], lang);
            }
          }
        });
        translateHelpFiles(lang);
      });
      mo.observe(document.body, { childList: true, subtree: true });
    }

    // Catch any icons already present and an initial non-English language.
    translateAll(currentLang());
    translateAllTooltips(currentLang());
    translateHelpFiles(currentLang());
  }

  if (typeof $ !== "undefined") {
    $(document).ready(init);
  } else {
    document.addEventListener("DOMContentLoaded", init);
  }
})();
