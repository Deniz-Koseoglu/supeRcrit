// Tab disable/enable handlers for output panels

// Handler to disable tabs (except the first one)
Shiny.addCustomMessageHandler('disableTabs', function(message) {
  var tabsetId = message.tabsetId;
  var keepFirst = message.keepFirst !== false; // default true
  
  var tabset = document.getElementById(tabsetId);
  if (tabset) {
    var tabs = tabset.querySelectorAll('.nav-tabs > li, .nav.nav-tabs > li');
    tabs.forEach(function(tab, index) {
      if (keepFirst && index === 0) {
        // Keep first tab enabled
        return;
      }
      tab.classList.add('disabled');
      var link = tab.querySelector('a');
      if (link) {
        link.setAttribute('data-toggle-disabled', 'true');
        link.removeAttribute('data-toggle');
      }
    });
  }
});

// Handler to enable all tabs
Shiny.addCustomMessageHandler('enableTabs', function(message) {
  var tabsetId = message.tabsetId;
  
  var tabset = document.getElementById(tabsetId);
  if (tabset) {
    var tabs = tabset.querySelectorAll('.nav-tabs > li, .nav.nav-tabs > li');
    tabs.forEach(function(tab) {
      tab.classList.remove('disabled');
      var link = tab.querySelector('a');
      if (link && link.hasAttribute('data-toggle-disabled')) {
        link.setAttribute('data-toggle', 'tab');
        link.removeAttribute('data-toggle-disabled');
      }
    });
  }
});

// Handler to disable specific tabs by index (0-based)
Shiny.addCustomMessageHandler('disableTabsByIndex', function(message) {
  var tabsetId = message.tabsetId;
  var indices = message.indices || [];
  
  var tabset = document.getElementById(tabsetId);
  if (tabset) {
    var tabs = tabset.querySelectorAll('.nav-tabs > li, .nav.nav-tabs > li');
    indices.forEach(function(index) {
      if (tabs[index]) {
        tabs[index].classList.add('disabled');
        var link = tabs[index].querySelector('a');
        if (link) {
          link.setAttribute('data-toggle-disabled', 'true');
          link.removeAttribute('data-toggle');
        }
      }
    });
  }
});
