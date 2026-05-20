// Expand/collapse all accordions within a box

$(document).ready(function() {
  
  window.toggleAllPanels = function(accordionId, button) {
    var $accordion = $('[id="' + accordionId + '"]');
    var $panels = $accordion.find('.collapse');
    var $button = $(button);
    var $icon = $button.find('i');
    var parentSelector = '#' + accordionId;
    
    // Check actual state of panels rather than icon
    var openPanels = $panels.filter('.show, .in').length;
    var allOpen = openPanels === $panels.length;
    var allClosed = openPanels === 0;
    
    // Decide action: if all open, collapse; otherwise expand
    var shouldCollapse = allOpen || ($icon.hasClass('fa-compress-alt') && !allClosed);
    
    if (shouldCollapse) {
      // COLLAPSE ALL
      $panels.each(function() {
        var $panel = $(this);
        $panel.data('bs.collapse', null);
        $panel.collapse({parent: parentSelector, toggle: false}).collapse('hide');
      });
      $icon.removeClass('fa-compress-alt').addClass('fa-expand-alt');
      $button.attr('title', $button.data('title-expand') || 'Expand all');
    } else {
      // EXPAND ALL
      // Step 1: Destroy any existing collapse instances and re-init WITHOUT parent
      $panels.each(function() {
        var $panel = $(this);
        $panel.data('bs.collapse', null);
        $panel.collapse({toggle: false}); // No parent = no auto-close
      });
      
      // Step 2: Show all panels at once
      $panels.collapse('show');
      
      // Step 3: Re-initialize with parent after animations complete
      setTimeout(function() {
        $panels.each(function() {
          var $panel = $(this);
          $panel.data('bs.collapse', null);
          $panel.collapse({parent: parentSelector, toggle: false});
        });
      }, 400);
      
      $icon.removeClass('fa-expand-alt').addClass('fa-compress-alt');
      $button.attr('title', $button.data('title-collapse') || 'Collapse all');
    }
  };
  
});
