/**
 * Settings Manager for supeRcrit
 * Export: tries showSaveFilePicker (Chromium) for user-chosen location,
 * falls back to Blob download. Signals Shiny on completion.
 */
(function() {
  'use strict';
  $(document).on('shiny:connected', function() {

    Shiny.addCustomMessageHandler('exportSettingsToJSON', function(message) {
      var filename = message.filename || 'supercrit_settings.json';
      if (!filename.endsWith('.json')) filename += '.json';
      var jsonStr = JSON.stringify(message.settings, null, 2);

      // Try modern File System Access API (Chromium: Chrome, Edge)
      if (window.showSaveFilePicker) {
        (async function() {
          try {
            var handle = await window.showSaveFilePicker({
              suggestedName: filename,
              types: [{
                description: 'JSON Files',
                accept: { 'application/json': ['.json'] }
              }]
            });
            var writable = await handle.createWritable();
            await writable.write(jsonStr);
            await writable.close();
            // Signal success to Shiny
            Shiny.setInputValue('settings_export_done', Math.random());
          } catch (e) {
            // User cancelled the dialog — no notification
            if (e.name !== 'AbortError') {
              console.error('[Settings] Save error:', e);
            }
          }
        })();
      } else {
        // Fallback: Blob download (no directory picker available)
        try {
          var blob = new Blob([jsonStr], { type: 'application/json' });
          var url = URL.createObjectURL(blob);
          var a = document.createElement('a');
          a.href = url;
          a.download = filename;
          document.body.appendChild(a);
          a.click();
          document.body.removeChild(a);
          URL.revokeObjectURL(url);
          // Signal success to Shiny
          Shiny.setInputValue('settings_export_done', Math.random());
        } catch (e) {
          console.error('[Settings] Download error:', e);
        }
      }
    });

  });
})();
