$(document).ready(function() {
      $('input[name=\"paymentMethods\"]:checked').each(function() {
        $(this).parent().addClass('active');
      });
      $('input[name=\"disabledParking\"]:checked').each(function() {
        $(this).parent().addClass('active');
      });
      $('input[name=\"parkingType\"]:checked').each(function() {
        $(this).parent().addClass('active');
      });

      $('body').on('change', 'input[name=\"paymentMethods\"]', function() {
        $('input[name=\"paymentMethods\"]').each(function() {
          if ($(this).is(':checked')) {
            $(this).parent().addClass('active');
          } else {
            $(this).parent().removeClass('active');
          }
        });
      });

      $('body').on('change', 'input[name=\"disabledParking\"]', function() {
        $('input[name=\"disabledParking\"]').each(function() {
          if ($(this).is(':checked')) {
            $(this).parent().addClass('active');
          } else {
            $(this).parent().removeClass('active');
          }
        });
      });

      $('body').on('change', 'input[name=\"parkingType\"]', function() {
        $('input[name=\"parkingType\"]').each(function() {
          if ($(this).is(':checked')) {
            $(this).parent().addClass('active');
          } else {
            $(this).parent().removeClass('active');
          }
        });
      });
    });
    
    Shiny.addCustomMessageHandler('openMapPopup', function(message) {
      var markerId = message.id;
      var marker = window.leafletMapMarkers[markerId];
      if (marker) {
        marker.openPopup();
        window.map.setView(marker.getLatLng(), window.map.getZoom());
      }
    });