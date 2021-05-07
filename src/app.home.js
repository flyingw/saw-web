"use strict";

exports.telegramLoginWidget = function(id) {
  return function(f) {
    return function() {
      window["telegramLoginF"] = f
      var element = document.createElement('script');
      element.setAttribute('async', '');
      element.setAttribute('src', 'https://telegram.org/js/telegram-widget.js?9');
      element.setAttribute('data-telegram-login', 'RideHubCityBot');
      element.setAttribute('data-size', 'large');
      element.setAttribute('data-onauth', 'telegramLoginF(user)()');
      element.setAttribute('data-request-access', 'write');

      var el = document.getElementById(id)
      if (el) {
        el.appendChild(element);
      }
      return{}
    }
  }
}
