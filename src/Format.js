"use strict";

exports.toLocaleDateString = function(date) {
  return function () {
    return date.toLocaleDateString();
  };
};

exports.toLocaleTimeString = function(date) {
  return function () {
    return date.toLocaleTimeString([], {hour: '2-digit', minute:'2-digit'});
  };
};
