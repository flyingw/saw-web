"use strict";

exports.createMap = function(id) {
  return function() {
    console.log("invoke")
    if (!document.maps) {
      console.log("invoke2")
      console.log(document.getElementById(id))
      document.maps = new google.maps.Map(document.getElementById(id), {
        center: { lat: -34.397, lng: 150.644 }, 
        zoom: 8,
      });
    }

    return document.maps
  }
}


exports.getLocation = success => error => {
  return function() {
    if(!navigator.geolocation) {
      return error();
    } else {
      navigator.geolocation.getCurrentPosition(
        pos => {
          success(pos.coords.latitude)(pos.coords.longitude)()
        }, function() {
          error()
        }
      );
    }
  }
}