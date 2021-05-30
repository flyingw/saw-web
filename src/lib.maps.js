"use strict";

exports.createMapImpl = id => settings => () => {
  var el = document.getElementById(id)
  var map = new google.maps.Map(el, {
    center: settings.center,
    zoom: settings.zoom,
    streetViewControl: false,
    mapTypeControl: false,
    fullscreenControl: false
  })
  return map
}

exports.setCenterImpl = map => location => () => {
  map.setCenter(location)
  return {}
}

exports.fitBoundsImpl = map => bounds => () => {
  var b = new google.maps.LatLngBounds(bounds.southwest, bounds.northeast)
  map.fitBounds(b)
  return {}
}

exports.createPolylineImpl = settings => () => {
  var polyline = new google.maps.Polyline(settings)
  return polyline
}

exports.attachPolylineImpl = polyline => map => () => {
  polyline.setMap(map)
  return {}
}

exports.detachPolylineImpl = polyline => () => {
  polyline.setMap(null)
  return {}
}

exports.getLocation = success => error => () => {
  if (!navigator.geolocation) {
    error()
  } else {
    navigator.geolocation.getCurrentPosition(
      pos => {
        success(pos.coords.latitude)(pos.coords.longitude)()
      }, 
      () => {
        error()
      }
    )
  }
  return {}
}