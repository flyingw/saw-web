"use strict";

exports.toLocaleTimeString_ = new Intl.DateTimeFormat('uk-UA', { hour: "2-digit", minute: "2-digit"}).format

exports.formatISO_ = date => new Intl.DateTimeFormat('uk-UA', { day: "2-digit", month: "2-digit", year: "numeric", hour: "2-digit", minute: "2-digit"}).formatToParts(date)

exports.nativeDate = () => {
  let test = document.createElement('input')
  test.type = 'datetime-local'
  return test.type === 'datetime-local'
}

exports.datepickerLoad = function() {
  if (!exports.nativeDate() && !window['ext']) {
    var el1 = document.createElement('script')
    el1.setAttribute('async', '')
    el1.setAttribute('src', 'ext.js')
    document.head.appendChild(el1)
    var el2 = document.createElement('link')
    el2.setAttribute('async', '')
    el2.setAttribute('href', 'ext.css')
    el2.setAttribute('rel', 'stylesheet')
    document.head.appendChild(el2)
  }
  return {}
}

exports.datepickerExtClass = function() {
  return window['ext']['DatePicker']
}

exports.uk = function() {
  return window['ext']['uk']
}

exports.ru = function() {
  return window['ext']['ru']
}
