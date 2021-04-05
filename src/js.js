"use strict"

exports._encodeURI = (fail, succ, s) => {
  try {
    return succ(encodeURI(s))
  } catch (e) {
    return fail(e.message)
  }
}
