function require(name) {
  if (name === "react") return window.React
  else if (name === "react-dom") return window.ReactDOM
  else if (name === "react-dom/server") return { renderToString: null, renderToStaticMarkup: null }
  else console.error("Unknown require=" + name)
}
