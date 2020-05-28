{ name = "web"
, dependencies =
  [ "affjax"
  , "console"
  , "effect"
  , "js-timers"
  , "numbers"
  , "protobuf"
  , "psci-support"
  , "react-dom"
  , "web-dom"
  , "web-html"
  , "web-socket"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
