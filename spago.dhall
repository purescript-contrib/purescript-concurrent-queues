{ name = "concurrent-queues"
, dependencies =
  [ "aff"
  , "arrays"
  , "assert"
  , "avar"
  , "console"
  , "control"
  , "effect"
  , "either"
  , "integers"
  , "maybe"
  , "partial"
  , "prelude"
  , "psci-support"
  , "transformers"
  , "tuples"
  , "unfoldable"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
