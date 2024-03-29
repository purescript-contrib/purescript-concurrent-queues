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
  , "transformers"
  , "tuples"
  , "unfoldable"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
