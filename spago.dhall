{ name = "concurrent-queues"
, dependencies =
  [ "aff", "avar", "console", "effect", "psci-support", "test-unit" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
