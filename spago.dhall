{ name = "concurrent-queues"
, dependencies =
  [ "aff", "assert", "avar", "console", "effect", "psci-support" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
