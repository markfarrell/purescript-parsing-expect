{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "parsing-expect"
, dependencies = [ "console", "effect", "parsing", "prelude", "psci-support" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
