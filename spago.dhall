{ sources = [ "./src/**/*.purs" ]
, name = "deku-starter"
, dependencies =
  [ "aff"
  , "arrays"
  , "control"
  , "deku"
  , "effect"
  , "filterable"
  , "foldable-traversable"
  , "foreign"
  , "foreign-object"
  , "hyrule"
  , "hyrule-paraglider"
  , "maybe"
  , "prelude"
  , "record"
  , "st"
  , "tuples"
  , "unsafe-coerce"
  , "variant"
  , "web-html"
  ]
, packages = ./packages.dhall
}
