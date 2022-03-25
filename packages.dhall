{-
Welcome to your new Dhall package-set!

Below are instructions for how to edit this file for most use
cases, so that you don't need to know Dhall to use it.

## Warning: Don't Move This Top-Level Comment!

Due to how `dhall format` currently works, this comment's
instructions cannot appear near corresponding sections below
because `dhall format` will delete the comment. However,
it will not delete a top-level comment like this one.

## Use Cases

Most will want to do one or both of these options:
1. Override/Patch a package's dependency
2. Add a package not already in the default package set

This file will continue to work whether you use one or both options.
Instructions for each option are explained below.

### Overriding/Patching a package

Purpose:
- Change a package's dependency to a newer/older release than the
    default package set's release
- Use your own modified version of some dependency that may
    include new API, changed API, removed API by
    using your custom git repo of the library rather than
    the package set's repo

Syntax:
where `entityName` is one of the following:
- dependencies
- repo
- version
-------------------------------
let upstream = --
in  upstream
  with packageName.entityName = "new value"
-------------------------------

Example:
-------------------------------
let upstream = --
in  upstream
  with halogen.version = "master"
  with halogen.repo = "https://example.com/path/to/git/repo.git"

  with halogen-vdom.version = "v4.0.0"
-------------------------------

### Additions

Purpose:
- Add packages that aren't already included in the default package set

Syntax:
where `<version>` is:
- a tag (i.e. "v4.0.0")
- a branch (i.e. "master")
- commit hash (i.e. "701f3e44aafb1a6459281714858fadf2c4c2a977")
-------------------------------
let upstream = --
in  upstream
  with new-package-name =
    { dependencies =
       [ "dependency1"
       , "dependency2"
       ]
    , repo =
       "https://example.com/path/to/git/repo.git"
    , version =
        "<version>"
    }
-------------------------------

Example:
-------------------------------
let upstream = --
in  upstream
  with benchotron =
      { dependencies =
          [ "arrays"
          , "exists"
          , "profunctor"
          , "strings"
          , "quickcheck"
          , "lcg"
          , "transformers"
          , "foldable-traversable"
          , "exceptions"
          , "node-fs"
          , "node-buffer"
          , "node-readline"
          , "datetime"
          , "now"
          ]
      , repo =
          "https://github.com/hdgarrood/purescript-benchotron.git"
      , version =
          "v7.0.0"
      }
-------------------------------
-}
let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.7-20220228/packages.dhall
        sha256:585403682c9378a55da644fb2edbc74d2592d18283bc9fa3457ad79398bb55bb

let overrides =
      { arraybuffer =
        { dependencies =
          [ "arraybuffer-types"
          , "arrays"
          , "effect"
          , "float32"
          , "functions"
          , "gen"
          , "maybe"
          , "nullable"
          , "prelude"
          , "tailrec"
          , "uint"
          , "unfoldable"
          ]
        , repo =
            "https://github.com/purescript-contrib/purescript-arraybuffer.git"
        , version = "v12.0.0"
        }
      }

let additions =
      { deku = {
        dependencies =  [ "arrays"
  , "behaviors"
  , "control"
  , "datetime"
  , "effect"
  , "either"
  , "event"
  , "foldable-traversable"
  , "foreign"
  , "foreign-object"
  , "indexed-monad"
  , "lists"
  , "maybe"
  , "newtype"
  , "nullable"
  , "ordered-collections"
  , "prelude"
  , "record"
  , "refs"
  , "simple-json"
  , "sized-vectors"
  , "transformers"
  , "tuples"
  , "typelevel"
  , "typelevel-peano"
  , "unordered-collections"
  , "unsafe-coerce"
  , "variant"
  , "web-dom"
  , "web-events"
  ], repo = "https://github.com/mikesol/purescript-deku.git"
        , version = "v0.0.2"
      },deku-toplevel = {
        dependencies =  [ "arrays"
  , "behaviors"
  , "control"
  , "datetime"
  , "effect"
  , "either"
  , "event"
  , "foldable-traversable"
  , "foreign"
  , "foreign-object"
  , "indexed-monad"
  , "lists"
  , "maybe"
  , "newtype"
  , "nullable"
  , "ordered-collections"
  , "prelude"
  , "record"
  , "refs"
  , "simple-json"
  , "sized-vectors"
  , "transformers"
  , "tuples"
  , "web-html"
  , "typelevel"
  , "typelevel-peano"
  , "unordered-collections"
  , "unsafe-coerce"
  , "variant"
  , "web-dom"
  , "web-events"
  ], repo = "https://github.com/mikesol/purescript-deku-toplevel.git"
        , version = "v0.0.2.0"
      }
      , event =
        { dependencies =
          [ "console"
          , "effect"
          , "filterable"
          , "nullable"
          , "unsafe-reference"
          , "js-timers"
          , "now"
          ]
        , repo = "https://github.com/mikesol/purescript-event.git"
        , version = "v1.4.2"
        }
      , behaviors =
        { dependencies =
          [ "psci-support"
          , "effect"
          , "ordered-collections"
          , "filterable"
          , "nullable"
          , "event"
          , "web-html"
          , "web-events"
          , "web-uievents"
          ]
        , repo = "https://github.com/mikesol/purescript-behaviors.git"
        , version = "v8.1.0"
        }
      }

in  upstream // overrides // additions
