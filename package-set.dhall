let foundation  = https://github.com/dfinity/vessel-package-set/releases/download/mo-0.6.18-20220107/package-set.dhall sha256:af8b8dbe762468ce9b002fb0c62e65e1a3ee0d003e793c4404a16f8531a71b59

let departure = https://github.com/DepartureLabsIC/package-set/releases/download/v0.1.0/package-set.dhall

let Package =
    { name : Text, version : Text, repo : Text, dependencies : List Text }

let
  -- This is where you can add your own packages to the package-set
  additions =
    [] : List Package

let
  {- This is where you can override existing packages in the package-set

     For example, if you wanted to use version `v2.0.0` of the foo library:
     let overrides = [
         { name = "foo"
         , version = "v2.0.0"
         , repo = "https://github.com/bar/foo"
         , dependencies = [] : List Text
         }
     ]
  -}
  overrides =
    [] : List Package

in  foundation # departure # additions # overrides
