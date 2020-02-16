{nixpkgs ? import <nixpkgs> {}}:
  { name = "Data.Serialize";
    sources-directory = ./.;
    sources = [
      "Data.Serialize.Decode"
      "Data.Serialize.Encode"
      "Data.Serialize"
      "Data.Serialize.Helpers"
      "Data.Serialize.Helpers.Serialized"
      "Data.Serialize.Helpers.Test"
      "Data.Serialize.Rep"
      "Data.Serialize.MakeNative"
      "Data.Serialize.Typeclasses"
      "Data.Serialize.Types"
    ];
    ocaml-sources = [];
    dependencies = [];
    compile = [];
  }
