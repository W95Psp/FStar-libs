{nixpkgs ? import <nixpkgs> {}}:
  { name = "Data.JSON";
    sources-directory = ./.;
    sources = [
      "Data.JSON"
      "Data.JSON.Parser"
      "Data.JSON.Stringify"
      "Data.JSON.Types"
    ];
    ocaml-sources = [];
    dependencies =
      with (import ./../..);
      [ ToString
        Data.Serialize
        (import (fetchTarball https://github.com/W95Psp/StarCombinator/archive/57e9ee99eb63fa5c4b61403a7fad8ba9b9cd0063.tar.gz))
      ];
    tactic-module = "Data.JSON";
    compile = [];
  }
