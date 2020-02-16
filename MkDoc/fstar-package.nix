{nixpkgs ? import <nixpkgs> {}}:
  { name = "MkDoc";
    sources-directory = ./.;
    sources = [
      "MkDoc"
    ];
    tactic-module = "MkDoc";
    ocaml-sources = [];
    dependencies = [
      (import (./..)).Data.JSON
    ];
    compile = [];
  }
