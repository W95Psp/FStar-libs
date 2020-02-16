{nixpkgs ? import <nixpkgs> {}}:
{ name = "PartialOrder";
  sources-directory = ./.;
  sources = [
    "PartialOrder"
  ];
  ocaml-sources = [];
  dependencies = [];
  compile = [];
}
