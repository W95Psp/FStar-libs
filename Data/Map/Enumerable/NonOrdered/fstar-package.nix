{nixpkgs ? import <nixpkgs> {}}:
  { name = "Data.Map.Enumerable.NonOrdered";
    sources-directory = ./.;
    sources = [
      "Data.Map.Enumerable.NonOrdered"
    ];
    ocaml-sources = [];
    dependencies =
      with (import ../../../..);
      [Data.Set.Computable.NonOrdered ToString DefaultValue];
    compile = [];
  }
