{nixpkgs ? import <nixpkgs> {}}:
  { name = "Data.Set.Computable.NonOrdered";
    sources-directory = ./.;
    sources = [
      "Data.Set.Computable.NonOrdered"
      "Data.Set.Computable.NonOrdered.PartialOrder"
    ];
    ocaml-sources = [];
    dependencies =
      with (import ../../../..);
      [PartialOrder];
    compile = [];
  }
