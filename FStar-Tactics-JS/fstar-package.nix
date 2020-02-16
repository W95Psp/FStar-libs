{nixpkgs ? import <nixpkgs> {}}:
  { name = "FStar-Tactics-JS";
    sources-directory = ./.;
    sources = [
      "FStar.Tactics.JavaScript.Core"
      "FStar.Tactics.JavaScript"
      "FStar.Tactics.JavaScript.Helpers"
      "FStar.Tactics.JavaScript.Natives"
      "FStar.Tactics.JavaScript.VirtualDOM"
      "FStar.Tactics.JavaScript.HTML"
    ];
    tactic-module = "FStar.Tactics.JavaScript";
    ocaml-sources = [];
    dependencies =
      with (import ../.);
      [
        Data.JSON
      ];
    compile = [];
  }
