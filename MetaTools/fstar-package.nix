let
  pkgs = import <nixpkgs> {};
in
{ name = "MetaTools";
  force-fstar-version = pkgs.fstar-clemma-reflection-smtpat;
  sources-directory = ./.;
  sources = [
    "MetaTools.BrowseTerm"
    "MetaTools.Compiled"
    "MetaTools.PatchTerm"
    "MetaTools.Env"
    "MetaTools.NamesOfTerm"
    "MetaTools.Util"
  ];
  tactic-module = "MetaTools.Compiled";
  ocaml-sources = [];
  dependencies =
    with (import ../.); [
      Control
      # Data.Serialize
    ];
  compile = [];
}
