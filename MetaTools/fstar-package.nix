{nixpkgs ? import <nixpkgs> {}}: 
{ name = "MetaTools";
  force-fstar-version = nixpkgs.fstar-clemma-reflection-smtpat;
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
