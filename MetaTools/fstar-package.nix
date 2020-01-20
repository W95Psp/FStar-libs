{ name = "MetaTools";
  sources-directory = ./.;
  sources = [
    "MetaTools.BrowseTerm"
    "MetaTools.Compiled"
    "MetaTools.Util"
  ];
  tactic-module = "MetaTools.Compiled";
  ocaml-sources = [];
  dependencies =
    with (import ../.); [
      Control
      Data.Serialize
    ];
  compile = [];
}
