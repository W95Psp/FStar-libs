{ name = "NormTools";
  sources-directory = ./.;
  sources = [
    "NormTools"
  ];
  ocaml-sources = [];
  dependencies = 
    with (import ../.); [
      Data.Serialize
    ];
  tactic-module = "NormTools";
  compile = [];
}
