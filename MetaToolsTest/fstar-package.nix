{nixpkgs ? import <nixpkgs> {}}:
{ name = "MetaToolsTest";
  force-fstar-version = nixpkgs.fstar-clemma-reflection-smtpat;
  sources-directory = ./.;
  sources = [
  ];
  ocaml-sources = [];
  dependencies =
    with (import ../.); [
      MetaTools
    ];
  compile = [];
}
