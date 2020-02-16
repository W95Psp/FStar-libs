let
  pkgs = import <nixpkgs> {};
in
{ name = "MetaToolsTest";
  force-fstar-version = pkgs.fstar-clemma-reflection-smtpat;
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
