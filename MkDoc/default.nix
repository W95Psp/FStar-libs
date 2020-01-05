let
  pkgs = import <nixpkgs> {};
  pm = pkgs.fstar-package-manager;
  mkDocPackage = (pm (import ./fstar-package.nix)).build;
in
mkDocPackage // {
  doc-builder = packages:
    let
      name = "doc-generator";#pkgs.lib.concatStringsSep "--" (map (p: p.module-name) packages);
      m = (pm
        { name = name;
          sources-directory = ./.;
          sources = [];
          ocaml-sources = [];
          dependencies = packages ++ [mkDocPackage];
          compile = [];
        })
      ;
      mname = "TheDoc";
    in
      pkgs.stdenv.mkDerivation {
        name = "doc" + name;

        buildInputs = m.wrapped-fstar.buildInputs;

        unpackPhase = ''true'';
        
        buildPhase = ''
        echo "module ${mname}" > ${mname}.fst
        
        ${pkgs.lib.concatStringsSep "\n"
          ( map
            (path: ''echo "open ${path}" >> ${mname}.fst'')
            (builtins.concatLists (map (m: m.sources) packages))
          )
        }
        echo "open FStar.Tactics" >> ${mname}.fst
        echo "open MkDoc" >> ${mname}.fst
        echo "let _: unit = _ by (" >> ${mname}.fst
        echo "  export_types_to_file None \"result.json\";" >> ${mname}.fst
        echo "  exact (\`())" >> ${mname}.fst
        echo ")" >> ${mname}.fst

        ${m.wrapped-fstar}/bin/fstar.exe --unsafe_tactic_exec ${mname}.fst
        '';
        installPhase = ''
        mkdir $out
        cp result.json $out/db.json
        '';
      }
      
  ;
}
