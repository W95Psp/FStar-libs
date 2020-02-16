{nixpkgs ? import <nixpkgs> {}}: nixpkgs.fstar-package-manager.build (import ./fstar-package.nix {nixpkgs = nixpkgs;})
