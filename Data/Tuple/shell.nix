{nixpkgs ? import <nixpkgs> {}}: nixpkgs.fstar-package-manager.shell (import ./fstar-package.nix {nixpkgs = nixpkgs;})
