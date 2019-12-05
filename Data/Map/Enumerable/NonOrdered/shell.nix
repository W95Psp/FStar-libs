with import <nixpkgs> {};
let parent = import ./.; in
stdenv.mkDerivation {
  name = parent.name + "env";
  buildInputs = parent.buildInputs;
}
