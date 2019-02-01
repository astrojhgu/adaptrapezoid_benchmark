# default.nix
with import <nixpkgs> {};
stdenv.mkDerivation {
    name = "controller"; # Probably put a more meaningful name here
    buildInputs =  [gbenchmark ];
}
