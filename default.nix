  let
    pkgs = import <nixpkgs> {};
  in
  { stdenv ? pkgs.stdenv }:
  
  stdenv.mkDerivation {
    name = "thinkgear";
    version = "0.0.0.0";
    src = ./.;
    buildInputs = with pkgs; [ 
      bluez fftw
    ];
  }
