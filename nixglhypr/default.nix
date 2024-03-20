pkgs:
with pkgs;
stdenv.mkDerivation {
  pname = "nixglhypr";
  version = "0.1.0";
  src = ./nixglhypr;
  checkInputs = [
    gnused
    gnugrep
    findutils
    patchelfStable
  ];
  buildCommand = ''
    mkdir -p $out/bin
    cp $src $out/bin/nixglhypr
  '';
}
