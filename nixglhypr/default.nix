pkgs:
with pkgs;
stdenv.mkDerivation {
  pname = "nixglhypr";
  version = "0.1.0";
  src = ./nixglhypr;
  buildCommand = ''
    mkdir -p $out/bin
    cp $src $out/bin/nixglhypr
    substituteInPlace $out/bin/nixglhypr \
      --replace "patchelf" \
      "${pkgs.patchelf}/bin/patchelf"
  '';
}
