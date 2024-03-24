useNvidia: pkgs:
let
  src = if useNvidia then ./nixglhypr_nvidia else ./nixglhypr_mesa;
  mesa = builtins.head (builtins.filter (x: builtins.match "^mesa-.*" x.name != null) pkgs.hyprland.buildInputs);
  dri = pkgs.lib.makeSearchPathOutput "lib" "lib/dri" [ mesa.drivers ];
  glvnd = pkgs.lib.makeSearchPathOutput "share" "share/glvnd" [ mesa.drivers ];
  lib_ = pkgs.lib.makeSearchPathOutput "lib" "lib" [ mesa.drivers ];
      # cp $src/nixglhypr_nvidia $out/bin/nixglhypr
      # cp $src/nixglhypr_mesa $out/bin/nixglhypr
  build =
    if useNvidia then ''
      substituteInPlace $out/bin/nixglhypr \
        --replace "patchelf" "${pkgs.patchelf}/bin/patchelf"
    '' else ''
      substituteInPlace $out/bin/nixglhypr \
        --replace "DRI_VALUE" "${dri}" \
        --replace "GLVND_VALUE" "${glvnd}" \
        --replace "LIB_VALUE" "${lib_}"
    '';
in
with pkgs;
stdenv.mkDerivation {
  inherit src;
  pname = "nixglhypr";
  version = "0.1.0";
  buildCommand = ''
    mkdir -p $out/bin
    cp $src $out/bin/nixglhypr
    ${build}
  '';
}
