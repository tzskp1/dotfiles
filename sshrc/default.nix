# TODO: Not tested
pkgs:
with pkgs;
stdenv.mkDerivation {
  pname = "sshrc";
  version = "0.1.0";
  src = ./sshrc;
  checkInputs = [
    openssl
  ];
  buildCommand = ''
    mkdir -p $out/bin
    cp $src $out/bin/sshrc
  '';
}
