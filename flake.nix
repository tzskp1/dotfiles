{
  description = "Home Manager configuration of tzskp1";

  inputs = {
    emacs.url = "github:nix-community/emacs-overlay";
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, emacs, nixpkgs, home-manager }:
    let
      options = import ./options.nix;
      home = import ./home.nix options;
      eols = builtins.attrValues emacs.overlays;
      pkgs = system: import nixpkgs {
        inherit system;
        overlays = eols;
        config = {
          allowUnfree = true;
        };
      };
      forAllSystems = nixpkgs.lib.genAttrs nixpkgs.lib.systems.flakeExposed;
      hmcfg = pkgs: home-manager.lib.homeManagerConfiguration {
        inherit pkgs;
        extraSpecialArgs = {
          isDarwin = pkgs.stdenv.hostPlatform.isDarwin;
        };
        modules = [ home ];
      };
    in
    {
      formatter = forAllSystems (system: (pkgs system).nixpkgs-fmt);
      packages = forAllSystems (system: { default = (hmcfg (pkgs system)).activationPackage; });
    };
}
