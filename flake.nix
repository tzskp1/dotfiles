{
  description = "Home Manager configuration of tzskp1";

  inputs = {
    hyprland.url = "github:hyprwm/Hyprland";
    emacs.url = "github:nix-community/emacs-overlay";
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, hyprland, emacs, nixpkgs, home-manager }:
    let
      opts = import ./options.nix;
      home = import ./home.nix opts;
      pkgs = system: import nixpkgs {
        inherit system;
        overlays = builtins.attrValues hyprland.overlays ++ builtins.attrValues emacs.overlays;
        config = {
          allowUnfree = true;
        };
      };
      forAllSystems = nixpkgs.lib.genAttrs nixpkgs.lib.systems.flakeExposed;
      hmcfg = pkgs: home-manager.lib.homeManagerConfiguration {
        inherit pkgs;
        modules = [ home ];
      };
    in
    {
      formatter = forAllSystems (system: (pkgs system).nixpkgs-fmt);
      # packages.${system}.default = hmcfg.activationPackage;
      packages = forAllSystems (system: { default = (hmcfg (pkgs system)).activationPackage; });
    };
}
