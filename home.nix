{ username, ... } @ options: { config, pkgs, isDarwin, ... }:
if isDarwin then
  {
    imports = [
      ./commons.nix
    ];
    home = {
      inherit username;
      packages = with pkgs;
        [
          coreutils-full
        ];
      homeDirectory = "/Users/${username}";
      stateVersion = "25.05";
    };
    programs.home-manager.enable = true;
  }
else
  {
    imports = [
      ./commons.nix
      (import ./de options)
    ];
    home = {
      inherit username;
      packages = [];
      homeDirectory = "/home/${username}";
      stateVersion = "25.05";
    };
    programs.home-manager.enable = true;
  }
