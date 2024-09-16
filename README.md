# How to Install
```bash
    cd /path/to/dotfiles
    nix run
    # for hyprlock
    echo 'auth include login' | sudo tee /etc/pam.d/hyprlock
    # change login shell
    sudo -S chsh -s $(which zsh)
```
