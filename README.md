# How to Install
```bash
    cd /path/to/dotfiles
    nix run
    # for swaylock
    echo 'auth include login' | sudo tee /etc/pam.d/swaylock
    # change login shell
    sudo -S chsh -s $(which zsh)
```
