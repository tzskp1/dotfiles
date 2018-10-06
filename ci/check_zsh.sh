#!/bin/bash
sudo login -f travis <<EOF
zsh -c "echo $PAGER" | grep "most"
EOF
