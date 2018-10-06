#!/bin/bash
exec zsh -l <<EOF
zsh -c "echo $PAGER" | grep "most"
EOF
