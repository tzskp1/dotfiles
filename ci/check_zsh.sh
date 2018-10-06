#!/bin/bash
exec zsh -l <<EOF 
echo $PAGER | grep most
EOF
