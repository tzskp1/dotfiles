#!/bin/bash
cat greeks.csv | awk -v q=\' -F ',' '{print "CODE="q$1q";KEY="q$2q";cat template|sed \"s/KEY/$KEY/;s/CODE/$CODE/\" > $KEY" }' > script.sh
bash ./script.sh

