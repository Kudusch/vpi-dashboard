#!/bin/bash

curl -s  https://www-genesis.destatis.de/genesisWS/rest/2020/data/tablefile\?username\=DE426X1JA4\&password\=Qof4-sab3-fuy1-muq7\&name\=61111-0002\&area\=all\&format\=csv\&startyear\=1900\&endyear\=2100 > "vpi.csv"

split -p "1991;Januar" vpi.csv 1_
split -p "__________" 1_ab 2_

mv 2_aa vpi.csv

rm 1_* 2_*