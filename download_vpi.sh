#!/usr/bin/env python3
# -*- coding: utf-8 -*-

MAXSIZE=5000

curl -s "https://www-genesis.destatis.de/genesisWS/rest/2020/data/tablefile?username=DE426X1JA4&password=Qof4-sab3-fuy1-muq7&name=61111-0002&area=all&format=ffcsv&startyear=1900&endyear=2100" > vpi_tmp.csv

FILESIZE=$(stat -f %z vpi_tmp.csv)

if (( FILESIZE > MAXSIZE )); then
  mv vpi_tmp.csv Data/vpi.csv
else
  echo "Nothing new"
  rm vpi_tmp.csv
fi  
