# This program calls Kegg rest api to get all the pathway names
# Usage: python keggRestTable.py > keggPathway.csv
# last modified: 10/07/2021

import urllib.request, json
keggURL = "http://rest.kegg.jp/list/pathway/"
import re

print("Map,Pathway Name,Pathway Class")
with urllib.request.urlopen(keggURL) as url:
  x = url.read().decode("utf-8")
  for line in x.split("\n"):
    #path:map07234	Neurotransmitter transporter inhibitors
    terms = line.split('\t')
    if len(terms) > 1:
      mapID = terms[0].split(':')[1]
      mapURL = 'http://rest.kegg.jp/get/' + mapID
      classname = terms[1]
      with urllib.request.urlopen(mapURL) as urla:
        y = urla.read().decode("utf-8")
        #CLASS       Human Diseases; Infectious disease: bacterial
        m = re.search("(CLASS)\s+(.*)\n",y)
        if m is not None:
          classname = m.group(2)
        print(mapID+","+re.sub(",",";",terms[1])+","+re.sub(",",";",classname))
