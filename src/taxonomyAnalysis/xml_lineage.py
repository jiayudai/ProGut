#pip install biopython
#pip install requests
# NOTES: Still need to think about id for "lineage is not available"
# Taxid: 147810=1606
import requests
import xmltodict
import time
import urllib.parse
import urllib.request
import json
import sys

# input file for literature work is unique_ncbi_taxid.csv
# input file for thesis work is myorganisms_taxid.csv

# output is xml_files for literature work
# output is xml_files_Thesis for thesis work
input = sys.argv[1]
output = sys.argv[2]

#file9=open("uri.csv","w")
with open(input) as f9:
    lines = f9.readlines()
    for line in lines:
        line = line.strip()
        # Get lineage
        link = "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=taxonomy&id=" + line
        response = requests.get(link)
        print("***********"+line)
        print(response.content)
        with open(output + "/" + line +".xml", 'wb') as f:
            f.write(response.content)
        time.sleep(3)


# Save the | for the lineage


#file9.close()

