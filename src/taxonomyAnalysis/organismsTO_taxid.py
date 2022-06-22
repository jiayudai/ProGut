#print("hello")
# pip install biopython
#import requests
import csv
import urllib.parse
from Bio import Entrez


all_taxids = []
file8=open("myorganisms_taxid.csv","w")
#with open("backup.csv") as f9:
with open("my_organisms_gs_unique.csv") as f9:
    lines = f9.readlines()
    for line in lines:
        line = line.strip()

        search_term = line
        print(search_term)

        #search_term = "Acinetobacter calcoaceticus"
        Entrez.email = "daijiayu789@gmail.com"
        handle = Entrez.esearch(db="taxonomy", term=search_term)
        records = Entrez.read(handle)
        print(records)
        tax_id = ''

        if len(records['IdList']) == 1:
            pidlist = records['IdList']
            print(tax_id)
            tax_id = pidlist[0]
            if tax_id not in all_taxids:
                all_taxids.append(tax_id)

        file_line = tax_id + "\t" + search_term
        file8.write(file_line + "\n")
file8.close()


with open("myorganisms_taxid_unique.csv",'w') as fh:
    fh.write('\n'.join(all_taxids))



