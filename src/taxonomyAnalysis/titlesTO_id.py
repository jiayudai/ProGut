#print("hello")
# pip install biopython
#import requests
import csv
import urllib.parse
from Bio import Entrez

file8=open("metabolic_PMID.csv","w")
#with open("backup.csv") as f9:
with open("articles_457_titles.csv") as f9:
    lines = f9.readlines()
    for line in lines:
        line = line.strip()

        search_term = line
        print(search_term)

        #search_term = "Survival and Metabolic Activity of Pediocin Producer Pediococcus acidilactici UL5: Its Impact on Intestinal Microbiota and Listeria monocytogenes in a Model of the Human Terminal Ileum."
        Entrez.email = "daijiayu789@gmail.com"
        #handle = Entrez.efetch(db="pubmed", id=search_term, retmax="200", rettype="xml", retmode="text")
        handle = Entrez.esearch(db="pubmed", term=search_term)
        records = Entrez.read(handle)
        print(records)
        pubmed_id = ''
        #print(records['IdList'])
        #print(records.keys())
        #if records['IdList']== []:
        #    file_line = search_term
            #file8.write(file_line + "\n")
        #elif len(records['IdList']) > 1:
        #    file_line = search_term
            #file8.write(file_line + "\n")
        if len(records['IdList']) == 1:
            pidlist = records['IdList']
            print(pubmed_id)
            pubmed_id = pidlist[0]
            # file8.write(pubmed_id[0] + "\n")

        file_line = pubmed_id + "\t" + search_term
        file8.write(file_line + "\n")








file8.close()







        #b_name = row[0]
        #T_number = row[1]
        # link = "http://rest.kegg.jp/link/ec/" + T_number
        #
        # page_link = requests.get(link)
        # page_link = page_link.text
        # page_link = page_link.replace("\t", ",")
        #
        # page_link = page_link.replace("\n", "," + b_name + "\n")
        # print(page_link)
        # file8.write(page_link)