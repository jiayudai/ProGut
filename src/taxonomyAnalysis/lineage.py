#pip install biopython
#pip install requests
# NOTES: Still need to think about id for "lineage is not available"!!!!!!!!!!!!!
# In lineage_list: MED:24188369	33277	55824(needs to be changed)	no lineage available
import requests
import xmltodict
import time
import csv
from Bio import Entrez
import urllib.parse
#import webbrowser
import urllib.request
import json
import re
import numpy as np
import xml.etree.ElementTree as ET

file9=open("uri.csv","w")
all_taxids = []
#Mannual_metabolic_PMIDs_only

#with open("test_pubid.csv") as f9:
#with open("All_Pubmed_ids.csv") as f9:
with open("Mannual_metabolic_PMIDs_only.csv") as f9:

    lines = f9.readlines()
    for line in lines:
        # This line is useful
        line = line.strip()
        words = line.split(',')
        pid = words[0]
        pid = pid.replace('﻿', '')


        #line = "10540745"
        #line = "30004273"
        #line = "29153882"
        #line = "24188369"
        print(pid)

        #line = "32036930" #no annotation available
        search_term = "MED:" + pid
        search_term = search_term.strip()
        #print(search_term)
        search_term_url = urllib.parse.quote(search_term)
        #print(search_term_url)

        link = "https://www.ebi.ac.uk/europepmc/annotations_api/annotationsByArticleIds?articleIds=" + search_term_url + '&type=Organisms'
        #print(link)

        u=urllib.request.urlopen(link)
        json_object = u.read()
        #print(json_object)


        # Convert JSON format to dictionary
        r = json.loads(json_object.decode())
        #print(r)
        #print(len(r))
        #print(r[0]['annotations'])

        #print(r[0]['annotations'].keys())

        uri_unique_save = 'no annotation available'
        uri=[]
        # Extract the data from the dictionary
        # 34023682, 30583046 - repeated
        if len(r)!=0 and len(r[0]['annotations']) != 0:
            #print('yes')
            test = list((object['annotations'] for object in r))
            #print(test)
            #print(type(test))
            #print(test[0])

            #'[]':list
            #'{}':dictionary

            test_tag = list((object2['tags'] for object2 in test[0]))
            #print(test_tag)
            #print(len(test_tag))
            for i in range(len(test_tag)):
                z = test_tag[i][0]['uri']
                z_id = z.split("taxonomy/", 1)[1]
                print(z_id) # need to save this id in the end



                if z_id not in uri:
                    uri.append(z_id)
                    print(uri)


            for z_id in uri:
                # Get lineage
                link = "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=taxonomy&id=" + z_id
                response = requests.get(link)
                print("***********"+z_id)
                #print(response.content)
                #with open(z_id+".xml", 'w') as f:
                    #f.write(response.content)
                try:
                    data = xmltodict.parse(response.content)
                except xmltodict.expat.ExpatError:
                    print(response.content)
                    print("*****EXCEPTION  CAUGHT******")
                    time.sleep(15)
                    data = xmltodict.parse(response.content)

                #print(type(data))

                if data['TaxaSet']:
                    tax_id = data['TaxaSet']['Taxon']['TaxId']
                    lineage = data['TaxaSet']['Taxon']['Lineage']
                    print(tax_id)
                    print(lineage)
                    time.sleep(3)
                    z_line = search_term + '|' + z_id + '|' + tax_id + '|' + lineage
                    print(z_line)
                    if tax_id not in all_taxids:
                        all_taxids.append(tax_id)
                else:
                    # For future Note: tax_id needs to be changed!!!!!!!!!!!!!!!!!!!!
                    #z_line = search_term + '|' + z_id + '|' + tax_id + '|' + "no lineage available"
                    z_line = search_term + '|' + z_id + '|' + '' + '|' + "no lineage available"
                file9.write(z_line + "\n")
file9.close()

with open('unique_ncbi_taxid.csv','w') as fh:
    fh.write('\n'.join(all_taxids))


# Test Example
# 29153882
# Need to extract the values for 'excact' and make them unique