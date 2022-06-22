# pip install biopython
# pip install requests
# NOTES: Still need to think about id for "lineage is not available"
import requests
import xmltodict
import time
import csv
from Bio import Entrez
import urllib.parse
# import webbrowser
import urllib.request
import json
import re
import numpy as np
import xml.etree.ElementTree as ET
import sys


# for literature work:
# input is unique_ncbi_taxid.csv
# xml_location is xml_files and
# output is lineageEX_xml_parse_results.csv

# for thesis work:
# input is myorganisms_taxid_unique.csv
# xml_location is xml_files_Thesis and
# output is Thesis_lineageEX_xml_parse_results.csv

input = sys.argv[1]
xml_location = sys.argv[2]
output = sys.argv[3]

file9 = open(output, "w")

with open(input) as f9:
    lines = f9.readlines()
    for line in lines:
        #line = "2"
        #line = "186826"

        #line = "1239"
        line = line.strip()
        #print(line)

        with open(xml_location + "/" + line + ".xml") as f10:

            data = xmltodict.parse(f10.read())
            # print(type(data))

            if data['TaxaSet']:
                tax_id = data['TaxaSet']['Taxon']['TaxId']
                scientific_name = data['TaxaSet']['Taxon']['ScientificName']
                lineage = data['TaxaSet']['Taxon']['Lineage']

                # if data['TaxaSet']['Taxon']['LineageEx']
                lineageEX = data['TaxaSet']['Taxon']['LineageEx']
                lineageTaxon = lineageEX['Taxon']
                if 'TaxId' in lineageTaxon:
                    print("only one taxon")
                    # lineageTaxon Dictionary
                    print(lineageTaxon['ScientificName'])

                else:
                    #1606 duplicated
                    #print("many taxons")
                    # lineageTaxon is a list
                    #for taxon in lineageTaxon:
                        # taxon is dictionary
                        #print(taxon['ScientificName'])
                    for i in lineageTaxon:
                        #print(len(i))
                        rank = i['Rank']
                        name = i['ScientificName']
                        #print(i)

                        if rank == "superkingdom" and name == "Bacteria":
                            # print(tax_id)
                            # print(scientific_name)
                            # print(lineageEX)
                            z_line = tax_id + '|' + scientific_name + '|' + lineage + '|' + name
                            print(z_line)
                            #print("hello")
                            file9.write(z_line + "\n")


file9.close()

# Test Example
# 29153882
# Need to extract the values for 'excact' and make them unique
