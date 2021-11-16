import requests
import csv
import sys

input = sys.argv[1]
output = sys.argv[2]

# Reference_Representative Part (Third Group)
file8=open(output,"w")
#with open("Data/GBFF_EC_no_Test.csv") as f9:
with open(input) as f9:
    read=csv.reader(f9, delimiter=',')
    b_name = None
    for row in read:
        enzyme_code_new = row[1]
        b_name = row[2]
        #print(enzyme_code_new)
        link = "http://rest.kegg.jp/link/pathway/" + enzyme_code_new
        page_link = requests.get(link)
        page_link = page_link.text
        if(len(page_link) > 1):
            page_link = page_link.replace("\t", ",")
            page_link = page_link.replace("\n", "," + b_name + "\n")
            #page_link = page_link.rstrip('\n')
            print(page_link)
            file8.write(page_link)

    #file8.write("," + b_name + "\n")
