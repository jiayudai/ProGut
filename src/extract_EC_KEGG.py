import requests
import csv

file8=open("tnumberECtable.csv","w")
with open("tnumber_reference.csv") as f9:
    read=csv.reader(f9, delimiter=',')
    next(read)
    for row in read:
        b_name = row[0]
        T_number = row[1]
        link = "http://rest.kegg.jp/link/ec/" + T_number

        page_link = requests.get(link)
        page_link = page_link.text
        #page_link = page_link.rstrip('\n')
        page_link = page_link.replace("\t", ",")

        page_link = page_link.replace("\n", "," + b_name + "\n")
        print(page_link)
        file8.write(page_link)






#http://rest.kegg.jp/link/ec/T00007
