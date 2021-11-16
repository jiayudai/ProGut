# This is the final version of the GBFF Files Batch !!!!! Please ignore other versions
import requests
import csv
from Bio import SeqIO
import sys

input_file = sys.argv[1]
output_file = sys.argv[2]
gbk_dir = sys.argv[3]

print("input_file:", input_file)
print("output_file is:", output_file)
print("GBK directory is:", gbk_dir)


file6=open(output_file,"w")

with open(input_file) as f1:
     read=csv.reader(f1, delimiter=',')
     next(read)
     for row in read:
         input = gbk_dir +  '/' + row[1]
         bacteria_name = row[0]
         print(bacteria_name)
         for record in SeqIO.parse(input, "genbank"):
             print("record id:" + record.id + "\n")
             print("description:" + record.description + "\n")
             for feat in record.features:
                 if (feat.type == "CDS"):
                     # print(feat.type)
                     # print(feat.location)
                     # print(feat.qualifiers)
                     qualifier = feat.qualifiers
                     if 'EC_number' in qualifier.keys():
                         #ec = ';'.join(qualifier['EC_number'])
                         ec = 'ec:'+' '.join(qualifier['EC_number'])
                         if 'gene' in qualifier.keys():
                             print(qualifier['gene'][0] + "," + ec)
                             # file6.write(qualifier['gene'][0] + "," + ec + "\n")
                             file6.write(qualifier['gene'][0] + "," + ec + ",")
                             file6.write(bacteria_name + "\n")


                         else:
                             print("No_gene_name" + "," + ec)
                             # file6.write("No_gene_name" + "," + ec + "\n")

                             file6.write("No_gene_name" + "," + ec + ",")
                             file6.write(bacteria_name + "\n")

#http://rest.kegg.jp/link/pathway/ec:2.7.1.26 2.7.7.2
#http://rest.kegg.jp/link/pathway/ec:4.1.1.36 6.3.2.5
# https://github.com/azavea/django-queryset-csv/issues/71
