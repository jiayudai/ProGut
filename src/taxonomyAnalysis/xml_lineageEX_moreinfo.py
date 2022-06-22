# pip install biopython
# pip install requests
# NOTES: Still need to think about id for "lineage is not available"
import requests
import xmltodict
import sys

# for literature work:
# input is lineageEX_xml_parse_results.csv
# xml_location is xml_files and
# output is lineageEX_xml_moreinfo.csv

# for thesis work:
# input is Thesis_lineageEX_xml_parse_results.csv
# xml_location is xml_files_Thesis and
# output is Thesis_lineageEX_xml_moreinfo.csv

input = sys.argv[1]
xml_location = sys.argv[2]
output = sys.argv[3]

file9 = open(output, "w")
file9.write('tax_id|scientific_name|phylum|class|order|family|genus|species groups|species|subspecies\n')

with open(input) as f9:
    lines = f9.readlines()
    for line in lines:
        line = line.strip()
        bactID = line.split('|')[0]
        print(bactID)

        with open(xml_location + "/" + bactID + ".xml") as f10:

            data = xmltodict.parse(f10.read())
            # print(type(data))

            if data['TaxaSet']:
                tax_id = data['TaxaSet']['Taxon']['TaxId']
                scientific_name = data['TaxaSet']['Taxon']['ScientificName']
                lineage = data['TaxaSet']['Taxon']['Lineage']
                lineageEX = data['TaxaSet']['Taxon']['LineageEx']
                lineageTaxon = lineageEX['Taxon']
                if 'TaxId' in lineageTaxon:
                    print("only one taxon")
                    # lineageTaxon Dictionary
                    print(lineageTaxon['ScientificName'])

                else:
                    #p = ""
                    p = "Unknown"
                    c = "Unknown"
                    o = "Unknown"
                    f = "Unknown"
                    g = "Unknown"
                    sg = "Unknown"
                    s = "Unknown"
                    ss = "Unknown"

                    for i in lineageTaxon:
                        rank = i['Rank']
                        name = i['ScientificName']


                        #if rank == "superkingdom" and name == "Bacteria":
                        if rank == "phylum":
                            p = name
                        elif rank == "class":
                            c = name
                        elif rank == "order":
                            o = name
                        elif rank == "family":
                            f = name
                        elif rank == "genus":
                            g = name
                        elif rank == "species group":
                            sg = name
                        elif rank == "species":
                            s = name
                        elif rank == "subspecies":
                            ss = name


                    #z_line = tax_id + '|' + scientific_name + '|' + "phylum" + '|' + p + '|' + "genus" + '|' + g
                    #z_line = tax_id + '|' + scientific_name + '|' + "phylum" + '|' + p + '|' + "class" + '|' + c + '|' + "order" + '|' + o + '|' + "family" + '|' + f + '|' + "genus" + '|' + g + '|' + "species groups" + '|' + sg + '|' + "species" + '|' + s + '|' + "subspecies" + '|' + ss
                    z_line = tax_id + '|' + scientific_name + '|' + p + '|' + c + '|' + o + '|' + f + '|' + g + '|' + sg + '|' + s + '|' + ss

                    print(z_line)
                    file9.write(z_line + "\n")







file9.close()

