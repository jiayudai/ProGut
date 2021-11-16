######################## Code for obtaining the metabolic index for each bacteria for the given pathway list######################
##### Required Packages ##########
import csv

########## Verify input files for ec list match in both files ###########
def returnNotMatches(a, b):
    return [[x for x in a if x not in b], [x for x in b if x not in a]]

Bact = [] # Unique list of Bacteria
Bact_ec = [] # unique list of ec from Bact_EC file
Path_ec = [] # unique list of ec from EC_Pathway file
Path = [] # unique list of pathways

with open("Input_files/Bact_EC.csv") as csv1:   # append bact names and ec to the lists
    read = csv.reader(csv1, delimiter = ",")
    next(read) # skip the header
    for row in read:
        bact = row[0]
        ec = row[1]
        if bact not in Bact:
            Bact.append(bact)
        if ec not in Bact_ec:
            Bact_ec.append(ec)
        
with open("Input_files/EC_Pathway.csv") as csv1:   # append path names and ec to the lists
    read = csv.reader(csv1, delimiter = ",")
    next(read) # skip the header
    for row in read:
        ec = row[0]
        path = row[1]
        if path not in Path:
            Path.append(path)
        if ec not in Path_ec:
            Path_ec.append(ec)

######### Check if both input files have exactly same list of ec number #########

if Bact_ec == Path_ec:
    print("The dataset passed validation. Proceed to next step!!!\n")
    
    print("Total number of entities analyzed\n")
    print("Total no. of bacteria: ",len(Bact))
    print("Total no. of pathways: ",len(Path))
    print("Total no. of enzymes: ",len(Bact_ec))
else:
    print("The ec list does not match in both input files!!!\n")
    print("Unmatched ietms in both the lists are:")
    print(returnNotMatches(Bact_ec, Path_ec))


############ Ranking based on Presence or Absence of bacterial enzymes in pathway ##################
import pandas as pd
from openpyxl import Workbook
import numpy as np

############ CREATING DATAFRAMES FOR INPUT FILES ##################
############ 1. Bact Gene dataframe ###############
df1 = pd.read_csv("Input_files/Bact_EC.csv")
df1.fillna(0)
MatrixA_df = df1.pivot(index="Bacterium", columns="Gene", values="Value")
MatrixA_df = MatrixA_df.fillna(0)
#############################################
# print(MatrixA_df)
MatrixA_df.to_excel("Input_files/MatrixA_BactGene.xlsx")


############ 2. Gene Pathway dataframe ###############
df2 = pd.read_csv("Input_files/EC_Pathway.csv")
df2.fillna(0)
MatrixB_df = df2.pivot(index="Gene", columns="Pathway", values="Value")
MatrixB_df = MatrixB_df.fillna(0)
############################################
# print(MatrixB_df)
MatrixB_df.to_excel("Input_files/MatrixB_GenePath.xlsx")

##### MAtrix Dot Product #######################
Matrix_AB_multi_df = MatrixA_df.dot(MatrixB_df)
# print(Matrix_AB_multi_df)
Matrix_AB_multi_df.to_excel("Output_files/Matrix_AB_BactPath.xlsx")

# # ############ Presence/absence RANK #####################
f1 = open("Output_files/Bact_Pathway_Pres_Abs_Ranking.csv","w")
f1.close()
f1 = open("Output_files/Bact_Pathway_Pres_Abs_Ranking.csv","a+")
f1.write("Bacterium"+","+"Pathway"+","+"Value"+"\n")

df = Matrix_AB_multi_df
name = ''
index, cols = np.where(np.triu(df, 1) >= 0.0)
res = [(df.index[i], df.columns[j], df.iloc[i, j]) for i, j in zip(index, cols)]
for i in range(len(res)):
    value= float(res[i][2])
    if value > 0.0:
        # print(res[i][0],res[i][1],res[i][2])
        bact=res[i][0]
        map=res[i][1]
        for item in Path:
            if item == map:
                name = item
        value=str(res[i][2])
        print(bact,map,name,value)
        f1.write(bact+","+name+","+value+"\n")
f1.close()

############ Ranking based fuzzy membership function ##################

######################## dataframe using Fuzzy membership function##################333
new_sj = MatrixA_df.sum(axis=0)
#print(len(new_sj==0))
m = MatrixA_df.shape[0]
#print(bact_df)
copy_df = MatrixA_df.copy()
for i in range(0,MatrixA_df.shape[1]):
    sj = new_sj[i]
    #print(sj)
    if sj == 0:
        #print(columns[i]+ "\t" + str(i))
        copy_df.iloc[:,i] = 0
    elif (sj > 0) and (sj < m):
        value = ((m-sj)+1)/m
        copy_df.iloc[:,i] = value * MatrixA_df.iloc[:,i]
    elif sj == m:
        value = 1/m
        copy_df.iloc[:,i] = value * MatrixA_df.iloc[:,i]
# print(copy_df)
copy_df.to_excel("Input_files/MatrixF_FuzzyBactGene.xlsx")

##### MAtrix Dot Product #######################
fuzzy_df = copy_df.dot(MatrixB_df)
print(fuzzy_df)
fuzzy_df.to_excel("Output_files/Matrix_FB_Multi_FuzzyBactPath.xlsx")

############ FUZZY RANK #####################

f2=open("Output_files/Bact_Pathway_Fuzzy_Ranking.csv","w")
f2.close()
f2=open("Output_files/Bact_Pathway_Fuzzy_Ranking.csv","a+")
f2.write("Bacterium"+","+"Pathway"+","+"Value"+"\n")

df = fuzzy_df
name = ''
index, cols = np.where(np.triu(df, 1) >= 0.0)
res = [(df.index[i], df.columns[j], df.iloc[i, j]) for i, j in zip(index, cols)]
for i in range(len(res)):
    value= float(res[i][2])
    if value > 0.0:
        # print(res[i][0],res[i][1],res[i][2])
        bact=res[i][0]
        map=res[i][1]
        for item in Path:
            if item == map:
                name = item
        value=str(round(res[i][2],2))
        print(bact,map,name,value)
        f2.write(bact+","+name+","+value+"\n")
f2.close()

