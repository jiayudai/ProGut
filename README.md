---
title: "ProGut Project Readme"
output:
  html_document:
    toc: TRUE
    toc_depth: 2
    toc_float: TRUE
editor_options: 
  chunk_output_type: inline
---

# Set markdown settings

```{r}
knitr::opts_chunk$set(echo = TRUE)
Sys.setenv(PYTHON="/Users/daijiayu/miniconda3/bin/python")
```

# Set the Enviroment to Allow Comments in zsh Commands

```{zsh}
setopt interactivecomments
```

# Overview

* Step1 to Step7 are for data collection.
* Step8 and Step9 are for data cleaning.
* Step10 to Step15 are for genomic enzymes and functional characterization.
* Step16 is for data exploration.
* Step17 to Step19 are for data visualization and data validation.
* Step20 to Step22 are for genus and species comparison.
* Step24 is for data visualization (R shiny app).

# 1. Extract EC numbers for Primary Group

## 1.1 Function

Extract EC numbers for bacteria in primary group.

## 1.2 Input file(s) for usage example

Two kinds of inputs are needed to complete Step1:

**Input1: primaryGBKtable.csv.** The table records the names of GenBank genome files associated with the bacteria in primary group.
```{zsh}
head primaryGBKtable.csv
```

**Input2: primaryGBK_files.** The GenBank genome files contain the gene (enzyme) information for bacteria in primary group.

## 1.3 Usage

```{zsh}
$PYTHON extract_EC_GBK.py primaryGBKtable.csv primaryECtable.csv primaryGBK_files
```

## 1.4 Output file(s) for usuage example

One output file will be generated from Step1:

**Output1: primaryECtable.csv.** The table contains three kinds of information for primary group, including the gene names, EC numbers, and bacteria names.
```{zsh}
head primaryECtable.csv
```

# 2. Extract EC numbers for Secondary Group

## 2.1 Function

Extract EC numbers for bacteria in secondary group.

## 2.2 Input file(s) for usage example

Two kinds of inputs are needed to complete Step2:

**Input1: secondaryGBKtable.csv.** The table records the names of GenBank genome files associated with the bacteria in secondary group.
```{zsh}
head secondaryGBKtable.csv
```

**Input2: secondaryGBK_files.** The GenBank genome files contain the gene (enzyme) information for bacteria in secondary group.

## 2.3 Usage

```{zsh}
$PYTHON extract_EC_GBK.py secondaryGBKtable.csv secondaryECtable.csv secondaryGBK_files
```

## 2.4 Output file(s) for usuage example

One output file will be generated from Step2:

**Output1: secondaryECtable.csv.** The table contains three kinds of information for secondary group, including the gene names, EC numbers, and bacteria names.
```{zsh}
head secondaryECtable.csv
```

# 3. Extract EC numbers for T number Group

## 3.1 Function

Extract EC numbers for bacteria in T number group.

## 3.2 Input file(s) for usage example

One input is needed to complete Step3 but it has been included in the codes:

**Input1:tnumber_reference.csv.** The table records T numbers associated with the bacteria in T number group.
```{zsh}
head tnumber_reference.csv
```

## 3.3 Usage

```{zsh}
$PYTHON extract_EC_KEGG.py
```

## 3.4 Output file(s) for usuage example

One output file will be generated from Step3:

**Output1: tnumberECtable.csv.** The table contains three kinds of information for T number group, including the gene names, EC numbers, and bacteria names.
```{zsh}
head tnumberECtable.csv
```

# 4. Extract Pathways for Primary Group

## 4.1 Function

Extract pathways for bacteria in primary group based on the EC numbers obtained from Output1 of Step1.

## 4.2 Input file(s) for usage example

One input is needed to complete Step4:

**Input1: primaryECtable.csv.** The table contains three kinds of information for primary group, including the gene names, EC numbers, and bacteria names and is obtained from Output1 of Step1.
```{zsh}
head primaryGBKtable.csv
```

## 4.3 Usage

```{zsh}
$PYTHON ec2pathway.py primaryECtable.csv primary_pathway.csv
```

## 4.4 Output file(s) for usuage example

One output file will be generated from Step4:

**Output1: primary_pathway.csv.** The table contains three kinds of information for primary group, including the EC numbers, pathways, and bacteria names.
```{zsh}
head primary_pathway.csv
```

# 5. Extract Pathways for Secondary Group

## 5.1 Function

Extract pathways for bacteria in secondary group based on the EC numbers obtained from Output1 of Step2.

## 5.2 Input file(s) for usage example

One input is needed to complete Step5:

**Input1: secondaryECtable.csv.** The table contains three kinds of information for secondary group, including the gene names, EC numbers, and bacteria names and is obtained from Output1 of Step2.
```{zsh}
head secondaryECtable.csv
```

## 5.3 Usage

```{zsh}
$PYTHON ec2pathway.py secondaryECtable.csv secondary_pathway.csv
```

## 5.4 Output file(s) for usuage example

One output file will be generated from Step5:

**Output1: secondary_pathway.csv.** The table contains three kinds of information for secondary group, including the EC numbers, pathways, and bacteria names.
```{zsh}
head secondary_pathway.csv
```

# 6. Extract Pathways for T number Group

## 6.1 Function

Extract pathways for bacteria in T number group based on the EC numbers obtained from Output1 of Step3.

## 6.2 Input file(s) for usage example

One input is needed to complete Step6:

**Input1: tnumberECtable.csv.** The table contains three kinds of information for T number group, including the gene names, EC numbers, and bacteria names and is obtained from Output1 of Step3.
```{zsh}
head tnumberECtable.csv
```

## 6.3 Usage

```{zsh}
$PYTHON ec2pathway.py tnumberECtable.csv tnumber_pathway.csv
```

## 6.4 Output file(s) for usuage example

One output file will be generated from Step6:

**Output1: tnumber_pathway.csv.** The table contains three kinds of information for T number group, including the EC numbers, pathways, and bacteria names.
```{zsh}
head tnumber_pathway.csv
```

# 7. Extract Biological Context from KEGG

## 7.1 Function

Extract biological context such as pathway name and pathway class for each pathway id (map id) from KEGG.

## 7.2 Input file(s) for usage example

No input is needed to complete Step7.

## 7.3 Usage

```{zsh}
$PYTHON keggRestTable.py > keggPathway.csv
```

## 7.4 Output file(s) for usuage example

One output file will be generated from Step7:

**Output1: keggPathway.csv.** The table contains biological context such as pathway name and pathway class for each pathway id (map id).
```{zsh}
head keggPathway.csv
```

# 8. Combine and Clean EC numbers

## 8.1 Function

Combine EC numbers of three bacteria sets (primary group, secondary group, T number group) obtained from NCBI GenBank files or KEGG database. Reformat EC numbers and delete redundant records.

## 8.2 Input file(s) for usage example

Three inputs are needed to complete Step8:

**Input1: primaryECtable.csv.** The table contains EC numbers of primary group.
```{zsh}
head primaryECtable.csv
```

**Input2: secondaryECtable.csv.** The table contains EC numbers of secondary group.
```{zsh}
head secondaryECtable.csv
```

**Input3: tnumberECtable.csv.** The table contains EC numbers of T number group.
```{zsh}
head tnumberECtable.csv
```

## 8.3 Usage

```{zsh}
Rscript enzymeDataCleaning.R
```

## 8.4 Output file(s) for usuage example

One output file will be generated from Step8:

**Output1: combined_ec.csv.** The table contains three kinds of information: 

  * EC_number: Enzymes
  * Pathway: Pathways
  * Bacteria: Bacteria names
```{zsh}
head combined_ec.csv
```

# 9. Combine and Clean the Pathways

## 9.1 Function

In this Step, two main things are performed:

  * Combine the pathways of three bacteria sets (primary group, secondary group, T number group) based on the EC numbers obtained from Step1, Step2, and Step3. Reformat the pathways and delete redundant records.
  * Prepare inputs for the next step (Step10).

## 9.2 Input file(s) for usage example

Similarly, three inputs are needed to complete Step9:

**Input1: primary_pathway.csv.** The table contains KEGG pathways for primary group.
```{zsh}
head primary_pathway.csv
```

**Input2: secondary_pathway.csv.** The table contains KEGG pathways for secondary group.
```{zsh}
head secondary_pathway.csv
```

**Input3: tnumber_pathway.csv.** The table contains KEGG pathways for T number group.
```{zsh}
head tnumber_pathway.csv
```

## 9.3 Usage

```{zsh}
Rscript pathwayDataCleaning.R
```

## 9.4 Output file(s) for usuage example

Three output files will be generated from Step9:

**Output1: combined_pathway.csv.** The table contains unique pathways for each bacteria EC number and is obtained from Step4, Step5, and Step6.
```{zsh}
head combined_pathway.csv
```

**Output2: Bact_EC.csv.** The table contains unique EC numbers for each bacterium and is one of the input files for the next step (Step10).
```{zsh}
head Bact_EC.csv
```

**Output3: EC_Pathway.csv.** The table includes unique pathways for each EC number and is another input file for Step10.
```{zsh}
head EC_Pathway.csv
```

# 10. Calculate fuzzy ranking values

## 10.1 Function

Computes fuzzy ranking values for each bacteria pathway combination using metabolic index generated by Sukanya's project. Sukanya's project can be obtained here: https://github.com/schandrababu/BMI.git

## 10.2 Input file(s) for usage example

Two inputs are needed to complete Step10:

**Input1: Bact_EC.csv.** The table contains unique EC numbers for each bacteria. This file should be inside a folder called Input_files.
```{zsh}
head Bact_EC.csv
```

**Input2: EC_Pathway.csv.** The table contains unique pathways for each enzyme. This file should be inside a folder called Input_files.
```{zsh}
head EC_Pathway.csv
```

## 10.3 Usage

```{zsh}
rm -rf Input_files Output_files
mkdir Input_files Output_files
cp Bact_EC.csv EC_Pathway.csv Input_files/
# add here how to download metabolicIndex.py
# git clone https://github.com/schandrababu/BMI.git
# cp BMI/Metabolic_index.py .
#pip install pandas openpyxl
$PYTHON Metabolic_index.py
cp Output_files/Bact_Pathway_Fuzzy_Ranking.csv .

```

## 10.4 Output file(s) for usage example

One output file will be generated from Step10:

**Output1: "Bact_Pathway_Fuzzy_Ranking.csv".** The table contains the fuzzy ranking values for bacteria pathways. This file will be created inside a folder called Output_files.
```{zsh}
head Bact_Pathway_Fuzzy_Ranking.csv
```

# 11. Create Genomic Enzyme Tree and Function Tree

## 11.1 Function

With the exhaustive bacteria EC number list from Step8 and the fuzzy ranking results of the bacteria pathways from Step10, a Genomic Enzyme Tree and a Function Tree are created.

## 11.2 Input file(s) for usage example

Four inputs are needed to complete Step11:

**Input1: Bact_Pathway_Fuzzy_Ranking.csv.** The table contains fuzzy ranking values obtained from Step10.
```{zsh}
head Bact_Pathway_Fuzzy_Ranking.csv
```

**Input2: keggPathway.csv.** The table contains biological context such as pathway name and pathway class for each pathway id (map id) and is obtained from Output1 of Step7.
```{zsh}
head keggPathway.csv
```

**Input3: combined_ec.csv.** The table contains unique EC numbers for each bacterium.
```{zsh}
head combined_ec.csv
```

**Input4: combined_pathway.csv.** The table contains pathway for each bacteria EC number.
```{zsh}
head combined_pathway.csv
```

## 11.3 Usage

```{zsh}
rm -rf heatmaps
# Create a new directory "heatmaps" to save the results of this step
mkdir heatmaps
```

```{zsh}
Rscript createTrees.R
```

## 11.4 Output file(s) for usuage example

Ten output files will be generated from Step11:

**Output1: Pathway_Bacteria_Fuzzy_Ranking.csv.** The table contains the formatted ranking values based on the orginal file obtained from Step10 for bacteria pathways.
```{zsh}
head Pathway_Bacteria_Fuzzy_Ranking.csv
```

**Output2: all_path_fuzzy_pathnames_sorted.csv.** The table contains the sorted pathway ranking values as well as biological context such as pathway name and pathway class for each pathway.
```{zsh}
head shiny_app/all_path_fuzzy_pathnames_sorted.csv
```

**Output3: A R file "all_path_fuzzy_pathnames_sorted_wide_before_removed.rfile".** The document contains a pathway matrix derived from the fuzzy ranking values of bacteria pathways before removing three big metabolic pathways:

  * map01100 (Metabolic pathways)
  * map01110 (Biosynthesis of secondary metabolites)
  * map01120 (Microbial metabolism in diverse environments))

In the pathway matrix, each row represents one bacterium while each column presents the pathway ids. Values of the cells are the fuzzy ranking values of bacteria pathways.

**Output4: A PDF file "heatmap_before_removed.pdf".** The heatmap was created based on the pathway fuzzy ranking values before removing three big metabolic pathways. It is saved under the directory "heatmaps".

**Output5: A JPG file "heatmap_before_removed.jpg".** The heatmap was created based on the pathway fuzzy ranking values before removing three big metabolic pathways. It is saved under the directory "heatmaps".

**Output6: A R file "all_path_fuzzy_pathnames_sorted_wide_removed.rfile".** The document contains a pathway matrix based on fuzzy ranking values of bacteria pathways after removing three big metabolic pathways. It has the same format as Output3 but 3 fewer pathways. 

**Output7: A PDF file "Bact_EC_Tree.pdf".** The document contains the whole Genomic Enzyme Tree.

**Output8: A R file "hc.rfile".** The document contains the clustering results of the Genomic Enzyme Tree.

**Output9: A PDF file "Function_Tree.pdf".** The document contains the whole Function Tree.

**Output10: A R file "hc_pathway_removed.rfile".** The document contains the clustering results of the Function Tree.

# 12. Cut Genomic Enzyme Tree and Function Tree at 8 Different Levels

## 12.1 Function

Cut the Genimic Enzyme tree and the Function tree at 8 different levels: Level 3,4,5,10,20,30,40,50 respectively and save the cut results for each level.

## 12.2 Input file(s) for usage example

Two inputs are needed to complete Step12:

**Input1: "hc.rfile".** The document contains the enzyme tree obtained from Output8 of Step11.

**Input2: "hc_pathway_removed.rfile".** The document contains the Function Tree obtained from Output10 of Step11.

## 12.3 Usage

```{zsh}
rm -rf trees
# Create a new directory "trees" to save the results of this step
mkdir trees

```

```{zsh}
Rscript cutTrees.R
```

## 12.4 Output file(s) for usuage example

Two kinds of outputs will be generated from Step12:

**Output set1: "geneTree_l3.rfile", "geneTree_l4.rfile", "geneTree_l5.rfile", "geneTree_l10.rfile", "geneTree_l20.rfile", "geneTree_l30.rfile", "geneTree_l40.rfile", "geneTree_l50.rfile".** These documents contain the cut results of the Genomic Enzyme Tree at each level. The cut results of each level include the cluster ids and the corresponding bacteria of each cluster.

**Output set2: "pathwayTree_l3.rfile", "pathwayTree_l4.rfile", "pathwayTree_l5.rfile", "pathwayTree_l10.rfile", "pathwayTree_l20.rfile", "pathwayTree_l30.rfile", "pathwayTree_l40.rfile", "pathwayTree_l50.rfile".** These documents contain the cut results of the Function Tree at each level. The cut results of each level include the cluster ids and the corresponding bacteria of each cluster.

# 13. Compare the Cut Results of the Genomic Enzyme Tree with the Cut Results of the Function Tree

## 13.1 Function

For each cut level, the presence of bacteria in every cluster are compared between the Genomic Enzyme Tree and the Funtion Tree. Each comparison pair compromises one cluster of the Genomic Enzyme Tree and one cluster of the Function Tree. Common bacteria and union bacteria list are created for each comparison pair.

## 13.2 Input file(s) for usage example

Two kinds of inputs are needed to complete Step13:

**Input set1: "geneTree_l3.rfile", "geneTree_l4.rfile", "geneTree_l5.rfile", "geneTree_l10.rfile", "geneTree_l20.rfile", "geneTree_l30.rfile", "geneTree_l40.rfile", "geneTree_l50.rfile".** They are the cut results of the Genomic Enzyme Tree at 8 different levels obtained from Step12.

**Input set2: "pathwayTree_l3.rfile", "pathwayTree_l4.rfile", "pathwayTree_l5.rfile", "pathwayTree_l10.rfile", "pathwayTree_l20.rfile", "pathwayTree_l30.rfile", "pathwayTree_l40.rfile", "pathwayTree_l50.rfile".** These doucments contain the cut results of the Function Tree at 8 different levels obtained from Step12.

## 13.3 Usage

```{zsh}
rm -rf cut_results
# Create a new directory "cut_results" to save the results of this step
mkdir cut_results
```

```{zsh}
Rscript analyzeTrees.R
```

## 13.4 Output file(s) for usuage example

Three kinds of outputs will be generated from Step13:

**Output set1: intersect_matrix_df_l3_removed.csv, intersect_matrix_df_l4_removed.csv, intersect_matrix_df_l5_removed.csv, intersect_matrix_df_l10_removed.csv, intersect_matrix_df_l20_removed.csv, intersect_matrix_df_l30_removed.csv, intersect_matrix_df_l40_removed.csv, intersect_matrix_df_l50_removed.csv.** These tables are the intersect matrixes that contain the Jaccard indexes of each cluster pair at 8 different levels. One of results of the intersect matrix (at Level3) can be:
```{zsh}
head cut_results/intersect_matrix_df_l3_removed.csv
```

**Output set2: res_l3_removed.csv, res_l4_removed.csv, res_l5_removed.csv, res_l10_removed.csv, res_l20_removed.csv, res_l30_removed.csv, res_l40_removed.csv, res_l50_removed.csv.** These tables keep the detailed comparision results of each pair and contain five properties: cluster ids of the Genomic Enzyme Tree, cluster ids of the Function Tree, common bacteria for each comparison pair, unduplicated bacteria for each comparison pair, and the length of the unduplicated bacteria list for each comparison pair. One of detailed comparison results (at Level3) can be:
```{zsh}
head -n2 cut_results/res_l3_removed.csv
```

**Output set3: freq_final_l3_removed.csv, freq_final_l4_removed.csv, freq_final_l5_removed.csv, freq_final_l10_removed.csv, freq_final_l20_removed.csv, freq_final_l30_removed.csv, freq_final_l40_removed.csv, freq_final_l50_removed.csv.** These tables contain the statistic results based on the Output set1. They keep the statistic results based on the frequencies of values appear in the interset matrix. They contain two properties: values observed in each intersect matrix in Output set1 and their frequencies in each intersect matrix. One of results of the statistic results (at Level3) can be:
```{zsh}
head cut_results/freq_final_l3_removed.csv
```

# 14. Analyze the Intersect and Union Results of Each Cluster Comparison

## 14.1 Function

Analyze the intersect matrixes with Jaccard indexes, generated from Step13. The percentages calculated based on the frequency of the values in each intersect matrix are calculated based on six categories: "Values = 0", "Values > 0 AND <= 0.5", "Values > 0.5 AND < 1", "Values = 1", "have intersect and union length is 1", and "have intersect and union length is not 1".

## 14.2 Input file(s) for usage example

Two kinds of inputs are needed to complete Step14:

**Input set1: freq_final_l3_removed.csv, freq_final_l4_removed.csv, freq_final_l5_removed.csv, freq_final_l10_removed.csv, freq_final_l20_removed.csv, freq_final_l30_removed.csv, freq_final_l40_removed.csv, freq_final_l50_removed.csv.** These tables contain the statistic results of each comparison of 8 different levels obtained from Output set3 of Step13.

**Input set2: res_l3_removed.csv, res_l4_removed.csv, res_l5_removed.csv, res_l10_removed.csv, res_l20_removed.csv, res_l30_removed.csv, res_l40_removed.csv, res_l50_removed.csv.** These tables contain the detailed comparision results of 8 different levels obtained from Output set2 of Step13.

## 14.3 Usage

```{zsh}
Rscript statisticsTrees.R
```

## 14.4 Output file(s) for usuage example

One output file will be generated from Step14:

**Output1: sta_result_all_levels_removed.csv.** The table contains the analysis results of each category for 8 different level: 
```{zsh}
head cut_results/sta_result_all_levels_removed.csv
```

# 15. Choose the optimal level manually

With the results obtained from Step14, an optimal level is decided manually by using the two rules: 

  * the level has more high values but less low values in its intersect matrix, which means the level possesses more common bacteria between the paired Genomic Enzyme Tree cluster and Function Tree cluster.
  * the level has intesrsect groups but less the union groups with the length of 1.

In the ProGut project, the optimal level was chosen to be Level3. Hence, we will use the res_l3_removed.csv file (from Output set2 of Step13) as input for downstream analysis.

# 16. Explore the Results of the Fuzzy Ranking Process After Three Big Pathways are Removed

## 16.1 Function

Explore the pathway matrix after three big pathways are removed from two different perspectives. First, create a heatmap to visualize the distribution of the bacteria pathway ranking values after these three big pathways are removed. Second, for each bacteria, sum up all the ranking values it and use these row sums to evaluate the importance of the bacteria generally.

## 16.2 Input file(s) for usage example

One input is needed to complete Step16:

**Input1: "all_path_fuzzy_pathnames_sorted_wide_removed.rfile.** The document contains a pathway matrix based on fuzzy ranking values of bacteria pathway after removing three big metabolic pathways and can be obtained from Output3 of Step11.

## 16.3 Usage

```{zsh}
Rscript postHeatmap_rowSum.R
```

## 16.4 Output file(s) for usuage example

Three output files will be generated from Step16:

**Output1: A PDF file "heatmap_removed.pdf."** The document contains the heatmap created based on the pathway fuzzy ranking values after removing three big metabolic pathways. It is saved under the directory "heatmaps".

**Ouput2: A JPG file "heatmap_removed.jpg".** The document contains the heatmap created based on the pathway fuzzy ranking values after removing three big metabolic pathways. It is saved under the directory "heatmaps".

**Output3: data_exploration_rowsum.csv.** The table not only contains a pathway matrix based on fuzzy ranking values of bacteria pathway after removing three big metabolic pathways which is obtained from Output4 of Step11, but also includes a sum of fuzzy ranking values across all the pathways for each bacteria.
```{zsh}
head -n2 data_exploration_rowsum.csv
```

# 17. Analyze the Optimal Level Based on the Genus Level

## 17.1 Function

Conduct the analysis process based on the optimal level (Level3) from three perspectives:

First, with the analysis outcomes, additonal information for each comparison pair can be attained at the genus level, including the unique genus list, the length of the unique genus list, pathways that are common between all genera within the pair, the length of common pathways, the length of the metabolism pathways, and the length of biosynthesis pathways.

Second, remove the comparison pairs that do not have any common bacteria. For the rest of the comparison pairs, the common bacteria appearing in both the Genomic Enzyme Tree clusters and the Function Tree clusters are finalize the bacteria groups respectively.

Third, compare the common pathways at the genus level among every two bacteria groups and figure out the unique pathways for each bacteria group. A Venn diagram is further created to show pathways common across the four communities.

## 17.2 Input file(s) for usage example

Three inputs are needed to complete Step17:

**Input1: "all_path_fuzzy_pathnames_sorted_wide_removed.rfile".** It is obtained from Output6 of Step11.

**Input2: keggPathway.csv.** The table contains biological context such as pathway name and pathway class for each pathway id (map id) and is obtained from Output1 of Step7.
```{zsh}
head keggPathway.csv
```

**Input3: res_l3_removed.csv.** The table is the detailed comparision results of Level3 obtained from Output set2 of Step13.
```{zsh}
head -n2 cut_results/res_l3_removed.csv
```

## 17.3 Usage

```{zsh}
rm -rf optimal_level
# Create a new directory "optimal_level" to save the results of this step
mkdir optimal_level
```

```{zsh}
rm -rf data_validation_results
# Create a new directory "data_validation_results" to save the results of this step
mkdir data_validation_results
```

```{zsh}
# manually choosing the level 3 as the optimal level
Rscript analyzeGenus.R cut_results/res_l3_removed.csv
```

## 17.4 Output file(s) for usuage example

Three output files will be generated from Step17:

**Output1: research_res_l3_removed_length.csv.** The table contains additional information for each bacteria group at the genus level.
```{zsh}
head -n2 optimal_level/research_res_l3_removed_length.csv
```

**Output2: research_res_l3_removed_nonempty.csv.** The table includes the same types of information as Output1 but the rows that do not have any common bacteria between the Genomic Enzyme Tree clusters and the Function Tree clusters are removed.
```{zsh}
head -n2 optimal_level/research_res_l3_removed_nonempty.csv
```

**Output3: research_res_l3_removed_compared.csv.** The table contains the comparison results of pathways between different groups at the genus level.
```{zsh}
head -n2 optimal_level/research_res_l3_removed_compared.csv
```

**Output4: A PNG file "Venn_diagram_groups_genus.png".** The Venn diagram shows the common pathways among genus in the corresponding bacterial communities.

# 18. Analyze the Optimal Level Based on the Species Level

## 18.1 Function

Conduct the analysis process based on the optimal level (Level3) from three perspectives:

First, with the comparison outcomes of the Genomic Enzyme Tree clusters and the Function Tree clusters, additonal information for each comparison pair can be attained at the species level, including the unique species list, the length of the unique species list, pathways that are common between all species within the pair, the length of common pathways, the length of the metabolism pathways, and the length of biosynthesis pathways.

Second, remove the comparison pairs that do not have any common bacteria. For the rest of the comparison pairs, the common bacteria appearing in both the Genomic Enzyme Tree clusters and the Function Tree clusters are finalized as the bacteria groups/communities respectively.

Third, compare the common pathways at the species level among every two bacteria groups and figure out the unique pathways for each bacteria group. A Venn diagram is further created to show pathways common across the four communities.

## 18.2 Input file(s) for usage example

Three inputs are needed to complete Step18:

**Input1: "all_path_fuzzy_pathnames_sorted_wide_removed.rfile".** It is obtained from Output6 of Step11.

**Input2: keggPathway.csv.** The table contains biological context such as pathway name and pathway class for each pathway id (map id) and is obtained from Output1 of Step7.
```{zsh}
head keggPathway.csv
```

**Input3: res_l3_removed.csv.** The table is the detailed comparision results of Level3 obtained from Output set2 of Step13.
```{zsh}
head -n2 cut_results/res_l3_removed.csv
```

## 18.3 Usage

```{zsh results=FALSE}
# manually choosing the level 3 as the optimal level
Rscript analyzeSpecies.R cut_results/res_l3_removed.csv
```

## 18.4 Output file(s) for usuage example

Three output files will be generated from Step18:

**Output1: research_res_l3_removed_length_species.csv.** The table contains additional information for each bacteria group at the species level.
```{zsh}
head -n2 optimal_level/research_res_l3_removed_length_species.csv
```

**Output2: research_res_l3_removed_nonempty_species.csv.** The table includes the same types of information as Output1 but the rows that do not have any common bacteria between the Genomic Enzyme Tree clusters and the Function Tree clusters are removed.
```{zsh}
head -n2 optimal_level/research_res_l3_removed_nonempty_species.csv
```

**Output3: research_res_l3_removed_compared_species.csv.** The table contains the comparison results of pathways between different groups at the species level.
```{zsh}
head -n2 optimal_level/research_res_l3_removed_compared_species.csv
```

**Output4: A PNG file "Venn_diagram_groups_species.png".** The Venn diagram shows the common pathways among species in the corresponding bacterial communities.

# 19. Verify the Bacteria Groups/Communities and Methods

## 19.1 Function

Verify the bacteria groups/communities and methods used to generate the communities in this study.

## 19.2 Input file(s) for usage example

Three inputs are needed to complete Step19:

**Input1: research_res_l3_removed_nonempty.csv.** The table contains the detailed comparision results of Level3 obtained from Output2 of Step17.
```{zsh}
head -n2 optimal_level/research_res_l3_removed_nonempty.csv
```

**Input2: Pathway_Bacteria_Fuzzy_Ranking.csv.** The table contains the results of pathway fuzzy ranking obtained from Output1 of Step11.
```{zsh}
head Pathway_Bacteria_Fuzzy_Ranking.csv
```

**Input3: keggPathway.csv.** The table contains biological context such as pathway name and pathway class for each pathway id (map id) and is obtained from Output1 of Step7.
```{zsh}
head keggPathway.csv
```

## 19.3 Usage

```{zsh}
Rscript bacteria_dataValidation.R optimal_level/research_res_l3_removed_nonempty.csv
```

## 19.4 Output file(s) for usage example

Five kinds of outputs will be generated from Step19:

**Output1: research_res_l3_removed_genus_species.csv.** The table contains the pathways and genu list for each bacteria group.
```{zsh}
head -n2 optimal_level/research_res_l3_removed_genus_species.csv
```

**Output2: research_res_l3_removed_genus_species_compared.csv.** The table contains the comparison results of every two bacteria groups at the bacteria (strain) level.
```{zsh}
head -n2 optimal_level/research_res_l3_removed_genus_species_compared.csv
```

**Output3: A PNG file "Venn_diagram_groups.png".** The venn diagram shows the relationships among unique pathways of bacteria groups.

**Output4: unique_bact_paths.csv.** The table contains six kinds of information about unique pathways for each bacteria group:

  * pathway.
  * map id.
  * bacteria that contain listed pathways.
  * group id of the bacterium.
  * ranking value of the bacteria pathway.
  * pathway size published in KEGG.
```{zsh}
head data_validation_results/unique_bact_paths.csv
```

**Output5: A PNG file "Venn_diagram_clusters.png".** The venn diagram shows the relationships among cluster1 in the Genomic Enzyme Tree and cluster1 and cluster2 in the Function Tree.

# 20. Normalize EC Counts of Each Bacteria Pathway Based on Genus Level and Species Level

## 20.1 Function

For each bacteria pathway, normalize EC counts for each pathway at genus level and species level respectively.

## 20.2 Input file(s) for usage example

Two inputs are needed to complete Step20:

**Input1: combined_pathway.csv.** The table contains unique pathways for each bacteria EC number and is obtained from Output1 of Step9.
```{zsh}
head combined_pathway.csv
```

**Input2: keggPathway.csv.** The table contains biological context such as pathway name and pathway class for each pathway id (map id) and is obtained from Output1 of Step7.
```{zsh}
head keggPathway.csv
```

## 20.3 Usage

```{zsh}
Rscript normalizeECcount.R
```

## 20.4 Output file(s) for usuage example

Two output files will be generated from Step20:

**Output1: normalized_genus_ec_count.csv.** The table mainly contains the EC counts before normalization, reference EC counts, and normalized EC counts for each pathway at genus level. 
```{zsh}
head normalized_genus_ec_count.csv
```

**Ouput2: normalized_species_ec_count.csv.** The table mainly contains the EC counts before normalization, reference EC counts, and normalized EC counts for each pathway at species level. 
```{zsh}
head normalized_species_ec_count.csv
```

# 21. Find Top 10 Pathways Based on Genus Level and Species Level

## 21.1 Function

According to the EC counts for each pathway, pick top 10 pathways for each genus as well as each genus-species.

## 21.2 Input file(s) for usage example

Two inputs are needed to complete Step21:

**Input1: normalized_genus_ec_count.csv.** The table is obtained from Output1 of Step20 and mainly contains the normalized EC counts for each pathway based on genus level.
```{zsh}
head normalized_genus_ec_count.csv
```

**Input2: normalized_species_ec_count.csv.** The table is obtained from Output2 of Step20 and mainly contains the normalized EC counts for each pathway based on species level.
```{zsh}
head normalized_species_ec_count.csv
```

## 21.3 Usage

```{zsh}
Rscript findTop10Pathway.R
```

## 21.4 Output file(s) for usuage example

Two output files will be generated from Step21:

**Output1: top10_genus_pathway.csv.** The table contains top 10 pathways based on the normalized EC counts for each genus.
```{zsh}
head -n2 top10_genus_pathway.csv
```

**Ouput2: top10_genus_species_pathway.csv.** The table contains top 10 pathways based on the normalized EC counts for each species. 
```{zsh}
head -n2 top10_genus_species_pathway.csv
```

# 22. Compare Top 10 Pathways Based on Genus Level and Species Level

## 22.1 Function

Compare top 10 pathways from two perspectives: 

  * Compare top 10 pathways between unduplicated genus pairs.
  * Compare top 10 pathways between unduplicated genus-species pairs. The comparisons between different genus and different species are ignored.

## 22.2 Input file(s) for usage example

Two inputs are needed to complete Step22:

**Input1: top10_genus_pathway.csv.** The table contains the top 10 pathways for each genus and is obtained from Output1 of Step21.
```{zsh}
head -n2 top10_genus_pathway.csv
```

**Input2: top10_genus_species_pathway.csv.** The table contains the top 10 pathways for each species and is obtained from Output2 of Step21.
```{zsh}
head -n2 top10_genus_species_pathway.csv
```

## 22.3 Usage

```{zsh}
Rscript intersectUnion.R
```

## 22.4 Output file(s) for usuage example

Two output files will be generated from Step22:

**Output1: intersect_union_genus_pathway.csv.** The table contains the comparison results between every pair with two genus.
```{zsh}
head -n2 intersect_union_genus_pathway.csv
```

**Ouput2: intersect_union_sameGenus_species_pathway.csv.** The table contains the comparison results between every pair with two genus-species. It is noted that the comparison results between the bacteria that coming from different genus and different species are removed and only the comparison pair with the bacteria that comes from same genus but different species are kept. 
```{zsh}
head -n2 intersect_union_sameGenus_species_pathway.csv
```

# 23. Create a Density Plot and Peform the Chi-Squared test Based on Genus Level and Species Level

## 23.1 Function

Develop a density plot and Chi-Squared test to compare the difference between delta genus and delta genus-species. Delta genus means the differences between paired genus groups while delta genus-species means differences between paired genus-species groups.

## 23.2 Input file(s) for usage example

Two inputs are needed to complete Step23:

**Input1: intersect_union_genus_pathway.csv.** The table contains the comparison results between every pair with two genus, which is obtained from Output1 of Step22.
```{zsh}
head -n2 intersect_union_genus_pathway.csv
```

**Input2: intersect_union_sameGenus_species_pathway.csv.** The table contains the comparison results between every pair with two genus-species, which is obtained from Output2 of Step22.
```{zsh}
head -n2 intersect_union_sameGenus_species_pathway.csv
```

## 23.3 Usage

```{zsh}
Rscript densityPlot_chiTest.R
```

## 23.4 Output file(s) for usuage example

Two output files will be generated from Step23:

**Output1: A PDF file "Density_Plot.pdf".** The document contains an overlapped density plot based on the distributions of genus and species comparison results.

**Ouput2: chi_squared.csv.** The table contains the results of the chi-squared test based on the genus and species comparison results. 
```{zsh}
head chi_squared.csv
```

# 24. Format the Data for the Shiny Application ProGut

## 24.1 Function

Format the data based on two data files obtained in the previous steps for the visulaization in the shiny application ProGut:

  * unique pathways for each bacteria EC number (combined_pathway.csv);
  * analysis results based on the optimal cut level (research_res_l3_removed_length.csv).

## 24.2 Input file(s) for usage example

Four inputs are needed to complete Step24:

**Input1: combined_pathway.csv.** The table contains unique pathways for each bacteria EC number and is obtained from Output1 of Step9.
```{zsh}
head combined_pathway.csv
```

**Input2: keggPathway.csv.** The table contains biological context such as pathway name and pathway class for each pathway id (map id) and is obtained from Output1 of Step7.
```{zsh}
head keggPathway.csv
```

**Input3: research_res_l3_removed_length.csv.** The table contains additional information for each bacteria group at the genus level. It is obtained from Output1 of Step17.
```{zsh}
head -n2 optimal_level/research_res_l3_removed_length.csv
```

**Input4: research_res_l3_removed_genus_species.csv.** The table contains the comparison results of every two bacteria groups at the bacteria level. It is obtained from Output1 of Step19.
```{zsh}
head -n2 optimal_level/research_res_l3_removed_genus_species.csv
```

## 24.3 Usage

```{zsh}
Rscript shiny_app_data_preparation.R optimal_level/research_res_l3_removed_length.csv optimal_level/research_res_l3_removed_genus_species.csv
```

## 24.4 Output file(s) for usuage example

Three output files will be generated from Step24:

**Output1: shiny_pie_chart_data.csv.** The table contains the EC counts for each bacteria pathway. This file will be saved under the directory "shiny_app".
```{zsh}
head shiny_app/shiny_pie_chart_data.csv
```

**Output2: groupID_bacteria_genus_pathway.csv.** The table contains four kinds of information: group id, the bacterium belonging to the correpsonding bacteria group, the genus pathway, and the genus name. This file will be saved under the directory "shiny_app".
```{zsh}
head shiny_app/groupID_bacteria_genus_pathway.csv
```

**Output3: groupID_bacteria_pathway.csv.** The table contains three kinds of information: group id, the bacterium belonging to the correpsonding bacteria group, and the bacteria pathway. This file will be saved under the directory "shiny_app".
```{zsh}
head shiny_app/groupID_bacteria_pathway.csv
```









