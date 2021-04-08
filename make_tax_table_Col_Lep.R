####################################################################
##### Insect Invasions Pursuit @ SESYNC                        #####
##### Script to update Coleoptera and Lepidoptera families     #####
##### created by Rachael Blake    03/23/2021                   #####
####################################################################

### load packages
library(tidyverse)

### load taxonomy table
tax_table <- read_csv("nfs_data/data/clean_data/taxonomy_table.csv")

### load Coleoptera fixes
coleop_fixes <- read_excel("nfs_data/data/raw_data/taxonomic_reference/Coleoptera_family_synonyms.xlsx ")

### load Lepidoptera fixes
lepi_fixes <- read_csv("nfs_data/data/raw_data/taxonomic_reference/Lep_genus_family_transfers.csv") 

### merge Coleoptera fixes with tax table
for (i in 1:nrow(ColFamSynz)){
  temp[temp$family== ColFamSynz[i,]$user_supplied_family_name &!is.na(temp$family),"family"]<- ColFamSynz [i,]$actual_family
}

temp[temp$family%in%c("Dryophthoridae","Brachyceridae"),"family"]<-"Curculionidae"


### merge Lepidoptera fixes with Coleop-tax table

# requires genus
temp$genus<-word(temp$genus_species,1)

