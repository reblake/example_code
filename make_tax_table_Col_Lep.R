####################################################################
##### Insect Invasions Pursuit @ SESYNC                        #####
##### Script to update Coleoptera and Lepidoptera families     #####
##### created by Rachael Blake    03/23/2021                   #####
####################################################################

### load packages
library(tidyverse) ; library(readxl)

### load taxonomy table
tax_table <- read_csv("nfs_data/data/clean_data/taxonomy_table.csv")

### load Coleoptera fixes
coleop_fixes <- read_excel("nfs_data/data/raw_data/taxonomic_reference/Coleoptera_family_synonyms.xlsx")

### load Lepidoptera fixes
lepi_fixes <- read_csv("nfs_data/data/raw_data/taxonomic_reference/Lep_genus_family_transfers.csv") 

### merge Coleoptera fixes with tax table
tt_c <- tax_table %>% 
        left_join(coleop_fixes, by = c("family" = "user_supplied_family_name")) %>% 
        mutate(family = ifelse(!is.na(actual_family), actual_family, family),
               family = ifelse(family %in% c("Dryophthoridae","Brachyceridae"), "Curculionidae", family)) %>% 
        select(-actual_family)
  
 
 
# tt3 <- coleop_fixes
# temp <- read.csv("nfs_data/data/clean_data/dated_tax_files/Establishment2021April01.csv")
#     
# for (i in 1:nrow(coleop_fixes)){
#   temp[temp$family== coleop_fixes[i,]$user_supplied_family_name &!is.na(temp$family),"family"]<- coleop_fixes [i,]$actual_family
# }
# 
# temp[temp$family%in%c("Dryophthoridae","Brachyceridae"),"family"]<-"Curculionidae"



### merge Lepidoptera fixes with Coleop-tax table

# requires genus
temp$genus<-word(temp$genus_species,1)



#####################################
### Write file                    ###
#####################################
# write the clean taxonomy table to a CSV file
readr::write_csv(XXXXX, "nfs_data/data/clean_data/taxonomy_table_coel_lep.csv")


