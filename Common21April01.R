##############################################
# Script to merge taxonomic and occurence files from Nextcloud
# to create a file ready for analysis
# files have a single row for each genus_species region combination

# three files are created:
# Establishmentunprocessed2021April01.csv - merged tax occ file but has multiple rows per species region combo.
# Establishment2021April01.csv  - default GBIF families
# Establishment2021April01ColLep.csv - has family names as per the Coleoptera and Lepidoptera papers.

# this runs on Rebecca's computer, so would need to be reset for alternative directories

##################################################
# loading libraries

library(tidyverse)
library(rgbif) # taxonomic package
library(taxize) # taxonomic package
library(readxl) 
library("gridExtra")

library(purrr) ; library(countrycode) ; library(DescTools)

##################################################
# set working directory

setwd("C:/Users/TurnerR/OneDrive - scion/Data/Raw_Data")

##################################################
# loading latest tax file

tax<-read.csv("C:/Users/TurnerR/OneDrive - scion/Data/Raw_Data/Establishments/taxonomy_table_downloaded2021April01.csv", stringsAsFactors=FALSE)

file_list <- dir(path="C:/Users/TurnerR/OneDrive - scion/Data/Raw_Data/raw_by_country", pattern='*.xlsx')  # makes list of the files
file_listp <- paste0("C:/Users/TurnerR/OneDrive - scion/Data/Raw_Data/raw_by_country/", file_list)         # adds path to file names

source("C:/Users/TurnerR/OneDrive - scion/R/custom_taxonomy_funcs.R")
# Note that Rebecca edited the custom_taxonomy file to make it work for her directories

#####################################
### Making the occurrence table   ###
#####################################

# apply that function over the list of dataframes
occurr_list <- lapply(file_listp, separate_occurrence) 

# put all occurrence dataframes into one large dataframe
df_occurr <- occurr_list %>% 
  purrr::reduce(full_join) %>% 
  mutate_all(~gsub("(*UCP)\\s\\+|\\W+$", "", . , perl=TRUE)) %>% # remove rogue white spaces
  # remove Arachnid
  filter(!(genus_species == "Trixacarus caviae")) %>% 
  # add blank country and present_status columns because they were removed in edits of the raw data files (Aug 14, 2020)
  mutate(country = '',
         present_status = '') %>% 
  # fill in country column with canada_or_us info
  mutate(country = ifelse(is.na(country) & canada_or_us %in% c("Canada", "Us", "Us, may not actually be adventive"), 
                          canada_or_us, country),
         present_status = ifelse(present_status == "Na", NA, present_status),
         notes = ifelse(country == "Us, may not actually be adventive", "may not actually be adventive", ""),
         country = ifelse(country == "Us, may not actually be adventive", "Us", country),
         notes = ifelse(origin == "New insect record for 1960  purposeful introduction", 
                        "New insect record for 1960  purposeful introduction", ""),
         origin = ifelse(origin == "New insect record for 1960  purposeful introduction",
                         "", origin),
         notes = ifelse(origin == "New insect record for 1963, chance immigrant", 
                        "New insect record for 1963, chance immigrant", ""),
         origin = ifelse(origin == "New insect record for 1963, chance immigrant",
                         "", origin)
  ) %>% 
  # clean up/fill in country column
  mutate(year = ifelse(year == -999, NA, year),
         country = ifelse(region %in% c("Okinawa", "Ogasawara", "Japan"), "Japan", country),
         country = ifelse(region == "Hawaii", "Us", country),
         country = ifelse(region == "Korea", "Korea", country),
         country = ifelse(region == "New Zealand", "New Zealand", country),
         notes = ifelse(grepl("Proceedings of the", .$origin), origin, notes),
         origin = ifelse(grepl("Proceedings of the", .$origin), "", origin)) %>% 
  # clean up some species names
  mutate(genus_species = gsub("Mycetophila\xa0propria", "Mycetophila propria", genus_species),
         genus_species = gsub("Mycetophila\xa0vulgaris", "Mycetophila vulgaris", genus_species),
         genus_species = gsub("Mycetophila\xa0marginepunctata", "Mycetophila marginepunctata", genus_species),
  ) %>%         
  # clean up year column
  mutate(year = ifelse(year %in% c("N/A", "Na"), NA_character_, year),
         year = gsub("\\s", "", year, perl=TRUE)) %>% 
  # clean up intentional release column
  mutate(intentional_release = ifelse(intentional_release %in% c("N"), "No", 
                                      ifelse(intentional_release %in% c("1", "I", "Y"), "Yes", intentional_release))) %>% 
  mutate(intentional_release = ifelse(intentional_release %in% c("Na"), NA_character_, intentional_release)) %>% 
  # clean up ecozone
  mutate(ecozone = ifelse(ecozone %in% c("Na"), NA_character_, ecozone)) %>% 
  # clean up eradicated
  mutate(eradicated = ifelse(eradicated %in% c("Na"), NA_character_, eradicated)) %>% 
  # clean up origin column
  mutate(origin = ifelse(origin %in% c("Na"), NA_character_, origin)) %>%
  # clean up confirmed establishment
  mutate(confirmed_establishment = ifelse(confirmed_establishment %in% c("Na"), NA_character_, confirmed_establishment)) %>% 
  # clean up host type
  mutate(host_type = str_to_lower(host_type)) %>% 
  # add country codes for country and origin columns
  mutate(country_code = countrycode(country, "country.name", "iso3n", warn = TRUE),
         origin_code = countrycode(origin, "country.name", "iso3n", warn = TRUE)) %>% 
  mutate(genus_species = gsub("\xa0", " ", genus_species , perl=TRUE)) %>% # trying to get rid of weird characters
  dplyr::select(-canada_or_us, -nz_region) %>% 
  dplyr::arrange(genus_species) 

# add the unique ID column and delete genus species column(s)
tax_table <- tax  # read in the taxonomy table - already loaded

# make final occurrence dataframe
occurr_df <- df_occurr %>%
  mutate_all(~gsub("(*UCP)\\s\\+|\\W+$", "", . , perl = TRUE)) %>%  # remove rogue white spaces
  dplyr::rename(user_supplied_name = genus_species) %>% # have to rename genus_species to user_supplied_name so matches are correct
  dplyr::left_join(y = select(tax_table, c(user_supplied_name, taxon_id, genus_species)),
                   by = "user_supplied_name") %>% # join in the taxonomy info
  mutate(genus_species = gsub("<a0>", " ", genus_species, perl=TRUE)) %>% 
  select(taxon_id, everything()) %>% # make taxon_id column the first column
  dplyr::arrange(taxon_id) # order by taxon_id



##### NOTE: there were no cases where there were different entries for intentional release in a
##### given genus_species/region combo

# extracting relevant columns

tax1<-tax[,c("user_supplied_name", "rank" ,"order" , "family" ,"genus_species")]
occ1<-occurr_df[,c("user_supplied_name", "year", "intentional_release","region"  ,"eradicated", "genus_species" )]

Est<-left_join(occ1,tax1,by="user_supplied_name") # 12807

##### Note: Several checks were completed at this stage
# check for cases with no rank
# check for spcies cases with no family
# check for user_supplied_names with no match
# check for cases where order, family or genus species name has been changed since previous run
# check that each genus_species name corresponds to one family only

#########################
# post edits required

Est[Est$user_supplied_name=="Listronotus silvestris","rank"]<-"species"

Est[Est$user_supplied_name=="Vazuezitocoris andinus","order"]<-"Hemiptera"
Est[Est$user_supplied_name=="Vazuezitocoris andinus","family"]<-"Coreidae"

Est[Est$user_supplied_name=="Labia annulata","order"]<-"Dermaptera"
Est[Est$user_supplied_name=="Labia annulata","family"]<-"Spongiphoridae"
Est[Est$user_supplied_name=="Labia annulata","genus_species.y"]<-"Labia annulata"

Est[Est$user_supplied_name=="Lema bilineata","order"]<-"Coleoptera"
Est[Est$user_supplied_name=="Lema bilineata","family"]<-"Chrysomelidae"
Est[Est$user_supplied_name=="Lema bilineata","genus_species.y"]<-"Lema bilineata"

################
# only species level records are required

Estgs<-Est[Est$rank=="species"&!is.na(Est$rank),] # 12444

Estgs[Estgs$family=="Eumenidae","family"]<-"Vespidae"
Estgs[Estgs$order=="Cynipoidea","order"]<-"Hymenoptera"

write_csv(Estgs,"Establishmentunprocessed2021April01.csv")
#############################################################

# relevant columns
Estgs1<-distinct(Estgs[,c("year","intentional_release", "region" ,"eradicated",  "order" , "family", "genus_species.y" )]) # 12417

# fixing error
Estgs1[Estgs1$intentional_release=="Eradicated"&!is.na(Estgs1$intentional_release),"eradicated"]<-"Yes"
Estgs1[Estgs1$intentional_release=="Eradicated"&!is.na(Estgs1$intentional_release),"intentional_release"]<-"No"

# preparing year
Estgs1$year<-gsub("\\D", "", Estgs1$year, perl = TRUE)
Estgs1[is.na(Estgs1$year),"year"]<-9999
Estgs1$year<-as.numeric(Estgs1$year)

# preparing intentional release
Estgs1[is.na(Estgs1$intentional_release),"intentional_release"]<-"0"
Estgs1[Estgs1$intentional_release=="No","intentional_release"]<-"0"
Estgs1[Estgs1$intentional_release=="Yes","intentional_release"]<-"1"
Estgs1$intentional_release<-as.numeric(Estgs1$intentional_release)

temp<-Estgs1%>%group_by(region,genus_species.y,order,family,eradicated)%>%summarise(year=min(year),intentional_release=min(sum(intentional_release),1))
names(temp)[names(temp) == "genus_species.y"] <- "genus_species"

write_csv(temp,"Establishment2021April01.csv")
# temp has 12360 rows

#####################################################
# version with updated family names

# Coleoptera

# For consistency, we adopted the classification of Bouchard et al. (2011) with three exceptions. # Dryophthoridae and Brachyceridae were merged into the Curculionidae, Passandridae were 
# merged into the Cucujidae, and Megalopodidae were merged into the Chrysomelidae to align 
# with source datasets.

ColFamSynz<-read_excel("C:/Users/TurnerR/OneDrive - scion/Data/Raw_Data/taxonomic_references/Coleoptera_family_synonyms.xlsx ")

for (i in 1:nrow(ColFamSynz)){
  temp[temp$family== ColFamSynz[i,]$user_supplied_family_name &!is.na(temp$family),"family"]<- ColFamSynz [i,]$actual_family
}

temp[temp$family%in%c("Dryophthoridae","Brachyceridae"),"family"]<-"Curculionidae"

# Lepidoptera

# requires genus

temp$genus<-word(temp$genus_species,1)


Lep_gf22<-read.csv("Lep_genus_family_transfers.csv",stringsAsFactors = FALSE)


for (i in 1:nrow(Lep_gf22)){
  temp[temp$genus==Lep_gf22[i,]$genus,"family"]<-Lep_gf22[i,]$family
}

# Lep at family level
temp[temp$family%in%c("Lymantriidae","Arctiidae"),"family"]<-"Erebidae"


write_csv(temp,"Establishment2021April01ColLep.csv")
