library(tidyverse)

# Comparing Rebecca's code

rt_estab <- read.csv("nfs_data/data/clean_data/dated_tax_files/Establishment2021April01.csv")
View(rt_estab)
occurr_gs <- read.csv("nfs_data/data/clean_data/occurrence_table.csv")
View(occurr_gs)

combo <- occurr_gs %>% 
         full_join(rt_estab, by = c("genus_species", "region")) %>% 
         select(genus_species, region, year.x, year.y, order, family, 
                intentional_release.x, intentional_release.y, everything())
View(combo)

tax_table <- read.csv("nfs_data/data/clean_data/taxonomy_table.csv", stringsAsFactors=F)  # read in the taxonomy table

est <- occurr_gs %>% 
       left_join(tax_table)

# bring in the manual fixes file
sal_taxa <- read_csv("nfs_data/data/raw_data/taxonomic_reference/genus_only_resolution_FIXED.csv", trim_ws = TRUE,
                     col_types = cols(up_to_date_name = col_character())) 

##### Rebecca's Note: Several checks were completed at this stage
nr <- filter(est, is.na(rank)) # check for cases with no rank
nf <- filter(est, is.na(family)) ; nrow(nf) # check for spcies cases with no family
filter(est, rank == "genus" & str_count(user_supplied_name) == 2) # check for user_supplied_names with no match
# check for cases where order, family or genus species name has been changed since previous run
of <- est %>%  # check that each genus_species name corresponds to one family only
      group_by(genus_species) %>% 
      mutate(n_fam = n_distinct(family)) %>% 
      ungroup() %>% 
      select(genus_species, rank, order, family, n_fam) %>% 
      filter(n_fam > 1)
# Polistes dominula is the only species with >1 family

pd1 <- filter(tax_table, genus_species == "Polistes dominula")
pd1
  
pd2 <- filter(occurr_gs, genus_species == "Polistes dominula")
pd2

pd3 <- filter(sal_taxa, genus_species == "Polistes dominula")    
pd3
#### Need to add Polistes dominua to the FIXED file, with the correct family; this will solve this
### Can do wholesale change of family Eumenidae to family Vespidae in the FIXED file or code.

##########

others <- filter(sal_taxa, user_supplied_name %in% c("Listronotus silvestris", "Vazuezitocoris andinus"))
others
# Listronotus silvestris is in the FIXED file already with the rank of genus (change rank to species), and a note of "??? Cannot find at all"
# Vazuezitocoris andinus is in the FIXED file already, but it looks like order and family are swapped

o2 <- filter(tax_table, user_supplied_name %in% c("Labia annulata", "Lema bilineata"))
o2
# NOTE: for these two, ignore GBIF entirely, and fix them in the FIXED file as per RT. 

######################################

# editing the tax table here so GBIF info doesn't change again
tax_table2 <- tax_table %>% 
              mutate(family = ifelse(user_supplied_name == "Polistes dominua" & family == "Eumenidae", "Vespidae", family)) %>%  
              mutate(family = ifelse(family == "Eumenidae", "Vespidae", family)) %>% 
              mutate(rank = ifelse(user_supplied_name == "Listronotus silvestris", "species", rank)) %>% 
              mutate(order = ifelse(user_supplied_name == "Vazuezitocoris andinus", "Hemiptera", order),
                     family = ifelse(user_supplied_name == "Vazuezitocoris andinus", "Coreidae", family)) %>% 
              mutate(order = ifelse(user_supplied_name == "Labia annulata", "Dermaptera", order),
                     family = ifelse(user_supplied_name == "Labia annulata", "Spongiphoridae", family),
                     genus = ifelse(user_supplied_name == "Labia annulata", "Labia", genus),
                     species = ifelse(user_supplied_name == "Labia annulata", "Labia annulata", species), 
                     genus_species = ifelse(user_supplied_name == "Labia annulata", "Labia annulata", genus_species),
                     status = ifelse(user_supplied_name == "Labia annulata", NA_character_, status),
                     matchtype = ifelse(user_supplied_name == "Labia annulata", NA_character_, matchtype),
                     usagekey = ifelse(user_supplied_name == "Labia annulata", NA_character_, usagekey),
                     taxonomy_system = ifelse(user_supplied_name == "Labia annulata", NA_character_, taxonomy_system),
                     taxonomic_authority = ifelse(user_supplied_name == "Labia annulata", NA_character_, taxonomic_authority)) %>% 
              mutate(order = ifelse(user_supplied_name == "Lema bilineata", "Coleoptera", order),
                     family = ifelse(user_supplied_name == "Lema bilineata", "Chrysomelidae", family),
                     genus = ifelse(user_supplied_name == "Lema bilineata", "Lema", genus),
                     species = ifelse(user_supplied_name == "Lema bilineata", "Lema bilineata", species),
                     genus_species = ifelse(user_supplied_name == "Lema bilineata", "Lema bilineata", genus_species),
                     status =  ifelse(user_supplied_name == "Lema bilineata", NA_character_, status),
                     matchtype = ifelse(user_supplied_name == "Lema bilineata", NA_character_, matchtype),
                     usagekey = ifelse(user_supplied_name == "Lema bilineata", NA_character_, usagekey),
                     taxonomy_system = ifelse(user_supplied_name == "Lema bilineata", NA_character_, taxonomy_system),
                     taxonomic_authority = ifelse(user_supplied_name == "Lema bilineata", NA_character_, taxonomic_authority)) %>% 
             mutate(order = ifelse(user_supplied_name %in% c("Gymnaetron pascuorum", "Gymnetron pascuorum"), "Coleoptera", order),
                    family = ifelse(user_supplied_name %in% c("Gymnaetron pascuorum", "Gymnetron pascuorum"), "Curculionidae", family),
                    genus = ifelse(user_supplied_name %in% c("Gymnaetron pascuorum", "Gymnetron pascuorum"), "Mecinus ", genus),
                    species = ifelse(user_supplied_name %in% c("Gymnaetron pascuorum", "Gymnetron pascuorum"), "Mecinus pascuorum", species),
                    genus_species = ifelse(user_supplied_name %in% c("Gymnaetron pascuorum", "Gymnetron pascuorum"), "Mecinus pascuorum", genus_species),
                    synonym = ifelse(user_supplied_name %in% c("Gymnaetron pascuorum", "Gymnetron pascuorum"), "TRUE", synonym),
                    status =  ifelse(user_supplied_name %in% c("Gymnaetron pascuorum", "Gymnetron pascuorum"), NA_character_, status),
                    matchtype = ifelse(user_supplied_name %in% c("Gymnaetron pascuorum", "Gymnetron pascuorum"), NA_character_, matchtype),
                    usagekey = ifelse(user_supplied_name %in% c("Gymnaetron pascuorum", "Gymnetron pascuorum"), NA_character_, usagekey),
                    taxonomy_system = ifelse(user_supplied_name %in% c("Gymnaetron pascuorum", "Gymnetron pascuorum"), NA_character_, taxonomy_system),
                    taxonomic_authority = ifelse(user_supplied_name %in% c("Gymnaetron pascuorum", "Gymnetron pascuorum"), NA_character_, taxonomic_authority))

# write the clean taxonomy table to a CSV file
# readr::write_csv(tax_table2, "nfs_data/data/clean_data/taxonomy_table.csv")

###########################################################
# Prep for data publication, Sept 2021
# overwrite "genus" column entries for any species-level taxa with the first word from the "genus_species" column

# begin by reading in the taxonomy table, so you have the object tax_table
tax_table <- read.csv("nfs_data/data/clean_data/tables_initial_publication/taxonomy_table.csv", stringsAsFactors=F)  # read in the taxonomy table
# head(tax_table)

# do the replacing
tax_table3 <- tax_table %>% 
              mutate(genus = ifelse(rank %in% c("genus", "species"), word(genus_species, 1), genus))

# write the clean taxonomy table to a CSV file
readr::write_csv(tax_table3, "nfs_data/data/clean_data/tables_initial_publication/taxonomy_table.csv")
               
              
tt2 <- tax_table %>% select(rank, order, family, genus, genus_species) %>% 
       distinct(genus_species, .keep_all = TRUE)

