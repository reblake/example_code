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

# tax_table <- read.csv("nfs_data/data/clean_data/taxonomy_table.csv", stringsAsFactors=F)  # read in the taxonomy table

est <- occurr_gs %>% 
       left_join(tax_table)


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
tax_table <- tax_table %>% 
             mutate(family = ifelse(user_supplied_name == "Polistes dominua" & family == "Eumenidae", "Vespidae", family)) %>%  
            # mutate(family = ifelse(family == "Eumenidae", "Vespidae", family)) %>% 
             mutate(rank = ifelse(user_supplied_name == "Listronotus silvestris"))


             
# write the clean taxonomy table to a CSV file
readr::write_csv(tax_table, "nfs_data/data/clean_data/taxonomy_table.csv")




