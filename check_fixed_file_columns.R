#################################################################
### Script to run taxonomic cleaning functions on the FIXED file
### Written by Rachael Blake
### Dec 2021
#################################################################

# install.packages("devtools")
# devtools::install_github("reblake/insectcleanr")
library(insectcleanr) ; library(purrr) ; library(tidyverse)

# load the genus_only_resolution_FIXED file
fixed <- read.csv("./nfs_data/data/raw_data/taxonomic_reference/genus_only_resolution_FIXED.csv")

# want to send both the user_supplied_name column and the genus_species column through the 
# get_accepted_taxonomy() function

##### start with the user_supplied_name column
usn <- fixed %>% 
       select(user_supplied_name) %>% 
       filter(!(user_supplied_name %in% c("Aphidiinae", "Arionidae", "Bostrychidae sinoxylon",
                                          "Corticariidae", "Eriosomatinae", "Lyctinae", 
                                          "Nemobiinae", "Omaliinae", "Pscoptera",
                                          "Psocopetra", "Psocoptera", "Scolytidae",
                                          "Scolytinae", "Spondylidinae", "Syntominae"))) %>%   # these taxa cause problems!!
       unlist(., use.names = FALSE) 

# run through accepted taxonomy function
usn_clean <- lapply(usn, get_accepted_taxonomy)

# make into one dataframe
suppressMessages(
usn_acc <- usn_clean %>%
           purrr::reduce(full_join) %>%
           mutate(genus_species = str_squish(genus_species)) 
)

# are the columns the same?
all(usn_acc$user_supplied_name == usn_acc$genus_species)
usn_acc[!(usn_acc$user_supplied_name == usn_acc$genus_species), c("user_supplied_name", "genus_species")]

# write out file
write_csv(usn_acc, "User_Supp_Nm_FIXED.csv")

# run non-species matches through get_more_info()
usn_non_s <- usn_acc %>% 
             filter(!(rank == "species")) %>%  # filter to keep rows not id'd to species
             select(user_supplied_name) %>% 
             unlist(., use.names = FALSE) 

usn_more_info <- lapply(usn_non_s, get_more_info)

suppressMessages(
usn_acc_2 <-  usn_more_info %>% 
              purrr::reduce(full_join) # join all data frames from list
)

# write out file
write_csv(usn_acc_2, "USN_FIXED_gmi.csv")

# run matched_name2 column back through get_accepted_taxonomy()
usn_gmi_gs <- usn_acc_2 %>% 
              filter(!(user_supplied_name == matched_name2)) %>% # filter to keep unmatched rows
              select(matched_name2) %>% 
              unlist(., use.names = FALSE)

usn_acctax2 <- lapply(usn_gmi_gs, get_accepted_taxonomy)

suppressMessages(
usn_acc_3 <- usn_acctax2 %>% 
             purrr::reduce(full_join) # join all data frames from list
)

# write out file
write_csv(usn_acc_3, "USN_FIXED_gmi_gat.csv")

##############################################

##### next do the genus_species column
gs <- fixed %>% 
      select(genus_species) %>% 
      filter(!(genus_species %in% "")) %>% 
      unlist(., use.names = FALSE)   

# run through accepted taxonomy function
gs_clean <- lapply(gs, get_accepted_taxonomy)

# make into one dataframe
suppressMessages(
gs_acc <- gs_clean %>%
          purrr::reduce(full_join) %>%
          mutate(genus_species = str_squish(genus_species)) 
)

# are the columns the same?
all(gs_acc$user_supplied_name == gs_acc$genus_species)
gs_acc[!(gs_acc$user_supplied_name == gs_acc$genus_species), c("user_supplied_name", "genus_species")]

# write out file
write_csv(gs_acc, "Genus_Sp_FIXED.csv")

# run non-species matches through get_more_info()
gs_non_s <- gs_acc %>% 
            filter(!(rank == "species")) %>%  # filter to keep rows not id'd to species
            select(user_supplied_name) %>% # note: user_supplied_name column is the genus_species column from
                                           # the FIXED file after having gone through GBIF the first time above 
            unlist(., use.names = FALSE) 

gs_more_info <- lapply(gs_non_s, get_more_info)

suppressMessages(
gs_acc_2 <- gs_more_info %>%  
            purrr::reduce(full_join) # join all data frames from list  
)

# write out file
write_csv(usn_acc_2, "GS_FIXED_gmi.csv")

# run matched_name2 column back through get_accepted_taxonomy()
gs_gmi_gs <- gs_acc_2 %>% 
             filter(!(user_supplied_name == matched_name2)) %>% # filter to keep unmatched rows
             select(matched_name2) %>% 
             unlist(., use.names = FALSE)

gs_acctax2 <- lapply(gs_gmi_gs, get_accepted_taxonomy)

suppressMessages(
gs_acc_3 <- gs_acctax2 %>% 
            purrr::reduce(full_join) # join all data frames from list
)

# write out file
write_csv(gs_acc_3, "GS_FIXED_gmi_gat.csv")

