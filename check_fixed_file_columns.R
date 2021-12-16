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

