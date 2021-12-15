#################################################################
### Script to run taxonomic cleaning functions on the FIXED file
### Written by Rachael Blake
### Dec 2021
#################################################################

# install.packages("devtools")
# devtools::install_github("reblake/insectcleanr")
library(insectcleanr)

# load the genus_only_resolution_FIXED file
fixed <- read.csv("./nfs_data/data/raw_data/taxonomic_reference/genus_only_resolution_FIXED.csv")

# want to send both the user_supplied_name column and the genus_species column through the 
# get_accepted_taxonomy() function

# start with the user_supplied_name column
usn <- fixed %>% 
       select(user_supplied_name) %>% 
       filter(!(user_supplied_name %in% c("Aphidiinae", "Arionidae", "Bostrychidae sinoxylon",
                                          "Corticariidae", "Eriosomatinae", "Lyctinae", 
                                          "Nemobiinae", "Omaliinae", "Pscoptera",
                                          "Psocopetra", "Psocoptera", "Scolytidae",
                                          "Scolytinae", "Spondylidinae"))) %>%   # these taxa cause problems!!
       unlist(., use.names = FALSE) 
       

usn_clean <- lapply(usn, get_accepted_taxonomy)

