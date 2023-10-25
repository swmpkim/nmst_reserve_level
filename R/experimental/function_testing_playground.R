test_fun <- function(data, specs){
specs <- specs

spec_multiGroup <- specs %in% data[["Species"]]


# make a list of all the species in each spec group
# and see if any species show up twice

df <- data %>% 
    mutate(Species_or_Group = case_when(Species %in% specs ~ Species,
                                        Plant_Categories %in% specs ~ Plant_Categories,
                                        NMST_Groupings %in% specs ~ NMST_Groupings,
                                        Cover_Categories %in% specs ~ Cover_Categories,
                                        .default = "Other"),
           Group = case_when(Plant_Categories %in% specs ~ Plant_Categories,
                             NMST_Groupings %in% specs ~ NMST_Groupings,
                             Cover_Categories %in% specs ~ Cover_Categories,
                             .default = "Other"))
df
}

test_fun(test_df, specs4)

make_spec_df <- function(data, specs){
    # input 'data' is long data frame
    # specs is a vector of choices, from the user input file
    # output is a long data frame
    
    # if a species was named as a choice, but also appears in a group
    # that was named, it will be presented only as the species
    # and the 'group' total will not include it
    df <- data
    tmp <- specs
    df %>% 
        left_join(species_info, by = "Species") %>% 
        mutate(Species_or_Group = case_when(Species %in% tmp ~ Species,
                                            Plant_Categories %in% tmp ~ Plant_Categories,
                                            .default = "Other")) %>% 
        group_by(Reserve, SiteID, TransectID, PlotID,
                 StTrns, StTrnsPlt,
                 Vegetation_Zone,
                 Year, Month, Day, Years_sinceStart,
                 Species_or_Group) %>% 
        summarize(Cover = sum(Cover, na.rm = TRUE)) %>% 
        ungroup() 
}
# would like to figure out how to look at the provided Plant_Categories
# and see if any species were removed because they were named separately
# and if so, rename it to "Other [category]"