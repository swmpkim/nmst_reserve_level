# tmp <- read_xlsx(file_specs,
#                  sheet = "Analysis_Specs") %>% 
#     select(R_anaName, R_varName, Choice)

tmp_list <- split(anaSpecs, anaSpecs$R_anaName)

# for each of the list components, want to:
# remove rows with NA (if, e.g., they didn't pick all the choices)
# make sure each has rows, except custom doesn't have to

# for everything BUT 'custom', want to:
# determine whether 'Choice' is a species or a group
# if it's a group, want to get all the species in that group
# if a species in that group was already listed above, remove it from the 
# rest of the group and name the group 'Other [Group]'
# get those vars as columns in the main data frame 

show_choices <- function(x){
    test <- tmp_list[[x]]
    test2 <- filter(test, !is.na(Choice))
    cat("There were", nrow(test), "variables that could be specified for", 
        names(tmp_list)[x], "and", nrow(test2), "was/were. They are:", "\n", 
        paste(test2$Choice, collapse = "\n"), "\n\n\n")
    
    # figure out whether what's given is a species or a group
    test2 <- test2 %>% 
        mutate(Species = Choice %in% species_info$Species,
               Group = Choice %in% species_info$Plant_Categories)
    
    # get the species list for each group
    spps_in_grps <- test2 %>% 
        filter(Group == TRUE) %>% 
        rowwise() %>% 
        mutate(SpeciesInGroup = paste(unique(species_info$Species[species_info$Plant_Categories == Choice]), collapse = ", "))
    
    # join back together
    test2 <- left_join(test2, spps_in_grps)   
    
    # return a list
    return(test2)
}


#purrr::walk(1:length(tmp_list), show_choices)

revised_list <- purrr::map(1:length(tmp_list), show_choices)
names(revised_list) <- names(tmp_list)


# if a species in the choices also belongs to a group in the choices, subtract it out
# first have to go analysis-by-analysis, then group-by-group and get a vector of species in each
# then see if a species is in it, and subtract it
test <- revised_list$multivar
test2 <- test %>% 
    select(Choice, SpeciesInGroup) %>% 
    pivot_wider(names_from = Choice,
                values_from = SpeciesInGroup)

# need a vector of species names for each group
# if (one of the other Choices) %in% that vector - 
# then make a vector of 'Other [Choice of group]' 
# that removes any of those
# and get rid of the vector that contains them

test8 <- species_info %>% 
    filter(Plant_Categories == "H-Halophyte") %>% 
    select(Species) %>% 
    unlist()
test9 <- revised_list$multivar$Choice[revised_list$multivar$Choice %in% test8]
test10 <- test8[!(test8 %in% test9)]
names(test10) <- NULL
test10  # this is the shortened vector


