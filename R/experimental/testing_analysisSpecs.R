# tmp <- read_xlsx(file_specs,
#                  sheet = "Analysis_Specs") %>% 
#     select(R_anaName, R_varName, Choice)

anaSpec_list <- split(anaSpecs, anaSpecs$R_anaName)

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
    test <- anaSpec_list[[x]]
    test2 <- filter(test, !is.na(Choice))
    cat("There were", nrow(test), "variables that could be specified for", 
        names(anaSpec_list)[x], "and", nrow(test2), "was/were. They are:", "\n", 
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

revised_list <- purrr::map(1:length(anaSpec_list), show_choices)
names(revised_list) <- names(anaSpec_list)

# need long data frame, With column for "grouping for this analysis"
# start with species; only evaluates what hasn't been assigned, so then do group next
# everything else becomes other
# then pivot wider

# don't know yet how to make it show "Other Brackish" or whatever






