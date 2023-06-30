tmp <- read_xlsx(file_specs,
                 sheet = "Analysis_Specs") %>% 
    select(R_anaName, R_varName, Choice)

tmp_list <- split(tmp, tmp$R_anaName)

# for each of the list components, want to:
# remove rows with NA (if, e.g., they didn't pick all the choices)
# make sure each has rows, except custom doesn't have to

# for everything BUT 'custom', want to:
# determine whether 'Choice' is a species or a group
# if it's a group, want to get all the species in that group
# if a species in that group was already listed above, remove it from the 
# rest of the group and name the group 'Other [Group]'
# get those vars as columns in the main data frame 

print_choices <- function(x){
    test <- tmp_list[[x]]
    test2 <- filter(test, !is.na(Choice))
    cat("There were", nrow(test), "variables that could be specified for", 
        names(tmp_list)[x], "and", nrow(test2), "was/were. They are:", "\n", 
        paste(test2$Choice, collapse = "\n"), "\n\n\n")
}


purrr::walk(1:length(tmp_list), print_choices)

# identify Plant Categories in the Choice input
test3 <- test2 %>% 
    select(Choice) %>% 
    filter(Choice %in% unique(species_info$Plant_Categories)) %>% 
    unlist()
# returns empty vector if nothing listed is a Plant Category

# identify all the species in the Plant Categories given in Choice input
# hm would want to group it by Category though, not just get a list of ALL, or they'll mix
spps_in_grps <- species_info %>% 
    filter(Plant_Categories %in% test2$Choice) %>% 
    select(Species) %>% 
    unlist()



get_anaSpecs <- function(file){
    read_xlsx(file,
              sheet = "Analysis_Specs") %>% 
        select(R_anaName, R_varName, Choice) 
}