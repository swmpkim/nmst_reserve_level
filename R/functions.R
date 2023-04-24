# Importing ----

get_data <- function(file){
    readxl::read_xlsx(file,
                          sheet = "Cover")
}

get_stn_table <- function(file){
    readxl::read_xlsx(file,
                          sheet = "Station_Table")
}

get_species_info <- function(file){
    readxl::read_xlsx(file,
                          sheet = "Species_Names")
}

get_eis <- function(file){
    read_xlsx(file,
              sheet = "Ecotone_Invaders") %>% 
        mutate(rownum = row_number()) %>% 
        pivot_longer(-rownum,
                     names_to = "Zone",
                     values_to = "Invader") %>% 
        filter(!is.na(Invader)) %>% 
        select(-rownum) %>% 
        arrange(Zone, Invader)
}


# Joins ----  

join_zones <- function(data = dat,
                       station_info = stn_tbl){
    zones_to_join <- station_info[, c("Reserve", "SiteID", "TransectID", "PlotID", "Vegetation_Zone")]
    
    dplyr::left_join(data, zones_to_join) %>% 
        relocate(Vegetation_Zone, .before = SiteID)
}


# Summaries ----

sum_category <- function(plant_cat,
                         data = dat, 
                         spp_data = species_info){
    
    cat_names <- spp_data$Species[which(spp_data$Plant_Categories == plant_cat)]
    cat_cols <- which(names(data) %in% cat_names)
    apply(data[,c(cat_cols)], 1, function(x) sum(x, na.rm = TRUE))
}


sum_spp_categories <- function(data = dat, 
                               spp_data = species_info){
    # uses sum_category() from above
    cats_to_summ <- unique(spp_data$Plant_Categories)
    as.data.frame(sapply(cats_to_summ, sum_category, USE.NAMES = TRUE))
}


# Plots ----

plot_through_time <- function(data,
                              group,
                              panels){
    ggplot(data, aes(x = Year, y = Cover, 
                     group = {{group}}, color = {{group}}, fill = {{group}})) +
        geom_point() +
        geom_smooth(method = "loess", se = FALSE) +
        facet_wrap(enquo(panels))
}