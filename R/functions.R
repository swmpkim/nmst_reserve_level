# Importing ----

get_data <- function(file,
                     cover_only = FALSE){
    
    if(cover_only == FALSE){
        readxl::read_xlsx(file,
                          sheet = "Cover",
                          guess_max = 5000)
    } else {
        readxl::read_xlsx(file,
                          sheet = "Cover",
                          guess_max = 5000) %>% 
            dplyr::select(-starts_with("Average Canopy Height"),
                          -starts_with("Maximum Canopy Height"),
                          -starts_with("Density"),
                          -starts_with("Height"),
                          -starts_with("Diameter"))
    }
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
                     names_to = "Vegetation_Zone",
                     values_to = "Species") %>% 
        filter(!is.na(Species)) %>% 
        mutate(Invader = 1) %>% 
        select(-rownum) %>% 
        arrange(Vegetation_Zone, Species)
}



# Modifying ----

remove_unsampleds <- function(data){
    to_find_empties <- data %>% 
        dplyr::select(-(Reserve:Notes),
                      -starts_with("F"))
    # from code behind janitor::remove_empty; line 12
    empty_rows <- rowSums(is.na(to_find_empties)) == ncol(to_find_empties)
    data[!empty_rows, , drop = FALSE]
}


na_to_0 <- function(data){
    dat_tmp <- data
    start <- which(names(dat_tmp) == "Notes") + 1  # first species
    end <- which(stringr::str_starts(names(dat_tmp), "F_"))[1] - 1  # last species
    dat_tmp[start:end][is.na(dat_tmp[start:end])] <- 0
    dat_tmp
    # data %>% 
    #     dplyr::mutate(across(all_of(start:end), 
    #                          ~tidyr::replace_na(.x, 0)))
}


remove_suspect_values <- function(data,
                                  flags){
    
    # THIS IS IMPERFECT -
    # if "1" is given as a flag, -1 will also be removed
    # negative flags work fine (e.g. specifying 3 will not remove -3)
    
    # split data frame into data and qaqc columns
    qaqc_cols_start <- which(str_starts(names(data), "F_"))[1]
    data_alone <- data[, 1:(qaqc_cols_start-1)]
    qaqc_cols <- data[, qaqc_cols_start:ncol(data)]
    
    # loop through qaqc columns and find any values containing the flags to be removed
    outs <- list()
    for(i in seq_along(qaqc_cols)){
        non_na <- which(!is.na(qaqc_cols[, i]))
        vals <- qaqc_cols[non_na, i]
        tmp <- data.frame(Species = rep(names(qaqc_cols)[i], nrow(vals)),
                          Rows = non_na,
                          Codes = vals)
        names(tmp)[3] <- "Codes"
        
        collapsed <- paste(flags, collapse = "|")
        tmp$kick_out <- grepl(collapsed, tmp$Codes)
        tmp <- tmp[tmp$kick_out == TRUE, ]
        
        outs[[i]] <- tmp
    }
    
    outs2 <- dplyr::bind_rows(outs) %>% 
        mutate(Species = str_remove(Species, "F_"))
    
    # now in the data-only frame, replace with NAs
    # loop through each species in output, ID rows, and replace
    for(i in seq_along(unique(outs2$Species))){
        nm <- unique(outs2$Species)[i]
        rows <- outs2 %>% 
            filter(Species == nm) %>% 
            select(Rows) %>% 
            unlist()
        data_alone[rows, nm] <- NA
    }
    
    return(data_alone)
}


lump_species <- function(data, summ_fun, n){
    summ <- data %>% 
        group_by(Species) %>% 
        summarize(y_mean = mean(Cover, na.rm = TRUE),
                  y_quantile = quantile(Cover, probs = 0.95, na.rm = TRUE),
                  y_nonzero = sum(Cover != 0, na.rm = TRUE)) %>% 
        ungroup()
    
    if(summ_fun == "mean"){
        tops <- summ %>% 
            slice_max(order_by = y_mean, n = n) %>% 
            select(Species) %>% 
            unlist()
    }
    
    if(summ_fun == "quantile"){
        tops <- summ %>% 
            slice_max(order_by = y_quantile, n = n) %>% 
            select(Species) %>% 
            unlist()
    }
    
    if(summ_fun == "nonzero"){
        tops <- summ %>% 
            slice_max(order_by = y_nonzero, n = n) %>% 
            select(Species) %>% 
            unlist()
    }
    
    out <- data %>% 
        mutate(Species = case_when(!(Species %in% tops) ~ "Other",
                                   .default = Species)) %>% 
        group_by(Reserve, SiteID, TransectID, PlotID,
                 Vegetation_Zone,
                 Year, Month, Day,
                 Species) %>% 
        summarize(Cover = sum(Cover, na.rm = TRUE)) %>% 
        ungroup()
    
    return(out)
    
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