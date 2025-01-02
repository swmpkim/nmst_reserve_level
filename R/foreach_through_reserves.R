library(foreach)
library(doParallel)

# identify them
veg_files <- dir(here::here("data"),
                 pattern = "_veg.xlsx$")
reserves <- stringr::str_remove(veg_files, "_veg.xlsx")

# reserves with updated metadata files
# 1/1/25
reserves <- c("GND", "CBM-JB", "GRB", "GTM")

# 
# cl<-makeCluster(8)  
# registerDoParallel(cl)
strt<-Sys.time()

# process all stations
foreach(res = reserves) %do% {
    outname <- paste0("Veg_analyses_", res, "_", Sys.Date(), ".html")
    
    try(
        xfun::Rscript_call(
            rmarkdown::render,
            list(input = here::here("R", "99_Veg_analyses.Rmd"), 
                 params = list("file_code" = res),
                 output_file = here::here("output", outname))
        )
    )
    
}

Sys.time() - strt
# 21.7 minutes for 19 files

# stopCluster(cl)

beepr::beep(sound = 8)
