library(foreach)
library(doParallel)

reserves <- c("GND", "CBV", "NIW", "GTM")
# 
# # batch one of manual files
# reserves1 <- c("APA-LSM",  "APA-PC", "CBM-JB", "CBM-MB", "DEL-BCR",
#                "ELK", "JAC", "NOC-RC", "NOC-ZI", "WEL")
# 
# # batch two of manual files
# reserves2 <- c("CBV", "DEL-SJR", "GTM", "MAR", "NAR", "NIW",
#               "NOC-MI", "SOS", "TJR")
# 
# # batch 3
# reserves3 <- c("CBM-OPC", "GND", "GRB", "HUD-PIER", "HUD-TIV",
#                "KAC", "WQB", "ACE")
# 
# reserves <- sort(c(reserves1, reserves2, reserves3))
# 
# # ACE only
# reserves <- c("ACE-EIN", "ACE-EIS")
# 
# # updated data files, 5/9/24
# reserves <- sort(c("GRB", "DEL-BCR", "DEL-SJR", "GTM", "JAC", "MAR"))
# 
# # updated data files, 6/13/2024
# reserves <- sort(c("WEL", "ELK", "NOC-RC", "NOC-ZI", "CBM-MB", "CBM-OPC",
#                    "WQB", "NIW"))
# 
# # updated for real, 6/14/24
# reserves <- c("ELK", "WEL")

# re-run all; 6/14/24

# identify them
veg_files <- dir(here::here("data"),
                 pattern = "_veg.xlsx$")
reserves <- stringr::str_remove(veg_files, "_veg.xlsx")


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
