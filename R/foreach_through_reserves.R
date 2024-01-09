library(foreach)
library(doParallel)

# reserves <- c("GND", "CBV", "NIW", "GTM")

# batch one of manual files
reserves1 <- c("APA-LSM",  "APA-PC", "CBM-JB", "CBM-MB", "DEL-BCR",
               "ELK", "JAC", "NOC-RC", "NOC-ZI", "WEL")

# batch two of manual files
reserves2 <- c("CBV", "DEL-SJR", "GTM", "MAR", "NAR", "NIW",
              "NOC-MI", "SOS", "TJR")

# batch 3
reserves3 <- c("CBM-OPC", "GND", "GRB", "HUD-PIER", "HUD-TIV",
               "KAC", "WQB", "ACE")

reserves <- sort(c(reserves1, reserves2, reserves3))

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
            list(input = here::here("R", "01_Veg_analyses.Rmd"), 
                 params = list("file_code" = res),
                 output_file = here::here("output", outname))
        )
    )
    
}

Sys.time() - strt
# 21.7 minutes for 19 files

# stopCluster(cl)

beepr::beep(sound = 8)
