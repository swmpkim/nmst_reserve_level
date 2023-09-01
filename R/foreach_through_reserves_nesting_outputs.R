library(foreach)
library(doParallel)

reserves <- c("GND", "CBV", "NIW", "GTM")

cl<-makeCluster(8)  
registerDoParallel(cl)
strt<-Sys.time()

# process all stations
foreach(res = reserves) %dopar% {
    outname <- paste0("Veg_analyses_nesting_effects", res, "_", Sys.Date(), ".html")
    
    try(
        xfun::Rscript_call(
            rmarkdown::render,
            list(input = here::here("R", "01dev_Veg_analyses_nesting_outputs.Rmd"), 
                 params = list("file_code" = res),
                 output_file = here::here("output", outname))
        )
    )
    
}

Sys.time() - strt
# 1.2 mins (vs 2.26 mins with 'for' loop) with 6 cores
# 1.4 mins with 9 cores (?) guess more isn't necessarily better
# though it could be for more than the 4 test reserves

# after adding multivariate: 
# 1.9 min; 2.1 min

stopCluster(cl)

beepr::beep(sound = 8)
