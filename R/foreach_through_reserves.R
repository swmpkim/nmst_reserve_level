library(foreach)
library(doParallel)

reserves <- c("GND", "CBV", "NIW", "TST")
# removed GTM until I can figure out why it took so long on the last run


cl<-makeCluster(6)  
registerDoParallel(cl)
strt<-Sys.time()

# process all stations
foreach(res = reserves) %dopar% {
    outname <- paste0("Veg_analyses_", res, ".html")
    
    xfun::Rscript_call(
        rmarkdown::render,
        list(input = here::here("R", "01_Veg_analyses_outputOption1.Rmd"), 
             params = list("file_code" = res),
             output_file = here::here("output", outname))
    )
    
}

Sys.time() - strt
# 1.2 mins (vs 2.26 mins with 'for' loop) with 6 cores
# 1.4 mins with 9 cores (?) guess more isn't necessarily better
# though it could be for more than the 4 test reserves

# after adding multivariate: 
# 1.9 min; 2.1 min

stopCluster(cl)
