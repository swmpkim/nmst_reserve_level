library(foreach)
library(doParallel)

reserves <- c("GND", "GTM", "CBV", "NIW")


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

# after adding multivariate: 
# 1.9 min; 2.1 min

stopCluster(cl)
