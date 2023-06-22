# render the Veg analyses Rmd for whichever reserves I want 
# (which is currently the main 4 I've been working with)
# this might be a good script to use in general - could loop through
# all of a reserve's components, if there are multiples

reserves <- c("GND", "GTM", "CBV", "NIW")

strt<-Sys.time()
for(i in seq_along(reserves)){
    outname <- paste0("Veg_analyses_", reserves[i], ".html")
    
    xfun::Rscript_call(
        rmarkdown::render,
        list(input = here::here("R", "01_Veg_analyses_outputOption1.Rmd"), 
             params = list("file_code" = reserves[i]),
             output_file = here::here("output", outname))
    )
    # per Yihui Xie, this function is equivalent to hitting the 'Knit' button -
    # it will run each markdown doc in a new R session
    # and so won't pollute one reserve's results with another's
    # https://bookdown.org/yihui/rmarkdown-cookbook/rmarkdown-render.html
    

}

Sys.time() - strt
# 2.26 minutes for these 4 reserves
