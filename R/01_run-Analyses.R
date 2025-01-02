# render the Veg analyses Rmd for whichever files we want 

# if your reserve has multiple input files, include them in the 'files' line
# e.g.:
# files <- c("APA-LSM", "APA-PC")

# select everything ( ctrl + a ) and run it ( ctrl + enter )
# then check our 'output' folder for the results!

files <- c("GND", "GRB")

for(i in seq_along(files)){
    outname <- paste0(files[i], "_Veg-analyses_", Sys.Date(), ".html")
    
    xfun::Rscript_call(
        rmarkdown::render,
        list(input = here::here("R", "99_Veg_analyses.Rmd"), 
             params = list("file_code" = files[i]),
             output_file = here::here("output", outname))
    )
    # per Yihui Xie, this function is equivalent to hitting the 'Knit' button -
    # it will run each markdown doc in a new R session
    # and so won't pollute one reserve's results with another's
    # https://bookdown.org/yihui/rmarkdown-cookbook/rmarkdown-render.html
    

}
