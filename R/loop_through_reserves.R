# render the Veg analyses Rmd for whichever reserves I want 
# (which is currently the main 4 I've been working with)
# this might be a good script to use in general - could loop through
# all of a reserve's components, if there are multiples

reserves <- c("GND", "GTM", "CBV", "NIW")

for(i in seq_along(reserves)){
    outname <- paste0("Veg_analyses_", reserves[i], ".html")
    rmarkdown::render(input = here::here("R", "01_Veg_analyses_outputOption1.Rmd"), 
                      params = list("file_code" = reserves[i]),
                      output_file = here::here("output", outname))
}


