# render the Veg analyses Rmd for whichever files we want 

# if your reserve has multiple input files, include them in the 'files' line
# e.g.:
# files <- c("APA-LSM", "APA-PC")

# select everything ( ctrl + a ) and run it ( ctrl + enter )
# then check our 'output' folder for the results!

library(rmarkdown)
library(here)
library(xfun)

veg_files <- dir(here::here("data"),
                 pattern = "_veg.xlsx$")
files <- stringr::str_remove(veg_files, "_veg.xlsx")

# to test -
# files <- c("ACE-EIN", "MAR", "HUD-PIER", "JAC", "GND", "GRB")

# List of files to process
failed_files <- c() # Create a log for failed files

strt<-Sys.time()

for(i in seq_along(files)){
    outname <- paste0(files[i], "_spatial-plots_", Sys.Date(), ".html")
    
    # Try rendering the Rmd file
    result <- try({
        xfun::Rscript_call(
            rmarkdown::render,
            list(
                input = here::here("R", "999_spatial.plots.Rmd"), 
                params = list("file_code" = files[i]),
                output_file = here::here("spatial_plots", "rendered", outname)
            )
        )
    }, silent = TRUE) # Suppress error messages
    
    # Check if an error occurred
    if (inherits(result, "try-error")) {
        cat("Error in processing file:", files[i], "\n")
        failed_files <- c(failed_files, files[i]) # Log failed files
    }
}

# Report results
if (length(failed_files) > 0) {
    cat("\nThe following files failed to process:\n")
    print(failed_files)
} else {
    cat("\nAll files processed successfully!\n")
    
}

coord_files <- dir(here::here("spatial_plots", "spreadsheets"),
                   full.names = TRUE)

library(dplyr)
coords_sprdsht <- coord_files %>% 
    purrr::map(read.csv) %>% 
    bind_rows()

write.csv(coords_sprdsht, file = here::here("spatial_plots", 
                                            "all_coords.csv"),
          row.names = FALSE,
          na = "")


Sys.time() - strt
beepr::beep(sound = 8)
