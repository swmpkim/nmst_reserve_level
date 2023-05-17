# load(here::here("data", "intermediate", "CBV_dfs.Rdata"))
load(here::here("data", "intermediate", "GND_dfs.Rdata"))

library(lme4)
library(tidyverse)

bySp <- bySp |> 
    dplyr::mutate(Date_centered = Date - median(Date))

byGroup <- byGroup |> 
    dplyr::mutate(Date = lubridate::decimal_date(lubridate::ymd(paste(Year, Month, Day, sep = "-"))),
                  Date_centered = Date - median(Date))

mod <- lm(`H-Halophyte` ~ Date_centered*Vegetation_Zone,
          data = byGroup)
anova(mod)
summary(mod)


mod2 <- lmer(`H-Halophyte` ~ Date_centered*Vegetation_Zone + (1|TransectID/PlotID),
            data = byGroup)
anova(mod2)
summary(mod2)




mod3 <- lm(EIR ~ Date_centered*Vegetation_Zone,
          data = bySp)
anova(mod3)
summary(mod3)


mod4 <- lmer(EIR ~ Date_centered*Vegetation_Zone + (1|TransectID/PlotID),
             data = bySp)
anova(mod4)
summary(mod4)



by_zone <- function(data, response, method = "lm"){
    # response should be a character vector
    dat <- data
    zones <- unique(dat$Vegetation_Zone)
    
    # resp <- response
    for(i in seq_along(zones)){
        subs <- filter(dat, 
                       Vegetation_Zone == zones[i])
        
        if(method == "lm"){
            formula <- eval(paste0("`", response, "` ~ Date_centered"))
            mod <- lm(formula,
                      data = subs)
        }

        if(method == "lmer"){
            formula <- eval(paste0("`", response, "` ~ Date_centered + (1|SiteID/TransectID/PlotID)"))
            mod <- lmer(formula,
                      data = subs)
        }
        
        
        cat(zones[i])
        cat('\n')
        print(anova(mod))
        cat('\n\n')
    }
}


test <- bySp 

by_zone(test, "EIR")
by_zone(test, "Spartina alterniflora")


test2 <- byGroup 

by_zone(test2, "H-Halophyte")
by_zone(test2, "H-Halophyte", method = "lmer")
