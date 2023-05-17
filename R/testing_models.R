# load(here::here("data", "intermediate", "CBV_dfs.Rdata"))
load(here::here("data", "intermediate", "GND_dfs.Rdata"))

library(lme4)
library(tidyverse)
library(emmeans)

bySp <- bySp |> 
    dplyr::mutate(Date_centered = Date - median(Date),
                  Years_sinceStart = Date - min(Date))

byGroup <- byGroup |> 
    dplyr::mutate(Date = lubridate::decimal_date(lubridate::ymd(paste(Year, Month, Day, sep = "-"))),
                  Date_centered = Date - median(Date),
                  Years_sinceStart = Date - min(Date))

mod <- lm(`H-Halophyte` ~ Date_centered*Vegetation_Zone,
          data = byGroup)
anova(mod)
summary(mod)


mod2 <- lmer(`H-Halophyte` ~ Date_centered*Vegetation_Zone + (1|TransectID/PlotID),
            data = byGroup)
anova(mod2)
summary(mod2)

emmeans(mod2, "Vegetation_Zone")


mod3 <- lm(EIR ~ Years_sinceStart*Vegetation_Zone,
          data = bySp)
anova(mod3)
summary(mod3)

###### Pull out the groupwise slopes!!!!! 
# simple effects was the term I needed
# https://stats.oarc.ucla.edu/r/seminars/interactions-r/
# https://cran.r-project.org/web/packages/emmeans/vignettes/interactions.html 

# Also does pairwise contrasts - can see if slopes are different from each other, pairwise
emtrends(mod3, pairwise ~ Vegetation_Zone, var = "Years_sinceStart")
# plot the predictions
emmip(mod3, Vegetation_Zone ~ Years_sinceStart, cov.reduce = range)


# want to pull out slope by zone, from the model output
# from research gate - remove the simple effect for the numerical variable to get all the slopes??
# https://www.researchgate.net/post/How_can_I_get_confidence_intervals_for_multiple_slopes_in_R
mod3b <- lm(EIR ~ Vegetation_Zone + Date:Vegetation_Zone,
            data = bySp)
summary(mod3b)



mod4 <- lmer(EIR ~ Date_centered*Vegetation_Zone + (1|TransectID/PlotID),
             data = bySp)
anova(mod4)
summary(mod4)

# need to do the following only if the interaction in the "big" model is significant

by_zone <- function(data, response, method = "lm"){
    # response should be a character vector
    dat <- data
    zones <- unique(dat$Vegetation_Zone)
    
    # resp <- response
    for(i in seq_along(zones)){
        cat(zones[i])
        cat('\n')
        
        subs <- filter(dat, 
                       Vegetation_Zone == zones[i])
        
        if(method == "lm"){
            formula <- eval(paste0("`", response, "` ~ Date_centered"))
            mod <- lm(formula,
                      data = subs)
            print(summary(mod))
            cat('\n\n')
        }

        if(method == "lmer"){
            formula <- eval(paste0("`", response, "` ~ Date_centered + (1|SiteID/TransectID/PlotID)"))
            mod <- lmer(formula,
                      data = subs)
            print(anova(mod))
            cat('\n\nCoefficients\n')
            print(fixef(mod))
            cat('\n\n\n')
        }
        
        
        

    }
}


test <- bySp 

by_zone(test, "EIR")
by_zone(test, "Spartina alterniflora")


test2 <- byGroup 

by_zone(test2, "H-Halophyte")
by_zone(test2, "H-Halophyte", method = "lmer")
