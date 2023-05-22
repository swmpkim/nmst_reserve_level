# load(here::here("data", "intermediate", "CBV_dfs.Rdata"))
load(here::here("data", "intermediate", "GND_dfs.Rdata"))
# load(here::here("data", "intermediate", "NIW_dfs.Rdata"))

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

# Halophyte ----
mod <- lm(`H-Halophyte` ~ Years_sinceStart*Vegetation_Zone,
          data = byGroup)
anova(mod)
summary(mod)
emtrends(mod, pairwise ~ Vegetation_Zone, var = "Years_sinceStart")
emmip(mod, Vegetation_Zone ~ Years_sinceStart, 
      cov.reduce = range,
      linearg = list(size = 1))


mod2 <- lmer(`H-Halophyte` ~ Years_sinceStart*Vegetation_Zone + (1|SiteID/TransectID/PlotID),
            data = byGroup)
anova(mod2)
summary(mod2)
emtrends(mod2, pairwise ~ Vegetation_Zone, var = "Years_sinceStart")
emmip(mod2, Vegetation_Zone ~ Years_sinceStart, 
      cov.reduce = range,
      linearg = list(size = 1))


# Brackish ----
mod5 <- lm(`B-Brackish` ~ Years_sinceStart*Vegetation_Zone,
          data = byGroup)
anova(mod5)
summary(mod5)
emtrends(mod5, pairwise ~ Vegetation_Zone, var = "Years_sinceStart")
emmip(mod5, Vegetation_Zone ~ Years_sinceStart, 
      cov.reduce = range,
      linearg = list(size = 1)) + ggtitle("Brackish - lm")


mod6 <- lmer(`B-Brackish` ~ Years_sinceStart*Vegetation_Zone + (1|SiteID/TransectID/PlotID),
             data = byGroup)
anova(mod6)
summary(mod6)
emtrends(mod6, pairwise ~ Vegetation_Zone, var = "Years_sinceStart")
emmip(mod6, Vegetation_Zone ~ Years_sinceStart, 
      cov.reduce = range,
      linearg = list(size = 1)) + ggtitle("Brackish - mixed mod")
# pseudo-rsquared
MuMIn::r.squaredGLMM(mod6)

## Compare lm to lmm
MuMIn::model.sel(mod5, mod6)


# EIR ----
mod3 <- lm(EIR ~ Years_sinceStart*Vegetation_Zone,
          data = bySp)
anova(mod3)
summary(mod3)
emtrends(mod3, pairwise ~ Vegetation_Zone, var = "Years_sinceStart")
# plot the predictions
emmip(mod3, Vegetation_Zone ~ Years_sinceStart, 
      cov.reduce = range,
      linearg = list(size = 1)) + ggtitle("EIR - lm")

bySp %>% 
    group_by(SiteID, TransectID, PlotID, Years_sinceStart)

mod4 <- lmer(EIR ~ Years_sinceStart*Vegetation_Zone + (1|SiteID/TransectID/PlotID),
             data = bySp)
anova(mod4)
summary(mod4)

emtrends(mod4, pairwise ~ Vegetation_Zone, var = "Years_sinceStart")
# plot the predictions
emmip(mod4, Vegetation_Zone ~ Years_sinceStart, 
      cov.reduce = range,
      linearg = list(size = 1)) + ggtitle("EIR - mixed mod")

# residuals vs. fitted plot
plot(mod4)
# qq plot
qqnorm(residuals(mod4))
# leverage
ggplot(data.frame(lev=hatvalues(mod4),pearson=residuals(mod4,type="pearson")),
       aes(x=lev,y=pearson)) +
    geom_point() +
    theme_bw()

# by variable
ggplot(data.frame(x1=mod4@frame[["EIR"]],pearson=residuals(mod4,type="pearson")),
       aes(x=x1,y=pearson)) +
    geom_point() +
    theme_bw()

# pseudo-rsquared
# marginal and conditional. 
# marginal is variance explained by fixed effects.
# conditional is variance explained by entire model.
MuMIn::r.squaredGLMM(mod4)



bySp2 <- bySp %>% 
   mutate(Plot2 = paste(SiteID, TransectID, PlotID, sep = "-"))

mod7 <- lmer(EIR ~ Years_sinceStart*Vegetation_Zone + (1|Plot2),
             data = bySp2)
emmip(mod7, Vegetation_Zone ~ Years_sinceStart, 
      cov.reduce = range,
      linearg = list(size = 1)) + ggtitle("EIR - 1|PlotOnly")

library(nlme)
mod8 <- lme(EIR ~ Years_sinceStart*Vegetation_Zone, random = ~1|SiteID/TransectID/PlotID,
            data = bySp2,
            na.action = na.omit)

# marginal seems to be similar to r^2 from simple lm
summary(mod3)$r.squared

# as of MuMIn version 1.41.0, r.squaredGLMM returns a revised statistics based on Nakagawa et al. (2017) paper.

## Compare lm to lmm
MuMIn::model.sel(mod3, mod4)



# refs ----
###### Pull out the groupwise slopes!!!!! 
# simple effects was the term I needed
# https://stats.oarc.ucla.edu/r/seminars/interactions-r/
# https://cran.r-project.org/web/packages/emmeans/vignettes/interactions.html 

# Also does pairwise contrasts - can see if slopes are different from each other, pairwise


# function no longer needed? ----
# need to do the following only if the interaction in the "big" model is significant
# update - shouldn't need to do this at all, because of emmeans::emtrends
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
