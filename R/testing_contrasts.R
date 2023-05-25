trnds <- emtrends(mod_nested2_lme4, 
                  pairwise ~ Vegetation_Zone, 
                  var = "Years_sinceStart")

pwpp(trnds)
cld(trnds) # typical compact letter display
test <- cld(trnds, Letters = letters)
testletters <- multcompLetters(test)

test2 <- glht(mod_nested2_lme4, mcp(Vegetation_Zone = "Tukey"))
cld(test2)
plot(cld(test2))

test3 <- test %>% 
    mutate(.group = str_trim(.group))


# letters just above each point
ggplot(test3, aes(x = reorder(Vegetation_Zone, Years_sinceStart.trend),
                  y = Years_sinceStart.trend,
                  label = .group)) +
    geom_point() +
    geom_text(nudge_y = max(test3$Years_sinceStart.trend)/15)



# letters all at the top
ggplot(test3, aes(x = reorder(Vegetation_Zone, Years_sinceStart.trend),
                  y = Years_sinceStart.trend,
                  label = .group)) +
    geom_point() +
    geom_text(aes(y = max(test3$Years_sinceStart.trend)),
              nudge_y = max(test3$Years_sinceStart.trend)/10) 


# error bars
ggplot(test3, aes(x = reorder(Vegetation_Zone, Years_sinceStart.trend),
                  y = Years_sinceStart.trend,
                  label = .group)) +
    geom_point() +
    geom_errorbar(aes(ymin = lower.CL, 
                      ymax = upper.CL)) +
    geom_text(aes(y = max(test3$upper.CL)),
              nudge_y = max(test3$upper.CL)/10) 


# my favorite so far
ggplot(test3, aes(x = reorder(Vegetation_Zone, Years_sinceStart.trend),
                  y = Years_sinceStart.trend,
                  label = .group,
                  color = Vegetation_Zone)) +
    geom_pointrange(aes(ymin = lower.CL, 
                      ymax = upper.CL),
                    linewidth = 0.8) +
    geom_text(aes(y = max(test3$upper.CL)),
              nudge_y = max(test3$upper.CL)/10) +
    theme(legend.position = "none")



ggplot(test3, aes(x = reorder(Vegetation_Zone, Years_sinceStart.trend),
                  y = Years_sinceStart.trend,
                  label = .group,
                  color = Vegetation_Zone)) +
    geom_crossbar(aes(ymin = lower.CL, 
                        ymax = upper.CL)) +
    geom_text(aes(y = max(test3$upper.CL)),
              nudge_y = max(test3$upper.CL)/10) +
    theme(legend.position = "none")




# some help with letters and plotting
# https://schmidtpaul.github.io/DSFAIR/compactletterdisplay.html
# https://rcompanion.org/handbook/G_06.html - scroll down to “one-way estimated marginal means and plot”



performance::check_normality(mod_plots_lme4)
plot(performance::check_normality(mod_plots_lme4))
plot(performance::check_heteroskedasticity(mod_plots_lme4))
performance::r2(mod_plots_lme4)

MuMIn::r.squaredGLMM(mod_plots_lme4)

# For our example, suppose, based on subject-matter considerations, 
# that two means that differ by less than 1.0 can be considered equivalent. 
# In the emmeans setup, we specify that we want equivalence testing simply 
# by providing this nonzero threshold value as a delta argument. 
# In addition, we typically will not make multiplicity adjustments to 
# equivalence tests. 
# above quote from https://cran.r-project.org/web/packages/emmeans/vignettes/re-engineering-clds.html

# for veg data, what would be significant? Difference of 5%/year? 
cld(trnds, delta = 0.05, adjust = "none") # equivalence sets


emm_example("cld-multcomp")

