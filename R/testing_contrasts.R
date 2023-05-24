trnds <- emtrends(mod_nested2_lme4, 
                  pairwise ~ Vegetation_Zone, 
                  var = "Years_sinceStart")

pwpp(trnds)
cld(trnds) # typical compact letter display
test <- cld(trnds, Letters = letters)
testletters <- multcompLetters(test)


# some help with letters and plotting
# https://schmidtpaul.github.io/DSFAIR/compactletterdisplay.html


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

