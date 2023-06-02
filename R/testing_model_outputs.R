library(easystats)
estimate_slopes(mod_nested_lme4)
estimate_slopes(mod_nested_lme4, trend = "Years_sinceStart", at = "Vegetation_Zone")

model_parameters(mod_nested_lme4, ci_method = "kenward")

cld(mod_nested_lme4)


test <- lmerTest::as_lmerModLmerTest(mod_nested_lme4)
summary(test)
anova(test) # satterthwaite is default
anova(test, ddf = "Kenward-Roger")

p <- emmip(mod_nested2_lme4, Vegetation_Zone ~ Years_sinceStart,
cov.reduce = range,
linearg = list(size = 1), CIs = TRUE)

p + geom_point(data = bySp, aes(x = Years_sinceStart, y = EIR,
                         col = Vegetation_Zone),
               alpha = 0.5)

ggplot() + geom_point(data = bySp, aes(x = Years_sinceStart, y = EIR,
                                       col = Vegetation_Zone),
                      alpha = 0.5)

pdf <- emmip(mod_nested2_lme4, Vegetation_Zone ~ Years_sinceStart,
             cov.reduce = range,
             linearg = list(size = 1), CIs = TRUE,
             plotit = FALSE)

p1 <- ggplot() +
    geom_line(data = pdf, aes(x = Years_sinceStart,
                  y = yvar,
                  col = Vegetation_Zone,
                  linetype = Vegetation_Zone),
              linewidth = 0.8) +
    theme_bw()



p2 <- ggplot() +
    geom_jitter(data = bySp, aes(x = Years_sinceStart, y = EIR,
                                 col = Vegetation_Zone),
                alpha = 0.7) +
    geom_line(data = pdf, aes(x = Years_sinceStart,
                              y = yvar,
                              col = Vegetation_Zone,
                              linetype = Vegetation_Zone),
              linewidth = 0.8) +
    facet_wrap(~Vegetation_Zone) +
    theme_bw() +
    theme(legend.position = "none")

p1 + ggtitle("Mixed Model fit")
p2

library(patchwork)
p1/p2


ggplot() +
    geom_jitter(data = bySp, aes(x = Years_sinceStart, y = EIR,
                                 col = Vegetation_Zone,
                                 shape = Vegetation_Zone),
                alpha = 0.3) +
    geom_line(data = pdf, aes(x = Years_sinceStart,
                              y = yvar,
                              col = Vegetation_Zone,
                              linetype = Vegetation_Zone),
              linewidth = 0.8) +
    theme_bw() 
