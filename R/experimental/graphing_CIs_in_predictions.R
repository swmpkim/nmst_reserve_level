# from: https://easystats.github.io/modelbased/
# can I use this for the prediction plots Brook was interested in? The CIs???


test <- emmip(mod_plts, Vegetation_Zone ~ Years_sinceStart,
             # cov.reduce = range,
             linearg = list(size = 1), CIs = TRUE,
             plotit = FALSE,
             at = list(Years_sinceStart = 0:5))

ggplot() +
    geom_ribbon(data = test, aes(x = Years_sinceStart,
                                 ymin = LCL,
                                 ymax = UCL,
                                 fill = Vegetation_Zone),
                alpha = 0.2) +
    geom_line(data = test, aes(x = Years_sinceStart,
                               y = yvar,
                               col = Vegetation_Zone,
                               linetype = Vegetation_Zone),
              linewidth = 0.8) +
    labs(x = "Years elapsed", 
         y = paste("Predicted", respn)) +
    theme_bw()



library(modelbased)
library(see)

data(mtcars)
mtcars$gear <- as.factor(mtcars$gear)
model <- lm(mpg ~ wt * gear, data = mtcars)

predicted <- estimate_expectation(model, data = "grid")
plot(predicted)


# for namaste purposes:
model <- mod_plts
predicted <- estimate_expectation(model, data = "grid")
plot(predicted, ribbon = list(alpha = 0.2),
     point = list(alpha = 0.5),
     line = list(alpha = 0.9, size = 0.8)) + 
    theme_bw() +
    facet_wrap(~Vegetation_Zone) + 
    labs(title = "Predicted response",
         x = "Years elapsed",
         y = respn)


ggplot() +
    geom_ribbon(data = predicted, aes(x = Years_sinceStart,
                                 ymin = CI_low,
                                 ymax = CI_high,
                                 fill = Vegetation_Zone),
                alpha = 0.2) +
    geom_line(data = predicted, aes(x = Years_sinceStart,
                               y = Predicted,
                               col = Vegetation_Zone,
                               linetype = Vegetation_Zone),
              linewidth = 0.8) +
    labs(x = "Years elapsed", 
         y = paste("Predicted", respn)) +
    theme_bw()



# namaste random effects
preds <- estimate_relation(model)
plot(preds, ribbon = list(alpha = 0))

model <- lmer(Reaction ~ Days + (1 | Subject), data = sleepstudy)
preds <- estimate_relation(model, include_random = TRUE)
plot(preds, ribbon = list(alpha = 0)) # Make CI ribbon transparent for clarity
# I'd want to not make the CI ribbon transparent....