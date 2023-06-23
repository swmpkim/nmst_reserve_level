# from: https://easystats.github.io/modelbased/
# can I use this for the prediction plots Brook was interested in? The CIs???

library(modelbased)
library(see)

data(mtcars)
mtcars$gear <- as.factor(mtcars$gear)
model <- lm(mpg ~ wt * gear, data = mtcars)

predicted <- estimate_expectation(model, data = "grid")
plot(predicted)


model <- lmer(Reaction ~ Days + (1 | Subject), data = sleepstudy)
preds <- estimate_relation(model, include_random = TRUE)
plot(preds, ribbon = list(alpha = 0)) # Make CI ribbon transparent for clarity
# I'd want to not make the CI ribbon transparent....