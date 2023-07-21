mod <- lm(Sepal.Width ~ Petal.Width, data = iris)
summary(mod)
anova(mod)

pf(22.91, 1, 148, lower.tail = FALSE)
pt(4.786, 148, lower.tail = FALSE) * 2

sqrt(22.91)
# F is t^2; t df = F denom df = nrow - 2


mod <- lm(Sepal.Width ~ Species, data = iris)
summary(mod)
anova(mod)
# df for F are 2 and 147 this time

# from anova code
P <- pf(f, df, dfr, lower.tail = FALSE)
# dfr is df.residual(mod)

# but permanova is a permutation test to get the p-value
# which is why it won't line up with pf outputs
# in theory could still take square root and figure out a tukey value, but that's all
# distribution-based so it's not great



pf(3.1917, 4, 78, lower.tail = FALSE)
pf(1.21, 4, 70, lower.tail = FALSE)




nfams <- length(zones)
pmvs_summ <- bind_rows(pmvs, .id = "Vegetation Zone") %>% 
    filter(term == "Time_group") %>% 
    select('Vegetation Zone', R2, statistic, df, p.value) %>% 
    mutate(nfams = nfams,
           Tukey_p = 1 - ptukey(sqrt(statistic) * sqrt(2), nfams, df))

adjsp <- 1 - ptukey(pmvs_summ$statistic * sqrt(2), nfams, p)



trnds <- emtrends(mod_plts, 
         pairwise ~ Vegetation_Zone, 
         var = "Years_sinceStart")$emtrends

pairwise <- data.frame(emtrends(mod_plts, 
                     pairwise ~ Vegetation_Zone, 
                     var = "Years_sinceStart")$contrasts)

# https://stats.stackexchange.com/a/402137
# he missed that you need absolute value, but I figured it out by comparing
pairwise$Tukey_p <- ptukey(abs(pairwise$t.ratio) * sqrt(2),
                           nmeans = 6,
                           df = pairwise$df, 
                           lower.tail = FALSE)

pairwise2 <- data.frame(emtrends(mod_plts, 
                                 pairwise ~ Vegetation_Zone, 
                                 var = "Years_sinceStart",
                                 adjust = "bonferroni")$contrasts)

pairwise2$Tukey_p <- ptukey(abs(pairwise2$t.ratio) * sqrt(2),
                           nmeans = 6,
                           df = pairwise2$df, 
                           lower.tail = FALSE)
pairwise2$unadj_p <- pt(abs(pairwise2$t.ratio),
                       df = pairwise2$df,
                       lower.tail = FALSE) * 2  # two-tailed
pairwise2$Bonf_p <- pairwise2$unadj_p * nrow(pairwise2)
pairwise2$Bonf_p <- ifelse(pairwise2$Bonf_p > 1, 1, pairwise2$Bonf_p)


pairwise2$test_tukeyp <- pairwise2$unadj_p * 6
pairwise2$test_tukeyp <- ifelse(pairwise2$test_tukeyp > 1, 1, pairwise2$test_tukeyp)
