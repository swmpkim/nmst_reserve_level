data(dune)
data(dune.env)
## default test by terms
adonis2(dune ~ Management*A1, data = dune.env)
## overall tests
adonis2(dune ~ Management*A1, data = dune.env, by = NULL)

### Example of use with strata, for nested (e.g., block) designs.
datdune <- expand.grid(rep=gl(2,1), NO3=factor(c(0,10)),field=gl(3,1) )
datdune
Agropyron <- with(datdune, as.numeric(field) + as.numeric(NO3)+2) +rnorm(12)/2
Schizachyrium <- with(datdune, as.numeric(field) - as.numeric(NO3)+2) +rnorm(12)/2
total <- Agropyron + Schizachyrium
dotplot(total ~ NO3, datdune, jitter.x=TRUE, groups=field,
        type=c('p','a'), xlab="NO3", auto.key=list(columns=3, lines=TRUE) )

Y <- data.frame(Agropyron, Schizachyrium)
mod <- metaMDS(Y, trace = FALSE)
plot(mod)
### Ellipsoid hulls show treatment
with(datdune, ordiellipse(mod, field, kind = "ehull", label = TRUE))
### Spider shows fields
with(datdune, ordispider(mod, field, lty=3, col="red"))

### Incorrect (no strata)
adonis2(Y ~ NO3, data = datdune, permutations = 199)
## Correct with strata
with(datdune, adonis2(Y ~ NO3, data = datdune, permutations = 999, 
                  strata = field))

adonis2(Y ~ NO3, data = datdune, permutations = 999, 
        strata = datdune$field)



dis <- vegdist(resp_pm) # Bray-Curtis distances
## First 16 sites grazed, remaining 8 sites ungrazed
mod <- betadisper(dis, expl_pm$Time_group)
mod
boxplot(mod)
permutest(mod)
plot(mod)

