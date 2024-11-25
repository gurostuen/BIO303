library(bio303.practicals)
library(tidyverse)
library(vegan)


## Task 1 ##
data(ponds_spp, package = "bio303.practicals")
data(ponds_env, package = "bio303.practicals")


## Task 2 ##
decorana(ponds_spp)
decorana(sqrt(ponds_spp))


## Task 3 ##
PCA <- rda(ponds_spp)
screeplot(PCA, bstick = TRUE) # PC1, PC2 and PC3 are interpretable, but with very long gradients?
plot(PCA)
PCA_sqrt <- rda(decostand(ponds_spp, method = "hellinger"))
screeplot(PCA_sqrt, bstick = TRUE) # PC1 and PC2 are interpretable
plot(PCA_sqrt)

CA <- cca(sqrt(ponds_spp))
screeplot(CA, bstick = TRUE) # CA1 and CA2 are interpretable for sqrt transformed data, but not raw data
plot(CA)

PCoA <- capscale(ponds_spp ~ 1, distance = "bray")
screeplot(PCoA)
plot(PCoA)

NMDS <- metaMDS(ponds_spp, k = 2)
screeplot(NMDS)
plot(NMDS)

# Choose CA because of horseshoe artefact in PCA
plot(CA, type = "n")
points(CA, disp = "species", pch = "+", col = "red")
points(CA, disp = "sites", pch = 21)
text(CA, disp = "species", cex = 0.5, col = "red")


## Task 4 ##
fit <- envfit(CA, ponds_env, perm = 999)
fit # TP, SiO2, Chla and Secchi are significantly correlated with CA1 and CA2

plot(CA)
plot(fit)
plot(fit, p.max = .05, col = "green")


## Task 5 ##
ordisurf(CA, ponds_env$TP)
ordisurf(CA, ponds_env$SiO2)
ordisurf(CA, ponds_env$Chla)
ordisurf(CA, ponds_env$Secchi)


## Task 6 ##
mod1 <- rda(decostand(ponds_spp, "hellinger") ~ ., data = ponds_env)
mod1 # ~ 18% of the variation is explained by the first axis
summary(mod1)
plot(mod1)

mod2 <- cca(sqrt(ponds_spp) ~ ., data = ponds_env)
mod2 # ~ 12% of the variation is explained by the first axis
summary(mod2)
plot(mod2)

mod3 <- capscale

# The constrained axes generally explain more of the variation than the unconstrained


## Task 7 ##
vif.cca(mod1) # Some are > 20

intercept <- rda(decostand(ponds_spp, "hellinger") ~ 1, data = ponds_env)
all <- rda(decostand(ponds_spp, "hellinger") ~ ., data = ponds_env)

model <- ordistep(intercept, scope = formula(all), direction = "forward")
model
model$anova
plot(model)