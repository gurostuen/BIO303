library(bio303.practicals)
library(tidyverse)
library(analogue)
library(ggvegan)
library(vegan)

data(atl_spp, package = "bio303.practicals")
data(atl_env, package = "bio303.practicals")


## Task 1 ##
dca <- decorana(atl_spp) 
dca                             # Long gradient
decorana(sqrt(atl_spp))         # Unimodal analyses are appropriate


## Task 2 ##
PCA <- rda(atl_spp, scale = TRUE)
plot (PCA)                     # Horseshoe artefact?
procrustes(atl_spp, PCA, symmetric = TRUE)

CA <- cca(atl_spp)
plot(CA)
procrustes(atl_spp, CA, symmetric = TRUE)   # Best?

plot(dca)
procrustes(atl_spp, dca, symmetric = TRUE)

PCoA <- dbrda(atl_spp ~ 1, distance = "bray")
plot(PCoA)                     # Looks weird ?
procrustes(atl_spp, PCoA, symmetric = TRUE)

d <- vegdist(atl_spp, "bray")
plot(dist(atl_env$WinSST), d) # distances saturating at one
d <- stepacross(d, toolong = .9) # use extended similarities
plot(dist(atl_env$WinSST), d) # more linear
PCoA2 <- capscale(d ~ 1, comm = atl_spp)

NMDS <- metaMDS(atl_spp)
plot(NMDS)                     # Curved ?
procrustes(atl_spp, NMDS, symmetric = TRUE)

pc <- prcurve(sqrt(atl_spp), plotit = TRUE)
plot(pc)
atl_env |>  mutate(lambda = pc$lambda) |> 
  ggplot(aes(WinSST, lambda)) +
  geom_point()


list(dca, CA, PCA, PCoA, NMDS) |> 
  map(~scores(.x, choice = 1, disp = "sites")) |> 
  map(~cor(.x, atl_env$SumSST)) # dca, CA, PCoA and NMDS have best correlation with SST
cor(pc$lambda, atl_env$SumSST) # dca still slightly better tha pc

list(CA, PCA, PCoA, PCoA2) |> 
  map(~(.x$CA$eig[2] / .x$CA$eig[1]))
dca$evals[2]/dca$evals[1] # PCoA2 almost as low as DCA

# CA is better than PCA, but still does not cope well with this gradient. DCA, PCoA with ex diss and nmds are all OK.


## Task 3 ##
cca <- cca(atl_spp ~ ., data = atl_env)
plot(cca)

ca <- cca(atl_spp ~ 1, data = atl_env) # empty model
add1(ca, scope = formula(cca), test = "perm") # Winter SST is best variable (lowest AIC)
cca2 <- cca(atl_spp ~ WinSST, data = atl_env)
summary(cca2, display = NULL) # Constrained axis accounts for 39.6% of the inertia

anova(cca, cca2)


## Task 4 ##
vp <- varpart(sqrt(atl_spp), atl_env$WinSST, atl_env$Salinity)
plot(vp) 