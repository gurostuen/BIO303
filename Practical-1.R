library(GGally)
library(bio303.practicals)
library(tidyverse)
library(vegan)
library(mgcv)

data(package = "bio303.practicals")


## 1.1 Loading data ##
data(ponds_spp, package = "bio303.practicals")
data(ponds_env, package = "bio303.practicals")


## 1.2 The environmental data ##
ponds_env |> 
  pivot_longer(everything(), names_to = "variable", values_to = "value") |> 
  ggplot(aes(x = value)) +
  geom_histogram(bins = 10) +
  facet_wrap(~ variable, scales = "free_x")

ggpairs(ponds_env)


## 1.3 Diversity-environment relationships ##
shannon <- diversity(ponds_spp)
env_shannon <- dplyr::mutate(ponds_env, shannon)

plot_shannon_TP <- ggplot(env_shannon, aes(x = TP, y = shannon)) +
  geom_point() +
  geom_smooth(method = "lm")

plot_shannon_TP

cor.test(env_shannon$shannon,
         env_shannon$TP) # moderate positive correlation


## 1.4 Species-environment relationships ##
(common <- which(colSums(ponds_spp > 0) > 15))

spp_TP <- dplyr::tibble(taxon = ponds_spp$AC013A, TP = ponds_env$TP) # AC001A most common?

plot_spp_TP <- ggplot(spp_TP, aes(x = TP, y = taxon)) +
  geom_point() +
  geom_smooth(method = "lm") 

plot_spp_TP

gam_spp_TP <- gam(taxon ~ s(TP), data = spp_TP)
summary(gam_spp_TP) # significant relationship


## 1.5 Distance metrics ##
species_composition <- vegdist(ponds_spp, method = "bray")
TP_space <- vegdist(ponds_env$TP, method = "euclidean")

plot(TP_space, species_composition)
mantel(TP_space, species_composition) # significant correlation


all <- vegdist(ponds_env, method = "euclidean")
mantel(all, species_composition)
plot(all, species_composition)
  
selected_env <- ponds_env[, c ("Ca", "Maxdept", "Mg", "TP", "pH")]
bioenv(ponds_spp, selected_env) # Maxdept and TP are the best variables
# bioenv(ponds_spp, ponds_env, upto = 5)

species_composition2 <- vegdist(ponds_spp, method = "euclidean")

plot(TP_space, species_composition2)
mantel(TP_space, species_composition2) # no correlation

plot(all, species_composition2)
mantel(species_composition2, all)
     