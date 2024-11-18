library(GGally)
library(bio303.practicals)
library(vegan)
library(mgcv)

data(package = "bio303.practicals")


## 1.1 Loading data ##
data(ponds_spp, package = "bio303.practicals")
data(ponds_env, package = "bio303.practicals")


## 1.2 The environmental data ##
hist(ponds_env$Maxdept)
hist(ponds_env$Secchi)
hist(ponds_env$Chla)
hist(ponds_env$SO4)
hist(ponds_env$Cl)
hist(ponds_env$Ca)
hist(ponds_env$Mg)
hist(ponds_env$K)
hist(ponds_env$Na)
hist(ponds_env$NO3)
hist(ponds_env$SiO2)
hist(ponds_env$TP)
hist(ponds_env$Alkalinity)
hist(ponds_env$Conductivity)
hist(ponds_env$pH)

pairs(ponds_env)
ggpairs(ponds_env)


## 1.3 Diversity-environment relationships ##
shannon <- diversity(ponds_spp)
env_shannon <- dplyr::mutate(ponds_env, shannon)

plot_shannon_TP <- ggplot(env_shannon, aes(x = TP, y = shannon)) +
  geom_point() +
  geom_smooth()

plot_shannon_TP

cor.test(env_shannon$shannon,
         env_shannon$TP) # moderate positive correlation


## 1.4 Species-environment relationships ##
spp_TP <- dplyr::tibble(taxon = ponds_spp$AC013A, TP = ponds_env$TP)

plot_spp_TP <- ggplot(spp_TP, aes(x = TP, y = taxon)) +
  geom_point() +
  geom_smooth() 

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

species_composition2 <- vegdist(ponds_spp, method = "euclidean")

plot(TP_space, species_composition2)
mantel(TP_space, species_composition2) # no correlation

plot(all, species_composition2)
mantel(species_composition2, all)
     