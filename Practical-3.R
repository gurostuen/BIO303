library(bio303.practicals)
library("vegan")


## Task 1.1 ##
data(sim_env1, package = "bio303.practicals")
data(sim_spp1, package = "bio303.practicals")


## Task 1.2 ##
plot_grid(sim_env1)


## Task 1.3 ##
decorana(sim_spp1)   # DCA1 has length 4.0703
                     # Correspondence analysis is appropriate - unimodal
dca1 <- decorana(sim_spp1) 
plot(dca1)


## Task 1.4 ##
plot_grid(dca1)      # Triangle configuration
plot_grid(dca1, choice = 2:3)


## Task 1.5 ##
pro <- procrustes(sim_env1, dca1, symmetric = TRUE, choices = 1:2)
pro                 # Procrustes sum of squares = 0.091
plot(pro)


## Task 1.6 ##
PCA <- rda(sim_spp1)     
plot(PCA)
plot_grid(PCA)
procrustes(sim_env1, PCA, symmetric = TRUE)   # Procrustes sum of squares = 0.24

PCA1s <- rda(decostand(sim_spp1, "hellinger"))
plot (PCA1s)
plot_grid(PCA1s)
procrustes(sim_env1, PCA1s, symmetric = TRUE) # Procrustes sum of squares = 0.07


CA <- cca(sim_spp1)
plot(CA)
plot_grid(CA)
procrustes(sim_env1, CA, symmetric = TRUE)    # Procrustes sum of squares = 0.10

CA1s <- cca(decostand(sim_spp1, "hellinger"))
plot(CA1s)
plot_grid(CA1s)
procrustes(sim_env1, CA1s, symmetric = TRUE)  # Procrustes sum of squares = 0.08


dbRDA <- dbrda(sim_spp1 ~ 1, distance = "bray")
plot(dbRDA)
plot_grid(dbRDA)
procrustes(sim_env1, dbRDA, symmetric = TRUE) # Procrustes sum of squares = 0.11


NMDS <- metaMDS(sim_spp1)
plot(NMDS)
plot_grid(NMDS)
procrustes(sim_env1, NMDS, symmetric = TRUE)  # Procrustes sum of squares = 0.04

trans <- decostand(sim_spp1, method = "standardize")
NMDS2 <- metaMDS(trans)
plot(NMDS2)
plot_grid(NMDS2)
procrustes(trans, NMDS2, symmetric = TRUE)    # Procrustes sum of squares = 0.88


## Task 2 ##
plot_grid(sim_env2)     # Baguette?
decorana(sim_spp2)      # DCA1 has length 3.2223

dca2 <- decorana(sim_spp2) 
plot_grid(dca2)
procrustes(sim_env2, dca2, symmetric = TRUE, choice = 1:2)                # 0.06


pca2 <- rda(sim_spp2)
plot_grid(pca2)
procrustes(sim_env2, pca2, symmetric = TRUE)                              # 0.43

pca2s <- rda(decostand(sim_spp2, "hellinger"))
plot_grid(pca2s)
procrustes(sim_env2, pca2s, symmetric = TRUE)                             # 0.47


ca2 <- cca(sim_spp2)
plot_grid(ca2)
procrustes(sim_env2, ca2, symmetric = TRUE)                               # 0.39


dbrda2 <- dbrda(sim_spp2 ~ 1, distance = "bray")
plot_grid(dbrda2)
procrustes(sim_env2, dbrda2, symmetric = TRUE)                            # 0.40


nmds2 <- metaMDS(sim_spp2)
plot_grid(nmds2)
procrustes(sim_env2, nmds2, symmetric = TRUE)                             # 0.06