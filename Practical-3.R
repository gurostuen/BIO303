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
pro_PCA <- procrustes(sim_env1, PCA, symmetric = TRUE)
pro_PCA            # Procrustes sum of squares = 0.24
plot(pro_PCA)

CA <- cca(sim_spp1)
plot(CA)
plot_grid(CA)
pro_CA <- procrustes(sim_env1, CA, symmetric = TRUE)
pro_CA            # Procrustes sum of squares = 0.10
plot(pro_CA)

dbRDA <- dbrda(sim_spp1 ~ Var1 + Var2, sim_env1, distance = "bray")
plot(dbRDA)
plot_grid(dbRDA)
pro_dbRDA <- procrustes(sim_env1, dbRDA, symmetric = TRUE)
pro_dbRDA         # Procrustes sum of squares = 0.089
plot(pro_dbRDA)

NMDS <- metaMDS(sim_spp1)
plot(NMDS)
plot_grid(NMDS)
pro_NMDS <- procrustes(sim_env1, NMDS, symmetric = TRUE)
pro_NMDS         # Procrustes sum of squares = 0.040
plot(pro_NMDS)


trans <- decostand(sim_spp1, method = "standardize")
NMDS2 <- metaMDS(trans)
plot(NMDS2)
plot_grid(NMDS2)
pro_NMDS2 <- procrustes(trans, NMDS2, symmetric = TRUE)
pro_NMDS2         # Procrustes sum of squares = 0.040
plot(pro_NMDS2)


## Task 2 ##
plot_grid(sim_env2)     # Baguette?
decorana(sim_spp2)      # DCA1 has length 3.2223

dca1 <- decorana(sim_spp2) 
plot(dca1)

pro <- procrustes(sim_env1, dca1, symmetric = TRUE, choices = 1:2)
pro                 # Procrustes sum of squares = 0.325
plot(pro)


NMDS <- metaMDS(sim_spp2)
plot(NMDS)
plot_grid(NMDS)
pro_NMDS <- procrustes(sim_env2, NMDS, symmetric = TRUE)
pro_NMDS         # Procrustes sum of squares = 0.064
plot(pro_NMDS)

dbRDA <- dbrda(sim_spp2 ~ Var1 + Var2, sim_env2, distance = "bray")
plot(dbRDA)
plot_grid(dbRDA)
pro_dbRDA <- procrustes(sim_env2, dbRDA, symmetric = TRUE)
pro_dbRDA         # Procrustes sum of squares = 0.38
plot(pro_dbRDA)

