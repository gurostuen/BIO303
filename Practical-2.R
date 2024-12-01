library(bio303.practicals)
library(densityClust)
library(tidyverse)
library(cluster)
library(vegan)


## Task 1.1 ##
data(ponds_spp, package = "bio303.practicals")


## Task 1.2 ##
d <- vegdist(ponds_spp, method = "bray")
vegdist(ponds_spp, method = "euclidean")


## Task 1.3 ##
cs <- hclust(d, method = "single")
plot(cs)
rect.hclust(hc, k = 2)
ggdendro::ggdendrogram(cs)
cst <- cutree(cs, k = 2)

cc <- hclust(d, method = "complete")
plot(cc)
rect.hclust(hc, k = 2)
ggdendro::ggdendrogram(cc)
cct <- cutree(cc, k = 2)

cw <- hclust(d, method = "ward.D")
plot(cw)
rect.hclust(cw, k = 2)
ggdendro::ggdendrogram(cw)
cwt <- cutree(cw, k = 2)

table(cct, cwt)


## Task 1.4 ##
km <- kmeans(sqrt(ponds_spp), centers = 2)
km$tot.withinss

ss <- map(.x = 1:10, ~kmeans(x = sqrt(ponds_spp), centers = .x))
purrr::map_dbl(ss, "tot.withinss")

tibble(k = 1:10, tot.within = ss) |> 
  ggplot(aes(x = k, y = tot.within)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = 1:10)

# Comparison with hclust
km$clu
table(cst, km$clu)

cKM_ssi <- cascadeKM(data = sqrt(ponds_spp), inf.gr = 2, sup.gr = 10, criterion = "ssi") 
plot(cKM_ssi)

cKM_cal <- cascadeKM(data = sqrt(ponds_spp), inf.gr = 2, sup.gr = 10, criterion = "calinski")
plot(cKM_cal)


## Task 1.5 ##
fanny(d, k = 2, memb.exp = 1.20)


## Task 1.6 ##
cmd <- capscale(d ~ 1) # runs principal coordinates analysis on the distance matrix d
plot(cmd, type = "n")
sco <- scores(cmd, display = "sites")
stars(f$membership, location = sco, draw.segments = TRUE, scale = FALSE, 
      add = TRUE, len = .4, labels = NULL) # where f is output of fuzzy clustering

km <- kmeans(sqrt(ponds_spp), centers = 2) # kmeans with 3 clusters

sapply(1:max(km$cluster), function(n){
  x <- sco[km$cluster == n,]
  polygon(x[chull(x), ], border = 2)
}) # where km is the output of the kmeans clustering analysis


## Task 1.7 ##
clust <- densityClust(d, gaussian = TRUE)
plot(clust) 

tibble(rho = clust$rho, delta = clust$delta, id = names(clust$rho)) %>% 
  ggplot(aes(x = rho, y = delta, label = id)) +
  geom_point() +
  ggrepel::geom_text_repel()

# No real evidence of any clusters
clust2 <- findClusters(clust, rho = 2, delta = 0.6)
plotMDS(clust2)


## Task 2 ##
n <- matrix(rnorm(40, 2), ncol = 2)
dn <- dist(n)
d <- vegdist(n, method = "bray")

cnw <- hclust(dn, method = "ward.D")
cnc <- hclust(dn, method = "complete")
cna <- hclust(dn, method = "average")

clu <- cutree(cnw, 2)
plot(n, col = clu)

par(mfrow = c(2, 2)) 
plot(cnw)
rect.hclust(cnw, k = 2) # Finds the best clusters

plot(cnc)
rect.hclust(cnc, k = 2)

plot(cna)
rect.hclust(cna, k = 2)