setwd("D:/R/LAKE/NMDS")
#Load R package
library(vegan)
library(ggplot2)
library(dplyr)

#Load microbial data
df <- read.csv("gene.csv",row.names = 1,check.names = F)
df <- data.frame(t(df))
group <- read.csv("Group.csv",row.names = 1,check.names = F)

#Calculate the Bray-curtis distance
bray_dis <- vegdist(df, method = 'bray')

#Output distance matrix
write.csv(as.matrix(bray_dis),"Microbial_bray_distance.csv")

#Sort the Bray-curtis distance matrix
bray_dis <- as.dist(bray_dis)

#NMDS sort, defining 2 dimensions
nmds_dis <- metaMDS(bray_dis, k = 2)

#NMDS stress value
nmds_dis$stress
#Quadrat score
nmds_dis_site <- data.frame(as.matrix(nmds_dis$points))

#Output quadrat score
write.csv(nmds_dis_site,"Nmds_dis_site.csv")

#Evaluation of the NMDS model
par(mfrow = c(1, 2))
stressplot(nmds_dis, main = 'Shepard diagram')
gof <- goodness(nmds_dis)
plot(nmds_dis,type = 'text', main = 'Degree of fitting')
points(nmds_dis, pch = '.', cex = gof * 100, col = 'red')

#Adonis test
adonis <- adonis2(df~group,group, permutations = 999, method="bray")
adonis

#Visualization

nmds <- cbind(nmds_dis_site, group)

p <- ggplot(data = nmds, aes(MDS1, MDS2,group)) +
  geom_point(aes(color = group), size = 2.5, alpha = 0.8) +
  scale_color_manual(limits = c("Australia","Bolivia",	"Brazil","Canada","China","Czechia","Finland","France","India","Israel","Italy","Japan","Republic of Korea","Russian Federation","Spain","Sweden","Switzerland","United Kingdom","United States of America"), 
                     values = c("#794296", "#a77822", "#147655", "#6bc1a8", "#24a3d6", "#e79d7b","#7FC97F", "#BEAED4","#FDC086","#F0027F","#FFFF99","#386CB0","#BF5B17","#CCFF00","#666633","#CC9933","#9900CC","#FF00CC","#993300"))+
  labs(x = 'NMDS1', y = 'NMDS2', title = '') +
  annotate('text', label = paste('Adonis : R2 = 0.12 p = 0.001 Stress =', round(nmds_dis$stress, 4)), x = -1, y = 2, size = 4, colour = 'black')
p