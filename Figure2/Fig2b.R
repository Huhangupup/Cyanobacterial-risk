setwd("D:/R/LAKE/LM")

# Load package
library(ggplot2)
library(ggpubr)
library(car)
library(patchwork)

Agriculture <- read.csv("demo.csv",row.names = 1)



#calculate R2 and P value
fit_lm<-lm(cent~gene, data = Agriculture)
summary(fit_lm)
lm(formula = Agriculture$cent ~ Agriculture$gene)
#Draw a picture
p1 <- ggplot(Agriculture,aes(x=gene,y=cent))+
  geom_point(size=2.6,aes(color=habitat),alpha=0.8)+
  geom_smooth(aes(color=habitat), method = 'lm', se = T, level=0.95,size=1.0) +
  scale_color_manual(values=c("#794296"))+
  scale_y_continuous(expand = c(0, 0), limit = c(0,50))+
  scale_x_continuous(expand = c(0, 0), limit = c(0,5))+
  theme_classic()+labs(y="risk",x="pred")+
  annotate('text', label = 'Adjusted R2 0.2124 , p-value < 0.001 , y = -0.033x + 2.6', x =0.5, y = 90000, size =3.7,color="black")+
  theme(axis.text=element_text(colour='black',size=9))+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())
p1