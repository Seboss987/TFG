
# Paquetes necesarios

install.packages("vegan")
library(vegan)
install.packages("ggplot2")
library(ggplot2)
install.packages("RColorBrewer")
library(RColorBrewer)
install.packages("ggpubr")
library(ggpubr)
install.packages("tidyr")
library("tidyr")
install.packages("conflicted")
library(conflicted)

# PERMANOVA - Análisis de la significancia de los factores de País y Uso de Suelo
# sobre la estructura de biomasa microbiana

Country = as.factor (Abundancia$Country)
Type = as.factor (Abundancia$Type)
AG=Abundancia[,3:ncol(Abundancia)]
mAG = as.matrix(AG)
adonis2(mAG~Type*Country, distance="bray", perm=9999)

#La estructura de la comunidad esta mas influenciada por el uso del suelo
# que por el pais 

# ANOSIM - Análisis para determinar que las muestras en función de los factores
# no son significativamente iguales

anosim(mAG, grouping = Type, distance="bray", perm=9999)
anosim(mAG, grouping = Country, distance="bray", perm=9999)

#Test NMDS para ordenar espacialmente las muestras en función de su disimilitud

nmds=metaMDS(mAG, distance="bray", autotransform=FALSE)

Scores = as.data.frame(scores(nmds)$sites)
Scores$Type = Abundancia$Type
Scores$Country = Abundancia$Country

conflicted::conflicts_prefer(dplyr::recode)

Scores$Type <- factor(Scores$Type, level= c("Natural","Maize", 
                                                 "Potato"))
Scores <- Scores %>%
  mutate(Type = recode(Type, `Natural` = "Natural", `Maize` = "Maíz", 
                       `Potato` = "Patata" ))

Scores$Country <- factor(Scores$Country, level= c("France", "Italy", "Poland", 
                                                            "Portugal", "Spain"))
Scores <- Scores %>%
  mutate(Country = recode(Country, `France` = "Francia", `Italy` = "Italia",
                          `Poland` = "Polonia", `Portugal` = "Portugal",
                          `Spain` = "España"))

###stat_chull
ggplot (Scores, aes (x=NMDS1, y=NMDS2)) + 
  geom_point(size=5,aes(color = Type, shape = Country))  + 
  theme_bw(base_size = 8.5) + 
  scale_color_brewer(palette="Dark2") + 
  scale_fill_brewer(palette="Dark2") +
  stat_chull(aes(color = Type, fill = Type), alpha = 0.3, geom = "polygon")
