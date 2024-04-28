
#Paquetes necesarios

install.packages("multcomp")
library(multcomp)
install.packages("multcompView")
library(multcompView)
install.packages("emmeans")
library(emmeans)
install.packages("tidyverse")
library(tidyverse)

#Preparar dataframe a usar

DataGG <- GNANOVA

DataGG <- DataGG[, c(2, 3, 4, 5, 6, 7, 8, 9, 10)]

conflicted::conflicts_prefer(dplyr::recode)

DataGG <- DataGG %>%
  mutate(Type = recode(Type, `Natural` = "Natural", `Maize` = "Maíz", 
                       `Potato` = "Patata" ))

#Análisis de ANOVA y TUKEY para las letras de significancia

DataGG$Type<-factor(DataGG$Type)

ANGR <- apply(DataGG[,2:ncol(DataGG)], 2, function(x) aov(log(x) ~ Type, data = DataGG))

Clrs <- lapply(ANGR, FUN = function(i) emmeans::emmeans(i, specs = "Type"))

Ltsg <- lapply(Clrs, cld, adjust= "sidak", Letters = letters,
               alpha = 0.05, reversed = T) 

Ltsg_df <- bind_rows(Ltsg, .id = "variable")

#Preparar Dataframe para la grafica

DataGG <- DataGG %>% pivot_longer(-Type)


Type <- DataGG$Type
Fraction <- DataGG$name
Quantity <- DataGG$value

DataGG <- data.frame(Type=Type,
                     Fraction=Fraction,
                     Quantity=Quantity)

DataGG <- transform(DataGG, Fraction=factor(Fraction,levels=c("Total","GramP",
                                                              "GramN", "RatioGPGN",
                                                              "Bacteria", "Actino",
                                                              "Fungi", "RatioFB")))

DataGG$Type <- factor(DataGG$Type , levels=c("Natural", "Maíz", "Patata"))

#Gráfica

ggplot(DataGG,aes(Type, Quantity)) +
  geom_boxplot(aes(fill=Type), outlier.size = 0.2) +
  geom_dotplot(binaxis="y", stackdir="centerwhole",stackratio = 0.1, dotsize=0.2) +
  ylab("Quantity") +
  facet_wrap(~Fraction, nrow = 2, scales = "free", 
             labeller = labeller(Fraction=c(`Total`="Total", `GramP`="GP",`GramN`="GN", `Bacteria`="Bacteria", 
                                            `RatioGPGN`="GP:GN", `Actino`="Actino", `Fungi`="Fungi",`RatioFB`="F:B"), 
                                            .default=label_parsed)) +
  stat_summary(geom = "text", fun = mean, label = trimws(Ltsg_df$.group), aes(y = 0), 
               size = 2.5) +
  labs(x = Uso~de~suelo, y = nmol~FAME~g^{-1}~suelo) +
  theme_bw(base_size = 8.5) +
  scale_fill_brewer(palette="Dark2")

