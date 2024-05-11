
install.packages("multcomp")
library(multcomp)

install.packages("multcompView")
library(multcompView)

install.packages("car")
library(car)

#Preparación de tabla de datos usada

SPANOVA <- data.frame(subset(GNANOVA, Country == "Spain"))

#Test Shapiro

for(i in colnames(SPANOVA[,3:10])){
  fracciones<-names(SPANOVA[i])
  ST<-shapiro.test(SPANOVA[, i])
  if(ST$p.value>0.05){
    print("---------")
    print(fracciones)
    print("---------")
    print("Distribucion normal sin log")
    print(ST$p.value)
  } else {
    ST<-shapiro.test(log(SPANOVA[, i]))
    if(ST$p.value>0.01){
      print("---------")
      print(fracciones)
      print("---------")
      print("Distribucion normal con log")
      print(ST$p.value)
    }
  }
}

#Test Levene

for(i in colnames(SPANOVA[,3:10])){
  fracciones<-names(SPANOVA[i])
  LT<-leveneTest((SPANOVA[, i]) ~ factor(Type), data = SPANOVA)
  print("---------")
  print(fracciones)
  print("---------")
  print("Test Levene - Type")
  print(LT)
}

# Test ANOVA y Tukey #1

for (i in colnames(SPANOVA[, 3:10])) {
  fracciones<-names(SPANOVA[i])
  ANt <- summary(aov((SPANOVA[, i]) ~ factor(Type), data=SPANOVA))
  TKg <- TukeyHSD(aov((SPANOVA[, i]) ~ factor(Type), data=SPANOVA))
  TKvst <- multcompLetters(TKg$`factor(Type)`[, 4])
  print("------------------")
  print(fracciones)
  print("------------------")
  print("Type")
  print("------------------")
  print(ANt)
  print(TKvst$Letters)
  print("------------------")
}

# Calculo de Medianas

NMedian <- SPANOVA[SPANOVA$Type == 'Natural',]
NMV <- c(median(NMedian$Total), median(NMedian$GramP),
         median(NMedian$GramN), median(NMedian$RatioGPGN), 
         median(NMedian$Bacteria), median(NMedian$Actino),
         median(NMedian$Fungi), median(NMedian$RatioFB))
print(NMV)

MMedian <- SPANOVA[SPANOVA$Type == 'Maize',]
MMV <- c(median(MMedian$Total), median(MMedian$GramP),
         median(MMedian$GramN), median(MMedian$RatioGPGN), 
         median(MMedian$Bacteria), median(MMedian$Actino),
         median(MMedian$Fungi), median(MMedian$RatioFB))
print(MMV)

PMedian <- SPANOVA[SPANOVA$Type == 'Potato',]
PMV <- c(median(PMedian$Total), median(PMedian$GramP),
         median(PMedian$GramN), median(PMedian$RatioGPGN), 
         median(PMedian$Bacteria), median(PMedian$Actino),
         median(PMedian$Fungi), median(PMedian$RatioFB))
print(PMV)
