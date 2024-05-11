
install.packages("multcomp")
library(multcomp)

install.packages("multcompView")
library(multcompView)

install.packages("car")
library(car)

#Preparación de tabla de datos usada

FRANOVA <- data.frame(subset(GNANOVA, Country == "France"))

#Test Shapiro

for(i in colnames(FRANOVA[,3:10])){
  fracciones<-names(FRANOVA[i])
  ST<-shapiro.test(FRANOVA[, i])
  if(ST$p.value>0.05){
    print("---------")
    print(fracciones)
    print("---------")
    print("Distribucion normal sin log")
    print(ST$p.value)
    print(ST$method)
    print(ST$data.name)
  } else {
    ST<-shapiro.test(log(FRANOVA[, i]))
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

for(i in colnames(FRANOVA[,3:10])){
  fracciones<-names(FRANOVA[i])
  LT<-leveneTest(log(FRANOVA[, i]) ~ factor(Type), data = FRANOVA)
  print("---------")
  print(fracciones)
  print("---------")
  print("Test Levene - Type")
  print(LT)
}

LT<-leveneTest(RatioFB ~ factor(Type), data = FRANOVA)
print(LT)

# Test ANOVA y Tukey 

for (i in colnames(FRANOVA[, 3:10])) {
  fracciones<-names(FRANOVA[i])
  ANt <- summary(aov(log(FRANOVA[, i]) ~ factor(Type), data=FRANOVA))
  TKg <- TukeyHSD(aov(log(FRANOVA[, i]) ~ factor(Type), data=FRANOVA))
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

NMedian <- FRANOVA[FRANOVA$Type == 'Natural',]
NMV <- c(median(NMedian$Total), median(NMedian$GramP),
         median(NMedian$GramN), median(NMedian$RatioGPGN), 
         median(NMedian$Bacteria), median(NMedian$Actino),
         median(NMedian$Fungi), median(NMedian$RatioFB))
print(NMV)

MMedian <- FRANOVA[FRANOVA$Type == 'Maize',]
MMV <- c(median(MMedian$Total), median(MMedian$GramP),
         median(MMedian$GramN), median(MMedian$RatioGPGN), 
         median(MMedian$Bacteria), median(MMedian$Actino),
         median(MMedian$Fungi), median(MMedian$RatioFB))
print(MMV)

PMedian <- FRANOVA[FRANOVA$Type == 'Potato',]
PMV <- c(median(PMedian$Total), median(PMedian$GramP),
         median(PMedian$GramN), median(PMedian$RatioGPGN), 
         median(PMedian$Bacteria), median(PMedian$Actino),
         median(PMedian$Fungi), median(PMedian$RatioFB))
print(PMV)







