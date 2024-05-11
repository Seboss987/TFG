
install.packages("multcomp")
library(multcomp)

install.packages("multcompView")
library(multcompView)

install.packages("car")
library(car)

#Preparación de tabla de datos usada

PLANOVA <- data.frame(subset(GNANOVA, Country == "Poland"))

#Test Shapiro

for(i in colnames(PLANOVA[,3:10])){
  fracciones<-names(PLANOVA[i])
  ST<-shapiro.test(PLANOVA[, i])
  if(ST$p.value>0.05){
    print("---------")
    print(fracciones)
    print("---------")
    print("Distribucion normal sin log")
    print(ST$p.value)
    print(ST$method)
    print(ST$data.name)
  } else {
    ST<-shapiro.test(log(PLANOVA[, i]))
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

for(i in colnames(PLANOVA[,3:10])){
  fracciones<-names(PLANOVA[i])
  LT<-leveneTest(log(PLANOVA[, i]) ~ factor(Type), data = PLANOVA)
  print("---------")
  print(fracciones)
  print("---------")
  print("Test Levene - Type")
  print(LT)
}

LT<-leveneTest(RatioFB ~ factor(Type), data = PLANOVA)
print(LT)

# Test ANOVA y Tukey #1

for (i in colnames(PLANOVA[, 3:10])) {
  fracciones<-names(PLANOVA[i])
  ANt <- summary(aov(log(PLANOVA[, i]) ~ factor(Type), data=PLANOVA))
  TKg <- TukeyHSD(aov(log(PLANOVA[, i]) ~ factor(Type), data=PLANOVA))
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

ANt <- summary(aov(GramN ~ factor(Type), data=PLANOVA))
print(ANt)
ANt <- summary(aov(RatioFB ~ factor(Type), data=PLANOVA))
print(ANt)
tukeyT <- TukeyHSD(aov(GramN ~ factor(Type), data = PLANOVA))
tukeyTvs <- multcompLetters(tukeyT$`factor(Type)`[,4])
tukeyTvs
tukeyT <- TukeyHSD(aov(RatioFB ~ factor(Type), data = PLANOVA))
tukeyTvs <- multcompLetters(tukeyT$`factor(Type)`[,4])
tukeyTvs

# Calculo de Medianas

NMedian <- PLANOVA[PLANOVA$Type == 'Natural',]
NMV <- c(median(NMedian$Total), median(NMedian$GramP),
         median(NMedian$GramN), median(NMedian$RatioGPGN), 
         median(NMedian$Bacteria), median(NMedian$Actino),
         median(NMedian$Fungi), median(NMedian$RatioFB))
print(NMV)

MMedian <- PLANOVA[PLANOVA$Type == 'Maize',]
MMV <- c(median(MMedian$Total), median(MMedian$GramP),
         median(MMedian$GramN), median(MMedian$RatioGPGN), 
         median(MMedian$Bacteria), median(MMedian$Actino),
         median(MMedian$Fungi), median(MMedian$RatioFB))
print(MMV)

PMedian <- PLANOVA[PLANOVA$Type == 'Potato',]
PMV <- c(median(PMedian$Total), median(PMedian$GramP),
         median(PMedian$GramN), median(PMedian$RatioGPGN), 
         median(PMedian$Bacteria), median(PMedian$Actino),
         median(PMedian$Fungi), median(PMedian$RatioFB))
print(PMV)







