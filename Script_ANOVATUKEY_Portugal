
install.packages("multcomp")
library(multcomp)

install.packages("multcompView")
library(multcompView)

install.packages("car")
library(car)

#Preparación de tabla de datos usada

PTANOVA <- data.frame(subset(GNANOVA, Country == "Portugal"))

#Test Shapiro

for(i in colnames(PTANOVA[,3:10])){
  fracciones<-names(PTANOVA[i])
  ST<-shapiro.test(PTANOVA[, i])
  if(ST$p.value>0.05){
    print("---------")
    print(fracciones)
    print("---------")
    print("Distribucion normal sin log")
    print(ST$p.value)
  } else {
    ST<-shapiro.test(log(PTANOVA[, i]))
    if(ST$p.value>0.01){
      print("---------")
      print(fracciones)
      print("---------")
      print("Distribucion normal con log")
      print(ST$p.value)
    }
  }
}

ST<-shapiro.test(log(PTANOVA[, 11]))
print(ST$p.value)

#Test Levene

for(i in colnames(PTANOVA[,3:10])){
  fracciones<-names(PTANOVA[i])
  LT<-leveneTest(log(PTANOVA[, i]) ~ factor(Type), data = PTANOVA)
  print("---------")
  print(fracciones)
  print("---------")
  print("Test Levene - Type")
  print(LT)
}

# Test ANOVA y Tukey #1

for (i in colnames(PTANOVA[, 3:10])) {
  fracciones<-names(PTANOVA[i])
  ANt <- summary(aov(log(PTANOVA[, i]) ~ factor(Type), data=PTANOVA))
  TKg <- TukeyHSD(aov(log(PTANOVA[, i]) ~ factor(Type), data=PTANOVA))
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

ANt <- summary(aov(RatioGPGN ~ factor(Type), data=PTANOVA))
print(ANt)
tukeyT <- TukeyHSD(aov(RatioGPGN ~ factor(Type), data = PTANOVA))
tukeyTvs <- multcompLetters(tukeyT$`factor(Type)`[,4])
tukeyTvs

# Calculo de Medianas

NMedian <- PTANOVA[PTANOVA$Type == 'Natural',]
NMV <- c(median(NMedian$Total), median(NMedian$GramP),
         median(NMedian$GramN), median(NMedian$RatioGPGN), 
         median(NMedian$Bacteria), median(NMedian$Actino),
         median(NMedian$Fungi), median(NMedian$RatioFB))
print(NMV)

MMedian <- PTANOVA[PTANOVA$Type == 'Maize',]
MMV <- c(median(MMedian$Total), median(MMedian$GramP),
         median(MMedian$GramN), median(MMedian$RatioGPGN), 
         median(MMedian$Bacteria), median(MMedian$Actino),
         median(MMedian$Fungi), median(MMedian$RatioFB))
print(MMV)

PMedian <- PTANOVA[PTANOVA$Type == 'Potato',]
PMV <- c(median(PMedian$Total), median(PMedian$GramP),
         median(PMedian$GramN), median(PMedian$RatioGPGN), 
         median(PMedian$Bacteria), median(PMedian$Actino),
         median(PMedian$Fungi), median(PMedian$RatioFB))
print(PMV)







