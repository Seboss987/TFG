
install.packages("multcomp")
library(multcomp)

install.packages("multcompView")
library(multcompView)

install.packages("car")
library(car)

#Preparación de tabla de datos usada

ITANOVA <- data.frame(subset(GNANOVA, Country == "Italy"))

#Test Shapiro

for(i in colnames(ITANOVA[,3:11])){
  fracciones<-names(ITANOVA[i])
  ST<-shapiro.test(ITANOVA[, i])
  if(ST$p.value>0.05){
    print("---------")
    print(fracciones)
    print("---------")
    print("Distribucion normal sin log")
    print(ST$p.value)
    print(ST$method)
    print(ST$data.name)
  } else {
    ST<-shapiro.test(log(ITANOVA[, i]))
    if(ST$p.value>0.01){
      print("---------")
      print(fracciones)
      print("---------")
      print("Distribucion normal con log")
      print(ST$p.value)
      print(ST$method)
    }
  }
}

#Test Levene

for(i in colnames(ITANOVA[,4:11])){
  fracciones<-names(ITANOVA[i])
  LT<-leveneTest(log(ITANOVA[, i]) ~ factor(TypeC), data = ITANOVA)
  print("---------")
  print(fracciones)
  print("---------")
  print("Test Levene - Type")
  print(LT)
}

# Test ANOVA y Tukey #1

for (i in colnames(ITANOVA[, 4:11])) {
  fracciones<-names(ITANOVA[i])
  ANt <- summary(aov(log(ITANOVA[, i]) ~ factor(TypeC), data=ITANOVA))
  ANc <- summary(aov(log(ITANOVA[, i]) ~ factor(Country), data=ITANOVA))
  ANg <- summary(aov(log(ITANOVA[, i]) ~ factor(TypeC) * factor(Country), data=ITANOVA))
  TKg <- TukeyHSD(aov(log(ITANOVA[, i]) ~ factor(TypeC) * factor(Country), data=ITANOVA))
  TKvst <- multcompLetters(TKg$`factor(TypeC)`[, 4])
  TKvsc <- multcompLetters(TKg$`factor(Country)`[, 4])
  TKvsg <- multcompLetters(TKg$`factor(TypeC):factor(Country)`[, 4])
  print("------------------")
  print(fracciones)
  print("------------------")
  print("Type")
  print("------------------")
  print(ANt)
  print(TKvst$Letters)
  print("------------------")
  print("Country")
  print("------------------")
  print(ANc)
  print(TKvsc$Letters)
  print("------------------")
  print("Type:Country")
  print("------------------")
  print(ANg)
  print(TKvsg$Letters)
}

tukeyT <- TukeyHSD(aov(Total ~ factor(Country), data = ITANOVA))
tukeyTvs <- multcompLetters(tukeyT$`factor(Country)`[,4])
tukeyTvs

# Calculo de Medianas

NMedian <- ITANOVA[ITANOVA$TypeC == 'Natural',]
NMV <- c(median(NMedian$Total), median(NMedian$GramP),
         median(NMedian$GramN), median(NMedian$RatioGPGN), 
         median(NMedian$Bacteria), median(NMedian$Actino),
         median(NMedian$Fungi), median(NMedian$RatioFB))
print(NMV)

MMedian <- ITANOVA[ITANOVA$TypeC == 'Maize',]
MMV <- c(median(MMedian$Total), median(MMedian$GramP),
         median(MMedian$GramN), median(MMedian$RatioGPGN), 
         median(MMedian$Bacteria), median(MMedian$Actino),
         median(MMedian$Fungi), median(MMedian$RatioFB))
print(MMV)

PMedian <- ITANOVA[ITANOVA$TypeC == 'Potato',]
PMV <- c(median(PMedian$Total), median(PMedian$GramP),
         median(PMedian$GramN), median(PMedian$RatioGPIT), 
         median(PMedian$Bacteria), median(PMedian$Actino),
         median(PMedian$Fungi), median(PMedian$RatioFB))
print(PMV)
median(PMedian$RatioGPIT)






