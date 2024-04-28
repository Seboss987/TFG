
#Paquetes necesarios

install.packages("multcomp")
library(multcomp)

install.packages("multcompView")
library(multcompView)

install.packages("car")
library(car)

#Creación de la tabla de datos empleada

Country <- FAMES_TODOS$Country
Type <- FAMES_TODOS$Type
Total<-as.numeric(FAMES_TODOS$Total)
`Gram+`<-as.numeric(FAMES_TODOS$`Gram+`)
`Gram-`<-as.numeric(FAMES_TODOS$`Gram-`)
`GP/GN` <-as.numeric(FAMES_TODOS$`GP/GN`)
Bacteria<-as.numeric(FAMES_TODOS$Bacteria)
Actino<-as.numeric(FAMES_TODOS$Actino)
Fungi<-as.numeric(FAMES_TODOS$Fungi)
`F/B`<-as.numeric(FAMES_TODOS$`F/B`)

GNANOVA <- data.frame(Country=unlist(Country),
                      Type=unlist(Type),
                      Total=unlist(Total),
                      `GramP`=unlist(`Gram+`),
                      `GramN`=unlist(`Gram-`),
                      `RatioGPGN`=unlist(`GP/GN`),
                      Bacteria=unlist(Bacteria),
                      Actino=unlist(Actino),
                      Fungi=unlist(Fungi),
                      `RatioFB`=unlist(`F/B`))

#Test Shapiro para observar Distribución normal

for(i in colnames(GNANOVA[,3:10])){
  fracciones<-names(GNANOVA[i])
  ST<-shapiro.test(GNANOVA[, i])
  if(ST$p.value>0.05){
    print("---------")
    print(fracciones)
    print("---------")
    print("Distribucion normal sin log")
    print(ST$p.value)
  } else {
    ST<-shapiro.test(log(GNANOVA[, i]))
    if(ST$p.value>0){
      print("---------")
      print(fracciones)
      print("---------")
      print("Distribucion normal con log")
      print(ST$p.value)
    }
  }
}

#Test Levene para observar equidad de varianza

for(i in colnames(GNANOVA[,3:10])){
  fracciones<-names(GNANOVA[i])
  LTc<-leveneTest(log(GNANOVA[, i]) ~ factor(Country), data = GNANOVA)
  LTt<-leveneTest(log(GNANOVA[, i]) ~ factor(Type), data = GNANOVA)
  LTd<-leveneTest(log(GNANOVA[, i]) ~ factor(Country) * factor(Type), data = GNANOVA)
  print("---------")
  print(fracciones)
  print("---------")
  print("Test Levene - Type")
  print(LTt)
  print("---------")
  print("Test Levene - Country")
  print(LTc)
  print("---------")
  print("Test Levene - Double")
  print(LTd)
  print("---------")
}

# Test ANOVA y Tukey para observar variación significativa

for (i in colnames(GNANOVA[, 3:10])) {
  fracciones<-names(GNANOVA[i])
  ANt <- summary(aov(log(GNANOVA[, i]) ~ factor(Type), data=GNANOVA))
  ANc <- summary(aov(log(GNANOVA[, i]) ~ factor(Country), data=GNANOVA))
  ANg <- summary(aov(log(GNANOVA[, i]) ~ factor(Type) * factor(Country), data=GNANOVA))
  TKg <- TukeyHSD(aov(log(GNANOVA[, i]) ~ factor(Type) * factor(Country), data=GNANOVA))
  TKvst <- multcompLetters(TKg$`factor(Type)`[, 4])
  TKvsc <- multcompLetters(TKg$`factor(Country)`[, 4])
  TKvsg <- multcompLetters(TKg$`factor(Type):factor(Country)`[, 4])
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

# Calculo de Medianas para la tabla de resultados

NMedian <- GNANOVA[GNANOVA$Type == "Natural",]
NMV <- c(median(NMedian$Total), median(NMedian$GramP),
         median(NMedian$GramN), median(NMedian$RatioGPGN), 
         median(NMedian$Bacteria), median(NMedian$Actino),
         median(NMedian$Fungi), median(NMedian$RatioFB))
print(NMV)

MMedian <- GNANOVA[GNANOVA$Type == "Maize",]
MMV <- c(median(MMedian$Total), median(MMedian$GramP),
         median(MMedian$GramN), median(MMedian$RatioGPGN), 
         median(MMedian$Bacteria), median(MMedian$Actino),
         median(MMedian$Fungi), median(MMedian$RatioFB))
print(MMV)

PMedian <- GNANOVA[GNANOVA$Type == "Potato",]
PMV <- c(median(PMedian$Total), median(PMedian$GramP),
         median(PMedian$GramN), median(PMedian$RatioGPGN), 
         median(PMedian$Bacteria), median(PMedian$Actino),
         median(PMedian$Fungi), median(PMedian$RatioFB))
print(PMV)
median(PMedian$RatioGPGN)

FRMedian <- GNANOVA[GNANOVA$Country == "France",]
FRMV <- c(median(FRMedian$Total), median(FRMedian$GramP),
          median(FRMedian$GramN), median(FRMedian$RatioGPGN), 
          median(FRMedian$Bacteria), median(FRMedian$Actino),
          median(FRMedian$Fungi), median(FRMedian$RatioFB))
print(FRMV)

ITMedian <- GNANOVA[GNANOVA$Country == "Italy",]
ITMV <- c(median(ITMedian$Total), median(ITMedian$GramP),
          median(ITMedian$GramN), median(ITMedian$RatioGPGN), 
          median(ITMedian$Bacteria), median(ITMedian$Actino),
          median(ITMedian$Fungi), median(ITMedian$RatioFB))
print(ITMV)

PLMedian <- GNANOVA[GNANOVA$Country == "Poland",]
PLMV <- c(median(PLMedian$Total), median(PLMedian$GramP),
          median(PLMedian$GramN), median(PLMedian$RatioGPGN), 
          median(PLMedian$Bacteria), median(PLMedian$Actino),
          median(PLMedian$Fungi), median(PLMedian$RatioFB))
print(PLMV)

PTMedian <- GNANOVA[GNANOVA$Country == "Portugal",]
PTMV <- c(median(PTMedian$Total), median(PTMedian$GramP),
          median(PTMedian$GramN), median(PTMedian$RatioGPGN), 
          median(PTMedian$Bacteria), median(PTMedian$Actino),
          median(PTMedian$Fungi), median(PTMedian$RatioFB))
print(PTMV)

SPMedian <- GNANOVA[GNANOVA$Country == "Spain",]
SPMV <- c(median(SPMedian$Total), median(SPMedian$GramP),
          median(SPMedian$GramN), median(SPMedian$RatioGPGN), 
          median(SPMedian$Bacteria), median(SPMedian$Actino),
          median(SPMedian$Fungi), median(SPMedian$RatioFB))
print(SPMV)





