library(questionr)
library(tidyverse)
library(haven)
library(forcats)
library(labelled)
library(sjlabelled)
library(foreign)
library(janitor)
library(funModeling)
library(summarytools)
library(tidyr)
library(writexl)
library(gtsummary)
library(ggplot2)
library(plotrix)
library(gtsummary)
library(labelled)
library(tidyverse)
library(dplyr)
setwd("//profils.uppa.univ-pau.fr/folderredir/bkande/Desktop/Cours r/projet")
load("L1eco.RData")
base <- L1eco
attach(base)
View(base)
reussite <- recode(reussite,"1"="Oui","0"="Non")
reussite <- factor(reussite)
pcs <- factor(PCS,ordered = T,labels=1:4)
pcs <- recode_factor(pcs,
                     "1"="D´efavoris´e",
                     "2"="Moins d´efavoris´e",
                     "3"="Favoris´e",
                     "4"="Tr`es favoris´e")
str(base)
sexe <- recode_factor(SEXE,"F"="Feminin","M"="Masculin")


retard <-recode_factor(retard,
                       "HE"="Pas de retard ",
                       "R1"="Un an de retard",
                       "R2"="Deux ans de retard")
Type_bac <- recode_factor(base$Type_bac,
                          "Gene"="Bac g´en´eral",
                          "Tech"="Bac technologique")
res_bac <- recode_factor(res_bac,
                         "A"="mention TBien",
                         "B"="mention Bien",
                         "C"="mention ABien",
                         "D"="mention passable",
                         "P"="passe au 2e groupe ")
repech <- recode_factor(repech,
                        "N"="Non",
                        "O"="Oui")

d1 <- base[,c(5,9:10)]
View(d1)
newdata <- cbind(d1,reussite,pcs,sexe,retard,Type_bac,res_bac,repech)
View(newdata)
attach(newdata)
# Analyse univari?e -------------------------------------------------------
# variable quantitative
note_math <- na.rm(newdata$Note_math)
summary(note_math)
boxplot(note_math,ylim=c(0,19),main="Boite `a moustache des r´esultats des ´etudiants",
        xlab="Note en Maths")
hist(note_math,breaks = c(0,5,10,15,20), xlab = "Notes en Maths",
     ylab = "Effectifs", main = "Histogramme des notes de Maths")
descr(note_math, stats = c("min","q1","mean","q3","med","max","skewness","kurtosis"),
      transpose = T)
cut(note_math,breaks = 4)
table(cut(note_math,breaks = c(0,5,10,15,20)))
freq(cut(note_math,breaks = c(0,5,10,15,20)),na.rm=T)


locator()
text(1.7604606,200.0000,"308")
text(7.734934,611.4859,"848")
text(12.756492,437.6278, "659")
text(17.77805,100.8233,"183")

# variable qualitative
# BAC
# t=table(BAC)
# p=round(t/sum(t)*100,2)
# p
# pie(t)
# p1=c(70.33 , 1.53, 24.10 , 0.34 , 0.39 , 3.30 )
# l=c("ES" , "L" , "S", "SMS", "STI" , "STT")
# lab=paste(l,"",p,"%",sep=" ")
# pie(t, labels = lab,
# main = "Diagramme circulaire (%)",cex=0.8,cex.main=0.8,
# col=2:7)
# library(plotrix)
#
# pie3D(t,labels= lab,
# explode = 0.2, radius = .9, theta =2,
# labelcex = 0.8, shade = 1.4,
# col=c("brown","#ddaa00","pink","blue","chartreuse4","forestgreen"),
# main="Diagramme circulaire (3D)",labelcol = 3,
# cex.main=1)
barplot(t,ylab = "Effectifs",xlab="S´eries bac",main = "Bar plot de la s´erie bac")
# Mention
t=table(Mention)
p=round(t/sum(t)*100,2)
p
pie(t)
p1=c(65.94,34.06 )
l=c("Non","Oui")
lab=paste(l,"",p,"%",sep=" ")
pie(t, labels = lab,
    main = "Diagramme circulaire de la mention au bac",cex=1,cex.main=1,col=c("brown2","blue"))

# library(plotrix)
#
pie3D(t,labels= lab,
      explode = 0.2, radius = 1, theta =.8,
      labelcex = 1, shade = 1.8,
      col=c("brown2","blue"),
      main="Diagramme circulaire de la mention au bac",labelcol = 1,
      cex.main=1)
barplot(t,ylab = "Effectifs",xlab="Mention",
        main = "Bar plot de la mention au bac bac")
# Reussite
t=table(reussite)
p=round(t/sum(t)*100,2)
p
pie(t)
p1=c( 47.56, 52.44 )
l=c("Non","Oui")
lab=paste(l,"",p,"%",sep=" ")
pie(t, labels = lab,
    main = "Diagramme circulaire de la r´eussite \n des ´etudiants L1 ´economie",
    cex=1,cex.main=1,
    col=c("brown2","blue"))
# library(plotrix)
#
pie3D(t,labels= lab,
      explode = 0.2, radius = 1, theta =.8,
      labelcex = 1, shade = 1.8,
      col=c("brown2","blue"),
      main="Diagramme circulaire de la r´eussite \n des ´etudiants L1 ´economie",
      labelcol = 1,
      cex.main=1)
barplot(t,ylab = "Effectifs",xlab="Mention",
        main = "Bar plot de la r´eussite \n des ´etudiants L1 ´economie")

# pcs
t=table(pcs)
p=round(t/sum(t)*100,2)
p
# pie(t)
# p1=c( 47.56, 52.44 )
# l=c("Non","Oui")
# lab=paste(l,"",p,"%",sep=" ")
# pie(t, labels = lab,
# main = "Diagramme circulaire de la r´eussite \n des ´etudiants L1 ´economie",
#cex=1,cex.main=1,
# col=c("brown2","blue"))
#
# # library(plotrix)
# #
# pie3D(t,labels= lab,
# explode = 0.2, radius = 1, theta =.8,
# labelcex = 1, shade = 1.8,
# col=c("brown2","blue"),
# main="Diagramme circulaire de la r´eussite \n des ´etudiants L1 ´economie",
# labelcol = 1,
# cex.main=1)
barplot(t,ylab = "Effectifs",xlab="Professions",
        main = "Bar plot de la profession des parents\n regroup´ee en 4 postes ",cex.names = 1)
text(0.5775083,200.4493,"17.35%")
text(1.845182,300.0541,"25.14%")
text(3.0,200.247,"16.61%")
text(4.253142,546.0597,"40.91%")
# sexe
t=table(sexe)
p=round(t/sum(t)*100,2)
p
pie(t)
p1=c( 39.08 , 60.92 )
l=c("Feminin", "Masculin")
lab=paste(l,"",p,"%",sep=" ")
pie(t, labels = lab,
    main = "Diagramme circulaire du genre\n des ´etudiants L1 ´economie",cex=1,cex.main=1, col=c("brown2","blue"))


# library(plotrix)
#
pie3D(t,labels= lab,
      explode = 0.2, radius = 1, theta =.8,
      labelcex = 1, shade = 1.8,
      col=c("brown2","blue"),
      main="Diagramme circulaire du genre \n des ´etudiants L1 ´economie",labelcol = 1,
      cex.main=1)
# barplot(t,ylab = "Effectifs",xlab="",
# main = "Bar plot de la r´eussite \n des ´etudiants L1 ´economie")
# retard
t=table(retard)
p=round(t/sum(t)*100,2)
p
pie(t)
p1=c( 58.75 , 33.76,7.49 )
l=c("A l’heure ou en avance", "Un an de retard","Deux ans de retard ")
lab=paste(l,"",p,"%",sep=" ")
pie(t, labels = lab,
    main = "Diagramme circulaire du retard au bac",cex=1,cex.main=1,
    col=c("brown2","blue","green"))
# library(plotrix)
#
pie3D(t,labels= lab,
      explode = 0.2, radius = 0.7, theta =.8,
      labelcex = 1, shade = 1.8,
      col=c("brown2","blue","green"),
      main="Diagramme circulaire du genre \n des ´etudiants L1 ´economie",labelcol = 1,
      cex.main=1)
barplot(t,ylab = "Effectifs",xlab="Retard",
        main = "Bar plot du retard au bac \n des ´etudiants L1 ´economie",cex.names = 1)
text(0.657931, 806.6261, "58.75%")
text(1.8,500.9139, " 33.76%")

text(3.176426, 110.6703, "7.49%")
#type_bac
t=table(Type_bac)
p=round(t/sum(t)*100,2)
p
pie(t)
p1=c( 95.96,4.04 )
l=c("Bac g´en´eral","Bac technologique" )
lab=paste(l,"",p,"%",sep=" ")
pie(t, labels = lab,
    main = "Diagramme circulaire du type de bac",cex=0.7,cex.main=1,
    col=c("blue","green"))
# library(plotrix)
#
pie3D(t,labels= lab,
      explode = 0.2, radius = 0.6, theta =1,
      labelcex = 1, shade = 1.8,
      col=c("blue","green"),
      main="Diagramme circulaire du type bac \n des ´etudiants L1 ´economie",
      labelcol = 1,
      cex=0.5)
# barplot(t,ylab = "Effectifs",xlab="Retard",
# main = "Bar plot du retard au bac \n des ´etudiants L1
# ´economie",cex.names = 0.8)
# text(0.657931, 806.6261, "58.75%")
# text(1.8,500.9139, " 33.76%")
# res_bac
t=table(res_bac)
p=round(t/sum(t)*100,2)
p
# pie(t)
# p1=c( 58.75 , 33.76,7.49 )
# l=c("A l’heure ou en avance", "Un an de retard","Deux ans de retard ")
# lab=paste(l,"",p,"%",sep=" ")
# pie(t, labels = lab,
# main = "Diagramme circulaire du retard au bac",cex=1,cex.main=1,
# col=c("brown2","blue","green"))
# library(plotrix)
# #
# pie3D(t,labels= lab,
# explode = 0.2, radius = 0.7, theta =.8,
# labelcex = 1, shade = 1.8,
# col=c("brown2","blue","green"),
# main="Diagramme circulaire du genre \n des ´etudiants L1 ´economie",
#labelcol = 1,
# cex.main=1)
barplot(t,ylab = "Effectifs",xlab="R´esultats du bac",
        main = "Bar plot r´esultats du bac \n des ´etudiants L1 ´economie",cex.names = 0.8)
# text(0.657931, 806.6261, "58.75%")
# text(1.8,500.9139, " 33.76%")
# text(3.176426, 110.6703, "7.49%")
# repech
t=table(repech)
p=round(t/sum(t)*100,2)
p
pie(t)
p1=c( 81.62, 18.38 )
l=c("Non", "Oui ")
lab=paste(l,"",p,"%",sep=" ")
pie(t, labels = lab,
    main = "Diagramme circulaire du repechement au bac",cex=1,cex.main=1,
    col=c("blue","green"))
library(plotrix)
pie3D(t,labels= lab,
      explode = 0.2, radius = 0.7, theta =.8,
      labelcex = 1, shade = 1.8, col=c("blue","green"),
      main="Diagramme circulaire du repechement \n des ´etudiants L1 ´economie",
      labelcol = 1,
      cex.main=1,cex=0.8)
# barplot(t,ylab = "Effectifs",xlab="R´esultats du bac",
#main = "Bar plot r´esultats du bac \n des ´etudiants L1
#´economie",cex.names = 0.7)
#
note_math1 <- note_math[is.na(note_math)!=T]
theme_gtsummary_language("fr",decimal.mark = ".",big.mark = "")
TBL <- newdata %>%
  select(BAC,Mention, reussite,pcs,sexe) %>%
  tbl_summary(label=list(BAC~"S´erie bac",Mention~"Mention bac",
                         reussite~"R´eussite L1 Eco",pcs~"Professions des parents",
                         sexe~"Genre"),
              statistic = list(all_continuous()~"{mean}({sd})"),
              digits=all_continuous()~1,
              missing = "no")
TBL%>%
  # modify_header(update=list(
  # label~"**Variables**"))%>%
  modify_footnote(everything()~"Effectifs (Fr´equences %)")
TBL <- newdata %>%
  select(retard,Type_bac,res_bac,repech) %>%
  tbl_summary(label=list(retard~"Retard pour l’obtention du bac",
                         Type_bac~"Type du Bac",
                         res_bac~"R´esultats du Bac",repech~"Repechement"),
              statistic = list(all_continuous()~"{mean}({sd})"),
              digits=all_continuous()~1,
              missing = "no"
  )
TBL%>%
  # modify_header(update=list(
  # label~"**Variables**"))%>%
  modify_footnote(everything()~"Effectifs (Fr´equences %)")

###################################
#### BIVARIEE
tbl <- newdata%>%
  select(reussite,sexe,BAC,Mention)%>%
  tbl_summary(label = list(sexe~"Genre",BAC~"S´erie du BAC",
                           Mention~"Mention au BAC"),
              include = c(reussite,sexe, BAC,Mention),
              by=reussite,
              missing = "no",
              statistic = list(all_continuous()~"{mean}({sd})")
  )%>%
  add_p( test=list(all_continuous() ~ "t.test", all_categorical() ~ "chisq.test"))
tbl%>%
  modify_spanning_header(all_stat_cols()~"**R´eussite**")
tbl <- newdata%>%
  select(reussite,retard,Type_bac,res_bac)%>%
  tbl_summary(label = list(retard~"Retard pour l’obtention du bac",
                           Type_bac~"Type du Bac",res_bac~"R´esultats du bac"),
              include = c(reussite,retard,Type_bac,res_bac),
              by=reussite
  )%>%
  add_p( test=list(all_continuous() ~ "t.test", all_categorical() ~ "chisq.test"))
tbl%>%
  modify_spanning_header(all_stat_cols()~"**R´eussite**")
tbl <- newdata%>%
  select(reussite,pcs,Note_math,repech)%>%
  tbl_summary(label = list(pcs~"Professions des parents",
                           Note_math~"Notes Maths obtenues au bac",repech~"Repechement"),
              include = c(reussite,pcs,Note_math,repech),
              by=reussite,
              missing = "no",
              statistic = list(all_continuous()~"{mean}({sd})")
  )%>%
  add_p( test=list(all_continuous() ~ "t.test", all_categorical() ~ "chisq.test"))
tbl%>%
  modify_spanning_header(all_stat_cols()~"**R´eussite**")
# reussite, note maths
DF<- filter(newdata, BAC!="L")
boxplot(DF$Note_math~DF$reussite,
        main="Box plot entre r´eussite et la note maths au BAC",
        ylab = "Notes maths",xlab = "R´eussite")
points(1:7, mean(DF$Note_math),col=2,pch=4)
lines(x=mean(DF$Note_math),col=8, lty=3,lwd=2)
abline(h=mean(DF$Note_math),col=3)
anova(lm(DF$Note_math~DF$reussite))
pairwise.t.test(DF$Note_math,DF$reussite)
sqrt(anova(lm(DF$Note_math~DF$reussite))[1,2]/
       (anova(lm(DF$Note_math~DF$reussite))[1,2]+anova(lm(DF$Note_math~DF$reussite))[2,2]))
# graphiques bivari´ees (quali-quali)
attach(newdata)
ef <- table(reussite,BAC)
barplot(ef, xlab = "S´erie BAC", ylab = "Effectifs", col = c("brown","yellow"),
        legend.text = c("Ne valide pas L1 eco","Valide L1 eco"),
        beside = T,cex.main=0.8,args.legend = list(cex=0.8),
        main = "Diagramme en barre de la r´eussite par rapport `a la s´erie du BAC")
ef <- table(reussite,Mention)
barplot(ef, xlab = "Mention au BAC", ylab = "Effectifs",
        col = c("brown","yellow"),
        legend.text = c("Ne valide pas L1 eco","Valide L1 eco"),
        beside = T,args.legend = list(cex=0.8),cex.main=0.8,
        main="Diagramme en barre de la r´eussite par rapport `a la mention au bac")
ef <- table(reussite,pcs)
barplot(ef, xlab = "Profession des parents", ylab = "Effectifs",
        col = c("brown","yellow"),
        legend.text = c("Ne valide pas L1 eco","Valide L1 eco"),
        beside = T,cex.main=0.8,
        main="Diagramme en barre de la r´eussite par rapport `a la profession des parents",
        args.legend = list(x="topleft",cex=0.8))
ef <- table(reussite,sexe)
barplot(ef, xlab = "Genre", ylab = "Effectifs", col = c("brown","yellow"),
        legend.text = c("Ne valide pas L1 eco","Valide L1 eco"),
        beside = T,cex.main=0.8,
        main="Diagramme en barre de la r´eussite par rapport au genre",
        args.legend = list(x="topleft",cex=0.8))
ef <- table(reussite,retard)
barplot(ef, xlab = "Retard pour l’obtention du bac", ylab = "Effectifs",
        col = c("brown","yellow"),
        legend.text = c("Ne valide pas L1 eco","Valide L1 eco"),
        beside = T,cex.main=0.8,
        main="Diagramme en barre de la r´eussite par rapport au retard de
l’obtention du BAC",
        args.legend = list(x="topright",cex=0.8))
ef <- table(reussite,Type_bac)
barplot(ef, xlab = "Type du BAC", ylab = "Effectifs", col = c("brown","yellow"),
        legend.text = c("Ne valide pas L1 eco","Valide L1 eco"),
        beside = T,cex.main=0.8,
        main="Diagramme en barre de la r´eussite par rapport au type du BAC",
        args.legend = list(x="topright",cex=0.8))
ef <- table(reussite,res_bac)
barplot(ef, xlab = "R´esultats au bac", ylab = "Effectifs",
        col = c("brown","yellow"),
        legend.text = c("Ne valide pas L1 eco","Valide L1 eco"),
        beside = T,cex.main=0.8,cex.names = 0.7,
        main="Diagramme en barre de la r´eussite par rapport au r´esultats",
        args.legend = list(x="topleft",cex=0.8))
ef <- table(reussite,repech)
barplot(ef, xlab = "Repechement", ylab = "Effectifs", col = c("brown","yellow"),
        legend.text = c("Ne valide pas L1 eco","Valide L1 eco"),
        beside = T,cex.main=0.8,main="Diagramme en barre de la r´eussite par rapport au repechement",
        args.legend = list(x="topright",cex=0.8))

#############################
library(questionr)
library(tidyverse)
library(haven)
library(forcats)
library(labelled)
library(sjlabelled)
library(foreign)
library(janitor)
library(funModeling)
library(summarytools)
library(tidyr)
library(writexl)
library(gtsummary)
library(ggplot2)
library(plotrix)
library(gtsummary)
library(labelled)
library(tidyverse)
library(dplyr)
library(Hmisc)
library(lmtest)
library(tseries)
library(smd)
setwd("//profils.uppa.univ-pau.fr/folderredir/bkande/Desktop/Cours r")
library(readxl)
Genre_Educ_Rev <- read_excel("//profils.uppa.univ-pau.fr/folderredir/bkande/Desktop
/Cours r/Genre_Educ_Rev.xlsx")
View(Genre_Educ_Rev)
data <- Genre_Educ_Rev
attach(data)
View(data)

Educ <- factor(Educ,ordered = T)
Educ <- recode_factor(Educ,
                      "1"="Avant Bac",
                      "2"="Bac",
                      "3"="Bac + 2",
                      "4"="Bac + 3",
                      "5"="Bac + 5 et plus")
Genre <- recode(data$Genre,
                "F"="Feminin",
                "M"="Masculin")
table(Genre)
margin.table(table(Genre))
table(Educ)
margin.table(table(Educ))
data1 <- cbind(data[,1],Educ,Genre)
View(data1)
attach(data1)
# statistique descriptives
theme_gtsummary_language("fr",decimal.mark = ".",big.mark = "")
TBL <- data1 %>%
  select(Educ,Genre,Salaire) %>%
  tbl_summary(label=list(Educ~"Niveau d’´education",Genre~"Genre de
l’individu", Salaire~"Revenu mensuel disponible (=C)"),
              statistic = list(all_continuous()~"moyenne: {mean} (Ecart type:
{sd})",all_categorical()~"Effectif: {n} (Proportion: {p}%)"),
              digits=all_continuous()~1,
              missing = "no")
TBL%>%
  modify_header(update=list(
    label~"**Variables**"))%>%
  modify_footnote(everything()~"Effectifs (Fr´equences %)")%>%add_n()
# Analyse univari´ee
# - proportion 

# - genre
t1 <- table(data1$Genre)
margin.table(t1)
print(t1)
# estimation pontuelle des femme
N=6400
n=3249
estpont <- n/N
round(estpont,3)
binconf(n,N, alpha = 0.05, method = "all")
round(binconf(n,N, alpha = 0.05, method = "all"),3)
# estimation pontuelle des hommes
N=6400
n=3151
binconf(n,N,alpha = 0.05,method = "all")
round(binconf(n,N,alpha = 0.05,method = "all"),3)
# educ
t2 <- table(data1$Educ)
print(t2)
margin.table(t2)
# avant bac
N=6400
n=600
binconf(n,N,alpha=0.05,method = "all")
round(binconf(n,N,alpha=0.05,method = "all")
      ,3)
# bac
n=1800
binconf(n,N,alpha = 0.05, method = "all")
round(binconf(n,N,alpha = 0.05, method = "all")
      ,3)
# bac+2
n=1500

binconf(n,N,alpha = 0.05,method = "all")
round(binconf(n,N,alpha = 0.05,method = "all")
      ,3)
# bac +3
n=1000
binconf(n,N, alpha = 0.05,method = "all")
round(binconf(n,N, alpha = 0.05,method = "all")
      ,3)
# bac+5 et plus
n=1500
binconf(n,N,alpha = 0.05,method = "all")
round(binconf(n,N,alpha = 0.05,method = "all")
      ,3)
# salaire
t.test(data1$Salaire,conf.level = 0.95,mu=2196.329)
TBL <- data1 %>%
  select(Educ,Genre,Salaire) %>%
  tbl_summary(label=list(Educ~"Niveau d’´education",
                         Genre~"Genre de l’individu", Salaire~"Revenu mensuel disponible (=C)"),
              statistic = list(all_continuous()~"moyenne: {mean} (Ecart type:
{sd})",all_categorical()~"Effectif: {n} (Proportion: {p}%)"),
              digits=all_continuous()~1,
              missing = "no")
TBL <- TBL%>%
  modify_header(update=list(
    label~"**Variables**"))%>%
  modify_footnote(everything()~NA)%>%add_ci(
    statistic=list(all_categorical()~"[{conf.low}%
,{conf.high}%]",all_continuous()~"[{conf.low}, {conf.high}]")
  )
TBL%>%modify_header(ci_stat_0="**Intervalle de confiance (95%)**")
tbl <- data1%>%
  select(Genre,Educ)%>%
  tbl_summary(by=Genre,
              label=list(Educ~"niveau d’´etude"),
              percent="col")%>%add_p()
tbl%>%modify_header(update=list( label~"**Variables**"))%>%modify_spanning_header(all_stat_cols(stat_0 = FALSE)
                                                                                  ~ "**Genre**")%>%add_significance_stars()
  

#################################################
TBL <- data1 %>%
  select(Genre,Salaire) %>%
  tbl_summary(by=Genre,
              label=Salaire~"Salaire",
              statistic = all_continuous()~c("{mean} ({sd})"),
              digits=all_continuous()~1,
              missing = "no")
TBL%>%modify_header(update=list(
  label~"**Variables**"))%>%modify_spanning_header(all_stat_cols(stat_0 = FALSE) ~
                                                     "**Genre**")%>%add_difference()%>%add_significance_stars()%>%
  add_ci(statistic=all_continuous()~"[{conf.low}, {conf.high}]")
TBL <- data1 %>%
  select(Genre,Salaire) %>%
  tbl_summary(by=Genre,
              label=Salaire~"Salaire",
              statistic = all_continuous()~c("{median} ({sd})"),
              digits=all_continuous()~1,
              missing = "no")
TBL%>%modify_header(update=list(
  label~"**Variables**"))%>%modify_spanning_header(all_stat_cols(stat_0 = FALSE) ~
                                                     "**Genre**")%>%add_difference(test = Salaire~"wilcox.test")%>%
  add_significance_stars()
TBL <- data1 %>%
  select(Educ,Salaire) %>%
  tbl_summary(by=Educ,
              label=Salaire~"Salaire",
              statistic = all_continuous()~c("{median} ({sd})"),
              digits=all_continuous()~1,
              missing = "no")
TBL%>%modify_header(update=list(
  label~"**Variables**"))%>%modify_spanning_header(all_stat_cols(stat_0 = FALSE) ~
                                                     "**Genre**")%>%add_difference(test = Salaire~"wilcox.test")%>%
  add_significance_stars()

TBL <- data1 %>%
  select(Educ,Salaire) %>%
  tbl_summary(by=Educ,
              label=list(Salaire~"Salaire",Educ~"Niveau d’´etudes"),
              statistic = all_continuous()~c("{mean} ({sd})"),
              digits=all_continuous()~1,
              missing = "no")
TBL%>%modify_header(update=list(
  label~"**Variables**"))%>%modify_spanning_header(all_stat_cols(stat_0 = FALSE) ~
                                                     "**Niveau d’´edutes**")%>%add_p(test=all_continuous()~"kruskal.test")%>%
  add_significance_stars()
#############################################
data1%>%
  select(Educ,Salaire)%>%
  tbl_summary(by=Educ,
              label=list(Salaire~"Salaire"),
              percent="col")%>%add_p()
data1%>%
  select(Educ,Genre)%>%
  tbl_cross(row=Educ, col=Genre,
            percent = "col" )
N1=3249
N2=3151
# pas bac
prop.test(c(299,301),c(N1,N2))
# bac
prop.test(c(870,930),c(N1,N2))
# bac+2
prop.test(c(791,709),c(N1,N2))
# bac +3
prop.test(c(495,505),c(N1,N2))
# bac + 5 et plus
prop.test(c(794,706),c(N1,N2))
varf=tapply(data1$Salaire,data1$Genre,var)
varf


var.test(data1$Salaire~data1$Genre)
kruskal.test(Salaire~Educ)
wilcox.test(Salaire~Genre)
pairwise.t.test(Salaire,Educ)
################
library(ggplot2)
library(dplyr)

# Chargement du jeu de données iris
data(iris)

# Calcul des pourcentages
iris_percent <- iris %>% 
  group_by(Species) %>% 
  summarise(percent = n() / nrow(iris) * 100)

# Définition d'une palette de couleurs personnalisée
my_colors <- c("#1f77b4", "#ff7f0e", "#2ca02c")

# Visualisation de la variable catégorielle "Species" avec un diagramme en barres
ggplot(data = iris_percent, aes(x = Species, y = percent, fill = Species)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(percent, 1), "%")), vjust = -0.5, color = "black", size = 3) +
  labs(title = "Répartition des espèces d'iris", x = "Espèces", y = "Pourcentage") +
  scale_fill_manual(values = my_colors)


library(ggplot2)
library(dplyr)

# Création d'un jeu de données exemple
data <- data.frame(
  Variable1 = c("A", "A", "B", "B", "C", "C"),
  Variable2 = c("X", "Y", "X", "Y", "X", "Y")
)

# Compter les occurrences de chaque combinaison de variables
data_count <- data %>% 
  count(Variable1, Variable2) %>%
  mutate(percentage = n / sum(n) * 100)  # Calcul des pourcentages

# Création du graphique à barres groupées avec pourcentages
ggplot(data = data_count, aes(x = Variable1, y = percentage, fill = Variable2)) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), position = position_dodge(width = 0.9), vjust = -0.5, size = 3) +
  labs(title = "Répartition des combinaisons de variables",
       x = "Variable 1", y = "Pourcentage") +
  scale_fill_manual(values = c("#1f77b4", "#ff7f0e"), name = "Variable 2")





