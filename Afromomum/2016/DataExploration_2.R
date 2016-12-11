library(vegan)
source("Load_Aframomum_data.r")#runs the script to load all the data
mean(stemlength)
median(stemlength)
barplot(spChar,col=1)
barplot(spChar,col=5)
barplot(spChar,col="green")
barplot(spChar,col=8)

#boxplots of everything vs forest type
x11(width=5,height=9);par(mfrow=c(3,2),mar=c(2,1,2,0.5),mgp=c(1,.3,0),oma=c(0,2,0,0),cex=.7,tcl=.25,xpd=T,las=1.55)
fortype<-xtabs(AvgOfStemLength~Category,compart2)
boxplot(AvgOfStemLength~Category,compart2,main="Stem length",notch=FALSE)
fortype<-xtabs(AvgOfLong_leaf~Category,compart2)
boxplot(AvgOfLong_leaf~Category,compart2,main="Leaf length", notch=FALSE) 
masstype<-xtabs(AvgOfLeavesWeight~Category,biomass)
boxplot(AvgOfLong_leaf~Category,compart2,main="Leaf length",notch=FALSE)
masstype<-xtabs(AvgOfRootsWeight~Category,biomass)
boxplot(AvgOfRootsWeight~Category,biomass,main="Root weight")
boxplot(AvgOfStemWeight~Category,biomass,main="Stem weight")
boxplot(AvgOfLeavesWeight~Category,biomass,main="Leaves weight")

#playing with models to match olav paper
#mod.glmlin.NLxSL<-glm(NumLeaves~poly(StemLength,2),quasipoison(link = "log"), data=fortype
#summary(mod.glmlin.NLxSL)

biomass$Above<-(biomass$AvgOfStemWeight+biomass$AvgOfLeavesWeight)
plot(biomass$Above~biomass$AvgOfRootsWeight)#gettign towards Olav's figure but is averages.
perform$Above<-(perform$StemWeight+perform$LeavesWeight)
plot(perform$Above~perform$RootsWeight, col=perform$TransID)
library(nlme)
explore.biomass.lme<-lme((Above/RootsWeight)~Site_ID|TransID, data=perform)#not working 

str(perform)#str means 'show me what this object is made of'
#rstudio is lovely
library(readxl)
df<-read_excel("test_data.xlsx")

df
library(ggplot2)
g<-ggplot (df,aes(x=Species, fill=as.factor(Compartment)))+
          geom_bar()
g+theme(axis.text.x = element_text(angle = 45,vjust = 1,hjust = 1))
ggplot(perform, aes(x = StemWeight, fill = as.factor(fortype)))#
g+coord_flip()

g<-ggplot(perform, aes(x=StemWeight, fill =as.factor(fortype)))


    