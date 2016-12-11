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
perform_types$Above<-(perform_types$StemWeight+perform_types$LeavesWeight)
perform_types$totweight<-perform_types$StemWeight+perform_types$LeavesWeight+perform_types$RootsWeight+perform_types$FruitsWeight+perform_types$SeedsWeight
perform_types$ab_ratio<-(perform_types$StemWeight+perform_types$LeavesWeight+perform_types$FruitsWeight+perform_types$SeedsWeight)/perform_types$RootsWeight

x11(); par(mfrow=c(2,2))
boxplot(totweight~Category, data=perform_types, ylab="total plant weight")
boxplot(ab_ratio~Category, data=perform_types, ylab="ratio of above to below ground biomass")
plot(perform_types$totweight, perform_types$ab_ratio, xlab="total plant weight", ylab="above to below ground ratio")

#straight in with lmes because we have transect as a random factor
biomassratio.lme<-lme(ab_ratio~Category,random =~1|TransID, data=na.omit(perform_types))# is the most intuitive model
summary(biomassratio.lme)#ns for forest types

hist(perform_types$totweight)
biomass.lme_scaled<-lme(totweight~scale(RootsWeight, center=TRUE, scale=FALSE)*Category,random =~1|TransID, data=na.omit(perform_types)) #Olav centered the data to help lme cope with all the values being above zero. The parameter estimates are the same, it's just nicer for the model. We out in an interaction because it might just be that bigge rplants grow in one ageclass and that bigger plants invest less in roots
summary(biomass.lme_scaled)## For every 1g of root you get 4g of plant. Sig increase in plant for OS (10% more investment above ground, as a super-approximation - of this maybe a quarter is about just having bigger plants).
 
#if we want to talk about the effect of plant size affecting scaling, we should do something so that we're looking at the difference from a 1:1 line, not from 0 (says Richard)

#when we have the new data, check the merge worked - some transects didn't get a forest type


str(perform)#str means 'show me what this object is made of'
