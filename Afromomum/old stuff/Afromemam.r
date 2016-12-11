library(RODBC)
#this bit loads the data

con<-odbcConnectAccess("\\\\helix.klient.uib.no\\biohome\\aey022\\rdata\\Afromomum\\research2.mdb")

#first sets
env<-sqlQuery(con,"SELECT Compartment.*, [Environmental data].* FROM Compartment LEFT JOIN  [Environmental data] ON (Compartment.TransID = [Environmental data].[Transect ID]) AND (Compartment.Site_ID = [Environmental data].[Site ID])")

perf<-sqlQuery(con,"SELECT Subplot.SiteID, Subplot.TransID, Subplot.PlotID, Subplot.NumofStems, Subplot.Canopy1, Subplot.Canopy2, Subplot.Canopy3, Subplot.Canopy4, Subplot.DBH, Subplot.Stumps, Subplot.Deadwood, Subplot.Location, Avg(Performance.StemLength) AS AvgOfStemLength, Avg(Performance.NumLeaves) AS AvgOfNumLeaves, Avg(Performance.Long_leaf) AS AvgOfLong_leaf, Avg(Performance.LeafBredth) AS AvgOfLeafBredth 
FROM Subplot INNER JOIN Performance ON (Subplot.PlotID = Performance.PlotID) AND (Subplot.TransID = Performance.TransID) AND (Subplot.SiteID = Performance.Site_ID) GROUP BY Subplot.SiteID, Subplot.TransID, Subplot.PlotID, Subplot.NumofStems, Subplot.Canopy1, Subplot.Canopy2, Subplot.Canopy3, Subplot.Canopy4, Subplot.DBH, Subplot.Stumps, Subplot.Deadwood, Subplot.Location;")

perfall<-sqlQuery(con,"SELECT Performance.*, Subplot.Location, Compartment.Category FROM Compartment INNER JOIN (Subplot INNER JOIN Performance ON (Subplot.PlotID = Performance.PlotID) AND (Subplot.TransID = Performance.TransID) AND (Subplot.SiteID = Performance.Site_ID)) ON (Compartment.TransID = Subplot.TransID) AND (Compartment.Site_ID = Subplot.SiteID);")

subplot<-sqlQuery(con,"SELECT Subplot.*, [Environmental data].*, Compartment.Category FROM (Compartment INNER JOIN Subplot ON (Compartment.TransID = Subplot.TransID) AND (Compartment.Site_ID = Subplot.SiteID)) INNER JOIN [Environmental data] ON (Subplot.TransID = [Environmental data].[Transect ID]) AND (Subplot.SiteID = [Environmental data].[Site ID]);")

#new ones
alldata<-sqlQuery(con, "SELECT Performance.*, Subplot.*, [Environmental data].*, Compartment.* FROM ((Compartment INNER JOIN [Environmental data] ON (Compartment.Site_ID = [Environmental data].[Site ID]) AND (Compartment.TransID = [Environmental data].[Transect ID])) 
INNER JOIN Subplot ON (Compartment.TransID = Subplot.TransID) AND (Compartment.Site_ID = Subplot.SiteID)) INNER JOIN Performance ON (Subplot.PlotID = Performance.PlotID) AND (Subplot.TransID = Performance.TransID) AND (Subplot.SiteID = Performance.Site_ID);")

avperfenv<-sqlQuery(con,"SELECT Compartment.Site_ID, Compartment.TransID, Compartment.Category, Subplot.Location, Avg(Subplot.NumofStems) AS AvgOfNumofStems, Avg(Performance.StemLength) AS AvgOfStemLength, Avg(Performance.NumLeaves) AS AvgOfNumLeaves, Avg(Performance.Long_leaf) AS AvgOfLong_leaf, Avg(Performance.LeafDamage) AS AvgOfLeafDamage, Avg(Performance.LeafBredth) 
AS AvgOfLeafBredth, [Environmental data].pH, [Environmental data].carbon, [Environmental data].Nitrogen, [Environmental data].Calcium, [Environmental data].potassium, [Environmental data].sodium, [Environmental data].cover 
FROM (([Environmental data] INNER JOIN Compartment ON ([Environmental data].[Site ID] = Compartment.Site_ID) AND ([Environmental data].[Transect ID] = Compartment.TransID)) INNER JOIN Subplot ON (Compartment.TransID = Subplot.TransID) AND (Compartment.Site_ID = Subplot.SiteID)) 
INNER JOIN Performance ON (Subplot.PlotID = Performance.PlotID) AND (Subplot.TransID = Performance.TransID) AND (Subplot.SiteID = Performance.Site_ID) GROUP BY Compartment.Site_ID, Compartment.TransID, Compartment.Category, Subplot.Location, [Environmental data].pH, [Environmental data].carbon, [Environmental data].Nitrogen, [Environmental data].Calcium, [Environmental data].potassium, [Environmental data].sodium, [Environmental data].cover;")

close(con)
###################### data loading ends here##############################################
setwd("\\\\helix.klient.uib.no\\biohome\\aey022\\rdata\\Afromomum")
ls()
head(subplot)
head(perfall)
head(alldata)
head(perf)                  
head(env)
head(avperfenv)

#Make a categorical varibale "Mature" vs "Recent" where mature is the old growth and old secondary together
env$Age2class<-ifelse(env$Category =="YS", c("Recent"), c("Mature"))
env$Age2class<-as.factor(env$Age2class) 
str(env)
perfall$Age2class<-ifelse(perfall$Category =="YS", c("Recent"), c("Mature"))
perfall$Age2class<-as.factor(perfall$Age2class) 
str(perfall)
alldata$Age2class<-ifelse(alldata$Category =="YS", c("Recent"), c("Mature"))
alldata$Age2class<-as.factor(alldata$Age2class) 
str(alldata)
avperfenv$Age2class<-ifelse(avperfenv$Category =="YS", c("Recent"), c("Mature"))
avperfenv$Age2class<-as.factor(avperfenv$Age2class) 
str(avperfenv)
subplot$Age2class<-ifelse(subplot$Category =="YS", c("Recent"), c("Mature"))
subplot$Age2class<-as.factor(subplot$Age2class) 
str(subplot)

#fix location to be a factor not a numeric
subplot$Location<-as.factor(subplot$Location) 
perfall$Location<-as.factor(perfall$Location) 
avperfenv$Location<-as.factor(avperfenv$Location)
alldata$Location<-as.factor(alldata$Location)

#fix zero cover to nodata
subplot$cover[subplot$cover==0]<-NA
alldata$cover[alldata$cover==0]<-NA
avperfenv$cover[avperfenv$cover==0]<-NA

alldata$LeafDamage[alldata$LeafDamage==-999]<-NA
#### end of data adjustment

#gives the number of plots per location and per forest age, then the number of plants measured per location and per forest age 
table(subplot$Location)
table(subplot$Age2class)
table(perfall$Age2class)
table(perfall$Location)
                                      
# working out whether the site georeferencing is ok
plot( env$Eastings,env$Northings)

# load a library that makes it easy to make descriptive stats by group
library(psych)
# get things like mean and median stem length for each location
describe.by(subplot, subplot$Location)
describe.by(subplot, subplot$TransID, subplot$Location)

# i need to report the sum of nstems by transect
 

#various plots about the number of stems. The first is a frequency plot for the different numbers of stems per plot, showing that most plots had few stems. The second is the same but logged.The third is about how the numer of stems varies with location; this one is then saved. 
hist(subplot$NumofStems, xlab="Number of Stems", main="")
hist(log(subplot$NumofStems+1, 10), xlab="Log10 Number of Stems + 1", main="")
boxplot((log(NumofStems+1))~Location, data=subplot, xlab="Location", ylab="log(numberof stems)")
savePlot("nstems_location_boxplot.emf")
#test whether there are differences in the number of stems between location groups
kruskal.test(NumofStems~Location, data=subplot)
slanova<-lm(log(NumofStems)~Location, data=subplot)
summary(slanova)
# test whether that's still significant just for the three locations closest to the edge, because group 4 is very different.
just123<-subplot[which(subplot$Location<4),]
head(just123)
kruskal.test(NumofStems~Location, data=just123)

hist(subplot$cover, xlab="Mean canopy cover", main="")
plot((log(NumofStems+1))~cover, data=subplot, xlab="Canopy openness", ylab="log(numberof stems)")
savePlot("nstems_cover_scatter.emf")
cor.test(subplot$NumofStems,subplot$cover, use="pairwise.complete.obs")

# canopy 1,2,3,4 are the four levels of veg - field, shrub, subcanopy, topcanopy. Mean might not be so meaningful but this code is kept for exploratory purposes.
str(subplot)
subplot$canopymean<-rowMeans(subplot[,c("Canopy1","Canopy2","Canopy3","Canopy4")])
hist(subplot$canopymean, xlab="Mean canopy cover", main="")
x11();par(mfrow=c(2,2))
hist(subplot$Canopy1, xlab="Mean canopy cover", main="", xlim=c(0,100))
hist(subplot$Canopy2, xlab="Mean canopy cover", main="", xlim=c(0,100))
hist(subplot$Canopy3, xlab="Mean canopy cover", main="", xlim=c(0,100))
hist(subplot$Canopy4, xlab="Mean canopy cover", main="", xlim=c(0,100))
par(mfrow=c(1,1))
hist(subplot$canopymean, xlab="Mean canopy cover", main="")
plot((log(NumofStems+1))~canopymean, data=subplot, xlab="Canopy openness", ylab="log(numberof stems)")
savePlot("nstems_canopymeans_scatter.emf")
cor(subplot$NumofStems,subplot$canopymean)


# same plots and tests again as line 80 but only considering those plots that have some aframomum in them, and we don't test the 'just locations 123' as the pattern is quite different
justdensity<-subplot[subplot$NumofStems>0,]
boxplot((log(NumofStems+1))~Location, data=justdensity, xlab="Location", ylab="log (numberof stems) where present")
savePlot("nstems_occupied only_location_boxplot.emf")
kruskal.test(NumofStems~Location, data=justdensity)
hist(justdensity$NumofStems)
hist(log(justdensity$NumofStems))
sldanova<-lm(log(NumofStems)~Location, data=justdensity)
summary(sldanova)

cor.test((log(justdensity$NumofStems+1)),justdensity$cover, use="pairwise.complete.obs")
plot((log(justdensity$NumofStems+1))~justdensity$cover)
scatter.smooth((log(justdensity$NumofStems+1))~justdensity$cover)
# is r=0.08, p=0.06

# same again but for forest age instead of distance from edge
boxplot((log(NumofStems+1))~Age2class, data=subplot, xlab="Forest type", ylab="log(number of stems)")
boxplot((log(NumofStems+1))~Age2class, data=justdensity, xlab="Forest type", ylab="log(number of stems) where present")
savePlot("nstems_occupied only_foresttype_boxplot.emf") 
kruskal.test(NumofStems~Age2class, data=justdensity)
scdanova<-lm(log(NumofStems)~Category, data=justdensity)
summary(scdanova)

#putting the two boxplots together for neatness for presentations and the paper
x11(); par(mfrow=c(1,2))
boxplot((log(NumofStems+1))~Age2class, data=justdensity, xlab="Forest type", ylab="log(number of stems) where present")
boxplot((log(NumofStems+1))~Location, data=justdensity, xlab="Location", ylab="log (numberof stems) where present")

#damage by location and forest age
boxplot(LeafDamage~Location, data=alldata, xlab="Location", ylab="% damage")
boxplot(LeafDamage~Age2class, data=alldata, xlab="Age class", ylab="% damage")
head(alldata)

# a group of plots about the impact of forest age on size measurements
x11(); par(mfrow=c(2,2), mar=c(2, 3, 1, 1), mgp=c(1.5, 0.5, 0))
boxplot(AvgOfStemLength~Age2class, data=avperfenv, xlab="", ylab="plot-mean stem length")
boxplot(AvgOfLong_leaf~Age2class, data=avperfenv, xlab="", ylab="plot-mean longest leaf")
boxplot(AvgOfLeafBredth~Age2class, data=avperfenv, xlab="", ylab="plot-mean leaf breadth")
boxplot(AvgOfNumLeaves~Age2class, data=avperfenv, xlab="", ylab="plot-mean number of leaves")               
savePlot("Ageclass_4x_boxplot.emf")

#a group of plots about the impact of location on the other size measurements
x11(); par(mfrow=c(2,2), mar=c(3, 3, 1, 1), mgp=c(1.5, 0.5, 0))
boxplot(perf$AvgOfNumLeaves~perf$Location, xlab="Distance category", ylab="Plot mean number of leaves", ylim=c(0,25))
boxplot(perf$AvgOfStemLength~perf$Location, xlab="Distance category", ylab="Plot mean stem length", ylim=c(0,150))
boxplot(perf$AvgOfLong_leaf~perf$Location, xlab="Distance category", ylab="Plot mean length of longest leaf (cm)", ylim=c(0,30))
boxplot(perf$AvgOfLeafBredth~perf$Location, xlab="Distance category", ylab="Plot mean width of widest leaf", ylim=c(0,8))
savePlot("Location_4x_boxplot.emf")

boxplot(perf$AvgOfLeafBredth~perf$Location, xlab="Distance category", ylab="Plot mean width of widest leaf", ylim=c(0,8))

# make a figure containing four graphs, plotting each of the size variables against the number of stems, and save it
x11();par(mfrow=c(2,2))
plot(perf$NumofStems,perf$AvgOfStemLength, ylim=c(0,160))
plot(perf$NumofStems,perf$AvgOfNumLeaves, ylim=c(0,25))
plot(perf$NumofStems,perf$AvgOfLong_leaf, ylim=c(0,30))
plot(perf$NumofStems,perf$AvgOfLeafBredth, ylim=c(0,8))
savePlot("nstems_size measurements.emf")

# make a figure containong plots o all the size variables against one another, and save it
x11()
pairs(perf[,c("AvgOfStemLength", "AvgOfNumLeaves",  "AvgOfLong_leaf",  "AvgOfLeafBredth")]  )
savePlot("pairs.emf")

#make plots examining the impact of cover on size measurements. Danger. Pseudoreplicated
x11();par(mfrow=c(2,2))
plot(perfall1$cover,perfall1$StemLength, ylim=c(0,160))
plot(perfall1$cover,perfall1$NumLeaves, ylim=c(0,25))
plot(perfall1$cover,perfall1$Long_leaf, ylim=c(0,30))
plot(perfall1$cover,perfall1$LeafBredth, ylim=c(0,8))

boxplot(LeafDamage[LeafDamage>-1,]~Location, data=alldata)

##### allometric modelling starts here #########
#none of these models are necessarily any good#
# variables are called StemLength (SL) NumLeaves (NL) Long_leaf (LL) LeafDamage (LD) LeafBredth (LB)

mod.lin.SLxNL<- lm(StemLength~NumLeaves, data=perfall)
plot(StemLength~NumLeaves, data=perfall)

mod.glmlin.NLxSL<- glm(NumLeaves~StemLength, poisson, data=perfall)
summary(mod.glmlin.NLxSL)
plot(mod.glmlin.NLxSL)
plot(NumLeaves~StemLength, data=perfall)
abline(lm(NumLeaves~StemLength, data=perfall))

mod.glmlin.LLxSL<- glm(Long_leaf~StemLength, poisson, data=perfall)
summary(mod.glmlin.LLxSL)
plot(mod.glmlin.LLxSL)

mod.glmlin.LBxSL<- glm(LeafBredth~StemLength, poisson, data=perfall)
summary(mod.glmlin.LBxSL)
plot(mod.glmlin.LBxSL)

mod.glmlin.LBxLL<- glm(LeafBredth~Long_leaf, poisson, data=perfall)
summary(mod.glmlin.LBxLL)
plot(mod.glmlin.LBxLL)

mod.glmlin.LBxNL<- glm(LeafBredth~NumLeaves, poisson, data=perfall)
summary(mod.glmlin.LBxNL)
plot(mod.glmlin.LBxNL)


mod.glmpol.NLxSL<- glm(NumLeaves~poly(StemLength,2), family="poisson"(link=log), data=perfall)
summary(mod.glmpol.NLxSL)
# both the above models have terrible overdispersion (null deviance div by resid df = 2.11)
mod.glmquasi.NLxSL<- glm(NumLeaves~StemLength, quasipoisson(link = "log"), data=perfall)
summary(mod.glmquasi.NLxSL)
mod.glmquasipol.NLxSL<- glm(NumLeaves~poly(StemLength,2), quasipoisson(link = "log"), data=perfall)
summary(mod.glmquasipol.NLxSL)
# in the latter three models, all the deviances and dfs are the same! What does it mean... So far I kept the linear quasipoisson glm

mod.glmquasi.LLxSL<- glm(Long_leaf~StemLength, quasipoisson(link = "log"), data=perfall)
summary(mod.glmquasi.LLxSL)

mod.glmquasi.LBxSL<- glm(LeafBredth~StemLength, quasipoisson(link = "log"), data=perfall)
summary(mod.glmquasi.LBxSL)

# a mixed effects model to include the effects of site (not subplot as plants are replicates and subplots are adjacent), this currently uses the defaults but Olav Skaarpas suggests the negative binomial as a good approximation to the quasi-poisson but still returning/allowing likilihood ratio tests. Estimables does not appear to work here.
library(lme4)
mod.lmer.NLxSL<- lmer(NumLeaves~StemLength+Age2class+(1|TransID) , data=perfall)
summary(mod.lmer.NLxSL)



#models with interactions - ignore
#mod.glmquasi.NLxSLbyAge<- glm(NumLeaves~StemLength+Age2class+StemLength:Age2class, quasipoisson(link = "log"), data=total)
#summary(mod.glmquasi.NLxSLbyAge)

## estimables
#
#estimable(mod.glmquasi.NLxSLbyAge)

