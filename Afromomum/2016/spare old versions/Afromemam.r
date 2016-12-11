library(RODBC)
#this bit loads the data
setwd("D:\\stat\\Afromomum") ## r\this is setting the working directory where all data are looked for and saved graphs/plots are kept
con<-odbcConnectAccess("research2.mdb")
sqlTables(con)



env<-sqlQuery(con,"SELECT Compartment.*, [Environmental data].* FROM Compartment LEFT JOIN  [Environmental data] ON (Compartment.TransID = [Environmental data].[Transect ID]) AND (Compartment.Site_ID = [Environmental data].[Site ID])")

perf<-sqlQuery(con,"SELECT Subplot.SiteID, Subplot.TransID, Subplot.PlotID, Subplot.NumofStems, Subplot.Canopy1, Subplot.Canopy2, Subplot.Canopy3, Subplot.Canopy4, Subplot.DBH, Subplot.Stumps, Subplot.Deadwood, Subplot.Location, Avg(Performance.StemLength) AS AvgOfStemLength, Avg(Performance.NumLeaves) AS AvgOfNumLeaves, Avg(Performance.Long_leaf) AS AvgOfLong_leaf, Avg(Performance.LeafBredth) AS AvgOfLeafBredth FROM Subplot INNER JOIN Performance ON (Subplot.PlotID = Performance.PlotID) AND (Subplot.TransID = Performance.TransID) AND (Subplot.SiteID = Performance.Site_ID) GROUP BY Subplot.SiteID, Subplot.TransID, Subplot.PlotID, Subplot.NumofStems, Subplot.Canopy1, Subplot.Canopy2, Subplot.Canopy3, Subplot.Canopy4, Subplot.DBH, Subplot.Stumps, Subplot.Deadwood, Subplot.Location;")


perfall<-sqlQuery(con,"SELECT Performance.*, Subplot.Location, Compartment.Category FROM Compartment INNER JOIN (Subplot INNER JOIN Performance ON (Subplot.PlotID = Performance.PlotID) AND (Subplot.TransID = Performance.TransID) AND (Subplot.SiteID = Performance.Site_ID)) ON (Compartment.TransID = Subplot.TransID) AND (Compartment.Site_ID = Subplot.SiteID);")


subplot<-sqlQuery(con,"SELECT Subplot.*, [Environmental data].*, Compartment.Category FROM (Compartment INNER JOIN Subplot ON (Compartment.TransID = Subplot.TransID) AND (Compartment.Site_ID = Subplot.SiteID)) INNER JOIN [Environmental data] ON (Subplot.TransID = [Environmental data].[Transect ID]) AND (Subplot.SiteID = [Environmental data].[Site ID]);")

 
close(con)
###################### data loading ends here##############################################

ls()      ##This mean r tell me what is in your brain'
str(env) ##This means tell me the structure of (env)
##data stored in data frame in r
head(subplot)
head(perfall)
head(perf)
head(env) ## another way of checking out what the data looks like, and useful if str returns a mess!

#Make a categorical varibale "Mature" vs "Recent" where mature is the old growth and old secondary together
env$Age2class<-ifelse(env$Category =="YS", c("Recent"), c("Mature"))
env$Age2class<-as.factor(env$Age2class)  ## changing from character string to categorical variable

## Generic 'making' or 'changing' functions look like the ones above. On the exteme left is the result that I want, in the middle is the function ot give me the results,and the extrem ritght is the thing the function act upon
## Generic looking out functions are without the left-hand part or arrow (e.g. str(env) ## searching functions 
t.test(pH~Age2class,data=env) ## to get ~ sign, altgr + key where sign is and press space
hist(env$pH[which(env$Age2class=="Recent")])
Recent<-env[which(env$Age2class=="Recent"),]
rm(Recent) #This removes the object whihc is in the bracket!!
savePlot("mature histogram")

str(env)

perfall$Age2class<-ifelse(perfall$Category =="YS", c("Recent"), c("Mature"))
perfall$Age2class<-as.factor(perfall$Age2class) 
str(perfall)

subplot$Age2class<-ifelse(subplot$Category =="YS", c("Recent"), c("Mature"))
subplot$Age2class<-as.factor(subplot$Age2class) 
str(subplot)

#fix location to be a factor not a numeric
subplot$Location<-as.factor(subplot$Location) 
perfall$Location<-as.factor(perfall$Location) 

#gives the number of plots per location and per forest age, then the number of plants measured per location and per forest age 
table(subplot$Location)
table(subplot$Age2class)
table(perfall$Age2class)
table(perfall$Location)


# working out whether the site georeferencing is ok
plot( env$Eastings,env$Northings)

#various plots about the number of stems. The first is a frequency plot for the different numbers of stems per plot, showing that most plots had few stems. The second is the same but logged.The third is about how the numer of stems varies with location; this one is then saved. 
hist(subplot$NumofStems, xlab="Number of Stems", main="")
hist(log(subplot$NumofStems+1, 10), xlab="Log10 Number of Stems + 1", main="")
boxplot((log(NumofStems+1))~Location, data=subplot, xlab="Location", ylab="log(numberof stems)")
savePlot("D:\\rdata\\Afromomum\\nstems_location_boxplot.emf")
#test whether there are differences in the number of stems between location groups
kruskal.test(NumofStems~Location, data=subplot)
# test whether that's still significant just for the three locations closest to the edge, because group 4 is very different.
just123<-subplot[which(subplot$Location<4),]
head(just123)
kruskal.test(NumofStems~Location, data=just123)


# same plots and tests gain but only considering those plots that have some aframomum in them, and we don't test the 'just 123' as the pattern is quite different
justdensity<-subplot[subplot$NumofStems>0,]
boxplot((log(NumofStems+1))~Location, data=justdensity, xlab="Location", ylab="log (numberof stems) where present")
savePlot("D:\\rdata\\Afromomum\\nstems_occupied only_location_boxplot.emf")
kruskal.test(NumofStems~Location, data=justdensity)

# same again but for forest age instead of distance from edge
boxplot((log(NumofStems+1))~Age2class, data=subplot, xlab="Forest type", ylab="log(number of stems)")
boxplot((log(NumofStems+1))~Age2class, data=justdensity, xlab="Forest type", ylab="log(number of stems) where present")
savePlot("D:\\rdata\\Afromomum\\nstems_occupied only_foresttype_boxplot.emf") 
kruskal.test(NumofStems~Age2class, data=justdensity)
#

#various plots about the impact of location on the other size measurements
boxplot(perf$AvgOfNumLeaves~perf$Location, xlab="Distance category", ylab="Plot mean number of leaves", ylim=c(0,25))
savePlot("D:\\rdata\\Afromomum\\nleaves_location_boxplot.emf")
boxplot(perf$AvgOfLong_leaf~perf$Location, xlab="Distance category", ylab="Plot mean length of longest leaf (cm)", ylim=c(0,30))
savePlot("D:\\rdata\\Afromomum\\longleaf_location_boxplot.emf")
boxplot(perf$AvgOfLeafBredth~perf$Location, xlab="Distance category", ylab="Plot mean width of widest leaf", ylim=c(0,10))
savePlot("D:\\rdata\\Afromomum\\Leafbreadth_location_boxplot.emf")

# load a library that makes it easy to make descriptive stats by group
library(psych)
# get things like mean and median stem length for each location
describe.by(subplot, subplot$Location)

# make a figure containing four graphs, plotting each of the size variables against the number of stems, and save it
x11();par(mfrow=c(2,2))
plot(perf$NumofStems,perf$AvgOfStemLength, ylim=c(0,160))
plot(perf$NumofStems,perf$AvgOfNumLeaves, ylim=c(0,25))
plot(perf$NumofStems,perf$AvgOfLong_leaf, ylim=c(0,30))
plot(perf$NumofStems,perf$AvgOfLeafBredth, ylim=c(0,8))
savePlot("D:\\rdata\\Afromomum\\nstems_size measurements.emf")
# make a figure containong plots o all the size variables against one another, and save it
x11()
pairs(perf[,c("AvgOfStemLength", "AvgOfNumLeaves",  "AvgOfLong_leaf",  "AvgOfLeafBredth")]  )
savePlot("D:\\rdata\\Afromomum\\pairs.emf")



str(perfall)

##### allometric modelling starts here #########
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


#models with interactions - ignore
#mod.glmquasi.NLxSLbyAge<- glm(NumLeaves~StemLength+Age2class+StemLength:Age2class, quasipoisson(link = "log"), data=total)
#summary(mod.glmquasi.NLxSLbyAge)

## estimables
#
#estimable(mod.glmquasi.NLxSLbyAge)

