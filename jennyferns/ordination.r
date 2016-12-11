library(vegan)
fernspps.df<-read.table("D:\\rdata\\jennyferns\\fernspps.txt", row.names="X", sep= "\t", header=T)
transenv.df<-read.table("D:\\rdata\\jennyferns\\transenv.csv", row.names="X", sep= ",", header=T)
names(fernspps.df)
names(transenv.df)
fernsppsDCA<-(decorana(fernspps.df))
fernsppsDCA
summary<-summary(fernsppsDCA)
fernsppsCA<-(cca(fernspps.df))
# (fernspps.df>0) turnsfernspps.df into binary data
# square brackets selects a subset
# if you do something before the comma in the square brackets it will apply it to the rows
fernsppsCA<-(cca(fernspps.df[,colSums(fernspps.df>0)>1]))
summary(fernsppsCA)
plot(fernsppsCA)
plot(fernsppsDCA)
plot(fernsppsDCA, xlim=c(-3,3), ylim=c(-3,3))
ef<-envfit(fernsppsDCA, transenv.df, permu=999)
ef
plot(ef)
savePlot("D:\\rdata\\jennyferns\\DCA.png")


# checking how much of the variance explained by any particular environmental variable is to do with the triplet they're located in
# step 1 triplet as environment
assign.df<-read.table("D:\\rdata\\jennyferns\\transectgroupings.txt", row.names="transect", sep= "\t", header=T)
names(assign.df)
tripletasenvt<-cca(fernspps.df~assign.df$triplet)
summary(tripletasenvt)
#step 2 pH as environment, triplet as covariate factor (this is pH within triplets)
pHbytriplet<-cca(fernspps.df ~ transenv.df$pH + assign.df$triplet)
summary(pHbytriplet)
#step 3 just pH as environment
justpH<-cca(fernspps.df ~ transenv.df$pH)
summary(justpH)

shrubbytriplet<-cca(fernspps.df ~ transenv.df$ShrubCover + assign.df$triplet)
justshrub<-cca(fernspps.df ~ transenv.df$ShrubCover)
summary(shrubbytriplet)
summary(justshrub)

BAbytriplet<-cca(fernspps.df ~ transenv.df$BasalArea + assign.df$triplet)
justBA<-cca(fernspps.df ~ transenv.df$BasalArea)
summary(BAbytriplet)
summary(justBA)

LLDbytriplet<-cca(fernspps.df ~ transenv.df$LLDepth + assign.df$triplet)
justLLD<-cca(fernspps.df ~ transenv.df$LLDepth)
summary(LLDbytriplet)
summary(justLLD)

Calciumbytriplet<-cca(fernspps.df ~ transenv.df$Ca + assign.df$triplet)
justCalcium<-cca(fernspps.df ~ transenv.df$Ca)
summary(Calciumbytriplet)
summary(justCalcium)

#STREAMDIST same idea as above
# step 1 streamdist as environment
distclassasenvt<-cca(fernspps.df~assign.df$distclass)
summary(distclassasenvt)
#step 2 pH as environment, streamdist as covariate factor (this is pH within distclasss)
pHbydistclass<-cca(fernspps.df ~ transenv.df$pH + assign.df$distclass)
summary(pHbydistclass)
#step 3 - this step not strictly necessary to repeat!


shrubbydistclass<-cca(fernspps.df ~ transenv.df$ShrubCover + assign.df$distclass)
summary(shrubbydistclass)
BAbydistclass<-cca(fernspps.df ~ transenv.df$BasalArea + assign.df$distclass)
summary(BAbydistclass)      
LLDbydistclass<-cca(fernspps.df ~ transenv.df$LLDepth + assign.df$distclass)
summary(LLDbydistclass)
Calciumbydistclass<-cca(fernspps.df ~ transenv.df$Ca + assign.df$distclass)
summary(Calciumbydistclass)

forestasenvt<-cca(fernspps.df~assign.df$Forest)
summary(forestasenvt)
shrubbyforest<-cca(fernspps.df ~ transenv.df$ShrubCover + assign.df$Forest)
summary(shrubbyforest)
BAbyforest<-cca(fernspps.df ~ transenv.df$BasalArea + assign.df$Forest)
summary(BAbyforest)      
LLDbyforest<-cca(fernspps.df ~ transenv.df$LLDepth + assign.df$Forest)
summary(LLDbyforest)
Calciumbyforest<-cca(fernspps.df ~ transenv.df$Ca + assign.df$Forest)
summary(Calciumbyforest)
pHbyforest<-cca(fernspps.df ~ transenv.df$pH + assign.df$Forest)
summary(pHbyforest)

DCA1<-read.table("D:\\rdata\\jennyferns\\DCA1.txt", sep= "\t", header=T)


# this will work when transenv.df is replaced by transenvnum: a file that only has numeric factors in - or maybe it needs to be continuous variables?
bioenv(fernspps.df, transenv.df, index = "euclid", upto = 5)