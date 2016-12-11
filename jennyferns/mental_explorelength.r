#import data
fernsize<- read.table("d:/rdata/jennyferns/Species_all.csv", header=TRUE, sep=",")
#load library for describe.by
library(psych)
#big mush of descriptive stats by species and forest age
describe.by(fernsize, list(fernsize$binomial,fernsize$ForestAge))

#I want to get the number of rows fitting certain criteria (forest age, and transect which is nested within forest age)

#crazy ass boxplot
boxplot(fernsize$TotalLength~fernsize$binomial*fernsize$ForestAge, col=(c("gold","darkgreen","red")), main="Stipe length", xlab="Species and Forest Age")
#table giving counts of samples in each forest age by each species
sizetable<- table(fernsize$binomial,fernsize$ForestAge)
sizetable

#Select out the data wanted
Asp_macdata <- fernsize[ which(fernsize$binomial=='Asplenium macrophlebium'),]
Chr_pardata <- fernsize[ which(fernsize$binomial=='Christella parasitica'),]
Pter_catdata <- fernsize[ which(fernsize$binomial=='Pteris catoptera'),]
Pter_hamdata <- fernsize[ which(fernsize$binomial=='Pteris hamulosa'),]
Tec_gemmdata <- fernsize[ which(fernsize$binomial=='Tectaria gemmifera'),]          



# make boxplts for continuous dependants
boxplot(Chr_pardata$LeafLength~Chr_pardata$ForestAge, col=(c("gold","darkgreen","red")), main="Chr par Leaf length", xlab="Forest Age")

#make crosstabs for category dependants
Tecgemmtable <- xtabs(~ForestAge+Gemmae, data=Tec_gemmdata)
ftable(Tecgemmtable) 
summary(Tecgemmtable)   #returns chisquare, but keeps returning "not a number"