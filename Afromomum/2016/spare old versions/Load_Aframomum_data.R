library(RODBC)
#1 use only one database
#2 If you update, and really want to keep an old version, keep that old version in a sub-folder and stop using it in your code.
#3 ID in two different tables isn't the same thing
#4 use only one connection per database
#5 links don't always update from database design to query design

# from here on stick with flat tables in excel, and use checks in r to make sure things got entered sensibly (e.g. levels, unique)

#note to amy: go into data exploration, find out what we need, then get rt to write the right query.

afram.con<-odbcConnectAccess("Biomass.mdb")#we switched to the mdb version so it would work on my computer as well as yours. Has the mdb version got mistakes, or just lacks queries?
research.con<-odbcConnectAccess("research2.mdb")#these have the same data as biomass. Why aren't we using just one database?
#biomass.con<-odbcConnectAccess2007("Biomass_3_17_04_2015.accdb")#no need to connect to the same db twice
perform<-sqlQuery(afram.con,"select * from performance")
compart<-sqlQuery(research.con,"select * from Compartment")
#compart2<-sqlQuery(research.con,"select * from comp")#plantID is linked to compartmentID 1:1
compart3<-sqlQuery(research.con,"select * from Avgperform")#plantID is linked to compartmentID 1:1
biomass<-sqlQuery(biomass.con,"select * from biomass")#query gives averages per site not per transect
NL<-sqlQuery(research.con,"select * from Avgperform")#this looks the same as compart3 and thus has the same problems
close(afram.con)#good practice to close connections

perform[rowSums(perform[,c(11:14,16)])==0,c(11:14,16)]<-NA#turn the unweighedplant to NAs else the lme falls over
#a lazy fix to get forest types into perform outside of the db
perform_types<-merge(perform, compart[c(2,3,7)], by="TransID", all=TRUE)

#when the database and extractions are finalised, do
#save("object1",) blah blah see help. Then the objects will be stable even if RODBC or drivers change

spChar<-xtabs(StemLength~PlantID,perform)
stemlength<-perform$StemLength
minlength<-min(stemlength)
maxlength<-max(stemlength)
sumlength<-sum(stemlength)


#we need to match forest type into perfom
