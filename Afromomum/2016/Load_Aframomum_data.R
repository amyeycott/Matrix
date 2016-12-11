library(RODBC)

research.con<-odbcConnectAccess("research2016.mdb")
biomass.con<-odbcConnectAccess("Biomass2016.mdb")

performsizes2011<-sqlQuery(research.con,"select * from performance")
performbiomass2013<-sqlQuery(biomass.con,"select * from Performance1")
#MAKE THE SAME KIND OF COMMAND TO EXTRACT ALL THE OTHER TABLES FROM BIOMASS AND RESEARCH (4 OF EACH IN TOTAL, USE THE SAME .CON FOR ALL but don't extract environment table from biomass)
environ2011<-sqlQuery(research.con, "select * from Environmental_data")

performCat2011<-sqlQuery(research.con,"select * from PerformanceplusCategoryandLocation")
performCat2013<-sqlQuery(biomass.con,"select * from PerformanceplusCategoryandLocation")
closeAllConnections()


#Try to find the same information out again: minimum, maximu and mean stem length in each database
stemlength<-perform$StemLength#just to show me
minlength<-min(stemlength)#just for show me
maxlength<-max(stemlength)# just to show me
sumlength<-sum(stemlength)#just to show me