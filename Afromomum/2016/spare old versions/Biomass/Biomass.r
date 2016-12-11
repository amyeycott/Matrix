file.choose()
 library(RODBC)
 afro<-odbcConnectAccess("D:\\stat\\Afromomum\\Biomass\\Biomass.mdb")
 env<-sqlQuery(afro,"select * from environmental data")
  env
 