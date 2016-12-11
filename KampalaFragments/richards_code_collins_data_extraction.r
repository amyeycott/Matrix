library(RJDBC)

wd<-"D:\\rdata\\KampalaFragments\\" #####working directory
oo<-"FS1database"    #####name of oo file - no extension
classPath="C:/Program Files/OpenOffice.org 3/Basis/program/classes/hsqldb.jar"            #####depending on you instalation of oo this might change
setwd(wd)

shell(paste("copy ",oo,".odb ",oo,".zip", sep=""))                                        #copy oo file as zip
fi<-c("backup","data","properties","script")                                              #required files
unzip(paste(oo,"zip", sep="."),files=paste("database", fi,sep="/"), junkpaths=T,exdir=oo) #unzip required files
sapply(fi, function(f)shell(paste("rename ",oo,"\\",f," ",oo,".",f, sep="")))             #rename required file

j<-JDBC(driverClass ="org.hsqldb.jdbcDriver",classPath=classPath,identifier.quote="'")

conn<-dbConnect(j, url=paste("jdbc:hsqldb:file:",wd,"\\",oo,"\\",oo,";default_schema=true", sep=""), user="SA", password="")
dbListTables(conn)
dbGetQuery(conn, statement='SELECT * FROM "Forests"') ######sql queries  NB case sensitive NBrequires quotes  NB use single quotes to surround SQL
seedlings<-dbGetQuery(conn, statement='SELECT "plot"."forestname", "plot"."plotid", "Seedlings"."SpeciesCode", "Seedlings"."Abundance" FROM "Seedlings" AS "Seedlings", "plot" AS "plot" WHERE "Seedlings"."plotkey" = "plot"."key"'  )

thintreetableR<-dbGetQuery(conn, 'SELECT "Forests"."ForestName", "tree"."stump", "tree"."dbh", "tree"."speciescode" FROM "Forests" AS "Forests", "tree" AS "tree" WHERE "tree"."stump" = 0')

dbDisconnect(conn)          #tidy up - close connection
shell(paste("del /Q", oo))  #tidy up - delete files. Manually delete if necessary

names(seedlings)

trees<-table(thintreetableR$speciescode,thintreetableR$ForestName)

seedlings2<-xtabs(Abundance~paste(forestname,plotid, sep=".")+SpeciesCode, data=seedlings)
seedlings2<-as.data.frame(unclass(seedlings2))


