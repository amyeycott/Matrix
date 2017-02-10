#tiny things that got done in excel: Christella dentata capitalised in Gogonya 2, 'dead' row deleted from end of env file. Easting from the first occurrence of Mpanga 21 placed into empty easting field for Mpanga 20, and easting from the northing field of the second line for Mpanga 21 placed into easting for the first line and then second line deleted (not serious because we only use this to plot where sites were, if at all - not analytical info unless we start distance analyses.)

#test line

library(readxl)
ferns.thindf<-as.data.frame(read_excel("MAIN MATRIX DATA SEPT 16.xls"))
fernenv.df<-as.data.frame(read_excel("MATRIX Fern-Environmental  data 16 final.xls"))#gives warnings of NA when expecting numeric, due to empty fields - can ignore.
#fernallom.df<-

library(tidyr)
ferns.thindf$SPECIES<-trimws(ferns.thindf$SPECIES, which="both")
ferns.fatdf<-spread(ferns.thindf[,c(1,2,4,5)], SPECIES, FREQUENCE, fill=0)#turns a fat table into a thin one
rownames(ferns.fatdf)<-paste(ferns.fatdf$FOR, ferns.fatdf$PLOT_NO)
ferns.roworder<-ferns.fatdf[,c(1, 2)]
ferns.fatdf<-ferns.fatdf[,c(-1, -2)]

rownames(fernenv.df)<-paste(fernenv.df$FOR, fernenv.df$PLOT_NO)
#tidying the column names
colnames(fernenv.df)<-sub(" ", "_", colnames(fernenv.df))
colnames(fernenv.df)<-sub(" ", "_", colnames(fernenv.df)) #needs running twice, as it only replaces the first instance.
colnames(fernenv.df)<-sub("\\(", "_", colnames(fernenv.df))
colnames(fernenv.df)<-sub(")", "", colnames(fernenv.df))



fernpatches<-fernenv.df[!is.na(fernenv.df$Patch_size_ha),9:11]#MAGIC NUMBERS splits out the patch levels characteristics. May want to trim the numbers off the ends.
rownames(fernpatches)<-sapply(strsplit(rownames(fernpatches), split=" "), "[[",1)

#is the number of plots roughly in proportion with the patch size?
fernpatches<-merge(fernpatches, as.data.frame(unclass(by(fernenv.df, fernenv.df$FOR, nrow))), by=0)
library(dplyr)
rename()
#rename(iris, petal_length = Petal.Length)

setdiff(rownames(fernenv.df), rownames(ferns.fatdf))#check all the sites are in both datasets. Currently a problem with Kituuza 16 and Kituuza 17 in env not in spp
setdiff(rownames(ferns.fatdf),rownames(fernenv.df))# "KITUUZA 1" "KITUUZA 2" "WAMALA 3" are in spp not in env


#NA species in empty quarats needs removing from fatferns, there's more rows in 

