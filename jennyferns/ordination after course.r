library(vegan)
fernspps.df<-read.table("D:\\rdata\\jennyferns\\fernspps.txt", row.names="X", sep= "\t", header=T)
transenv.df<-read.table("D:\\rdata\\jennyferns\\transenv.csv", row.names="X", sep= ",", header=T)
str(fernspps.df)
str(transenv.df)
transenv.df$Compartment<-factor(transenv.df$Compartment)
str(transenv.df)


fernsppsDCA<-(decorana(fernspps.df))
fernsppsDCA

# a CA of either no species which only occur once, or no sites that only have one thing in them
subsetCA<-(cca(fernspps.df[,colSums(fernspps.df>0)>1]))
subsetCA2<-(cca(fernspps.df[rowSums(fernspps.df>0)>1,])

totaloccs<- sum(colSums(fernspps.df>0)>1)



sppca<-cca(fernspps.df)
plot(sppca)
summary(sppca)
# plot looks ok, not bad outliers, no need for dca, 
screeplot(sppca, bstick=T)
savePlot("D:\\rdata\\jennyferns\\CAscree.eps")
# plot looks horrid -flat slope compared to bstick, (note: no bstick for dca, no screeplot for nmds)

sppccafull<-cca(fernspps.df~., data=transenv.df)
plot(sppccafull, type="n")
fitall <- envfit(sppccafull, transenv.df, perm = 999, display = "lc")
plot(fitall, p.max = 0.05, col = "red")
savePlot("D:\\rdata\\jennyferns\\CCAenvfit.emf")


modelwithnothingin<-cca(fernspps.df~1, data=transenv.df)
ordistep(modelwithnothingin, scope=formula(sppccafull), perm.max = 1000)

envsubset<-transenv.df[c(2,4:22)]
str(envsubset)
modelwithnothinginss<-cca(fernspps.df~1, data=envsubset)
ccasubset<-cca(fernspps.df~., data=envsubset)
ordistep(modelwithnothinginss, scope=formula(ccasubset), perm.max = 1000)

envsubset2<-transenv.df[c(2,5:22)]
str(envsubset2)
modelwithnothinginss2<-cca(fernspps.df~1, data=envsubset)
ccasubset2<-cca(fernspps.df~., data=envsubset2)
ordistep(modelwithnothinginss, scope=formula(ccasubset2), perm.max = 1000)

 

sppsmds<-metaMDS(fernspps.df)
plot(sppsmds)
#also looks ok

# colour by forest type
p<-plot(sppsmds, type="n")
points(sppsmds, display="sites", col=as.numeric(transenv.df$Forest, pch=16)
identify(p, what="sites")