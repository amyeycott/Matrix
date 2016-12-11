library(vegan)
#load data
spp<-read.csv( "D:\\rdata\\Budongo\\Mist netting data1.csv", row.names=1)
head(spp)
spp<-t(spp)
spp

                      
env<-read.csv("D:\\rdata\\Budongo\\all environmental variables.csv", row.names=1)
env<-t(env)
head(env)

spp<-spp[order(rownames(spp)),]
spp
env<-env[order(rownames(env)),]
env
pairs(env)
round(cor(env),2)
env.pca
prcomp(env,scale=T)
(env.pca<-prcomp(env,scale=T))
plot(env.pca)
biplot(env.pca)
identical(rownames(spp),rownames(env))
#cbind(rownames(spp),rownames(env))


########################################
plot(rev(sort(colSums(spp>0))))  #rank-occurance plots

rare<-sapply(1:max(rowSums(spp)),function(n){
  r<-rarefy(spp,n)
  r[rowSums(spp)<n]<-NA
  r
})

matplot(1:max(rowSums(spp)),t(rare), type="l")
 ######
#rarefaction
rowSums(spp>0)
rare<-rarefy(spp,min(rowSums(spp)))                                                                   
round(rare,1)
 diversity(spp,index="shannon")
 min(rowSums(spp))
 rarefy(spp,min(rowSums(spp)))
 diversity(spp,index="shannon")
 fisher.alpha(spp)
 
plot(log(sort(spp[1,],dec=T))) 
 
 matplot(1:ncol(sppf), log(apply(sppf/rowSums(sppf),1,sort,dec=T)), type="l", xlim=c(0,max(rowSums(sppf>0))))
 legend("topright",legend=rownames(sppf),lty=1:5, col=1:6) 