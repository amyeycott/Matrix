source("\\\\helix.klient.uib.no\\biohome\\aey022\\rdata\\Mabira vegetation survey data\\matrix veg load data.r")
library(vegan)

#'index' - so we can only use those samples for which we have all five plant groups, otherwise it will not work.
keepleast<- !is.na(trueherbs[,1])&rowSums(trueherbs)>0& !is.na(subcanopywoody[,1])&rowSums(subcanopywoody)>0 & !is.na(trees[,1])&rowSums(trees)>0 & !is.na(ferns[,1])&rowSums(ferns)>0
keepleast
#n=25

#The next lines make CAs using the 'keepleast' subset index and removing species that only occur once (because that is what we do for the CCAs and we need it to match them). Then the scores are saved as separate objects to feed into the next step. ( We use CA as we're using ccas elswhere in the analyses).

ccatrueherbs.null<-cca(sqrt(trueherbs[keepleast,colSums(trueherbs[keepleast,]>0, na.rm=T)>1])) 
summary(ccatrueherbs.null)[6]#38% explained in first two axes
herbsc<-scores(ccatrueherbs.null, choice=1:2)
ccascwoody.null<-cca(sqrt(subcanopywoody[keepleast,colSums(subcanopywoody[keepleast,]>0, na.rm=T)>1])) 
summary(ccascwoody.null)[6]#27%
scwoodysc<-scores(ccascwoody.null, choice=1:2)
ccabigtrees.null<-cca(trees[keepleast,colSums(trees[keepleast,]>0, na.rm=T)>1]) 
summary(ccabigtrees.null)[6]# 26%
bigtreesc<-scores(ccabigtrees.null, choice=1:2)
ccafern.null<-cca(fernabun[keepleast,colSums(fernabun[keepleast,]>0, na.rm=T)>1])
summary(ccafern.null)[6]#27%
fernsc<-scores(ccafern.null, choice=1:2)


# protest - does a procrustes rotation of one dataset against another and returns the correlation of the two datasets after rotation. Here on plain CAs with singletons removed

crustyherbfern<-protest(herbsc, fernsc, scores = "sites", permutations = 9999) #0,107 P=0,877
crustyherbfern
crustyherbsmalltree<-protest(herbsc, scwoodysc, scores = "sites", permutations = 9999) #0,271 0,237
crustyherbsmalltree
crustyherbbigtree<-protest(herbsc, bigtreesc, scores = "sites", permutations = 9999)   #0,363, 0,091 
crustyherbbigtree
crustyfernsmalltree<-protest(fernsc, scwoodysc, scores = "sites", permutations = 9999) #0,108 P=0,751
crustyfernsmalltree
crustyfernbigtree<-protest(fernsc, bigtreesc, scores = "sites", permutations = 9999)    #0,170 0,536
crustyfernbigtree
crustysmallbigtree<-protest(scwoodysc, bigtreesc, scores = "sites", permutations = 9999) #0,365 0,099
crustysmallbigtree


###testing whether downweight/no alteration and no singletons out/PCA/PCA with standardisation by row(site)/wisconsin/all the axes (in that order) makes any difference. Note that ".more" scores comes from lines 171-174. We already have fernsc, herbsc, scwoodysc, and bigtreesc, which have singletons removed  and the percent data are square rooted.  to call the % variance explained use  summary(caferns.plain)[6]

caferns.dw<-cca(downweight(fernabun[keepleast,colSums(fernabun[keepleast,]>0, na.rm=T)>1]))
summary(caferns.dw)[6] #28%  cf 27% with no downweight
fernsc.dw<-scores(caferns.dw, choice=1:2) 
caferns.plain<-cca(fernabun[keepleast,])
summary(caferns.plain)[6]#28% 
fernsc.plain<-scores(caferns.plain, choice=1:2)#28%
pcaferns<-rda(fernabun[keepleast,colSums(fernabun[keepleast,]>0, na.rm=T)>1])
summary(pcaferns)[6]#73%
fernsc.pca<-scores(pcaferns, choice=1:2)
fernpca.stand.sqrt<-rda(sqrt(decostand(fernabun[keepleast,colSums(fernabun[keepleast,]>0, na.rm=T)>1],method="total")))
summary(fernpca.stand.sqrt)[6]#38%
fernsc.pca.stand.sqrt<-scores(fernpca.stand.sqrt, choice=1:2)
caferns.wisc<-cca(wisconsin(fernabun[keepleast,colSums(fernabun[keepleast,]>0, na.rm=T)>1]))
summary(caferns.wisc)[6]#23%
fernsc.wisc<-scores(caferns.wisc, choice=1:2)

#do the different fern ordinations produce the same pattern? If they are significant then yes. First is the one reviewer 3 asks for - is a CA with singletons removed similar to a PCA with row standardisation? nb "plain" is without singleton removal
protest(fernsc, fernsc.pca.stand.sqrt, scores = "sites", permutations = 9999) #corr=0.368, P=0.050 unstable significance!!!
protest(fernsc.dw, fernsc.plain, scores = "sites", permutations = 9999) # corr=0.974, P=1e-04
protest(fernsc.dw, fernsc, scores = "sites", permutations = 9999)# corr=0.983, P= 1e-04   
protest(fernsc, fernsc.plain, scores = "sites", permutations = 9999) # corr=0.984, P= 1e-04
protest(fernsc.plain, fernsc.pca, scores = "sites", permutations = 9999) #corr=0.162, P=0.568   LOW NS
protest(fernsc.plain, fernsc.pca.stand.sqrt, scores = "sites", permutations = 9999) #corr=0.326, P=0.101  LOW NS
protest(fernsc.plain, fernsc.wisc, scores = "sites", permutations = 9999) #corr=0.671, P=0.001
protest(fernsc, fernsc.more, scores = "sites", permutations = 9999) #corr=0.641, P=1e-04
#so PCAs are different to CAs for ferns

#now the herbs. Needs sqrt because it's percent.
catrueherbs.dw<-cca(sqrt(downweight(trueherbs[keepleast,colSums(trueherbs[keepleast,]>0, na.rm=T)>1])))
summary(catrueherbs.dw)[6]#38%
herbsc.dw<-scores(catrueherbs.dw, choice=1:2)
catrueherbs.plain<-cca(sqrt(trueherbs[keepleast,])) 
summary(catrueherbs.plain)[6]#36%
herbsc.plain<-scores(catrueherbs.plain, choice=1:2)
pcatrueherbs<-rda(sqrt(trueherbs[keepleast,colSums(trueherbs[keepleast,]>0, na.rm=T)>1])) 
summary(pcatrueherbs)[6]#77% horrible plot!
herbsc.pca<-scores(pcatrueherbs, choice=1:2)
herbpca.stand.sqrt<-rda(sqrt(decostand(trueherbs[keepleast,colSums(trueherbs[keepleast,]>0, na.rm=T)>1],method="total")))
summary(herbpca.stand.sqrt)[6]#73%
herbsc.pca.stand.sqrt<-scores(herbpca.stand.sqrt, choice=1:2)
catrueherbs.wisc<-cca(wisconsin(trueherbs[keepleast,colSums(trueherbs[keepleast,]>0, na.rm=T)>1]))
summary(catrueherbs.wisc)[6]#21% 
herbsc.wisc<-scores(catrueherbs.wisc, choice=1:2)

protest(herbpca.stand.sqrt, herbsc, scores = "sites", permutations = 9999)   #corr=0.600, P=1e-04
protest(herbsc.dw, herbsc.plain, scores = "sites", permutations = 9999) # corr=0.949, P=1e-04
protest(herbsc.dw, herbsc, scores = "sites", permutations = 9999)# corr=0.999, P= 1e-04
protest(herbsc, herbsc.plain, scores = "sites", permutations = 9999) # corr=0.954, P= 1e-04
protest(herbsc.pca, herbsc.plain, scores = "sites", permutations = 9999) # corr=0.498, P=0.002    MID S
protest(herbsc.wisc, herbsc.plain, scores = "sites", permutations = 9999) # corr=0.703, P= 0.002   MID S
protest(herbsc.more, herbsc, scores = "sites", permutations = 9999) # corr=1, P= 1e-04 
#so it makes no difference to herbs which we use as long as we remove singletons

#next is scw
cascwoodys.dw<-cca(downweight(subcanopywoody[keepleast,colSums(subcanopywoody[keepleast,]>0, na.rm=T)>1])) 
summary(cascwoodys.dw)[6]#40%
scwoodysc.dw<-scores(cascwoodys.dw, choice=1:2)
cascwoodys.plain<-cca(subcanopywoody[keepleast,]) 
summary(cascwoodys.plain)[6]#35%
scwoodysc.plain<-scores(cascwoodys.plain, choice=1:2)
pcascwoodys<-rda(subcanopywoody[keepleast,colSums(subcanopywoody[keepleast,]>0, na.rm=T)>1]) 
summary(pcascwoodys)[6]#97%
scwoodysc.pca<-scores(pcascwoodys, choice=1:2)
scwpca.stand.sqrt<-rda(sqrt(decostand(subcanopywoody[keepleast,colSums(subcanopywoody[keepleast,]>0, na.rm=T)>1],method="total")))
summary(scwpca.stand.sqrt)[6]#97%
scwsc.pca.stand.sqrt<-scores(scwpca.stand.sqrt, choice=1:2) 
cascwoodys.wisc<-cca(wisconsin(subcanopywoody[keepleast,colSums(subcanopywoody[keepleast,]>0, na.rm=T)>1])) 
summary(cascwoodys.wisc)[6]#13%
scwoodysc.wisc<-scores(cascwoodys.wisc, choice=1:2)

protest(scwpca.stand.sqrt, scwoodysc, scores = "sites", permutations = 9999)# corr=0.677, P= 2e-04
protest(scwoodysc.dw, scwoodysc.plain, scores = "sites", permutations = 9999) # corr=0.995, P=1e-04
protest(scwoodysc.dw, scwoodysc, scores = "sites", permutations = 9999)# corr=0.924, P= 1e-04
protest(scwoodysc, scwoodysc.plain, scores = "sites", permutations = 9999) # corr=0.941, P=1e-04
protest(scwoodysc.pca, scwoodysc.plain, scores = "sites", permutations = 9999) # corr=0.274, P= 0.226 LOW NS
protest(scwoodysc.wisc, scwoodysc.plain, scores = "sites", permutations = 9999) # corr=0.617, P= 0.007 MID S
protest(scwoodysc.more, scwoodysc, scores = "sites", permutations = 9999) # corr=1, P= 1e-04 
# Doesn't seem to matter though a straight PCA is a bit different to a plain - can be explained by singletons

#last is big trees             
cabigtrees.dw<-cca(downweight(trees[keepleast,colSums(trees[keepleast,]>0, na.rm=T)>1]))
summary(cabigtrees.dw)[6]#26%
bigtreesc.dw<-scores(cabigtrees.dw, choice=1:2)
cabigtrees.plain<-cca(trees[keepleast,])
summary(cabigtrees.plain)[6] #21%
bigtreesc.plain<-scores(cabigtrees.plain, choice=1:2)
pcabigtrees<-rda(trees[keepleast,colSums(trees[keepleast,]>0, na.rm=T)>1])
summary(pcabigtrees)[6]#40%
bigtreesc.pca<-scores(pcabigtrees, choice=1:2)
bigtreespca.stand.sqrt<-rda(sqrt(decostand(trees[keepleast,colSums(trees[keepleast,]>0, na.rm=T)>1],method="total")))
summary(bigtreespca.stand.sqrt)[6]
bigtreessc.pca.stand.sqrt<-scores(herbpca.stand.sqrt, choice=1:2)
cabigtrees.wisc<-cca(wisconsin(trees[keepleast,colSums(trees[keepleast,]>0, na.rm=T)>1]))
summary(cabigtrees.wisc)[6] #16%
bigtreesc.wisc<-scores(cabigtrees.wisc, choice=1:2)

protest(bigtreespca.stand.sqrt, bigtreesc, scores = "sites", permutations = 9999)# corr=0.382, P=0.053 MID NS
protest(bigtreesc.dw, bigtreesc.plain, scores = "sites", permutations = 9999) # corr=0.597, P=0.006
protest(bigtreesc.dw, bigtreesc, scores = "sites", permutations = 9999)# corr=0.698, P=3e-04
protest(bigtreesc, bigtreesc.plain, scores = "sites", permutations = 9999) # corr=0.644, P= 0.003 
protest(bigtreesc.pca, bigtreesc.plain, scores = "sites", permutations = 9999) # corr=0.498, P= 0.037   MID S
protest(bigtreesc.wisc, bigtreesc.plain, scores = "sites", permutations = 9999) # corr=0.599, P= 0.005
protest(bigtreesc.more, bigtreesc, scores = "sites", permutations = 9999) # corr=1, P= 1e-04 
#bigtrees are a bit more bothered, the PCA-stant-sqrt is different to the CA. But is it just about singleton removal? Yes - this has now had singletons out for the pca-stand-sqqrt and it became significant
 
x11();par(mfrow=c(4,4), cex=0.4)
plot(caferns.plain)
plot(caferns.dw)
plot(ccafern.null) #this is really very different
plot(catrueherbs.plain)
plot(catrueherbs.dw)
plot(ccatrueherbs.null)
plot(cascwoodys.plain)
plot(cascwoodys.dw)
plot(ccascwoody.null)
plot(cabigtrees.plain)
plot(cabigtrees.dw)
plot(ccabigtrees.null)
savePlot("testing downweighting.emf", type="emf")

x11();par(mfrow=c(2,4), cex=0.4)
plot(bigtreespca.stand.sqrt, main="pca.stand.sqrt")
plot(scwpca.stand.sqrt)
plot(herbpca.stand.sqrt)
plot(fernpca.stand.sqrt)
plot(ccabigtrees.null, main="CA")
plot(ccascwoody.null)
plot(ccatrueherbs.null)
plot(ccafern.null) #this is really very different
savePlot("testing pca standardisation.emf", type="emf")

ferns[,9] # worst outlier is bolbitis, and is a singleton :-(
ferns[,28]# second worst outlier is in four places


#testing whether it affects the between-group scores (testing all the plains, all the downweights, all the pcas then all the standardised square rooted pcas)

protest(herbsc.plain, fernsc.plain, scores = "sites", permutations = 9999) # 0,090 0,866
protest(herbsc.plain, scwoodysc.plain, scores = "sites", permutations = 9999) #0,134, 0,579
protest(herbsc.plain, bigtreesc.plain, scores = "sites", permutations = 9999) #0,106, 0,924
protest(fernsc.plain, scwoodysc.plain, scores = "sites", permutations = 9999) #0,147, 0,603
protest(fernsc.plain, bigtreesc.plain, scores = "sites", permutations = 9999) # 0,153, 0,398 
protest(scwoodysc.plain, bigtreesc.plain, scores = "sites", permutations = 9999) #0,260, 0,147

protest(herbsc.dw, fernsc.dw, scores = "sites", permutations = 9999) # 0,131 0,767
protest(herbsc.dw, scwoodysc.dw, scores = "sites", permutations = 9999) #0,146, 0,601
protest(herbsc.dw, bigtreesc.dw, scores = "sites", permutations = 9999) #0,361, 0,098
protest(fernsc.dw, scwoodysc.dw, scores = "sites", permutations = 9999) #0,048, 0,963
protest(fernsc.dw, bigtreesc.dw, scores = "sites", permutations = 9999) # 0,196, 0,476 
protest(scwoodysc.dw, bigtreesc.dw, scores = "sites", permutations = 9999) #0,314, 0,094

#cf for the original version   LINES 22-35

protest(herbsc.pca, fernsc.pca, scores = "sites", permutations = 9999) # 0,329 0,140 
protest(herbsc.pca, scwoodysc.pca, scores = "sites", permutations = 9999) #0,348, 0,096
protest(herbsc.pca, bigtreesc.pca, scores = "sites", permutations = 9999) #0,229, 0,526
protest(fernsc.pca, scwoodysc.pca, scores = "sites", permutations = 9999) #0,247, 0,369
protest(fernsc.pca, bigtreesc.pca, scores = "sites", permutations = 9999) # 0,182, 0,615
protest(scwoodysc.pca, bigtreesc.pca, scores = "sites", permutations = 9999) #0,181, 0,607

protest(herbsc.wisc, fernsc.wisc, scores = "sites", permutations = 9999) #  0,210 0,495
protest(herbsc.wisc, scwoodysc.wisc, scores = "sites", permutations = 9999) #  0,303 0,202 
protest(herbsc.wisc, bigtreesc.wisc, scores = "sites", permutations = 9999) #  0,448 0,042  SIG
protest(fernsc.wisc, scwoodysc.wisc, scores = "sites", permutations = 9999) #   0,156 0,791
protest(fernsc.wisc, bigtreesc.wisc, scores = "sites", permutations = 9999) #     0,296 0,192
protest(scwoodysc.wisc, bigtreesc.wisc, scores = "sites", permutations = 9999) #     0,156 0,754

#this is comparing groups using reviewer 3's favourite method
protest(herbpca.stand.sqrt, fernpca.stand.sqrt, scores = "sites", permutations = 9999) #  0,212 0,585
protest(herbpca.stand.sqrt, scwpca.stand.sqrt, scores = "sites", permutations = 9999) #  0,478 0,004 SIG 
protest(herbpca.stand.sqrt, bigtreespca.stand.sqrt, scores = "sites", permutations = 9999) #  0,207 0,612  SIG
protest(fernpca.stand.sqrt, scwpca.stand.sqrt, scores = "sites", permutations = 9999) #   0,144 0,875
protest(fernpca.stand.sqrt, bigtreespca.stand.sqrt, scores = "sites", permutations = 9999) #   0,234 0,484
protest(scwpca.stand.sqrt, bigtreespca.stand.sqrt, scores = "sites", permutations = 9999) #     0,168 0,796





x11();par(mfrow=c(2,2))
screeplot(ccafern.null)
screeplot(ccatrueherbs.null)
screeplot(ccascwoody.null)
screeplot(ccabigtrees.null)  #suggests I should use more scores
savePlot("testing screeplots ca.emf", type="emf")


fernsc.more<-scores(ccafern.null)
herbsc.more<-scores(ccatrueherbs.null)
scwoodysc.more<-scores(ccascwoody.null)
bigtreesc.more<-scores(ccabigtrees.null)
protest(herbsc.more, fernsc.more, scores = "sites", permutations = 9999) # 0,107 0,873
protest(herbsc.more, scwoodysc.more, scores = "sites", permutations = 9999) #  0,271 0,231
protest(herbsc.more, bigtreesc.more, scores = "sites", permutations = 9999) #   0,363 0,098 
protest(fernsc.more, scwoodysc.more, scores = "sites", permutations = 9999) #   0,108 0,754
protest(fernsc.more, bigtreesc.more, scores = "sites", permutations = 9999) #   0,170 0,534
protest(scwoodysc.more, bigtreesc.more, scores = "sites", permutations = 9999) #  0,365 0,104


#plotting weight against frequency. 'null' is the thing in the original ms - sqrt for %, singletons removed. Points are plotted where size is proportional to abundance

x11();par(mfrow=c(2,2))
plot(ccafern.null$CA$v[,1]~colSums(fernabun[keepleast,colSums(fernabun[keepleast,]>0)>1]>0), ylab="weight (CA1 no singletons)", xlab="Number of ocurrences", main="ferns", col="white", xlim=c(0,25), ylim=c(-6,6))
points(ccafern.null$CA$v[,1]~colSums(fernabun[keepleast,colSums(fernabun[keepleast,]>0)>1]>0), cex=(colSums(fernabun[keepleast,colSums(fernabun[keepleast,]>0)>1])/100))
lines(x=c(0,25), y=c(0,0))
plot(ccatrueherbs.null$CA$v[,1]~colSums(trueherbs[keepleast,colSums(trueherbs[keepleast,]>0)>1]>0), ylab="weight (CA1 no singletons sqrt)", xlab="Number of ocurrences", main="herbs", col="white", xlim=c(0,25), ylim=c(-6,6))
lines(x=c(0,25), y=c(0,0))
points(ccatrueherbs.null$CA$v[,1]~colSums(trueherbs[keepleast,colSums(trueherbs[keepleast,]>0)>1]>0),cex=sqrt(colSums(trueherbs[keepleast,colSums(trueherbs[keepleast,]>0)>1]))/10)
plot(ccascwoody.null$CA$v[,1]~colSums(subcanopywoody[keepleast,colSums(subcanopywoody[keepleast,]>0)>1]>0), ylab="weight (CA1 no singletons sqrt)", xlab="Number of ocurrences", main="scw", col="white", xlim=c(0,25), ylim=c(-6,6))
lines(x=c(0,25), y=c(0,0))
points(ccascwoody.null$CA$v[,1]~colSums(subcanopywoody[keepleast,colSums(subcanopywoody[keepleast,]>0)>1]>0),, cex=sqrt(colSums(subcanopywoody[keepleast,colSums(subcanopywoody[keepleast,]>0)>1]))/10)
plot(ccabigtrees.null$CA$v[,1]~colSums(trees[keepleast,colSums(trees[keepleast,]>0)>1]>0), ylab="weight (CA1 no singletons)", xlab="Number of ocurrences", main="bigtrees", col="white", xlim=c(0,25), ylim=c(-6,6))
lines(x=c(0,25), y=c(0,0))
points(ccabigtrees.null$CA$v[,1]~colSums(trees[keepleast,colSums(trees[keepleast,]>0)>1]>0), cex=colSums(trees[keepleast,colSums(trees[keepleast,]>0)>1])/10)

 savePlot("testing scores vs frequency and abun in CA.emf", type="emf")
 
plot(ccafern.null$CA$v[,1]~colSums(fernabun[keepleast,colSums(fernabun[keepleast,]>0)>1]), ylab="weight (CA1 no singletons)", xlab="Sum abundance", main="ferns", ps=)
plot(ccatrueherbs.null$CA$v[,1]~colSums(trueherbs[keepleast,colSums(trueherbs[keepleast,]>0)>1]), ylab="weight (CA1 no singletons sqrt)", xlab="Sum abundance", main="herbs")
plot(ccascwoody.null$CA$v[,1]~colSums(subcanopywoody[keepleast,colSums(subcanopywoody[keepleast,]>0)>1]), ylab="weight (CA1 no singletons sqrt)", xlab="Sum abundance", main="scw")
plot(ccabigtrees.null$CA$v[,1]~colSums(trees[keepleast,colSums(trees[keepleast,]>0)>1]), ylab="weight (CA1 no singletons)", xlab="Sum abundance", main="bigtrees")
 savePlot("testing scores vs abundance in CA.emf", type="emf") 
 
plot(fernpca.stand.sqrt$CA$v[,1]~colSums(fernabun[keepleast,colSums(fernabun[keepleast,]>0)>1]>0), ylab="weight (PCA1 standardised (total))", xlab="Number of ocurrences", main="ferns")
plot(herbpca.stand.sqrt$CA$v[,1]~colSums(trueherbs[keepleast,colSums(trueherbs[keepleast,]>0)>1]>0), ylab="weight (PCA1 standardised (total)sqrt)", xlab="Number of ocurrences", main="herbs")
plot(scwpca.stand.sqrt$CA$v[,1]~colSums(subcanopywoody[keepleast,colSums(subcanopywoody[keepleast,]>0)>1]>0), ylab="weight (PCA1 standardised (total) sqrt)", xlab="Number of ocurrences", main="scw")
plot(bigtreespca.stand.sqrt$CA$v[,1]~colSums(trees[keepleast,colSums(trees[keepleast,]>0)>1]>0), ylab="weight (PCA1 standardised (total))", xlab="Number of ocurrences", main="bigtrees")
 savePlot("testing scores vs frequency in standPCA.emf", type="emf")
 
 plot(fernpca.stand.sqrt$CA$v[,1]~colSums(fernabun[keepleast,colSums(fernabun[keepleast,]>0)>1]), ylab="weight (PCA1 standardised (total))", xlab="Sum abundance", main="ferns")
plot(herbpca.stand.sqrt$CA$v[,1]~colSums(trueherbs[keepleast,colSums(trueherbs[keepleast,]>0)>1]), ylab="weight (PCA1 standardised (total)sqrt)", xlab="Sum abundance", main="herbs")
plot(scwpca.stand.sqrt$CA$v[,1]~colSums(subcanopywoody[keepleast,colSums(subcanopywoody[keepleast,]>0)>1]), ylab="weight (PCA1 standardised (total) sqrt)", xlab="Sum abundance", main="scw")
plot(bigtreespca.stand.sqrt$CA$v[,1]~colSums(trees[keepleast,colSums(trees[keepleast,]>0)>1]), ylab="weight (PCA1 standardised (total))", xlab="Sum abundance", main="bigtrees")
 savePlot("testing scores vs abun in standPCA.emf", type="emf")
 
x11();par(mfrow=c(2,2))
plot(colSums(fernabun[keepleast,colSums(fernabun[keepleast,]>0)>1]>0)~colSums(fernabun[keepleast,colSums(fernabun[keepleast,]>0)>1]), xlab="abun", ylab="freq", log=c("xy"))
plot(colSums(trueherbs[keepleast,colSums(trueherbs[keepleast,]>0)>1]>0)~colSums(trueherbs[keepleast,colSums(trueherbs[keepleast,]>0)>1]), xlab="abun", ylab="freq", log=c("xy"))
plot(colSums(subcanopywoody[keepleast,colSums(subcanopywoody[keepleast,]>0)>1]>0)~colSums(subcanopywoody[keepleast,colSums(subcanopywoody[keepleast,]>0)>1]), xlab="abun", ylab="freq", log=c("xy"))
plot(colSums(trees[keepleast,colSums(trees[keepleast,]>0)>1]>0)~colSums(trees[keepleast,colSums(trees[keepleast,]>0)>1]), xlab="abun", ylab="freq", log=c("xy"))

x11();par(mfrow=c(2,2))
screeplot(fernpca.stand.sqrt)
screeplot(herbpca.stand.sqrt)
screeplot(scwpca.stand.sqrt)
screeplot(bigtreespca.stand.sqrt) 
savePlot("testing screeplots pca_sqrt_stand.emf", type="emf")


#more tests - does a CA plot a site with 2 % species A and 4 % species B the same as 20% species A and 40% species B? This was to check that CA was its' own standardisation, before we understood what reviewer 3 actually wanted...
ferns.testset<-ferns[keep3,]
ferns.testset<-ferns.testset[,1:10]
ferns.testset<-ferns.testset[rowSums(ferns.testset)>0,]
ferns.testset<-ferns.testset[1:10,]
ferns.testset<-ferns.testset[,colSums(ferns.testset)>0]
ferns.testset[10,]<-c(10,20,0,20,0,0)
ferns.testset
plot(cca(ferns.testset))
test<-cca(ferns.testset)
scores(test)
test
test.stand<-cca(decostand(ferns.testset,method="total"))
test.stand
scores(test.stand)   #variation on first axis changes between the two, even though in both, scores for sites 19 and 20 are the same (site 20 is just site 19 times ten!)

#from here on the notes belong to Mathias
testtrees<- qbar(decostand(trees[keepleast,], method =
"total")

colSums(trees[keepleast,])
colSums(testtrees^2)


aa <- cov(testtrees[,round(colSums(testtrees)
) == 0])



bb <- cov(decostand(trees[keepleast,], method = "total"))

rowSums(aa^2)
rowSums(bb^2)