source("\\\\helix.klient.uib.no\\biohome\\aey022\\rdata\\Mabira vegetation survey data\\matrix veg load data.r")
library(vegan)

#'index' - so we can only use those samples for which we have all five plant groups, otherwise it will not work.
keepleast<- !is.na(trueherbs[,1])&rowSums(trueherbs)>0& !is.na(subcanopywoody[,1])&rowSums(subcanopywoody)>0 & !is.na(trees[,1])&rowSums(trees)>0 & !is.na(ferns[,1])&rowSums(ferns)>0
keepleast
#n=25

#The next lines make CAs using the 'keepleast' subset index and removing species that only occur once (because that is what we do for the CCAs and we need it to match them. Then the scores are saved as separsate objects to feed into the next step. ( We use CA as we're using ccas elswhere in the analyses).

ccatrueherbs.null<-cca(sqrt(trueherbs[keepleast,colSums(trueherbs>0, na.rm=T)>1]))
ccatrueherbs.null
herbsc<-scores(ccatrueherbs.null, choice=1:2)
ccascwoody.null<-cca(sqrt(subcanopywoody[keepleast,colSums(subcanopywoody>0, na.rm=T)>1]))
scwoodysc<-scores(ccascwoody.null, choice=1:2)
ccabigtrees.null<-cca(trees[keepleast,colSums(trees>0, na.rm=T)>1])
bigtreesc<-scores(ccabigtrees.null, choice=1:2)
ccafern.null<-cca(ferns[keepleast,colSums(ferns>0, na.rm=T)>1])
fernsc<-scores(ccafern.null, choice=1:2)


# protest - does a procrustes rotation of one dataset against another and returns the correlation of the two datasets after rotation.

crustyherbfern<-protest(herbsc, fernsc, scores = "sites", permutations = 9999)
crustyherbfern
crustyherbsmalltree<-protest(herbsc, scwoodysc, scores = "sites", permutations = 9999)
crustyherbsmalltree
crustyherbbigtree<-protest(herbsc, bigtreesc, scores = "sites", permutations = 9999)
crustyherbbigtree
crustyfernsmalltree<-protest(fernsc, scwoodysc, scores = "sites", permutations = 9999)
crustyfernsmalltree
crustyfernbigtree<-protest(fernsc, bigtreesc, scores = "sites", permutations = 9999)
crustyfernbigtree
crustysmallbigtree<-protest(scwoodysc, bigtreesc, scores = "sites", permutations = 9999)
crustysmallbigtree