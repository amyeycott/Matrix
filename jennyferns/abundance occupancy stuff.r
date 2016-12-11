loading the different subsets
abun_occ.df<-read.table("D:\\rdata\\jennyferns\\abun_occ.txt", sep= "\t", header=T)
fernsog.df<-read.table("D:\\rdata\\jennyferns\\fernsppsjustog.txt", sep= "\t", header=T)
fernsos.df<-read.table("D:\\rdata\\jennyferns\\fernsppsjustos.txt", sep= "\t", header=T)
fernsys.df<-read.table("D:\\rdata\\jennyferns\\fernsppsjustys.txt", sep= "\t", header=T)
fernsog.df
fernsos.df
fernsys.df
abun_occ.df


# row sum on a dataframe called fernsos.df
rowSums(fernsos.df)
# count the number of nonzero cells in a row, and plot those counts in a histogram
hist(rowSums(fernsos.df>0), main="Species richness of transects in Old Secondary")
savePlot("D:\\rdata\\jennyferns\\sample_richness_hist_os_only.emf")
hist(rowSums(fernsog.df>0), main="Species richness of transects in Old Growth")
savePlot("D:\\rdata\\jennyferns\\sample_richness_hist_og_only.emf")
hist(rowSums(fernsys.df>0), main="Species richness of transects in Young Secondary")
savePlot("D:\\rdata\\jennyferns\\sample_richness_hist_ys_only.emf")

hist(colSums(fernsos.df>0), breaks=22, xlab="number of occurences", ylab="number of species", main="Old Secondary")
savePlot("D:\\rdata\\jennyferns\\species_occ_hist_os_only.emf")
hist(colSums(fernsog.df>0), breaks=22, xlab="number of occurences", ylab="number of species", main="Old Grwoth")
savePlot("D:\\rdata\\jennyferns\\species_occ_hist_og_only.emf")
hist(colSums(fernsys.df>0), breaks=22, xlab="number of occurences", ylab="number of species", main="Young Secondary")
savePlot("D:\\rdata\\jennyferns\\species_occ_hist_ys_only.emf")

plot()

plot (jitter(abun_occ.df$nocurrences, amount = 0.1)~jitter(abun_occ.df$median.density, amount=1), xlab="Median abundance index", ylab="n ocurrences")

plot (jitter(abun_occ.df$Median_richness, amount = 0.1)~jitter(abun_occ.df$nocurrences, amount=0.1), xlab="n ocurrences", ylab="Median richness where present")