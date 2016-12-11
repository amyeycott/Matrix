#load csv file
jennyenv <- read.table("d:/rdata/jennyferns/envi_all.csv", header=TRUE, sep=",")
#OR
jennyenvall <- read.table("d:/rdata/jennyferns/envi_alltransects.txt", header=TRUE, sep="\t")
#list the variable names available
names(jennyenv)
names(jennyenvall)


#the following works and need importing into our data import script
        #Make the missing value for Na by using a linear model of the other cations, to make a variable TCat (total cations) to append to env. To start, turn Na from a factor into a number with NA
        jennyenvall$Na<-as.numeric(as.character(jennyenvall$Na))
        is.numeric(jennyenvall$Na)
        #then run a linear model on the plots with full data - are the missing values automatically missed?
        cationmodel<-lm(Na~K*Ca*Mg, data=jennyenvall)
        summary(cationmodel)
        #then get r to predict the value of na given the values for the other cations in the plot with missing data (transect 74)
        missingsodium<-predict(cationmodel, data.frame(K=0.45, Ca=31.35, Mg=4.59))
        #returns 0.03149051, which is within the range. Replace NA with this in the Na vector
        jennyenvall$Na[is.na(jennyenvall$Na)]<-missingsodium 
        jennyenvall$TCat<-(jennyenvall$K+jennyenvall$Mg+jennyenvall$Ca+jennyenvall$Na)


#then I select the columns that are cation fields and all except the last row, which is where the missing data is 
cations<-jennyenvall[(1:60),(15:18)]
pairs(cations)
savePlot("D:\\rdata\\jennyferns\\cationpairs.emf")

#plot boxplots (here is an example)
boxplot(jennyenv$Mg~jennyenv$Forest, col=(c("gold","darkgreen","red")), main="Soil Magnesium by Forest Age", xlab="Forest Age", ylab="Mg unknown units")
#note that Na has one missing value which is entered as a . by Jenny, I took the average for that forest type which is bad                              
# boxplots can be notched to give a sort of confidence of the median, although this often leads to very odd looking plots in skewed distributions
boxplot(jennyenv$Slope~jennyenv$Forest, col=(c("gold","darkgreen","red")), notch=TRUE, main="Slope by Forest Age", xlab="Forest Age", ylab="Slope (degrees from horizontal)")
savePlot("D:\\rdata\\jennyferns\\Slope by forest type boxplot notched.emf")
boxplot(jennyenv$BasalArea~jennyenv$Forest, col=(c("gold","darkgreen","red")), notch=TRUE, main="Basal Area by Forest Age", xlab="Forest Age", ylab="basal area (unkown units)")
savePlot("D:\\rdata\\jennyferns\\Basal area by forest type boxplot notched.emf")
boxplot(jennyenv$ShrubCover~jennyenv$Forest, col=(c("gold","darkgreen","red")), notch=TRUE, main="Shrub cover by Forest Age", xlab="Forest Age", ylab="Shrub cover (%)")
savePlot("D:\\rdata\\jennyferns\\Shrub cover by forest type boxplot notched.emf")
boxplot(jennyenv$Deadwood~jennyenv$Forest, col=(c("gold","darkgreen","red")), notch=TRUE, main="Deadwood score by Forest Age", xlab="Forest Age", ylab="Deadwood score")
savePlot("D:\\rdata\\jennyferns\\Deadwood score by forest type boxplot notched.emf")
boxplot(jennyenv$LLCover~jennyenv$Forest, col=(c("gold","darkgreen","red")), notch=TRUE, main="Leaf litter cover by Forest Age", xlab="Forest Age", ylab="Leaf litter cover (%)")
savePlot("D:\\rdata\\jennyferns\\Leaf litter cover by forest type boxplot notched.emf")
boxplot(jennyenv$LLDepth~jennyenv$Forest, col=(c("gold","darkgreen","red")), notch=TRUE, main="Leaf litter depth by Forest Age", xlab="Forest Age", ylab="Leaf litter depth (cm)")
savePlot("D:\\rdata\\jennyferns\\Leaf litter depth by forest type boxplot notched.emf")
boxplot(jennyenv$pH~jennyenv$Forest, col=(c("gold","darkgreen","red")), notch=TRUE, main="Soil pH by Forest Age", xlab="Forest Age", ylab="pH")
savePlot("D:\\rdata\\jennyferns\\pH by forest type boxplot notched.emf")
boxplot(jennyenv$N.~jennyenv$Forest, col=(c("gold","darkgreen","red")), notch=TRUE, main="Soil nitrogen by Forest Age", xlab="Forest Age", ylab="Nitrogen %")
savePlot("D:\\rdata\\jennyferns\\Soil nitrogen by forest type boxplot notched.emf")
boxplot(jennyenv$C.~jennyenv$Forest, col=(c("gold","darkgreen","red")), notch=TRUE, main="Soil carbon by Forest Age", xlab="Forest Age", ylab="Carbon %")
savePlot("D:\\rdata\\jennyferns\\Soil carbon by forest type boxplot notched.emf")
boxplot(jennyenv$K~jennyenv$Forest, col=(c("gold","darkgreen","red")), notch=TRUE, main="Soil Potassium by Forest Age", xlab="Forest Age", ylab="K (unknown units)")
savePlot("D:\\rdata\\jennyferns\\Soil K forest type boxplot notched.emf")
boxplot(jennyenv$Ca~jennyenv$Forest, col=(c("gold","darkgreen","red")), notch=TRUE, main="Soil Calcium by Forest Age", xlab="Forest Age", ylab="Ca (unknown units)")
savePlot("D:\\rdata\\jennyferns\\Soil Ca forest type boxplot notched.emf")
boxplot(jennyenv$Mg~jennyenv$Forest, col=(c("gold","darkgreen","red")), notch=TRUE, main="Soil Magnesium by Forest Age", xlab="Forest Age", ylab="Mg (unknown units)")
savePlot("D:\\rdata\\jennyferns\\Soil Mg forest type boxplot notched.emf")
boxplot(jennyenv$Na~jennyenv$Forest, col=(c("gold","darkgreen","red")), notch=TRUE, main="Soil Sodium by Forest Age", xlab="Forest Age", ylab="Na (unknown units)")
savePlot("D:\\rdata\\jennyferns\\Soil Na forest type boxplot notched.emf")
boxplot(jennyenv$CanO~jennyenv$Forest, col=(c("gold","darkgreen","red")), notch=TRUE, main="Canopy openness by Forest Age", xlab="Forest Age", ylab="Canopy openness (%)")
savePlot("D:\\rdata\\jennyferns\\Canopy openess forest type boxplot notched.emf")
boxplot(jennyenv$Cnrat~jennyenv$Forest, col=(c("gold","darkgreen","red")), notch=TRUE, main="Soil C:N ratio by Forest Age", xlab="Forest Age", ylab="C:N")
savePlot("D:\\rdata\\jennyferns\\Soil CNratio forest type boxplot notched.emf")
boxplot(jennyenv$spprich~jennyenv$Forest, col=(c("gold","darkgreen","red")), notch=TRUE, main="Fern species richness by Forest Age", xlab="Forest Age", ylab="Number of species")
savePlot("D:\\rdata\\jennyferns\\Spp rich by forest type boxplot notched.emf")

#plot spprich against env vars, with points coloured by forest age
plot (jennyenv$spprich~jennyenv$pH, col=as.factor(jennyenv$Forest), xlab="Soil pH", ylab="n fern species")
#for plots with too many overlapping points, add jitter. Each plot will need the amount of jitter tinkering with.
plot (jitter(jennyenv$spprich, amount = 0.1)~jitter(jennyenv$m.from.stream, amount=1), col=as.factor(jennyenv$Forest), xlab="Distance from stream (m)", ylab="n fern species")
plot (jitter(jennyenvall$SPPRICH, amount = 0.1)~jitter(jennyenvall$Deadwood, amount=0.01), col=as.factor(jennyenvall$Forest), xlab="Number of large deadwood items", ylab="Number of fern species")
#add a legend. Note that R will wait for you to click the bit of the graph where you want the legend>
legend(locator(1),c("og","os","ys"),pch=c(1,1,1),col=c(1,2,3))

#pull out the variables for a matrix of scatterplots, where the numbers after the c are the column numbers of the variables of interest)
structuredata<- jennyenv[c(10,11,13:15)]
#plot a matrix of scatterplots of soil nutrients or structural variables
pairs(structuredata)
#plot with histograms in the diagonals          NOT TRIED
pairs(structuredata, )

# histograms for each continuous env var
attach(jennyenv)
hist(Slope, freq=TRUE, plot=TRUE, col="darkgreen")
savePlot("D:\\rdata\\jennyferns\\Slope histogram.emf")
hist(BasalArea, breaks=12, freq=TRUE, plot=TRUE, col="darkgreen")
savePlot("D:\\rdata\\jennyferns\\Basal Area histogram.emf")
hist(Deadwood, freq=TRUE, plot=TRUE, col="darkgreen")
savePlot("D:\\rdata\\jennyferns\\Deadwood histogram.emf")
hist(Stumps, freq=TRUE, plot=TRUE, col="darkgreen")
savePlot("D:\\rdata\\jennyferns\\Stumps histogram.emf")
hist(LLCover, breaks=10,freq=TRUE, plot=TRUE, col="darkgreen")
savePlot("D:\\rdata\\jennyferns\\LLCover histogram.emf")
hist(LLDepth, freq=TRUE, plot=TRUE, col="darkgreen")
savePlot("D:\\rdata\\jennyferns\\LLDepth histogram.emf")
hist(ShrubCover, breaks=12, freq=TRUE, plot=TRUE, col="darkgreen")
savePlot("D:\\rdata\\jennyferns\\ShrubCover histogram.emf")
hist(pH, freq=TRUE, plot=TRUE, col="darkgreen")
savePlot("D:\\rdata\\jennyferns\\pH histogram.emf")
hist(N., freq=TRUE, plot=TRUE, col="darkgreen", main="Soil Nitrogen")
savePlot("D:\\rdata\\jennyferns\\Nitrgogen histogram.emf")
hist(C., freq=TRUE, plot=TRUE, col="darkgreen", main = "Soil Carbon")
savePlot("D:\\rdata\\jennyferns\\Soil Carbon histogram.emf")
hist(K, freq=TRUE, breaks=14, plot=TRUE, col="darkgreen", main="Soil Potassium")
savePlot("D:\\rdata\\jennyferns\\Soil Potassium histogram.emf")
hist(Ca, freq=TRUE, breaks=14, plot=TRUE, col="darkgreen", main = "Soil Calcium")
savePlot("D:\\rdata\\jennyferns\\Soil Calcium histogram.emf")
hist(Mg, freq=TRUE, breaks=14, plot=TRUE, col="darkgreen", main = "Soil Magnesium")
savePlot("D:\\rdata\\jennyferns\\Soil Magnesium histogram.emf")
hist(Na, freq=TRUE, breaks=14, plot=TRUE, col="darkgreen", main = "Soil Sodium")
savePlot("D:\\rdata\\jennyferns\\Soil Sodium histogram.emf")
hist(CanO, breaks=16, freq=TRUE, plot=TRUE, col="darkgreen", main = "Canopy percent openness")
savePlot("D:\\rdata\\jennyferns\\Canopy percent openness.emf")
hist(Cnrat, freq=TRUE, plot=TRUE, col="darkgreen", main = "Soil Carbon:Nitrogen Ratio")
savePlot("D:\\rdata\\jennyferns\\Soil CN ratio histogram.emf")
hist(spprich, breaks=12, freq=TRUE, plot=TRUE, col="darkgreen", main = "Species Richness")
savePlot("D:\\rdata\\jennyferns\\Species richness histogram.emf")
detach(jennyenv)



# bar charts for factor env vars
attach(jennyenv)
compartmentcounts <- table(Compartment)
barplot(compartmentcounts, xlab="Forest Compartment", ylab="number of transects") 
savePlot("D:\\rdata\\jennyferns\\Compartment count barchart.emf")
detach(jennyenv)
#This one doesn't seem to work
attach(jennyenv)
aspectcounts <- table(Slope)
barplot(aspectcounts, xlab="Aspect class", ylab="number of transects") 
savePlot("D:\\rdata\\jennyferns\\Aspect count barchart.emf")
detach(jennyenv)

# notched boxplots for env vars by stream dist
boxplot(jennyenv$Slope~jennyenv$m.from.stream, col=("red"),notch=TRUE, main="Slope by Distance from stream", xlab="Distance from stream", ylab="Slope (degrees from horizontal)")
savePlot("D:\\rdata\\jennyferns\\Slope by distance from stream boxplot notched.emf")
boxplot(jennyenv$BasalArea~jennyenv$m.from.stream, col=("red"),notch=TRUE, main="Basal Area by Distance from stream", xlab="Distance from stream", ylab="basal area (unkown units)")
savePlot("D:\\rdata\\jennyferns\\Basal area by distance from stream boxplot notched.emf")
boxplot(jennyenv$ShrubCover~jennyenv$m.from.stream, col=("red"),notch=TRUE, main="Shrub cover by Distance from stream", xlab="Distance from stream", ylab="Shrub cover (%)")
savePlot("D:\\rdata\\jennyferns\\Shrub cover by distance from stream boxplot notched.emf")
boxplot(jennyenv$Deadwood~jennyenv$m.from.stream, col=("red"),notch=TRUE, main="Deadwood score by Distance from stream", xlab="Distance from stream", ylab="Deadwood score")
savePlot("D:\\rdata\\jennyferns\\Deadwood score by distance from stream boxplot notched.emf")
boxplot(jennyenv$LLCover~jennyenv$m.from.stream, col=("red"),notch=TRUE, main="Leaf litter cover by Distance from stream", xlab="Distance from stream", ylab="Leaf litter cover (%)")
savePlot("D:\\rdata\\jennyferns\\Leaf litter cover by distance from stream boxplot notched.emf")
boxplot(jennyenv$LLDepth~jennyenv$m.from.stream, col=("red"),notch=TRUE, main="Leaf litter depth by Distance from stream", xlab="Distance from stream", ylab="Leaf litter depth (cm)")
savePlot("D:\\rdata\\jennyferns\\Leaf litter depth by distance from stream boxplot notched.emf")
boxplot(jennyenv$pH~jennyenv$m.from.stream, col=("red"),notch=TRUE, main="Soil pH by Distance from stream", xlab="Distance from stream", ylab="pH")
savePlot("D:\\rdata\\jennyferns\\pH by distance from stream boxplot notched.emf")
boxplot(jennyenv$N.~jennyenv$m.from.stream, col=("red"),notch=TRUE, main="Soil nitrogen by Distance from stream", xlab="Distance from stream", ylab="Nitrogen %")
savePlot("D:\\rdata\\jennyferns\\Soil nitrogen by distance from stream boxplot notched.emf")
boxplot(jennyenv$C.~jennyenv$m.from.stream, col=("red"),notch=TRUE, main="Soil carbon by Distance from stream", xlab="Distance from stream", ylab="Carbon %")
savePlot("D:\\rdata\\jennyferns\\Soil carbon by distance from stream boxplot notched.emf")
boxplot(jennyenv$K~jennyenv$m.from.stream, col=("red"),notch=TRUE, main="Soil Potassium by Distance from stream", xlab="Distance from stream", ylab="K (unknown units)")
savePlot("D:\\rdata\\jennyferns\\Soil K distance from stream boxplot notched.emf")
boxplot(jennyenv$Ca~jennyenv$m.from.stream, col=("red"),notch=TRUE, main="Soil Calcium by Distance from stream", xlab="Distance from stream", ylab="Ca (unknown units)")
savePlot("D:\\rdata\\jennyferns\\Soil Ca distance from stream boxplot notched.emf")
boxplot(jennyenv$Mg~jennyenv$m.from.stream, col=("red"),notch=TRUE, main="Soil Magnesium by Distance from stream", xlab="Distance from stream", ylab="Mg (unknown units)")
savePlot("D:\\rdata\\jennyferns\\Soil Mg distance from stream boxplot notched.emf")
boxplot(jennyenv$Na~jennyenv$m.from.stream, col=("red"),notch=TRUE, main="Soil Sodium by Distance from stream", xlab="Distance from stream", ylab="Na (unknown units)")
savePlot("D:\\rdata\\jennyferns\\Soil Na distance from stream boxplot notched.emf")
boxplot(jennyenv$CanO~jennyenv$m.from.stream, col=("red"),notch=TRUE, main="Canopy openness by Distance from stream", xlab="Distance from stream", ylab="Canopy openness (%)")
savePlot("D:\\rdata\\jennyferns\\Canopy openess distance from stream boxplot notched.emf")
boxplot(jennyenv$Cnrat~jennyenv$m.from.stream, col=("red"),notch=TRUE, main="Soil C:N ratio by Distance from stream", xlab="Distance from stream", ylab="C:N")
savePlot("D:\\rdata\\jennyferns\\Soil CNratio distance from stream boxplot notched.emf")



#plot abundance against env vars, with points coloured by forest age
plot (jennyenvall$abun_ish~jennyenvall$pH, col=as.factor(jennyenvall$Forest), xlab="Soil pH", ylab="n ferns estimate")
#for plots with too many overlapping points, add jitter. Each plot will need the amount of jitter tinkering with.
plot (jitter(jennyenvall$spprich, amount = 0.1)~jitter(jennyenv$m.from.stream, amount=1), col=as.factor(jennyenv$Forest), xlab="Distance from stream (m)", ylab="n fern species")
#add a legend. Note that R will wait for you to click the bit of the graph where you want the legend>
legend(locator(1),c("og","os","ys"),pch=c(1,1,1),col=c(1,2,3))
savePlot("D:\\rdata\\jennyferns\\Spp abun by pH.emf")

#plot abundance against the rest of the env vars, with points coloured by forest age
plot (jennyenvall$abun_ish~jennyenvall$Slope, col=as.factor(jennyenvall$Forest), xlab="Degrees from horizontal", ylab="n ferns estimate")
legend(locator(1),c("og","os","ys"),pch=c(1,1,1),col=c(1,2,3))
savePlot("D:\\rdata\\jennyferns\\Spp abun by slope.emf")
plot (jennyenvall$abun_ish~jennyenvall$BA, col=as.factor(jennyenvall$Forest), xlab="Basal area", ylab="n ferns estimate")
legend(locator(1),c("og","os","ys"),pch=c(1,1,1),col=c(1,2,3))
savePlot("D:\\rdata\\jennyferns\\Spp abun by BA.emf")
plot (jennyenvall$abun_ish~jennyenvall$Deadwood, col=as.factor(jennyenvall$Forest), xlab="Deadwood score", ylab="n ferns estimate")
legend(locator(1),c("og","os","ys"),pch=c(1,1,1),col=c(1,2,3))
savePlot("D:\\rdata\\jennyferns\\Spp abun by Deadwood.emf"
plot (jennyenvall$abun_ish~jennyenvall$LLCover, col=as.factor(jennyenvall$Forest), xlab="leaf litter cover %", ylab="n ferns estimate")
legend(locator(1),c("og","os","ys"),pch=c(1,1,1),col=c(1,2,3))
savePlot("D:\\rdata\\jennyferns\\Spp abun by LLcover.emf"
plot (jennyenvall$abun_ish~jennyenvall$LLDepth, col=as.factor(jennyenvall$Forest), xlab="leaf litter depth %", ylab="n ferns estimate")
legend(locator(1),c("og","os","ys"),pch=c(1,1,1),col=c(1,2,3))
savePlot("D:\\rdata\\jennyferns\\Spp abun by LLdepth.emf")
plot (jennyenvall$abun_ish~jennyenvall$ShrubCover, col=as.factor(jennyenvall$Forest), xlab="Shrub cover %", ylab="n ferns estimate")
legend(locator(1),c("og","os","ys"),pch=c(1,1,1),col=c(1,2,3))
savePlot("D:\\rdata\\jennyferns\\Spp abun by shrub cover.emf")
plot (jennyenvall$abun_ish~jennyenvall$N., col=as.factor(jennyenvall$Forest), xlab="Soil Nitrogen (unknown units)", ylab="n ferns estimate")
legend(locator(1),c("og","os","ys"),pch=c(1,1,1),col=c(1,2,3))
savePlot("D:\\rdata\\jennyferns\\Spp abun by soil N.emf")
plot (jennyenvall$abun_ish~jennyenvall$C., col=as.factor(jennyenvall$Forest), xlab="Soil Carbon (unknown units)", ylab="n ferns estimate")
legend(locator(1),c("og","os","ys"),pch=c(1,1,1),col=c(1,2,3))
savePlot("D:\\rdata\\jennyferns\\Spp abun by soil C.emf")
plot (jennyenvall$abun_ish~jennyenvall$Ca, col=as.factor(jennyenvall$Forest), xlab="Soil Calcium (unknown units)", ylab="n ferns estimate")
legend(locator(1),c("og","os","ys"),pch=c(1,1,1),col=c(1,2,3))
savePlot("D:\\rdata\\jennyferns\\Spp abun by soil Ca.emf")
plot (jennyenvall$abun_ish~jennyenvall$CanO, col=as.factor(jennyenvall$Forest), xlab="Canopy percent openness", ylab="n ferns estimate")
legend(locator(1),c("og","os","ys"),pch=c(1,1,1),col=c(1,2,3))
savePlot("D:\\rdata\\jennyferns\\Spp abun by Canopy.emf")
plot (jennyenvall$abun_ish~jennyenvall$SPPRICH, col=as.factor(jennyenvall$Forest), xlab="Species richness", ylab="n ferns estimate")
legend(locator(1),c("og","os","ys"),pch=c(1,1,1),col=c(1,2,3))
savePlot("D:\\rdata\\jennyferns\\Spp abun by richness.emf")



#boxplots for category vars
boxplot(jennyenvall$abun_ish~jennyenvall$m.from.stream, col=("red"),notch=TRUE, main="Fern abundance estimate by Distance from stream", xlab="Distance from stream", ylab="Abundance estimate")
savePlot("D:\\rdata\\jennyferns\\Abun_ish by distance from stream boxplot notched.emf")
boxplot(jennyenvall$abun_ish~jennyenvall$Forest, col=("red"),notch=TRUE, main="Fern abundance estimate by Forest type", xlab="Distance from stream", ylab="Abundance estimate")
savePlot("D:\\rdata\\jennyferns\\Abun_ish by forest type boxplot notched.emf")
boxplot(jennyenvall$abun_ish~jennyenvall$Deadwood, col=("red"),notch=TRUE, main="Fern abundance estimate by Deadwood score", xlab="Deadwood score", ylab="Abundance estimate")
savePlot("D:\\rdata\\jennyferns\\Abun_ish by Deadwood score boxplot notched.emf")


boxplot(jennyenvall$SPPRICH~jennyenvall$m.from.stream, notch=TRUE, main="Fern species richness by distance from stream", xlab="Distance from stream (m)", ylab="Number of species", col=(c("gold","darkgreen","red")))
savePlot("D:\\rdata\\jennyferns\\Spp rich by distance from stream boxplot notched.emf")