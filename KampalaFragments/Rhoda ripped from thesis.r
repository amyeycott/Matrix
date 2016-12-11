lifted<-read.table("\\\\helix.klient.uib.no\\biohome\\aey022\\rdata\\KampalaFragments\\lifted3.txt", sep="\t", header=T)
lifted$reduction<-lifted$X2012Groupsize/lifted$X1995Groupsize
lifted$areachange<-lifted$X2010area/lifted$X1990area
lifted$arearevised<-lifted$colllins2010/lifted$collins1990
lifted$forest_type<-c("isolated","grouped","grouped","grouped","grouped","large reserve","grouped","isolated","isolated","small reserve")

x11();par (fin=c(5,5))
plot(lifted$arearevised,lifted$reduction, col= c("red","blue","yellow", "black")[as.factor(lifted$forest_type)], xlab="Proportion fragment area remaining", ylab="Proportion group size ", pch=16, xlim=c(0.5,1.0), cex=2) 
legend("topleft",c("grouped","isolated","large reserve","small reserve"),pch=16,col=c("red","blue","yellow", "black"))

plot(lifted$X2010area,lifted$X2012Groupsize, col= c("red","blue","yellow", "black")[as.factor(lifted$forest_type)], xlab="Fragment area", ylab="Group size", pch=16, xlim=c(0,20)) 
legend("topleft",c("grouped","isolated","large reserve","small reserve"),pch=16,col=c("red","blue","yellow", "black")) 

               