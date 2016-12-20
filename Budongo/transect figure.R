x11(5,3); par(mgp=c(1.2,0.3,0), pin=c(4,2))
plot(x=c(0,400), y=c(-100,100), type="n", xlab="Distance from edge in metres", bty="n",yaxt="n", ylab="",tcl=-0.2)
mtext("Patch edge", side=2,line=0)
mtext("Patch interior", side=4,line=0)

lines(c(1,400),c(0,0), lty=3)
lines(c(1,1), c(-75, 75), col="grey30", lwd=3)
#18 plus 18 up
lines(c(50,50), c(0,18))
lines(c(50,50), c(20,38))
#4x13s down
lines(c(100,100), c(0,-13))
lines(c(100,100), c(-15,-28))
lines(c(100,100), c(-15,-28))
lines(c(100,100), c(-30,-43))
lines(c(100,100), c(-45,-58))
#4x13s up
lines(c(150,150), c(0,13))
lines(c(150,150), c(15,28))
lines(c(150,150), c(15,28))
lines(c(150,150), c(30,43))
lines(c(150,150), c(45,58))
#18 plus 18 down
lines(c(200,200), c(0,-18))
lines(c(200,200), c(-20,-38))
#18 plus 18 up
lines(c(250,250), c(0,18))
lines(c(250,250), c(20,38))
#4x13s down
lines(c(300,300), c(0,-13))
lines(c(300,300), c(-15,-28))
lines(c(300,300), c(-15,-28))
lines(c(300,300), c(-30,-43))
lines(c(300,300), c(-45,-58))
#4x13s up
lines(c(350,350), c(0,13))
lines(c(350,350), c(15,28))
lines(c(350,350), c(15,28))
lines(c(350,350), c(30,43))
lines(c(350,350), c(45,58))
#18 plus 18 down
lines(c(400,400), c(0,-18))
lines(c(400,400), c(-20,-38))
savePlot("Uwimbabazi Fig 1 inset transect.eps", type="eps")
savePlot("Uwimbabazi Fig 1 inset transect.pdf", type="pdf")
savePlot("Uwimbabazi Fig 1 inset transect.emf", type="emf")

#figure on temporal patterns in sampling
source("moreen fragments load data.r")
library(ggplot2)
library(tidyr)

unique(sppthin$Date)
forblobplot<-as.data.frame(unclass(table(sppthin$Date, sppthin$plot)))
head(forblobplot)
forblobplot$date<-rownames(forblobplot)     
forblobplot2<-gather(forblobplot,"transect","count", 1:21)
forblobplot2<-forblobplot2[!forblobplot2$count==0,]

plotty<-ggplot(forblobplot2, aes(x=transect, y=date))
plotty+ geom_point()
