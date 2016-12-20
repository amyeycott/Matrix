#this is not Budongo data, but Kibale data on chimp feeding phenology. It is placed here for convenience.
#load the data (I have messed around with the excel file a bit beforehand - taken away the graph and replaces spaces with underscores in the column names)
library(readxl)
foods<-read_excel("Data for the graph.xls", sheet=1,col_names = TRUE)

#just the plot, the line and the symbols
x11(5,5)
plot(foods$average_feeding_time, foods$average_energy_intake, xlab=expression(paste("Average percent feeding time day"^-1)), ylab=expression(paste("Average energy intake day"^-1)), xlim=c(0,80), ylim=c(0,80), pch=1:nrow(foods))#note the use of expression and paste to make the per day thing. 
abline(a=0, b=1) #a is the intercept, b is the slope, and a slope of 1 gives a 1:1 relationship.
legend("bottomright", pch=1:nrow(foods), legend=foods$Food_item)

#with error bars - we need a function from another package
library(Hmisc)
errbar(foods$average_feeding_time, foods$average_energy_intake, xlab=expression(paste("Average percent feeding time day"^-1)), ylab=expression(paste("Average energy intake day"^-1)), xlim=c(-10,100), ylim=c(-10,100), pch=1:nrow(foods), yplus=foods$average_energy_intake+foods$standard_deviation_energy_intake, yminus=foods$average_energy_intake-foods$standard_deviation_energy_intake) #neat y error bars. Note the change in xlim and ylim to make the error bars fir in the plot.

#X error bars are a little trickier. I had a look on the ointernet and you can either learn ggplot package or make arrows with horizontal tips!
arrows(x0=foods$average_feeding_time-foods$standard_deviation_feeding_time,x1=foods$average_feeding_time+foods$standard_deviation_feeding_time,y0=foods$ average_energy_intake,angle=90, length=0.03)
arrows(x0=foods$average_feeding_time+foods$standard_deviation_feeding_time,x1=foods$average_feeding_time-foods$standard_deviation_feeding_time,y0=foods$ average_energy_intake,angle=90, length=0.03)#you have to do it twice to get the caps on both ends, swap around the plus and minuses.
abline(a=0, b=1) #a is the intercept, b is the slope, and a slope of 1 gives a 1:1 relationship.
legend("bottomright", pch=1:nrow(foods), legend=foods$Food_item)
savePlot("Feeding efficiency figure for Moreen.jpg", type="jpg")
savePlot("Feeding efficiency figure for Moreen.emf", type="emf")
savePlot("Feeding efficiency figure for Moreen.wmf", type="wmf")
savePlot("Feeding efficiency figure for Moreen.eps", type="eps")
savePlot("Feeding efficiency figure for Moreen.pdf", type="pdf")
