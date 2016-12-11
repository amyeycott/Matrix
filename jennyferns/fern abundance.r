setwd("D:\\rdata\\jennyferns\\")
#fern data including empty transects
fernabun<-read.table("foresttypeabundances.txt", sep="\t", header=T, row.names=1)
head(fernabun)
fernabun<-fernabun[order(rownames(fernabun)),]
dim(fernabun)

fill.na<-function(x){
  x1<-setdiff(1:74, rownames(x))
  x2<-matrix(NA, nrow=length(x1), ncol=ncol(x))
  rownames(x2)<-x1
  colnames(x2)<-colnames(x)
  x<-rbind(x,x2)
  x<-x[order(as.numeric(rownames(x))),]
  x
}

fernabun<-fill.na(fernabun)
dim(fernabun)

boxplot(rowSums(fernabun)~env$Forest, col=(c("gold","darkgreen","red")), notch=TRUE, main="Fern abundance by Forest age", xlab="Forest Age", ylab="Total abundance (index)")