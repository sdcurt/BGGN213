#'---
#'title: "Class 5 Graphics"
#'author: "Stephanie Curtis"
#'date: "Friday January 25th"
#'output:github_document
#'---
#'
#' CLASS 05 GRAPHICS AND PLOTS WITH R
#' Stylish text **bold** and things. 
#' 
#Section2A: line plot
weight<-read.table("bimm143_05_rstats/weight_chart.txt",header=TRUE)
plot(weight,pch=1,sub="Interesting subtitle",cex=1.5,lwd=2,ylim=c(2,10),type="o",col="purple",xlab="Age (months)", ylab="Weight (kg)",main="Baby weight with age")
#2B: It's bar plot time
feat<-read.table("bimm143_05_rstats/feature_counts.txt",header=T,sep="\t")
par(mar=c(10,10,4,4))
barplot(feat$Count,names.arg=feat$Feature,las=1,horiz=T,xlab="Counts",main="Counts of features")
#Section3
mfc<-read.table("bimm143_05_rstats/male_female_counts.txt",header=T,sep="\t")
par(mar=c(6,6,4,4))
#hmanual hardwired version of saying 10 colors bc i literally counted how many samples i had and said make a color for each sampple
#barplot(mfc$Count,names.arg=mfc$Sample,las=2,col=rainbow(10))
#written more usefully/flexibly using nrow
barplot(mfc$Count,names.arg=mfc$Sample,las=2,col=rainbow(nrow(mfc)))
#Section 3B
genes<-read.delim("bimm143_05_rstats/up_down_expression.txt")
#Table function: so beautiful if i tell it to table a specific column containing descriptors of unchanging vs increasing vs decreasing it will tell me how many genes fall into each category wow
table(genes$State)
#color will return every data point (gene) in the color of hte number of states that exist (3) aka the unchanging increasing and decreasing wow
#save old color palette to be able to reset it later
ogpalette <- palette()
palette(c("blue","gray","red"))
plot(genes$Condition1,genes$Condition2,col=genes$State,xlab="Expression Condition 1", ylab="Expression Condition 2")
palette(ogpalette)
#versus the following which will give me the same number of colors as i have data points lol hashtagsomanycolorz
#but for that I need to clear palette somehow to get out of the blue gray red that I specified which is why I reset palette to the ogpalette that i saved
plot(genes$Condition1,genes$Condition2,col=genes$Gene,xlab="Expression Condition 1", ylab="Expression Condition 2")
