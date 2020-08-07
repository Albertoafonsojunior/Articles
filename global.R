

rm(list=ls(all=TRUE))
# Download .asc file
# setwd("/SIOC290-ClimateMath2016/Rcodes/NOAAGlobalTemp")
da1=scan("NOAAGlobalTemp.gridded.v4.0.1.201701.asc")
length(da1)

#Install maps package if not done before
#install.packages("maps")
library(maps)
Lat= seq(-87.5, 87.5, length=36)
Lon=seq(2.5, 357.5, length=72)
mapmat=matrix(gpcpst[,1634],nrow=72)
#column 1634 corresponding to Dec 2015
#Covert the vector into a lon-lat matrix for R map plotting
mapmat=pmax(pmin(mapmat,6),-6)
#This command compresses numbers larger than 6 to 6
plot.new()
par(mar=c(4,5,3,0))
int=seq(-6,6,length.out=81)

rgb.palette=colorRampPalette(c('black','blue', 'darkgreen','green',
'yellow','pink','red','maroon'),interpolate='spline')
mapmat= mapmat[,seq(length(mapmat[1,]),1)]
filled.contour(Lon, Lat, mapmat, color.palette=rgb.palette, levels=int,
               plot.title=title(main="NOAAGlobalTemp Anomalies Dec 2015 [deg C]",
                                xlab="Latitude",ylab="Longitude", cex.lab=1.5),
               plot.axes={axis(1, cex.axis=1.5);
                 axis(2, cex.axis=1.5);map('world2', add=TRUE);grid()},
               key.title=title(main="[oC]"),
               key.axes={axis(4, cex.axis=1.5)})


plot(density(tmean15), main="Kernel estimate of density",
     xlab="Temperature") #Kernel estimate density
lines(xfit,dnorm(xfit, mean=mean(tmean15),
                 sd=sd(tmean15)), col="blue") #Moment estimated normal

