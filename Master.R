# TODO: Add comment
# 
# Author: quinn
# http://tolstoy.newcastle.edu.au/R/help/04/10/6138.html
###############################################################################
library("rgdal")
source("/Users/quinn/Documents/workspace/MasterCalibration/sensorBand.R")

master <- function(fn,spectral_file='data/master_bandpass.txt') {
	sfnt<-read.table(spectral_file,header=FALSE,sep="\t")

	master<-list()
	master$filename<-fn
	lb<-readGDAL(paste("HDF4_SDS:UNKNOWN",fn,"0",sep=":"))
	cb<-readGDAL(paste("HDF4_SDS:UNKNOWN",fn,"1",sep=":"))
	rb<-readGDAL(paste("HDF4_SDS:UNKNOWN",fn,"2",sep=":"))
	# bands in nanometers (How to standardize?)
	spc<-data.frame(l=lb@data$band1*1000,lambda=cb@data$band1*1000,r=rb@data$band1*1000)
	spc$fwhm<-(spc$r-spc$l)

	# gau=gaussian and apx=spectral approx function
	i<-list(apx=function(x) {1})
	spc$info<-rep(i,50)

	bands<-list();
	for (i in 1:50) {
		l<-spc$l[i]
		r<-spc$r[i]
		c<-spc$lambda[i]
		if (i > 25) 
		{	sens<-data.frame(lambda=c(l,c,r),R=c(0.5,1,0.5))
			bands[[i]]<-sensor.band(lambda=c,fwhm=r-l)
		} else {
			bands[[i]]<-sensor.band(lambda=sfnt[sfnt[,i+1]>0,1]*1000,R=sfnt[sfnt[,i+1]>0,i+1])
			sens<-data.frame(lambda=sfnt[,1]*1000,R=sfnt[,i+1])
		}
		gau<-make.gaussian(c,r-l)	
		apx<-approxfun(sens$lambda,sens$R)
		fs<-splinefun(sens$lambda,sens$R)
		spc$info[i]$apx<-apx
	}
	master$bands<-spc
	sensor(bands=sensor.bands(bands),filename=fn)
}
