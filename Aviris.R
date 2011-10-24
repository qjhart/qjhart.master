# TODO: Add comment
# 
# Author: quinn
###############################################################################
source("/Users/quinn/Documents/workspace/MasterCalibration/sensorBand.R")

aviris <- function(fn) {
	spc<-read.table(paste(fn,"spc",sep="."),header=FALSE,sep="\t",col.names=c("lambda","fwhm"))
	gain<-read.fwf(paste(fn,"gain",sep="."),widths=c(11,16),col.names=c("gain","band"))
	spc$gain<-10/gain$gain
	
	bands<-sensor.bands(lapply(data.frame(t(spc)),
					function(x) sensor.band(lambda=x[1],fwhm=x[2],gain=x[3])))
	sensor(filename=fn,bands=bands)
}


