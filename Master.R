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

	bands<-list();
	for (i in 1:50) {
		l<-spc$l[i]
		r<-spc$r[i]
		c<-spc$lambda[i]
#		print(sprintf("%d:",i))
		if (i > 25) 
		{	# This is default spline, which you could use everywhere if you didn't have
			# the spectral response functions.  We don't use the more simple sensor.band(lambda=c,fwhm=r-l)
			# becase the l and r aren't symettric around c
			bands[[i]]<-sensor.band(lambda=c(l-(c-l),l,c,r,r+(r-c)),
									R=c(0,0.5,1,0.5,0))
		} else {
			bands[[i]]<-sensor.band(lambda=sfnt[sfnt[,i+1]>0,1]*1000,R=sfnt[sfnt[,i+1]>0,i+1])
		}
	}
	sensor(bands=sensor.bands(bands),filename=fn)
}

# This is how you plot one set of multipliers
master.plot_aviris_multipliers <- function(master,i) {
	band<-master$bands[[i]]
	av<-band$av
	plot(band$R,xlim=c(band$lambda-band$fwhm,band$lambda+band$fwhm),
			main=sprintf('MASTER Band #%d\nAVIRIS Replication',i),
			xlab="Wavelength",
			ylab="Response"
	)
	r<-av$rcombfun
	curve(r,add=TRUE,col='blue')
	b<-av$bestcombfun
	curve(b,add=TRUE,col='green')
	legend("bottom",c("Weights","Best"),lwd=1,col=c("blue","green"))
}

master.get_aviris_factors <- function(m,a,i) {
	band<-m$bands[[i]]
	use<-rep(TRUE,length(lambda(a$bands)))
	# band 160 is always bad
	use[160]<-FALSE
	if (i==5) {
		use[31]<-FALSE
		use[32]<-FALSE
	} else if (i==16) { # These two bands useless for MODIS band 16
		use[161]<-FALSE
		use[162]<-FALSE
	}
	# use factors > %3 to eliminate end problems, and simplify master edges
	af<-weights(a$bands,band$f,interval=c(band$min,band$max),minweight=0.03,use=use)
	weights<-data.frame(band=which(af>0),r1=af[af>0])
	# Get a Linear combination of the default and optimum weights.
	fun<-combine.sensor.bands(a$bands,
			weights=data.frame(band=weights$band,weight=weights$r1))
	
	area<-list()
	area$r<-integrate(fun,lower=band$min,upper=band$max)$value
	
	weights$r<-weights$r1*band$area/area$r
	
	best<-optimizedweights.sensor.bands(a$bands,band$f,
			interval=c(band$min,band$max),
			guess=data.frame(band=weights$band,weight=weights$r))
	weights$opt1<-best$weight
	
	fun<-combine.sensor.bands(a$bands,
			weights=data.frame(band=weights$band,weight=weights$opt1))
	area$best<-integrate(fun,lower=band$min,upper=band$max)$value
	weights$opt<-weights$opt1*band$area/area$best
	
	
	# We make a new list of AVIRIS parameters, including, 
	# the AVIRIS band closest to the MASTER band, max_band,
	av<-list(max_band=which(af==max(af[af>0]))[1],
			area=area,
			weights=weights)
	
	av$rcombfun<-combine.sensor.bands(a$bands,
			weights=data.frame(band=weights$band,weight=weights$r))
	
	av$bestcombfun<-combine.sensor.bands(a$bands,
			weights=data.frame(band=weights$band,weight=weights$opt))
	
	return(av)
}

# Calculate the factors needed for replicating the MASTER bands w/ AVIRIS
# Pass in the AVIRIS sensror, returns a MASTER  w/ a set an AVIRIS slot, that 
# shows the best linear combinations to use for replicate the AVIRIS bands
master.get_all_aviris_factors <- function(m,a) {
	for (i in 1:25) {
		av <- master.get_aviris_factors(m,a,i)
		m$bands[[i]]$av<-av
	}
}

