# TODO: Add comment
# 
# Author: quinn
# http://tolstoy.newcastle.edu.au/R/help/04/10/6138.html
###############################################################################

make.gaussian <- function(lambda, fwhm)
{
		m<-lambda
		s<-fwhm/(2*sqrt(2*log(2)))
		function(x) exp(-(x-m)^2/(2*s^2))
}

library("rgdal")

create.aviris <- function(fn) {
	aviris<-list()
	aviris$filename<-fn
	spc<-read.table(paste(fn,"spc",sep="."),header=FALSE,sep="\t",col.names=c("lambda","fwhm"))
	gain<-read.fwf(paste(fn,"gain",sep="."),widths=c(11,16),col.names=c("gain","band"))
	spc$gain<-10/gain$gain
	
	i<-list(g=function(x) 1)
	spc$info<-rep(i,224)
#	info<-list()
	for (i in 1:224) {
		l<-spc$lambda[i]
		f<-spc$fwhm[i]
		g<-make.gaussian(l,f)
#		info[[i]]<-list(g=g)
		spc$info[i]$g<-g
	}
#	aviris$info<-info
	aviris$bands<-spc
	aviris
}

aviris<-create.aviris("/Users/quinn/master/popo.jpl.nasa.gov/pub/SUstin/f110608t01p00r08rdn_a/f110608t01p00r08rdn_a")


create.modis <- function(fn) {
	modis<-list()
	modis$filename<-fn
	lb<-readGDAL(paste("HDF4_SDS:UNKNOWN",fn,"0",sep=":"))
	cb<-readGDAL(paste("HDF4_SDS:UNKNOWN",fn,"1",sep=":"))
	rb<-readGDAL(paste("HDF4_SDS:UNKNOWN",fn,"2",sep=":"))
	# bands in nanometers (How to standardize?)
	spc<-data.frame(l=lb@data$band1*1000,lambda=cb@data$band1*1000,r=rb@data$band1*1000)
	spc$fwhm<-(spc$r-spc$l)

	i<-list(g=function(x) 1)
	spc$info<-rep(i,50)
	
#	info<-list()
	for (i in 1:50) {
		l<-spc$l[i]
		r<-spc$r[i]
		c<-spc$lambda[i]
		sens<-data.frame(lambda=c(l,c,r),R=c(0.5,1,0.5))
		g<-make.gaussian(c,r-l)	
		fs<-splinefun(sens$lambda,sens$R)
		spc$info[i]$g<-g
#		spc$info[i]$fs<-fs
#		info[[i]]<-list(g=g,fs=fs,sens=sens)
	}
#	modis$info<-info
	modis$bands<-spc
	modis
}

modis<-create.modis("/Users/quinn/master/MASTERL1B_1165100_04_20110608_2305_2310_V00.hdf")


# Now get the factors that you need for AVIRIS calculation
afacts<-list()
for (i in 1:50) {
	g<-modis$bands$info[i]$g
	af<-g(aviris$bands$lambda)
	# Remove factors for 160 (questionable lambda) 
	af[160]<-0
	# Remove 1200 factors from MODIS band 16
	if (i==16) {af[161]<-0; af[162]<-0}
	gf<-af*aviris$bands$gain
	
	bands<-which(af>0.03)
	norm<-sum(af[bands])
	# Normalize the sum back to radiance
	gnorm<-sum(gf[bands])
		
	# This should be the starting point for function minimiation on the integral of these functions w/ the modis function
	# But for now, let's say they are correct.
	# use factors > %3 to eliminate end problems, and simplify modis edges
	afacts[[i]]<-list(band=bands,r=af[bands],g=gf[bands],gnorm=gnorm,norm=norm)
}

baz<-rep.int(0,length(modis$bands$lambda))
for (i in 1:length(modis$bands$lambda)) { baz[i]<-length(afacts[[i]]$band) }
which(baz>0)

A<-c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R")

afacts[[1]]$A<-A[1:length(afacts[[1]]$band)]

paste(paste(afacts[[1]]$A,afacts[[1]]$r,sep='*'),collapse='+')

# This creates the 25 MODIS functions that are used 
for (m in 1:25) { 
	eval(parse(text=
	paste("m",m,"<-function(x){",paste(afacts[[m]]$r,"*aviris$info[[",afacts[[m]]$band,"]]$g(x)",sep="",collapse="+"),"}",sep="")
	))}

plot(function(x) m1(x),420,500)
curve(modis$info[[1]]$g(x)/30,add=TRUE,col=2)

plot(function(x) mband[[1]]$g(x),400,2500)
for (i in 1:25) {
	curve(mband[[i]]$g(x),add=TRUE,col=i)
}

