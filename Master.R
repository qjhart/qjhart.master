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

# This could be a 
sensor.band <- function(lambda,fwhm,response)
{
	
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


create.master <- function(fn,spectral_fn) {
	sfn<-'data/master_bandpass.txt'
	sfnt<-read.table(sfn,header=FALSE,sep="\t")

	master<-list()
	master$filename<-fn
	lb<-readGDAL(paste("HDF4_SDS:UNKNOWN",fn,"0",sep=":"))
	cb<-readGDAL(paste("HDF4_SDS:UNKNOWN",fn,"1",sep=":"))
	rb<-readGDAL(paste("HDF4_SDS:UNKNOWN",fn,"2",sep=":"))
	# bands in nanometers (How to standardize?)
	spc<-data.frame(l=lb@data$band1*1000,lambda=cb@data$band1*1000,r=rb@data$band1*1000)
	spc$fwhm<-(spc$r-spc$l)

#	sfnt<-read.table(paste(spectral_fn,header=FALSE,sep="\t",col.names=c("lambda",))

	# Obviously from here, you see I don't really understand the best way to create a data.frame template, that includes lists.
	# This step allows the data to include a list, of functions, but is this really the best way to do this?  I guess I should have another
	# "Band" class.
	
	# gau=gaussian and apx=spectral approx function
	i<-list(apx=function(x) {1})
	spc$info<-rep(i,50)

	for (i in 1:50) {
		l<-spc$l[i]
		r<-spc$r[i]
		c<-spc$lambda[i]
		if (i > 25) 
		{	sens<-data.frame(lambda=c(l,c,r),R=c(0.5,1,0.5))
		} else {
			sens<-data.frame(lambda=sfnt[,1]*1000,R=sfnt[,i+1])
		}
		gau<-make.gaussian(c,r-l)	
		apx<-approxfun(sens$lambda,sens$R)
		fs<-splinefun(sens$lambda,sens$R)
#		spc$info[i]$gau<-gau
		spc$info[i]$apx<-apx
#		spc$info[i]$fs<-fs
#		info[[i]]<-list(g=g,fs=fs,sens=sens)
	}
#	master$info<-info
	master$bands<-spc
	master
}


master<-create.master("/Users/quinn/master/MASTERL1B_1165100_04_20110608_2305_2310_V00.hdf",
		'/Users/quinn/master/av_master/master_bandpass_2011_08_20.txt')
# Now get the factors that you need for AVIRIS calculation
master$bands$av_max_band<-0
afacts<-list()
for (i in 1:50) {
	g<-master$bands$info[i]$apx
	af<-g(aviris$bands$lambda)
	# Remove factors for 160 (questionable lambda) 
	af[160]<-0
	# Remove 1200 factors from MODIS band 16
	if (i==16) {af[161]<-0; af[162]<-0}
	gf<-af*aviris$bands$gain
	
	bands<-which(af>0.03)
	norm<-sum(af[bands])
	master$bands$av_max_band[i]<-which(af==max(af[bands]))[1]
	
	# Normalize the sum back to radiance
	gnorm<-sum(gf[bands])
		
	# This should be the starting point for function minimiation on the integral of these functions w/ the master function
	# But for now, let's say they are correct.
	# use factors > %3 to eliminate end problems, and simplify master edges
	afacts[[i]]<-list(band=bands,r=af[bands],g=gf[bands],gnorm=gnorm,norm=norm)
}

# Get a list of the MASTER bands we can fake from AVIRIS
baz<-rep.int(0,length(master$bands$lambda))
for (i in 1:length(master$bands$lambda)) { baz[i]<-length(afacts[[i]]$band) }
which(baz>0)

# This creates the 25 gdal_calc.py functions that can be used to create this outside of R, since raster for AVIRIS makes my computer die
# (from memory)
A<-c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R")

gdal_calc<-rep("1",25)
for (m in 1:25) { 
	caps<-A[1:length(afacts[[m]]$band)]	
	gdal_calc[m]<-sprintf("m=%s;b=%s;${gc} --calc '(%s)/%s' > master${b}",m,afacts[[m]]$band[1],
		paste(paste(caps,afacts[[m]]$g,sep='*'),collapse='+'),
		afacts[[m]]$norm)
}

# This creates a new list for aviris estimations of MASTER bands
i<-list(g=function(x) 1)
master$bands$av_info<-rep(i,50)
# This creates the 25 MODIS functions that are used 
for (m in 1:25) { 
	eval(parse(text=
	paste("master$bands[",m,",]$av_info$g<-function(x){",paste(afacts[[m]]$r,"*aviris$bands[",afacts[[m]]$band,",]$info$g(x)",sep="",collapse="+"),"}",sep="")
	))}

# See Marginal axis at 
#http://www.cl.cam.ac.uk/~sjm217/projects/graphics/#download
par(ask = TRUE)
for (i in 1:25) {
	b<-master$bands[i,]
	plot(function(x) b$info$apx(x),b$lambda-b$fwhm,b$lambda+b$fwhm,ylab="Filter Response",xlab="Wavelength [nm]")
	curve(b$av_info$g(x),add=TRUE,col=2)
	
#	points(sfnt[,1],sfnt[,1+i],col=3)
	# Create a title with a red, bold/italic font
	title(main=sprintf("Master Band %d",i), font.main=4)
#	title(xlab="Wavelength [nm]")
#	title(ylab="Filter Response")
	
	mx<-integrate(function(x) b$info$apx(x),b$lambda-b$fwhm,b$lambda+b$fwhm)$value/
			integrate(function(x) b$av_info$g(x),b$lambda-b$fwhm,b$lambda+b$fwhm)$value
	print(mx)
}

plot(function(x) master$bands[1,]$info$g(x),400,2500)
for (i in 1:25) {
	curve(master$bands[i,]$info$g(x),add=TRUE,col=i)
}

# This is how you plot one set of multipliers
plot_one_multipliers <- function(i) {
 plot(aviris$bands[afacts[[2]]$band,]$lambda,master$bands[2,]$info$g(aviris$bands[afacts[[2]]$band,]$lambda),main='AVIRIS Multipliers Band 2',xlab='Wavelength [nm]',ylab='Response')
 curve(master$bands[2,]$info$g(x),add=TRUE)
 lines(c(master$bands[2,]$lambda-master$bands[2,]$fwhm/2,master$bands[2,]$lambda+master$bands[2,]$fwhm/2),c(.5,.5),type="o", pch=22, lty=2, col="red")
 lines(c(master$bands[2,]$lambda),c(1),type="o", pch=22, lty=2, col="red")
}
