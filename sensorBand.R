# TODO: Add comment
# 
# Author: quinn
###############################################################################

fwhm <- function(x) UseMethod("fwhm")
lambda <- function(x) UseMethod("lambda")
gain <- function(x) UseMethod("gain")
weights <- function(x,f,interval,minweight,use) UseMethod("weights")

# This class holds the resonse function
# Still need units.
sensor.band.response <- function(x,lambda,R) {
	sens<-data.frame(lambda=lambda,R=R)
	class(sens) <- "sensor.band.response"
	return(sens)
}

plot.sensor.band.response <-function(x,...) {
	args=list(...)
	plot(x=x$lambda,y=x$R,...)
}

# This is the object of a single sensor band,  You initialize the band with either a 
# response function, or with a center and fwhm, and an approximation
# Band approx: spline, linear, square 
# Examples of Making bands
#baz=sensor.band(lambda=480,fwhm=50,control=list(approx='linear'))
#foo=sensor.band(lambda=450,fwhm=50)
#bar<-sensor.band(lambda=c(400,405,410,420,425,430),R=c(0,.5,1,1,.3,0),control=list(approx='linear'))
#fuz<-sensor.band(lambda=c(400,405,410,420,425,430),R=c(0,.5,1,1,.3,0),control=list(approx='spline'))

#make.gaussian <- function(lambda, fwhm)
#{
#	m<-lambda
#	s<-fwhm/(2*sqrt(2*log(2)))
#	function(x) exp(-(x-m)^2/(2*s^2))
#}

sensor.band <- function(lambda,fwhm,gain=1,R,control=list(approx='spline'))
{
	# By default we do spline control
	if (is.null(control$approx)) 
		apx<-'spline'
	else if (control$approx=='spline' || control$approx=='linear' || contol$approx=='rect')
		apx<-control$approx
	else
		stop("Unknown control$approx")

	if(length(lambda)==1) { # Then Need fwhm value
		center<-lambda
		if (apx=='spline' || apx=='linear' ) {
			fwhm.min<-lambda-fwhm/2
			fwhm.max<-lambda+fwhm/2
			min<-lambda-fwhm
			max<-lambda+fwhm
			sens <- sensor.band.response(
				lambda=c(min,fwhm.min,lambda,fwhm.max,max),
				R=c(0,0.5,1,0.5,0))
		} else if (apx=='rect') {
			min<-lambda-fwhm/2
			max<-lamdda+fwhm/2
			fwhm.min<-lambda-fwhm/2
			fwhm.max<-lamdda+fwhm/2
			sens <- sensor.band.response(
				lambda=c(min,fwhm.min,lambda,fwhm.max,max),
				R=c(0,1.0,1,1.0,0))
		}
		g <- switch(control$approx,
			linear = approxfun(x=sens$lambda,y=sens$R,ties=ordered),
			spline = splinefun(x=sens$lambda,y=sens$R),
			rect = approxfun(x=sens$lambda,y=sens$R,ties=ordered))
		} else {
		sens<-sensor.band.response(lambda=lambda,R=R)
		g <-switch(control$approx,
				linear = approxfun(x=sens$lambda,y=sens$R),
				spline = splinefun(x=sens$lambda,y=sens$R),
				rect = approxfun(x=sens$lambda,y=sens$R,ties=ordered))
		min<-lambda[1]
		max<-lambda[length(lambda)]
		center<-optimize(g,interval=c(min,max),maximum=TRUE)$maximum
#		print(sprintf("%f->%f %f->%f %f->%f\n",min,g(min),center,g(center),max,g(max)))
		fwhm.min<-uniroot(function(x) { g(x) - 0.5},interval=c(min,center))$root
		fwhm.max<-uniroot(function(x) { g(x) - 0.5},interval=c(center,max))$root
		fwhm<-fwhm.max-fwhm.min
	}
	f<-function(x) (min<x)*(x<max)*g(x)
	area<-integrate(g,lower=min,upper=max)$value
	b<-structure(list(lambda=center,fwhm=fwhm,fwhm.min=fwhm.min,fwhm.max=fwhm.max,
					R=sens,gain=gain,
		f=f,g=g,min=min,max=max,area=area),class="sensor.band")
	return(b)
	}
	
plot.sensor.band <-function(band,...) {
	args=list(...)
	f<-band$f
	max<-band$f(band$lambda)
	plot(band$R,ylim=c(0,max),pch=16,col="green",type="p",
			xlab='Wavelength',ylab='Sensor Response',main=args$main)
	curve(band$f(x),add=TRUE,col=2)
	points(x=c(band$min,band$fwhm.min,band$lambda,band$fwhm.max,band$max),
			y=c(f(band$min),0.5,f(band$lambda),0.5,f(band$max)),col="blue")
}
		
fwhm.sensor.band <- function(x) x$fwhm
lambda.sensor.band <-function(x) x$lambda
gain.sensor.band <-function(x) x$gain

# Set of Bands - This is a list of all the bands that are included in the sensor.  
# 
# Example of making a set of bands
#df<-data.frame(lambda=c(420,440,460),fwhm=c(10,11,12))
#t.df <- data.frame(t(df))
#bands=lapply(t.df,function(x) sensor.band(lambda=x[1],fwhm=x[2]))
# Exampl of appending a band
#bands[[length(bands)+1]]<-sensor.band(lambda=c(240,245,250),R=c(0,1,0))

sensor.bands<-function(bands) {	
	class(bands)<-'sensor.bands'
	return(bands)
}

# This function returns the weights of the sensor bands for a given function
# use is a list of TRUE/FALSE values for inclusion of 
# minweight is a single value specifing the minimum weight to include
# interval is the interval on the passed function
weights.sensor.bands <- function(bands,f,interval=c(-Inf,Inf),minweight=0,use=TRUE) {
	lambda<-lambda(bands)
	factors<-0*lambda
	factors[use & lambda>=interval[1] & lambda<=interval[2]] <-
			f(lambda[use & lambda>=interval[1] & lambda<=interval[2]])
	factors[factors<minweight]<-0
	factors
}

indices.sensor.bands <-function(bands,use=TRUE) { which(use) }
# This function itself returns a function that allows linear combinations of
# a certain number of the sensor bands.  The bands to be used are passed in the USE
# function.  The resulting function, when passed a set of constants, will return a function
# that matches a linear combination of the bands included and their weights.
# 'use' shows which bands to include use=[TRUE,FALSE...] of the number of bands
lincomb.sensor.bands <- function(bands,use=TRUE) {
	touse<-indices.sensor.bands(bands,use)
	eval(parse(text=paste("function(y){ function(x) {",
						paste("y[",c(1:length(touse)),"]*bands[[",touse,"]]$f(x)",sep="",collapse="+")
						,"}}",sep="")))
}

# This function will optimize the weights of a set of sensor bands to match the input function
# over the interval choosen.  The band weights of the used bands are returned.  The indices to the 
# bands to be used are passed into this function, and the result vector is only that long.  You might
# want to include all the bands too, I'm not sure which is better.
optimizedweights.sensor.bands <- function(bands,f,interval=c(-Inf,Inf),guess) {
	use<-rep(FALSE,length(lambda(bands)))
	use[guess$band]<-TRUE
	lc<-lincomb.sensor.bands(bands,use)
	mindifferencefun<-function(y) {
	integrate(function(x) {(f(x)-lc(y)(x))^2},
			lower=interval[1],upper=interval[2],subdivisions=500)$value} 
	optimum<-optim(guess$weight,mindifferencefun)
	data.frame(band=guess$band,weight=optimum$par)
}

#This function creates a Linear combination of bands and weights.  The input is a data.frame of
# bands and weights, and the output is a function that is a combination of those weights
combine.sensor.bands <- function(bands,weights) {
	use<-rep(FALSE,length(lambda(bands)))
	use[weights$band]<-TRUE
	lc<-lincomb.sensor.bands(bands,use)
	lc(weights$weight)
}

fwhm.sensor.bands <- function(bands) {
	sapply(bands,function(x) fwhm(x))
}

lambda.sensor.bands <- function(bands) {
	sapply(bands,function(x) lambda(x))
}

gain.sensor.bands <- function(bands) {
	sapply(bands,function(x) gain(x))
}

# Sensor Object - This is the overall sensor project, right now, it's pretty much only the
# spectral bandpass list.
sensor <- function(x,bands,filename) {
	sensor <- list(filename=filename,bands=bands)
	class(sensor) <- 'sensor'
	sensor
}

fwhm.sensor <-function(sensor) {
	fwhm(sensor$bands)
}

lambda.sensor <-function(sensor) {
	lambda(sensor$bands)
}

gain.sensor<-function(sensor) {
	gain(sensor$bands)
}