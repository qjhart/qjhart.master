# Sensor Specification
# 
# Author: quinn
###############################################################################

sensorBand.Response.validity <- function(object) {
	if (is.null(object@lambda) || is.null(object@R))
		return("Missing @lambda or @R")
	if(length(object@lambda) != length(object@R))
		return("length of @lambda != length of @R")
	TRUE
}

setClass("sensorBand.Response",
	representation(lambda="numeric",R="numeric"),
	validity=sensorBand.Response.validity
)

setMethod("show", "sensorBand.Response", function(object) {
	cat("sensorBand.Response\n")
	print(data.frame(lambda=object@lambda,R=object@R), right = TRUE, quote = FALSE)
})

setMethod("plot", "sensorBand.Response", signature="sensorBand.Response",
		function(object) {
			plot(x=c(0,1),x=c(1,1))
#	plot(data.frame(lambda=object@lambda,R=object@R))
})

sensorBand.validity <- function(object) {
#	if (object@approx=='spline' || object@approx=='linear' || object@approx=='rect')
#		return("Unknown value for @approx")
	TRUE
}
	
sensorBand.initialize <- function(object,lambda,fwhm,R,approx='spline')
{
#	if(length(lambda)==1) { # Then Need fwhm value
#		center<-lambda
#		if (apx=='spline' || apx=='linear' ) {
#			min<-lambda-fwhm
#			max<-lambda+fwhm
#			sens <- sensor.band.response(lambda=c(lambda-fwhm,lambda-fwhm/2,lambda,
#							lambda+fwhm/2,lambda+fwhm),
#					R=c(0,0.5,1,0.5,0))
#		} else if (apx=='rect') {
#			min<-lambda-fwhm/2
#			max<-lamdda+fwhm/2
#			sens <- sensor.band.response(lambda=c(lambda-fwhm/2,lambda-fwhm/2,lambda,
#							lambda+fwhm/2,lambda+fwhm/2),
#					R=c(0,1.0,1,1.0,0))
#		}
#		g <- switch(control$approx,
#				linear = approxfun(x=sens$lambda,y=sens$R,ties=ordered),
#				spline = splinefun(x=sens$lambda,y=sens$R),
#				rect = approxfun(x=sens$lambda,y=sens$R,ties=ordered))
#	} else {
#		sens<-sensor.band.response(lambda=lambda,R=R)
#		g <-switch(control$approx,
#				linear = approxfun(x=sens$lambda,y=sens$R),
#				spline = splinefun(x=sens$lambda,y=sens$R),
#				rect = approxfun(x=sens$lambda,y=sens$R,ties=ordered))
#		min<-lambda[1]
#		max<-lambda[length(lambda)]
##		center<-optimize((min+max)/2,f,control=list(fnscale=-1))
#		center<-(min+max)/2
#		fwhm.min<-uniroot(function(x) { g(x) - 0.5},interval=c(min,center))
#		fwhm.max<-uniroot(function(x) { g(x) - 0.5},interval=c(center,max))
##		fwhm<-fwhm.max-fwhm.min
#		fwhm<-max-min
#	}
#	f<-function(x) (min<x)*(x<max)*g(x)
#	area<-integrate(g,lower=min,upper=max)
#	b<-structure(list(lambda=center,fwhm=fwhm,R=sens,
#					f=f,g=g,min=min,max=max,area=area),class="sensor.band")
#	return(b)
}

setClass("sensorBand",
		representation(
				R = "sensorBand.Response",
				fwhm = "numeric",
				lambda= "numeric",
				approx="character",
				f = "function"),
		prototype(approx="spline"),
#		validity=sensorBand.validity
#		initialize=sensorBand.initialize
)


