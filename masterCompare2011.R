# TODO: Add comment
# 
# Author: quinn
###############################################################################
# Return our Test points in the AVIRIS bands as a data frame 
read_av_test <-function(fn='/Users/quinn/master/av_master/aviris_cc_1p.csv') {
	comp<-read.csv(fn)
}

source("/Users/quinn/Documents/workspace/MasterCalibration/Aviris.R")
source("/Users/quinn/Documents/workspace/MasterCalibration/Master.R")

calculate_master <- function (
	afn="/Users/quinn/master/popo.jpl.nasa.gov/pub/SUstin/f110608t01p00r08rdn_a/f110608t01p00r08rdn_a",
	mfn="/Users/quinn/master/MASTERL1B_1165100_04_20110608_2305_2310_V00.hdf") {
	a<-aviris(afn)
	m<-master(mfn)
	
	par(ask = TRUE)
	for (i in 1:25) {
		print(sprintf("Band %d\n",i))
		m$bands[[i]]$av<-master.get_aviris_factors(m,a,i)
#	master.plot_aviris_multipliers(m,i)
	}
	save(m,a,file='Master.RData')
}

load('Master.RData')

comp<-read.csv('/Users/quinn/master/av_master/master_avm_cc_1p.csv')

aviris_pseudo_master <- function(i) {
	band<-m$bands[[i]]
	av<-band$av
	
	master<-comp[,i+2]
	max<-data.matrix(ex[,av$max_band]*gain(a)[av$max_band])
	av_idx<-av$weight$band
	av_bands<-data.matrix(ex[,av_idx]*gain(a)[av_idx]*fwhm(a)[av_idx])
	r<-av_bands %*% data.matrix(av$weight$r/av$area$r)
	opt<-av_bands %*% data.matrix(av$weight$opt/av$area$best)
#	aviris<-data.frame(bands=av_idx,value=av_bands)	
	pseudo<-data.frame(master=master,max=max,r=r,opt=opt)
#	list(aviris=aviris,pseudo=pseudo)
	pseudo
}

aviris_pseudo_fits <- function(i) {
	pseudo<-aviris_pseudo_master(i)
	min<-pseudo<mean(pseudo)
	max<-pseudo>mean(pseudo)
	
	r<-lm(pseudo$r ~ pseudo$master)
#	r0<-lm(pseudo$r ~ -1 + pseudo$master)
	
	opt_ex<-av_bands %*% data.matrix(av$weight$opt/av$area$best)
	opt<-lm(pseudo$opt ~ pseudo$master)
#	opt0<-lm(pseudo$opt ~ -1 + pseudo$master)
	opt.min<-lm(pseudo$opt[min] ~ pseudo$master[min])
	opt.max<-lm(pseudo$opt[max] ~ pseudo$master[max])
	
	max<-lm(pseudo$max ~ pseudo$master)
#	max0<-lm(pseudo$max ~ -1 + pseudo$master)
	
	list(max=max,r=r,opt=opt,opt.min=opt.min,opt.max=opt.max)
}

master_plot_test <- function () {
	lm<-data.frame(band=1:25,
			r.m=1,r.b=1,r.r2=0,
			max.m=1,max.b=1,max.r2=0,
			opt.m=1,opt.b=1,opt.r2=0,
			r0.m=1,r0.r2=0,
			max0.m=1,max0.r2=0,
			opt0.m=1,opt0.r2=0
			)
			
	for (i in 1:25) { 
		lin<-aviris_pseudo_fits(i)

		lm[i,]$r.m<-lin$r$coefficients[[2]];
		lm[i,]$r.b<-lin$r$coefficients[[1]];
		lm[i,]$r.r2<-summary(lin$r)$adj.r.squared
		lm[i,]$opt.m<-lin$opt$coefficients[[2]];
		lm[i,]$opt.b<-lin$opt$coefficients[[1]];
		lm[i,]$opt.r2<-summary(lin$opt)$adj.r.squared
		lm[i,]$max.m<-lin$max$coefficients[[2]];
		lm[i,]$max.b<-lin$max$coefficients[[1]];
		lm[i,]$max.r2<-summary(lin$max)$adj.r.squared

		lm[i,]$r0.m<-lin$r0$coefficients[[1]];
		lm[i,]$r0.r2<-summary(lin$r0)$adj.r.squared
		lm[i,]$opt0.m<-lin$opt0$coefficients[[1]];
		lm[i,]$opt0.r2<-summary(lin$opt0)$adj.r.squared
		lm[i,]$max0.m<-lin$max0$coefficients[[1]];
		lm[i,]$max0.r2<-summary(lin$max0)$adj.r.squared		
	} 
	return(lm)
}

print_all <- function() {
	# Background colour
#	par("bg"="#fffff0")
	#par("bg"="transparent")
	par(las=1)
	# Output device. If outputing to a file, remember to use dev.off()
	#png(file="/tmp/faithful.png",width=480,height=380,bg=par("bg"))
	
	for (i in c(1:15,23:25)) {
		print(i)
#		pdf(file=sprintf("/Users/quinn/master/pdf/pseudo_%d.pdf",i), width=8, height=8, bg=par("bg"))
		pdf(file=sprintf("/Users/quinn/master/pdf/pseudo_%d.pdf",i), width=8, height=8)
		plot_pseudo(i)
		dev.off()
	}	
}

# See Marginal axis at 
#http://www.cl.cam.ac.uk/~sjm217/projects/graphics/#download
# Example shows individual plotting of colours, maybe vs, row and column? rectangle
source('fancyaxis.R')


plot_pseudo <- function(i,divisions=1000) {
	source('fancyaxis.R')
	pseudo<-aviris_pseudo_master(i)
	lm<-aviris_pseudo_fits(i)
	xdata<-pseudo$master
	xsum<-summary(xdata)
	xmin<-xsum[4]-3*(xsum[4]-xsum[2])
	xmax<-xsum[4]+3*(xsum[5]-xsum[4])
	ydata<-pseudo$opt
	ysum<-summary(ydata)
	ymin<-ysum[4]-3*(ysum[4]-ysum[2])
	ymax<-ysum[4]+3*(ysum[5]-ysum[4])
	
	min<-min(xmin,ymin)
	max<-max(xmax,ymax)
	good<-xdata>min & ydata>min & xdata<max & ydata<max
	xdata<-xdata[good]
	ydata<-ydata[good]
			
	plot(xdata,ydata,
			# Omit axes
			axes=FALSE,
			pch=20,
#			main=sprintf("Master Band %d",i),
			xlab=sprintf("MASTER Band %d",i),
			ylab="Pseudo AVIRIS",
			# Leave some space for the rug plot
			xlim=c(min,max),
			ylim=c(min,max),
			col='black',
			cex=0.25)
	
	axp=par("xaxp")
	axp[3] <- axp[3]*2
	par("xaxp"=axp)
	abline(a=0,b=1,col='black',lty="dashed",lwd=2)
	abline(lm$max,col="red")
	abline(lm$r,col='blue')
	abline(lm$opt,col='green')
	abline(lm$opt.min,col='darkgreen')
	abline(lm$opt.max,col='darkgreen')
	legend("topleft",
		c(sprintf("max: %0.2fx%+0.2f",lm$max$coefficients[[2]],lm$max$coefficients[[1]]),
		  sprintf("  r: %0.2fx%+0.2f",lm$r$coefficients[[2]],lm$r$coefficients[[1]]),
		  sprintf("opt: %0.2fx%+0.2f",lm$opt$coefficients[[2]],lm$opt$coefficients[[1]]),  
  		  sprintf("opt.min: %0.2fx%+0.2f",lm$opt.min$coefficients[[2]],lm$opt.min$coefficients[[1]]),
  		  sprintf("opt.max: %0.2fx%+0.2f",lm$opt.max$coefficients[[2]],lm$opt.max$coefficients[[1]])),
			lwd=1,col=c("red","blue","green","darkgreen",'darkgreen'))
	
	# Add the axes, passing in the summary to provide quartile and mean
	fancyaxis(1,summary(xdata), digits=0)
	fancyaxis(2,summary(ydata), digits=0)
	
	# Add the stripcharts
	axisstripchart(xdata, 1,divisions=divisions,xlim=c(min,max))
	axisstripchart(ydata, 2,divisions=divisions,xlim=c(min,max))
}
