# Representation of a Radiometric Atmosphere
# 
# Author: quinn
###############################################################################
setClass(Class="Atm",
		representation=representation(
				p0 = "data.frame",
				p5 = "data.frame",
				p1 = "data.frame"
				)
)

setClass(Class="Atm.Spectral")
		
		
setClass(Class="Atm.Path",
		representation=representation=(
			airmass = "numeric"
			),
	contains = "matrix"
)

setMethod("Read","PathRadiometry",function(.Object ...)
{
atp7<-read.fwf(file="11651_Ivanpah_sonde2_r0.tp7",
		widths=c(8,11,11,11,11,11,11,11,11,11,11,11,11,11,11),header=FALSE,skip=11,
		col.names=c("FREQ","TOT_TRANS","PTH_THRML","THRML_SCT","SURF_EMIS","SOL_SCAT","SING_SCAT",
				"GRND_RFLT","DRCT_RFLT","TOTAL_RAD","REF_SOL","SOL@OBS","DEPTH","DIR_EM","TOA_SUN"));
})


# This contains wavelength or number, tau, extinction, and scattering coeff
# this is how to do the h2o for AVRIS
# Separate wavelength from valus, so we can pass around more easily?
setClass(Class="AtmosphericConstituent",
		contains= "data.frame",
)
# How to set units?, need to check for auto conversion
# Should be a subclass of Radiometry somehow?

setValidity("AtmosphericConstituent",function (d) {
			if (! "tau" %in% colnames(d))
				return("input missing tau")
			TRUE
		})

setMethod("initialize", "AtmosphericConstituent", function(.Object, ...)
		{
			value <- callNextMethod()
			validObject(value)
			value[c("lambda","tau")]
		})

setMethod("show", "AtmosphericConstituent", function(object) {
			value <- getDataPart(object)
			rownames(value) <- as.character(slot(object, "time"))
			flag <- as.matrix(slot(object, "flag"))
			colnames(flag) <- paste(colnames(flag), "*", sep ="")
			cat("AtmosphericConstituent\n")
			print(cbind(value, flag), right = TRUE, quote = FALSE)
		})

atp7<-read.fwf(file="11651_Ivanpah_sonde2_r0.tp7",
			widths=c(8,11,11,11,11,11,11,11,11,11,11,11,11,11,11),header=FALSE,skip=11,
			col.names=c("FREQ","TOT_TRANS","PTH_THRML","THRML_SCT","SURF_EMIS","SOL_SCAT","SING_SCAT",
					"GRND_RFLT","DRCT_RFLT","TOTAL_RAD","REF_SOL","SOL@OBS","DEPTH","DIR_EM","TOA_SUN"));

plot(x=atp7$FREQ[0:2000],y=atp7$TOTAL_RAD[0:2000],ylab="Trans",type="l")
