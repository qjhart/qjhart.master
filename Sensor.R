# Sensor Specification
# 
# Author: quinn
###############################################################################


setClass("Sensor",
		representation=representation(
				bandpasses="Sensor.BandPass",
		)
	contains="character"
)


setClass("Sensor.Bandpass")
setMethod("response","Sensor.BandPass")
setMethod("integrate","Sensor.BandPass")

#
# This is a representation of a bandpass, where you only have the center and FWHM of the sensor
# 
setClass("Sensor.BandPass.centerFwhm",
		representation=representation(fwhm="numeric"),
		contains="numeric")

setClass("Sensor.bandPalss")

# From this you can calculate fwhm, palmer method, etc.
setClass("Sensor.BandPass.Scan",
		contains="data.frame")

