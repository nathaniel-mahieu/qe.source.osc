library(xcms)

xr = xcmsRaw("infusion 1.mzXML")
writeLines(paste(xr@tic,sep=",", collapse=","), con="infusion1.tic.csv")

xr = xcmsRaw("infusion 2.mzXML")
writeLines(paste(xr@tic,sep=",", collapse=","), con="infusion2.tic.csv")

xr = xcmsRaw("infusion 3.mzXML")
writeLines(paste(xr@tic,sep=",", collapse=","), con="infusion3.tic.csv")




segments = data.frame(
  label = as.character(c("50 uL", "40 uL", "30 uL", 
                         "10 uL", "30 uL 2", 
                         "45 sheath", "35 sheath", 
                         "25 sheath", "20 sheath", 
                         "3.5V, 20 sheath", "3.3V, 20 sheath, 10 aux", 
                         "3.3V, 20 sheath, 20 aux")),
  start = c(0, 835, 1223, 
            1674, 2465, 
            3196, 3442, 
            3745, 4392, 
            4638, 5132, 
            5554)
  )
write.csv(segments, "segments.csv")
