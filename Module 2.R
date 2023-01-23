### MSE 2100 Calculator ###

# Clear console
cat("\014")  # ctrl+L
# Clear global environment
rm(list = ls())
'-------------------------------------------------------------------------'
## Ionic character between two atoms:
# INPUT
i1 = 1.7
i2 = 3.5
# END INPUT
IC = 100*(1-exp((-0.25)*((i1-i2)^2)))

writeLines(c(paste("Percent Ionic Character:",IC)))
'-------------------------------------------------------------------------'
## Force of attraction between two ions:
# INPUT
z1 = 2
z2 = -1
r = 0.82+0.134
# END INPUT
r = r*10e-10 # convert to meters from nm
Given = 2.31e-28

Fa = Given*abs(z1)*abs(z2)/r^2
writeLines(c(paste("Force of Attraction:",Fa),
           paste("Force of Attraction:",-Fa))) # answer is in meters
'-------------------------------------------------------------------------'
## Find atomic radius (in nm):
# INPUT
z1 = 2
z2 = -2
r1 = 0.079
Fa = 1.83e-8
# END INPUT
r1 = r1*10e-10 # convert nm to m (move decimal 9 places back)
Given = 2.31e-28

r2 = sqrt((Given*abs(z1)*abs(z2))/Fa)-r1
r2 = r2*10e8 # m to nm
writeLines(c(paste("Atomic Radius:",r2))) # answer is in nano-meters
'-------------------------------------------------------------------------'