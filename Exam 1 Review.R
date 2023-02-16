### MSE 2100 Exam 1 Calculator ###

rm(list = ls(all = TRUE)) #start with empty workspace
'-------------------------------------------------------------------------'
## Ionic character between two atoms:
# INPUT
i1 = 0.8
i2 = 3.0
# END INPUT
IC = 100*(1-exp((-0.25)*((i1-i2)^2)))

writeLines(c(paste("Percent Ionic Character:",IC)))
'-------------------------------------------------------------------------'
## Force of attraction between two ions:
# INPUT
z1 = 1
z2 = -1
r = 0.227+0.175
# END INPUT
r = r*10e-10 # convert to meters from nm
Given = 2.31e-28

Fa = (Given*abs(z1)*abs(z2))/r^2
writeLines(c(paste("Force of Attraction:",Fa),
           paste("Force of Attraction:",-Fa))) # answer is in newtons
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
## Planar Density (Face-Centered Cubic)
# Copper = 0.128nm, what is the PD of the (100) plane for copper in m^-2?

# INPUT
R = 0.128 # nm
# END INPUT

R = R*1e-9
PD = 1/(4*(R^2))

writeLines(c(paste("PD of the ", "plane:",PD, "m^-2")))

# What is the PD of the same element on the (111) plane in m^-2?

# INPUT
R = 0.128 # nm
# END INPUT

R = R*1e-9
PD = 1/(2*(R^2)*(sqrt(3)))

writeLines(c(paste("PD of the ", "plane:",formatC(PD, format = "e"), "m^-2")))
'-------------------------------------------------------------------------'
## Atomic Packing Factor for tetragonal symmetry

# INPUT

p = 10.2 # g/cm^3
Aw = 132.3 # atomic weight in g/mol
R = 0.116 # nm
a_lattice = 0.511
c_lattice = 0.330

# END INPUT

Na = 6.022*1e+23 # Avogadro's number

Vn = floor((p*(a_lattice^2)*(c_lattice)*Na/Aw)*1e-21)
APF = Vn*(4/3)*((pi*(R^3))/((a_lattice^2)*c_lattice))

writeLines(c(paste("APF: ",APF)))
'-------------------------------------------------------------------------'
## Diffraction Planes
# INPUT

theta = 44.53 # given angle in degrees
planeType = 'FCC' # FCC, BCC, or SC (face, body, or simple cubic?)
n = 1 # Monochromatic radiation?
wavL = 0.1542 # wavelength in nano-meters
Ratom = 0.1246 # atomic radius in nano-meters

# END INPUT

{
  wavL = wavL*1e-9 # convert nm to meters
  Ratom = Ratom*1e-9
  theta = theta/2
  
  if (planeType == 'FCC') {
    a = 2 * sqrt(2) * Ratom
  } else if (planeType == 'BCC') {
    a = (4 * Ratom) / sqrt(3)
  } else if (planeType == 'SC') {
    a = 2 * Ratom
  } else {
    print("Invalid plane type. Please state either FCC, BCC, or SC")
  }
  
  d = (n*wavL) / (2*sin(pi*theta/180)) # conv angle to radians while solving d
  solution = (a/d)^2 
  h = round(sqrt(solution/(1+(a/d)^2)), 0)
  k = round(sqrt(solution/(1+(d/a)^2)), 0)
  l = round(sqrt(solution/(h^2+k^2)), 0)
  
  writeLines(c(paste("Diffraction Value:",solution)))
  writeLines(c(paste("Miller indices: (",h,k,l,")")))
} # Solution
'-------------------------------------------------------------------------'
## Theoretical Molecular Density

# INPUT

d_ZnS = 0.234 # nm
theta <- 109.5 # degrees
atomic_weight_1 <- 65.41 # g/mol
atomic_weight_S <- 32.06 # g/mol

# END INPUT
{
# Conditionals
if (d_ZnS <= 0.155) {
  coordN = 2
} else if (d_ZnS > 0.155 && d_ZnS <= 0.225) {
  coordN = 3
} else if (d_ZnS > 0.225 && d_ZnS <= 0.414) {
  coordN = 4
} else if (d_ZnS > 0.414 && d_ZnS <= 0.732) {
  coordN = 6
} else if (d_ZnS > 0.732 && d_ZnS <= 1.0) {
  coordN = 8
}

# Conversion of units
theta = theta * pi / 180 # degrees to radians
theta = theta / 2 # divide by 2
d_ZnS = d_ZnS*1e-9 # nm to meters


# Calculation of parameters
N_A <- 6.022e23 # Avogadro's number
a = (coordN*d_ZnS*sin(theta))/sqrt(2)
rho_ZnS = 4*(atomic_weight_1 + atomic_weight_S) / ((a^3)*N_A)
rho_ZnS = rho_ZnS*1e-6  # theoretical density of ZnS in g/cm^3

# Output
cat("Theoretical density of ZnS:", round(rho_ZnS, 2), "grams/cm^3")
} # Solution

