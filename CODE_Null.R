
# INPUT: Contemporary U.S. counties
# Vars: FIPS, Name, 2010 Census pop and centroids (lat and long) 
# Source: http://www.census.gov/geo/maps-data/data/gazetteer.html

# OUTPUT: Null datasets of county-level rates 
# Assumptions: Constant relative risk of 1 (pop-adjusted)
# 1st scenario: Homogenous populations 
# 2nd scenario: Heterogenous populations from 2010 Census 
# Maintain same average national rate in both scenarios

##################### (User Defined Inputs) ######################

# Set working directory
setwd("C:/Users/Jess Chen/Dropbox/Jess-LISA/Simulations")

# Read in county file
countypop = read.csv("countypop_census.csv", header = T)

# Use continental US counties
# Remove AK (FIPS = 02), HI (FIPS = 15), and PR (FIPS = 72)
countypop = countypop[-which(countypop$FIPS>2000 & countypop$FIPS<3000),]
countypop = countypop[-which(countypop$FIPS>15000 & countypop$FIPS<16000),]
countypop = countypop[-which(countypop$FIPS>72000 & countypop$FIPS<73000),]

# Test plot - check if only continental states remain
# plot(countypop$longitude, countypop$latitude)

# Define variables
N = 5000 # Total number of cases in nation
T = 999 # Number of null datasets to create
C = dim(countypop)[1] # Number of continental counties
P = 5000 # Set homogenous population for 1st scenario

##### Generate Null Data (1st Scenario: Constant RR, Homog Pop) #####

# Divide interval [0,1] into C equal segments (constant RR of 1)
# Generate N uniform [0,1] random variates (N total cases)
# Count number of cases in each interval, divide by P (homog pop) for rates
# Do this T times to construct null datasets

breaks_homog = seq(0, 1, 1/C)

nulldata_homog = matrix( , C, T)
for (i in 1:T)
{
nulldata_homog[ , i] = table(cut(runif(N, 0, 1),
	breaks = breaks_homog))/
	P
}

export_homog = cbind(countypop, nulldata_homog)
write.table(export_homog,file = "DATA_Null homog.csv", 
	sep = ",", row.names = FALSE)

# Test plots - should be approximately normal (censored at zero)
# hist(nulldata_homog)

##### Generate Null Data (2nd Scenario: Constant RR, Heterog Pop) #####

# Divide interval [0,1] into N segments proportional to county pop 
# Generate [0,1] uniform random variates, keep avg national rate of N/(C*P)
# Count number of cases in each interval, divide by county pop for rates
# Do this T times to construct null datasets

breaks_heterog = c(0,cumsum(countypop$POP10)/sum(countypop$POP10))
N_heterog = (N/(C*P))*sum(countypop$POP10)

nulldata_heterog = matrix( , C, T)
for (i in 1:T)
{
nulldata_heterog[ , i] = table(cut(runif(N_heterog, 0, 1),
	breaks = breaks_heterog))/
	countypop$POP10
}

export_heterog = cbind(countypop, nulldata_heterog)
write.table(export_heterog,file = "DATA_Null heterog.csv", 
	sep = ",", row.names = FALSE)

# Test plots
# hist(nulldata_heterog)
# meanrate = rowMeans(nulldata_heterog)
# plot(countypop$longitude, countypop$latitude, 
	# col = ifelse(meanrate> N/(C*P),'red','green'))


