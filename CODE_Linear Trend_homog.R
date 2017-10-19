
# INPUT: Contemporary U.S. counties
# Vars: FIPS, Name, 2010 Census pop and centroids (lat and long) 
# Source: http://www.census.gov/geo/maps-data/data/gazetteer.html

# OUTPUT: Alternative datasets of county-level rates 
# Assumptions: Homogenous populations across counties
# Left-right global trend, increasing monotone
# Scenarios: Linear trend and exponential trend

##################### (User Defined Inputs) ######################

# Set working directory
setwd("C:/Users/Jess Chen/Dropbox/Jess-LISA/Simulations and Code/Data")

# Read in county file
countypop = read.csv("countypop_census.csv", header = T)

# Use continental US counties
# Remove AK (FIPS = 02), HI (FIPS = 15), and PR (FIPS = 72)
countypop = countypop[-which(countypop$FIPS>2000 & countypop$FIPS<3000),]
countypop = countypop[-which(countypop$FIPS>15000 & countypop$FIPS<16000),]
countypop = countypop[-which(countypop$FIPS>72000 & countypop$FIPS<73000),]

# Test plot - check if only continental counties remain
# plot(countypop$longitude, countypop$latitude)

# Define variables
N = 5000	# Total number of cases in nation
T = 1000	# Number of alt datasets to create
C = dim(countypop)[1] # Number of continental counties
I = 100 	# Number of left-right intervals
P = 5000 	# Set homogenous population 
RR = 1.5	# Set upper limit for increased RR

################### Left-Right Linear Trend ######################

# Divide the U.S. into I equal longitudal intervals 
intervals = seq(min(countypop$longitude), max(countypop$longitude), 
	by = ((max(countypop$longitude) - min(countypop$longitude))/I))
counties = findInterval(countypop$longitude, intervals, rightmost.closed = T)

# Define risks for I intervals (risk increases monotone from 1 to RR)
l_risk = seq(1, RR, (RR-1)/(I-1))

# Assign counties to risks based on longitude
assign_lrisk = l_risk[counties]

# List breakpoints according to assigned risk
breakpts_l = c(0, cumsum(assign_lrisk)/sum(assign_lrisk))

# Create alt data
altlinear_homog = matrix( , C, T)	
for (j in 1:T)
{
altlinear_homog[ , j] = table(cut(runif(N, 0, 1),	# Generate runif RV
	breaks = breakpts_l))/	# Cut according to breakpts defined by risk				
	P # Normalize by homog pop to get rate
}

export_linear = cbind(countypop, altlinear_homog)
write.table(export_linear, file="DATA_Alt Linear Data Homog.csv", 
	sep = ",", row.names = FALSE)

################## Left-Right Exponential Trend ###################

# Define risks for I intervals 
# Risk increases exponentially from 1 to RR: exp(x) = risk
x_risk = seq(log(1), log(RR), (log(RR)-log(1))/(I-1))
exp_risk = exp(x_risk)

# Assign counties to risks based on longitude
assign_exp = exp_risk[counties]

# List breakpoints according to assigned risk
breakpts_exp = c(0, cumsum(assign_exp)/sum(assign_exp))

# Create alt data
altexp_homog = matrix( , C, T)	
for (j in 1:T)
{
altexp_homog[ , j] = table(cut(runif(N, 0, 1),
	breaks = breakpts_exp))/					
	P # Normalize by homog pop to get rate
}

export_exp = cbind(countypop, altexp_homog)
write.table(export_exp, file="DATA_ Alt Exponential Homog.csv", 
	sep = ",", row.names = FALSE)

###################### Test Plots ######################

# Get the mean count generated for each county
mean_lrate = rowMeans(altlinear_homog)
mean_exp = rowMeans(altexp_homog)

# Select number of colors for mapping
# Cut rates by this number
num_col = 100
cut_lrate = cut(mean_lrate, breaks = num_col)
cut_exprate = cut(mean_exp, breaks = num_col)
color = rev(heat.colors(num_col))

# Make choropleth maps
par(mfrow=c(1,2))
plot(countypop$longitude, countypop$latitude, main = "Linear Trend Homog",
	 col = color[cut_lrate])
plot(countypop$longitude, countypop$latitude, main = "Exp Trend Homog",
	 col = color[cut_exprate])


