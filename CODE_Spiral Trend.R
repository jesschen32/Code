
# INPUT: Contemporary U.S. counties
# Vars: FIPS, Name, 2010 Census pop and centroids (lat and long) 
# Source: http://www.census.gov/geo/maps-data/data/gazetteer.html

# OUTPUT: Alternative datasets of county-level rates 
# Assumptions: Cluster of counties with increased RR
# Decreasing monotone from center to relative risk of 1
# Scenarios: Homogenous and heterogenous pops

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

# Test plot - check if only continental counties remain
# plot(countypop$longitude, countypop$latitude)

# Define variables
N = 5000	# Total number of cases in nation
T = 1000	# Number of alt datasets to create
C = dim(countypop)[1] # Number of continental counties
I = 100 	# Number of left-right intervals
P = 5000 	# Set homogenous population 
RR = 1.5	# Set upper limit for increased RR
CS = 0.02 	# Set cluster size
N_heterog = (N/(C*P))*	# Keep same avg national rate as under homog pop
	sum(countypop$POP10)	# In setting number of total cases

# Define cluster centered at Scott County, KS (FIPS = 20171)
cluster_center = which(countypop$FIPS==20171)
centroids = cbind(countypop$latitude, countypop$longitude)
distance = as.matrix(as.matrix(dist(centroids, method = "euclidean", 
	diag = TRUE, upper = TRUE))[ , cluster_center])
closest = order(distance)	# Orders counties by distance from center

################### Spiral Trend: Homog Pop ######################

# Divide the U.S. into I equal intervals based on distance from center
intervals = seq(min(distance), max(distance),
	by = ((max(distance) - min(distance))/I))
counties = findInterval(distance, intervals, rightmost.closed = T)

# Define risks for I intervals
# Risk decreases monotone from RR to 1 as distance increases
s_risk = rev(seq(1, RR, (RR-1)/(I-1)))

# Assign counties to risks based on distance
assign_srisk = s_risk[counties]

# Assign center cluster RR
assign_srisk[order(distance)[1:(CS*C)]] = RR

# List breakpoints according to assigned risk
breakpts_homog = c(0, cumsum(assign_srisk)/sum(assign_srisk))

# Create alt data
spiral_homog = matrix( , C, T)	
for (j in 1:T)
{
spiral_homog[ , j] = table(cut(runif(N, 0, 1),	# Generate runif RV
	breaks = breakpts_homog))/	# Cut according to breakpts defined by risk				
	P # Normalize by homog pop to get rate
}

export_spiral_homog = cbind(countypop, spiral_homog)
write.table(export_spiral_homog, file="DATA_Spiral Alt Homog.csv", 
	sep = ",", row.names = FALSE)

################### Spiral Trend: Heterog Pop ######################

# Assign counties to risks based on distance
# Adjust risk by population
assign_srisk2 = assign_srisk*countypop$POP10

# List breakpoints according to assigned risk
breakpts_heterog = c(0, cumsum(assign_srisk2 )/sum(assign_srisk2 ))

# Create alt data
spiral_heterog = matrix( , C, T)	
for (j in 1:T)
{
spiral_heterog[ , j] = table(cut(runif(N_heterog, 0, 1),	
	breaks = breakpts_heterog))/					
	countypop$POP10 # Normalize by heterog pop to get rate
}

export_spiral_heterog = cbind(countypop, spiral_heterog)
write.table(export_spiral_heterog, file="DATA_Spiral Alt Heterog.csv", 
	sep = ",", row.names = FALSE)

###################### Test Plots ######################

# Get the mean count generated for each county
mean_spiral_homog = rowMeans(spiral_homog)
mean_spiral_heterog = rowMeans(spiral_heterog)

# Select number of colors for mapping
# Cut rates by this number
num_col = 100
cut_spiral_homog = cut(mean_spiral_homog, breaks = num_col)
cut_spiral_heterog = cut(mean_spiral_heterog, breaks = num_col)
color = rev(rainbow(num_col))

# Make choropleth maps
par(mfrow=c(1,2))
plot(countypop$longitude, countypop$latitude, main = "Spiral Trend Homog",
	col = color[cut_spiral_homog])
plot(countypop$longitude, countypop$latitude, main = "Spiral Trend Heterog",
	col = color[cut_spiral_heterog])



