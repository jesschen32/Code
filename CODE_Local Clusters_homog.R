
# INPUT: Contemporary U.S. counties
# Vars: FIPS, Name, 2010 Census pop and centroids (lat and long) 
# Source: http://www.census.gov/geo/maps-data/data/gazetteer.html

# OUTPUT: Alternative datasets of county-level rates 
# Assumptions: Homogenous populations across counties
# Increased RR for counties inside cluster (1 RR for all others)
# Scenarios: Clusters containing various % of national pop

##################### (User Defined Inputs) ######################

# Set working directory
setwd("C:/Users/Jess Chen/Dropbox/Jess-LISA/Simulations and Code/Data/")

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
N = 5000 # Total number of cases in nation
T = 1000 # Number of alt datasets to create
C = dim(countypop)[1] # Number of continental counties
P = 5000 # Set homogenous population 
RR = 4 # Set increased RR for counties within cluster
CS = list(.05, .10, .15, .20, .30) # Set cluster sizes as % of national pop

# Define cluster centered at Columbia County, GA (FIPS = 13073)
cluster_center = which(countypop$FIPS==13073)
centroids = cbind(countypop$latitude, countypop$longitude)
distance = as.matrix(as.matrix(dist(centroids, method = "euclidean", 
	diag = TRUE, upper = TRUE))[ , cluster_center])

######################## Create Alt Data ###########################

# Divide interval [0,1] into CS*C segments of RR and (1-CS)*C segments of 1 
# Generate N uniform [0,1] random variates (N total cases)
# Count number of cases in each interval, divide by P (homog pop) for rates
# Do this T times to construct alt datasets

cluster_RR = c(rep(1, C)) # Start off with RR of 1 for each county

# Write function that inputs cluster size and outputs alt data
createdata = function(i)
{

# Define higher RR counties
closest = order(distance)[1:(i*C)] # Identify closest counties to center
cluster_RR[closest] = RR # Label those with higher RR
breakpts = c(0,cumsum(cluster_RR)/sum(cluster_RR)) # Set intervals accordingly

altdata_homog = matrix( , C, T)
for (j in 1:T)
{
altdata_homog[ , j] = table(cut(runif(N, 0, 1),
	breaks = breakpts))/
	P
}
return(altdata_homog)
}

# Creates array of alt datasets, separated into cluster size by 3rd dimension
altdata = sapply(CS, createdata, simplify = "array")

# Write these data to tables
for (i in 1:length(CS))
{ filename = paste("DATA_Local cluster_", CS[i], "%_homog.csv")
export_data = cbind(countypop, altdata[ , , i])
write.table(export_data, file = filename, 
	sep = ",", row.names = FALSE) }

###################### Test Plots ######################

# Get the mean count generated for each county
# County is red if it exceeds the nth percentile 
# Where n is based on cluster size 

sum_5=rowMeans(altdata[ , , 1])
sum_10=rowMeans(altdata[ , , 2])
sum_15=rowMeans(altdata[ , , 3])
sum_20=rowMeans(altdata[ , , 4])
sum_30=rowMeans(altdata[ , , 5])

par(mfrow=c(2,3))
plot(countypop$longitude, countypop$latitude, main = "5% Cluster", 
	col = ifelse(sum_5> quantile(sum_5, 0.95),'red','green'))
plot(countypop$longitude, countypop$latitude, main = "10% Cluster", 
	col = ifelse(sum_10> quantile(sum_10, 0.9),'red','green'))
plot(countypop$longitude, countypop$latitude, main = "15% Cluster", 
	col = ifelse(sum_15> quantile(sum_15, 0.85),'red','green'))
plot(countypop$longitude, countypop$latitude, main = "20% Cluster", 
	col = ifelse(sum_20> quantile(sum_20, 0.8),'red','green'))
plot(countypop$longitude, countypop$latitude, main = "30% Cluster", 
	col = ifelse(sum_30> quantile(sum_30, 0.7),'red','green'))
