# Set working directory
setwd("C:/Users/Jess Chen/Dropbox/Jess-LISA/Simulations")

# Read in county file
countypop = read.csv("countypop_census.csv", header = T)

# Use continental US counties
# Remove AK (FIPS = 02), HI (FIPS = 15), and PR (FIPS = 72)
countypop = countypop[-which(countypop$FIPS>2000 & countypop$FIPS<3000),]
countypop = countypop[-which(countypop$FIPS>15000 & countypop$FIPS<16000),]
countypop = countypop[-which(countypop$FIPS>72000 & countypop$FIPS<73000),]
ID = c(1:nrow(countypop))
countypop = cbind(ID, countypop)

# Define variables
N = 5000 # Total number of cases in nation
T = 1000 # Number of alt datasets to create
C = dim(countypop)[1] # Number of continental counties
P = 5000 # Set homogenous population 
RR = 4 # Set increased RR for counties within cluster
CS = c(.05, .10, .15, .20, .30) # Set cluster sizes as % of national pop

# Define cluster centered at Columbia County, GA (FIPS = 13073)
cluster_center = which(countypop$FIPS==13073)
centroids = cbind(countypop$latitude, countypop$longitude)
distance = as.matrix(as.matrix(dist(centroids, method = "euclidean", 
	diag = TRUE, upper = TRUE))[ , cluster_center])

# Write function that inputs cluster size and outputs alt data
for (i in 1:5)
{
# Define higher RR counties
closest = order(distance)[1:(CS[i]*C)] # Identify closest counties to center
filename = paste("Cluster Counties", CS[i], "%.csv")
write.table(countypop[closest,], file = filename, 
	sep = ",", row.names = FALSE)
}

