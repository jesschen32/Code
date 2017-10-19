# This code conducts estimation and inference for Local Modified Moran's I.
# Local Modified Moran's I is a local indicator of spatial association (LISA), 
# derived from Jackson et al's (2010) Modified Moran's I 
# and Anselin's (1995) Local Moran's I.

# This new test statistic corrects for common problems with LISAs, including
# lack of a clear null distribution, population heterogeneity, multiple testing issues, 
# fixed neighborhood sizes and spatial correlation of test statistics.

# The code is mainly complete, but still needs to apply Tango's (2000) correction 
# multiple testing for Moran's I in the LISA context.

# INPUTS: Data file of regional rates and coordinate data for each corresponding region
# OUTPUTS: CSV of original data and p-values (likelihood of being a cluster) under
# the Local Modified Moran's I test

# Note: Code calls upon R libraries "snow" and "snowfall" for parallel computing.

# References: 
# Jackson MC, Huang L, Xie Q, Tiwari RC. (2010). 
# A modified version of Moran’s I. International Journal of Health Geography, 9:33.
# Anselin, L. (1995). Local indicators of spatial association – LISA. 
# Geographical Analysis, 27, 93-115.

##################################################################
    			#  USER INPUTS 
##################################################################

# Set path 
setwd("C:/Users/Jess Chen/Dropbox/Jess-LISA/Simulations and Code")

# Load data
filename = "Local cluster_ 0.3 %_homog"
data = read.csv(paste("./Data/DATA_", filename, ".csv", sep = ""),header=T)

# Run code on sample of data for faster testing. comment out otherwise
# data = data[sample(nrow(data), 50),  ] 

# Identify rate data and coordinate data (of country centroids)
ys = data[ , 6:dim(data)[2]]
coor = data[c("latitude", "longitude")]

# Distance threshold for exponential distance decay weight matrix 
# Counties max(distance)/k apart won't influence each other
# min(k) = 2
k = 2

# Select number of sampling iterations (999 unless for testing)
T = 999

##################################################################
     		   # CALCULATE LOCAL MODIFIED MORAN'S I
	         #   Follows Jackson et al (2010)

    		   # & CALCULATE PSEUDO-SIGNIFICANCE LEVELS
  # Conditional randomization per Anselin (1992) and GeoDa Center
##################################################################


####################### Create Weight Matrix #####################

# Create exponential distance decay weight matrix (dimensions: obs x obs)
# Wii is zero
# Restrict weights to zero for places that are over half the study area apart
# Row-standardize the weight matrix

distance = as.matrix(dist(coor, method = "euclidean", diag=TRUE, upper=TRUE))
w = exp(-distance)
diag(w) = 0
w[distance > max(distance)/k] = 0
w = w/rowSums(w)

###################### Define Functions ##########################

# Function to calculate Local Modified Moran's I for one place
localmod = function(y, wi, i, meandev, ybar)
{

# y = vector of rates indexed by place
# wi = ith row of weight matrix
# i = index of current place
# meandev = vector of mean deviation of ith rate
# ybar = mean rate

teststat = meandev*sum(wi*(y-ybar))/sum(wi*((y[i]-y)^2)) 
return(teststat)
}

# Function to randomize T times
# Get simulated test stats and p-value for all places
sim = function(y)
{
ybar = mean(y) # mean rate (constant across places)

# Loop over places
output = matrix(,length(y))
for (i in 1:length(y))
{
# Variables constant over randomization
wi = w[i,] # row of weight matrix for this place
meandev = y[i] - ybar # mean deviation of ith rate

# Simulate test stats for this place
Is = vector(,length=T)
for (j in 1:T) 
{
newy = y
newy[-i] = sample(newy[-i]) # Sample by conditional randomization
Is[j] = localmod(newy, wi, i, meandev, ybar)
}

# Calculate p-value for this place
Iws = localmod(y, wi, i, meandev, ybar)
empdistr = ecdf(Is) # Empirical distribution of sampled test statistics
pval = 1 - empdistr(Iws)
output[i] = pval # Output p-val
}

# Return all pvals
return(output)
}

# Implementing Tango's (2000) mulitiple testing correction, under development
#pvalT = # set number of iterated p-values
#pdistr = matrix(,pvalT,1)
#meanpval = function(obs)
#{
#for (m in 1:pvalT)
#{
#pdistr[m]=sim(obs)[1001]
#}
#return(pdistr)
#}
#mean(pdistr)

############ Iterate Over Places (In Parallel Computing)##########

Start = Sys.time()

library(snow)
library(snowfall)

# Sets up the machines which will be used in the parallel calculation
sfInit(parallel=TRUE, cpus=4)

# Distribute copies of data/functions to slave processors
sfExport("sim", "localmod", "ys", "w", "T")

# Main computations
simulate = sfApply(ys,2,sim) # Apply computations across places

sfStop()

elapsed = Sys.time() - Start
elapsed

# Some test plots, under development
# Plot empirical distributions (test one)
# hist(simulate[1,1:T])
# abline(v=simulate[1,ncol(simulate)-1])

###################### Export Results ############################

# Export results to csv
write.table(simulate,file=paste("./Results/RESULTS_", filename, ".csv", sep = ""),sep=",",col.names=NA)

# Test plots
p.val = simulate[,2]
lat = coor[,c("latitude")]
long = coor[,c("longitude")]
plot(long, lat, col = ifelse(p.val<0.05,'red','green'))



