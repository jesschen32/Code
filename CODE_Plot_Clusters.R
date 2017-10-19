# Set working directory
setwd("C:/Users/jess_chen/Dropbox/Jess-LISA/Simulations/Data/")

###################### Read Files ######################

data5 = read.csv("DATA_Local cluster_05_heter.csv")
data10 = read.csv("DATA_Local cluster_10_heter.csv")
data15 = read.csv("DATA_Local cluster_15_heter.csv")
data20 = read.csv("DATA_Local cluster_20_heter.csv")
data30 = read.csv("DATA_Local cluster_30_heter.csv")

###################### Test Plots ######################

# Get the mean count generated for each county
# County is red if it exceeds the nth percentile 
# Where n is based on cluster size 

sum_5=rowMeans(data5[,6:1005])
sum_10=rowMeans(data10[,6:1005])
sum_15=rowMeans(data15[,6:1005])
sum_20=rowMeans(data20[,6:1005])
sum_30=rowMeans(data30[,6:1005])

par(mfrow=c(3,2))
plot(data5$longitude, data5$latitude, main = "5% Cluster", 
	xlab = "Longitude", ylab = "Latitude",
	col = ifelse(sum_5> quantile(sum_5, 0.95),'red','green'))
plot(data5$longitude, data5$latitude, main = "10% Cluster", 
	xlab = "Longitude", ylab = "Latitude",
	col = ifelse(sum_10> quantile(sum_10, 0.9),'red','green'))
plot(data5$longitude, data5$latitude, main = "15% Cluster", 
	xlab = "Longitude", ylab = "Latitude",
	col = ifelse(sum_15> quantile(sum_15, 0.85),'red','green'))
plot(data5$longitude, data5$latitude, main = "20% Cluster", 
	xlab = "Longitude", ylab = "Latitude",
	col = ifelse(sum_20> quantile(sum_20, 0.8),'red','green'))
plot(data5$longitude, data5$latitude, main = "30% Cluster", 
	xlab = "Longitude", ylab = "Latitude",
	col = ifelse(sum_30> quantile(sum_30, 0.7),'red','green')) 

###################### Output for ArcGIS ######################

sum_5[sum_5>quantile(sum_5, 0.95)]<-1
sum_5[sum_5<=quantile(sum_5, 0.95)]<-0
sum_10[sum_10>quantile(sum_10, 0.95)]<-1
sum_10[sum_10<=quantile(sum_10, 0.95)]<-0
sum_15[sum_15>quantile(sum_15, 0.95)]<-1
sum_15[sum_15<=quantile(sum_15, 0.95)]<-0
sum_20[sum_20>quantile(sum_20, 0.95)]<-1
sum_20[sum_20<=quantile(sum_20, 0.95)]<-0
sum_30[sum_30>quantile(sum_30, 0.95)]<-1
sum_30[sum_30<=quantile(sum_30, 0.95)]<-0

cluster5 = cbind(data5[,1:5], sum_5)
cluster10 = cbind(data10[,1:5], sum_10)
cluster15 = cbind(data15[,1:5], sum_15)
cluster20 = cbind(data20[,1:5], sum_20)
cluster30 = cbind(data30[,1:5], sum_30)

write.table(countypop[closest,], file = filename, 
	sep = ",", row.names = FALSE)

