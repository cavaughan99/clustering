install.packages(c("cluster", "rattle","NbClust"))
library(dplyr)
library(cluster)
library(rattle)
library(NbClust)

# load the data and examine the first few rows
data(wine, package="rattle")
head(wine)

str(wine)
summary(wine$Type)
wine1 <- select(wine, -Type)
str(wine1)
#Remove the first column from the data and scale
# it using the scale() function
##attach(wine)
##wine$Type = NULL
scale(wine1)

# cluster the data using K-Means. 

# Method 1: plot the total within-groups sums of squares against the 
# number of clusters in a K-means solution can be helpful. 

wssplot <- function(data, nc=15, seed=1234){
	              wss <- (nrow(data)-1)*sum(apply(data,2,var))
               	      for (i in 2:nc){
		        set.seed(seed)
	                wss[i] <- sum(kmeans(data, centers=i)$withinss)}
	                
		      plot(1:nc, wss, type="b", xlab="Number of Clusters",
	                        ylab="Within groups sum of squares")
	   }

wssplot(wine1)

##This method appears to suggest 2 or 3 clusters based on how the slope of the curve changes
##in between 2 and 3 clusters and from 3 to 4 clusters. After 4 clusters, the slope is nearly flat and changes 
##very little, suggesting that additional clusters beyond 4 are of little benefit in terms of reducing
##the total within-groups sums of squared errors. 


# Use the NbClust library to determine number of clusters.

library(NbClust)
set.seed(1234)
nc <- NbClust(df, min.nc=2, max.nc=15, method="kmeans")
barplot(table(nc$Best.n[1,]),
	          xlab="Number of Clusters", ylab="Number of Criteria",
		            main="Number of Clusters Chosen by 26 Criteria")

library(NbClust)
set.seed(1234)
nc <- NbClust(wine1, min.nc=2, max.nc=15, method="kmeans")
barplot(table(nc$Best.n[1,]),
        xlab="Number of Clusters", ylab="Number of Criteria",
        main="Number of Clusters Chosen by 26 Criteria")

##It appears that 2 clusters are chosen by 10 criteria--more than twice as many criteria
##as any other number of clusters, suggesting that a 2-cluster solution 
##works well based on this method of determining clusters.

# run k-means and output the result

fit.km <- kmeans(wine1, centers=2, iter.max=1000)
str(fit.km)

# evaluate how well this clustering does by showing how the clusters in fit.km$clusters
# compare to the actual wine types in wine$Type. 

table(fit.km$cluster, wine$Type)

##One cluster maps on very well to one wine type, and the other
##two wine types are generally combined to form the other cluster. 
##In this sense, the clustering solution succeeded in reducing the complexity of the
##original data (by showing that two wine types are actually very similar on the dimensions
##used in the cluster analysis and can be combined into a single category or cluster).

# * Visualize these clusters using  function clusplot() from the cluster library.

clusplot(wine, fit.km$cluster)

##The plot shows a fair amount of overlap between the two clusters, indicating that
##this solution leaves some room for improvement to create a more distinct set of clusters.

