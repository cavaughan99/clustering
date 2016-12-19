# This mini-project is based on the K-Means exercise from 'R in Action'
# Go here for the original blog post and solutions
# http://www.r-bloggers.com/k-means-clustering-from-r-in-action/

# Exercise 0: Install these packages if you don't have them already

install.packages(c("cluster", "rattle","NbClust"))
library(dplyr)
library(cluster)
library(rattle)
library(NbClust)

# Now load the data and look at the first few rows
data(wine, package="rattle")
head(wine)

str(wine)
summary(wine$Type)
wine1 <- select(wine, -Type)
str(wine1)
# Exercise 1: Remove the first column from the data and scale
# it using the scale() function
##attach(wine)
##wine$Type = NULL
scale(wine1)

# Now we'd like to cluster the data using K-Means. 
# How do we decide how many clusters to use if you don't know that already?
# We'll try two methods.

# Method 1: A plot of the total within-groups sums of squares against the 
# number of clusters in a K-means solution can be helpful. A bend in the 
# graph can suggest the appropriate number of clusters. 

wssplot <- function(data, nc=15, seed=1234){
	              wss <- (nrow(data)-1)*sum(apply(data,2,var))
               	      for (i in 2:nc){
		        set.seed(seed)
	                wss[i] <- sum(kmeans(data, centers=i)$withinss)}
	                
		      plot(1:nc, wss, type="b", xlab="Number of Clusters",
	                        ylab="Within groups sum of squares")
	   }

wssplot(wine1)




# Exercise 2:
#   * How many clusters does this method suggest?

##This method appears to suggest 2 or 3 clusters based on how the slope of the curve changes
##in between 2 and 3 clusters and from 3 to 4 clusters. After 4 clusters, the slope is nearly flat and changes 
##very little, suggesting that additional clusters beyond 4 are of little benefit in terms of reducing
##the total within-groups sums of squared errors. 
#   * Why does this method work? What's the intuition behind it?
##I think the intuition behind this method is that it allows one to understand the
##benefit of each additional cluster in terms of how much it reduces classification errors (as indexed by the 
##total within-groups sums of squared errors). Each additional cluster reduces these errors somewhat, 
##but, as is the case when adding more predictors to a model, the increased complexity of the solution is 
##not justified by the reduction in errors at some point. This plot allows one to see very clearly how much
##benefit is gained in classification (i.e., errors reduced) by each additional cluster and to identify the 
##point at which such added complexity ceases to be beneficial.
#   * Look at the code for wssplot() and figure out how it works
##The code for wssplot() works by creating a function to generate the total within groups sums of squared errors for 
##between 2 and 15 clusters for a particular data frame and plot the number of clusters on the x-axis
## against the corresponding total within-groups sums of squared errors on the y-axis.

# Method 2: Use the NbClust library, which runs many experiments
# and gives a distribution of potential number of clusters.

library(NbClust)
set.seed(1234)
nc <- NbClust(df, min.nc=2, max.nc=15, method="kmeans")
barplot(table(nc$Best.n[1,]),
	          xlab="Number of Clusters", ylab="Number of Criteria",
		            main="Number of Clusters Chosen by 26 Criteria")


# Exercise 3: How many clusters does this method suggest?

library(NbClust)
set.seed(1234)
nc <- NbClust(wine1, min.nc=2, max.nc=15, method="kmeans")
barplot(table(nc$Best.n[1,]),
        xlab="Number of Clusters", ylab="Number of Criteria",
        main="Number of Clusters Chosen by 26 Criteria")

##It appears that 2 clusters are chosen by 10 criteria--more than twice as many criteria
##as any other number of clusters, suggesting that a 2-cluster solution 
##works well based on this method of determining clusters.

# Exercise 4: Once you've picked the number of clusters, run k-means 
# using this number of clusters. Output the result of calling kmeans()
# into a variable fit.km

fit.km <- kmeans(wine1, centers=2, iter.max=1000)
str(fit.km)

# Now we want to evaluate how well this clustering does.

# Exercise 5: using the table() function, show how the clusters in fit.km$clusters
# compares to the actual wine types in wine$Type. Would you consider this a good
# clustering?

table(fit.km$cluster, wine$Type)

##One cluster maps on very well to one wine type, and the other
##two wine types are generally combined to form the other cluster. 
##In this sense, the clustering solution succeeded in reducing the complexity of the
##original data (by showing that two wine types are actually very similar on the dimensions
##used in the cluster analysis and can be combined into a single category or cluster).

# Exercise 6:
# * Visualize these clusters using  function clusplot() from the cluster library
# * Would you consider this a good clustering?

clusplot(wine, fit.km$cluster)

##The plot shows a fair amount of overlap between the two clusters, indicating that
##this solution leaves some room for improvement to create a more distinct set of clusters.

