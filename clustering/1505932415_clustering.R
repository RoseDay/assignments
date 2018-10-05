# This mini-project is based on the K-Means exercise from 'R in Action'
# Go here for the original blog post and solutions
# http://www.r-bloggers.com/k-means-clustering-from-r-in-action/

# Exercise 0: Install these packages if you don't have them already

# install.packages(c("cluster", "rattle.data","NbClust"))
library(cluster)
library(rattle.data)
library(NbClust)

# Now load the data and look at the first few rows
data(wine, package="rattle.data")
str(wine)
head(wine)

# Exercise 1: Remove the first column from the data and scale
# it using the scale() function
df <- scale(wine [-1])
str(df)
head(df)

# Now we'd like to cluster the data using K-Means. 
# How do we decide how many clusters to use if you don't know that already?
# We'll try two methods.

# Method 1: A plot of the total within-groups sums of squares against the 
# number of clusters in a K-means solution can be helpful. A bend in the 
# graph can suggest the appropriate number of clusters. 

wssplot <- function(df, nc=15, seed=1234){
  wss <- (nrow(df)-1)*sum(apply(df,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(df, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}

wssplot(df)


# Exercise 2:
#   * How many clusters does this method suggest?
# Three, there are the greatest vertical distances between 1 to 2, and 2 to 3
#   * Why does this method work? What's the intuition behind it?
# The greatest distances between groupings of data points
#   * Look at the code for wssplot() and figure out how it works
# the code determines the variances for groups around center means

# Method 2: Use the NbClust library, which runs many experiments
# and gives a distribution of potential number of clusters.

library(NbClust)
set.seed(1234)
nc <- NbClust(df, min.nc=2, max.nc=15, method="kmeans")
barplot(table(nc$Best.n[1,]),
	          xlab="Numer of Clusters", ylab="Number of Criteria",
		            main="Number of Clusters Chosen by 26 Criteria")


# Exercise 3: How many clusters does this method suggest?
# It's very difficult to read because of the x-axis numbering versus the bars.
# The tallest bar comes directly after 2, so three clusters, agreeing with prior test. 

# Exercise 4: Once you've picked the number of clusters, run k-means 
# using this number of clusters. Output the result of calling kmeans()
# into a variable fit.km

k = 3
set.seed(144)
fit.km <- kmeans(df, centers = k, nstart = 26)
str(fit.km)

# Now we want to evaluate how well this clustering does.

# Exercise 5: using the table() function, show how the clusters in fit.km$clusters
# compares to the actual wine types in wine$Type. Would you consider this a good
# clustering?
str(fit.km$cluster)
str(wine$Type)

table(fit.km$cluster, wine$Type)
#This is a good fit because the true positives (diagonal matrix cells) account for
(59 + 65 + 48)/(59 + 65 + 48 + 6) 
# 96.6% of the total observations. 

# Exercise 6:
# * Visualize these clusters using  function clusplot() from the cluster library
# * Would you consider this a good clustering?

clusplot(pam(df, 3))
#This appears to be a good cluster fit. I am not sure what the scales mean on the x- and y-
#axes because I don't know how the partitioning function "pam" works.
