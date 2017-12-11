#set working directory
setwd("C:/Users/hp/Desktop/Mindstix")
list.files()

##Read and Load the data into R.
item1<-read.table("item_properties_part1.csv",header = TRUE,sep = ",")
item2<-read.table("item_properties_part2.csv",header = TRUE,sep = ",")
event<-read.csv("events.csv",header = TRUE,sep = ",")
c_tree<-read.csv("category_tree.csv",header = TRUE,sep = ",")

##Data Exploratory analysis

##seeing Head and Tail of the data
head(item1,10)
tail(item1,10)
head(item2,10)
tail(item1,10)
head(event,10)
tail(event,10)
head(c_tree,10)
tail(c_tree,10)

##Summary of data
summary(item1)
summary(item2)
summary(event)
summary(c_tree)

##structure of the data
str(item1)
str(item2)
str(event)
str(c_tree)
#View(item1)
##comdine data set we have
##by using rbind
df<-rbind(item1,item2)

##names of data set
names(df)
names(event)
names(c_tree)
dim(df)
dim(event)
dim(c_tree)

##Checking miss values in data set
sum(is.na(item1))
sum(is.na(item2))
sum(is.na(event))
sum(is.na(c_tree))

##Imputing missing values in Events and C_tree
#By using kkn impution
library(DMwR)
event<-centralImputation(event)
c_tree<-centralImputation(c_tree)
sum(is.na(event))
sum(is.na(c_tree))

#View(event)

## merging data frames and convert into as one dataframe
##Then some how to aggregate the amount by "ItemId" and "timestamp"
#itm_evnt<-df[event, on=c(itemid="itemid", timestamp="timestamp"), nomatch=0L]


##Merge the data sets
#merge_data_final <- merge(df, event, 
                          #df$itemid == event$itemid && df$timestamp == event$timestamp)
dim(df)
dim(event)
total <- merge(df,event,by=c("itemid","timestamp"))

#View(total)
names(total)

dim(total)

##Merge the data set "itm_evnt" and "C_tree"

real_data<-merge(total,c_tree)

names(real_data)
#View(real_data)

##sample the data into train and test


library(factoextra)
# Use the get_dist() function from the factoexrtra to calculate inter-observation distances

distance <- get_dist(real_data)

# The fviz_dist() function plots a visual representation of the inter-observation distances

fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))


##Hierarchical Clustering

##Let's now perform hierarchical clustering using the hclust() function,for which we'll first need to calculate the distance measures

# We use the euclidean distance measure

dist <- dist(real_data, method = "euclidean")

hc_fit <- hclust(dist, method = "ward.D2")

plot(hc_fit)

points_hc <- cutree(hc_fit, k = 6)

# Store the clusters in a data frame along with the cereals data

retail_clusts_hc <- cbind(points_hc, real_data)

# Have a look at the head of the new data frame

colnames(retail_clusts_hc)[1] <- "cluster_hc"

head(cereals_clusts_hc)

plot(hc_fit)

rect.hclust(hc_fit, k = 6, border = "red")



##K-Means Clustering
##Build a basic kmeans model with k = 2, using the kmeans() function


set.seed(123)
km_basic <- kmeans(real_data, centers = 2)

#km<-kmeans(real_data, 2, nstart=10)

str(km_basic)


fviz_cluster(km_basic, real_data)


# Initialize wss to 0

wss <- 0

# From 1 upto upto 10 cluster centers, fit the kmeans model

for (i in 1:10) {
  
  cfit = kmeans(real_data, centers = i)
  
  # Store the sum of within sum of square
  
  wss[i] <- sum(cfit$withinss)
  
}
plot(1:10, wss, type = "b")


set.seed(123)

fviz_nbclust(real_data, kmeans, method = "wss")

km_clust <- kmeans(real_data, 6, nstart = 10)

# after choosing k as 6, let's store the cluster groupings along with the data in a new data frame

km_points <- km_clust$cluster

# Store the cluster assignments in a new data frame

retail_clusts_km <- as.data.frame(cbind(km_clust$cluster, real_data))

# Look at the head of the data

head(retail_clusts_km)

colnames(retail_clusts_km)[1] <- "cluster_km"
fviz_cluster(km_clust, real_data)





