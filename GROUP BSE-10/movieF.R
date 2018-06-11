#The User-Based Collaborative Filtering Approach

library(reshape2)
library(recommenderlab)
library(ggplot2)
library(data.table)

datam <- read.csv("C:/Users/Pc2/Desktop/r/DATA/movieR/movies.csv", header=TRUE)
View(datam)
datar <- read.csv("C:/Users/Pc2/Desktop/r/DATA/movieR/ratings.csv", header=TRUE)
View(datar)

#Create ratings matrix. Rows = userId, Columns = movieId
ratingmat <- dcast(datar, userId~movieId, value.var = "rating", na.rm=FALSE)
ratingmat <- as.matrix(ratingmat[,-1]) #remove userIds

library(recommenderlab)
#Convert rating matrix into a recommenderlab sparse matrix
ratingmat <- as(ratingmat, "realRatingMatrix")

#Normalize the data
ratingmat_norm <- normalize(ratingmat)

#Create Recommender Model. "UBCF" stands for User-Based Collaborative Filtering
recommender_model <- Recommender(ratingmat_norm, method = "UBCF", param=list(method="Cosine",nn=30))
recom <- predict(recommender_model, ratingmat[1], n=10) #Obtain top 10 recommendations for 1st user in dataset
recom_list <- as(recom, "list") #convert recommenderlab object to readable list

#Obtain recommendations
recom_result <- matrix(0,10)
for (i in c(1:10)){
  recom_result[i] <- datam[as.integer(recom_list[[1]][i]),2]
}
View(recom_result)
#evaluation_scheme <- evaluationScheme(ratingmat, method="cross-validation", k=5, given=3, goodRating=5) #k=5 meaning a 5-fold cross validation. given=3 meaning a Given-3 protocol
#evaluation_results <- evaluate(evaluation_scheme, method="UBCF", n=c(1,3,5,10,15,20))
#eval_results <- getConfusionMatrix(evaluation_results)[[1]]
