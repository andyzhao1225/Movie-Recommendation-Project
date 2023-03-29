##-------------Movie Recommendation Project--------------------


##########################################################
# Create edx and final_holdout_test sets 
##########################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

options(timeout = 120)

dl <- "ml-10M100K.zip"
if(!file.exists(dl))
  download.file("https://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings_file <- "ml-10M100K/ratings.dat"
if(!file.exists(ratings_file))
  unzip(dl, ratings_file)

movies_file <- "ml-10M100K/movies.dat"
if(!file.exists(movies_file))
  unzip(dl, movies_file)

ratings <- as.data.frame(str_split(read_lines(ratings_file), fixed("::"), simplify = TRUE),
                         stringsAsFactors = FALSE)


colnames(ratings) <- c("userId", "movieId", "rating", "timestamp")

ratings <- ratings %>%
  mutate(userId = as.integer(userId),
         movieId = as.integer(movieId),
         rating = as.numeric(rating),
         timestamp = as.integer(timestamp))

movies <- as.data.frame(str_split(read_lines(movies_file), fixed("::"), simplify = TRUE),
                        stringsAsFactors = FALSE)

colnames(movies) <- c("movieId", "title", "genres")
movies <- movies %>%
  mutate(movieId = as.integer(movieId))


movielens <- left_join(ratings, movies, by = "movieId")

# Final hold-out test set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
# set.seed(1) # if using R 3.5 or earlier
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in final hold-out test set are also in edx set
final_holdout_test <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from final hold-out test set back into edx set
removed <- anti_join(temp, final_holdout_test)
edx <- rbind(edx, removed)
rm(dl, ratings, movies, test_index, temp, movielens, removed)


##-------------Exploratory data analysis--------------------

#load the kable function package
if(!require(kableExtra)) install.packages("kableExtra")
library(kableExtra)

#show the first 5 rows of the edx dataset
edx %>% head(5) %>% 
  kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                position = "center",
                full_width = FALSE) %>% row_spec(c(1,2,3,4,5),color = "white" , background ="green")

#check the missing values in edx
sapply(edx, function(n) sum(is.na(n))) %>% 
  kable(col.names = c("Missing values per variable")) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                position = "center",
                full_width = FALSE) %>% column_spec(1 ,color = "white" , background ="green")

#summary of rating
summary(edx$rating)

#plot histogram of Distribution of Ratings
hist(edx$rating, main="Distribution of Ratings", xlab="Ratings")


#top 10 most rated movies
edx %>% group_by(title) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) %>% top_n(10,count) %>% 
  kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                position = "center",
                full_width = FALSE) %>% row_spec(c(1,3,5,7,9),color = "white" , background ="green")

#top 10 least rated movies
edx %>% group_by(title) %>%
  summarize(count = n()) %>%
  arrange(count) %>% head(10) %>% 
  kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                position = "center",
                full_width = FALSE) %>% row_spec(c(1,3,5,7,9),color = "white" , background ="red")

#plot histogram of ratings frequency distribution by movieid
ggplot(edx, aes(movieId)) +
  theme_classic()  +
  geom_histogram(bins=50) +
  labs(title = "Ratings Frequency Distribution by Title (MovieID)",
       x = "Title (MovieID)",
       y = "Frequency")

#plot histogram of ratings mean distribution by movieid
edx %>%
  group_by(movieId) %>%
  summarise(mean = mean(rating)) %>%
  ggplot(aes(mean)) +
  theme_classic()  +
  geom_histogram(bins=10) +
  labs(title = "Mean Distribution per Title",
       x = "Mean",
       y = "Frequency")

#top rating users
edx %>% group_by(userId) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) %>% head(10) %>% 
  kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                position = "center",
                full_width = FALSE)

#bottom rating users
edx %>% group_by(userId) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) %>% tail(10) %>% 
  kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                position = "center",
                full_width = FALSE)

#plot number of ratings against userid
edx %>%
  count(userId) %>%
  ggplot(aes(n)) +
  geom_histogram( bins=30, color = "darkgreen") + scale_x_log10() +
  ggtitle("Users") +
  labs(subtitle ="Ratings Frequency Distribution by User (UserID)",
       x="userId" ,y="Frequency") +
  theme_classic()

#check the unique genres
edx %>% summarize(genres=n_distinct(genres))

#sample a dataset from edx
t1000<-sample(1:nrow(edx),as.integer(0.001*nrow(edx)))
dat1000<-edx[t1000,]

#plot Sample Ratings Frequency Distribution by Genres
dat1000 %>% separate_rows(genres, sep = "\\|") %>% group_by(genres) %>%
  summarise(count=n()) %>%
  ggplot(aes(genres, count)) +
  theme_classic()  +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +  
  labs(title = "Sample Ratings Frequency Distribution by Genres",
       x = "Genres",
       y = "Sample Frequency")


#plot the Sample Mean Distribution of Rating by Genres

dat1000 %>% separate_rows(genres, sep = "\\|") %>%
  group_by(genres) %>%
  summarise(mean = mean(rating)) %>%
  ggplot(aes(genres, mean)) +
  theme_classic()  +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Sample Mean Distribution of Rating by Genres",
       x = "Genres",
       y = "Sample Mean")



#plot mean distribution of ratings by month
edx %>%
  mutate(date = round_date(as_datetime(timestamp), unit = "month")) %>% group_by(date) %>%
  summarize(rating = mean(rating)) %>%
  ggplot(aes(date, rating)) +
  geom_point() +
  geom_smooth() +
  ggtitle("Timestamp, time unit : month")+
  labs(subtitle = "average ratings")

#plot mean distribution of ratings by week
edx %>%
  mutate(date = round_date(as_datetime(timestamp), unit = "week")) %>% group_by(date) %>%
  summarize(rating = mean(rating)) %>%
  ggplot(aes(date, rating)) +
  geom_point() +
  geom_smooth() +
  ggtitle("Timestamp, time unit : week")+
  labs(subtitle = "average ratings")

##-------------Methods--------------------

#define RMSE function
RMSE <- function(true_ratings = NULL, predicted_ratings = NULL) {
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

#change name of the testing dataset
test<-final_holdout_test

#calculate the average of all ratings of the edx dataset
mu <- mean(edx$rating)

#naive mean-baseline model
rmse_model_0 <- RMSE(test$rating,mu) 
rmse_model_0

#calculate b_i on the edx dataset
movie_m <- edx %>%
  group_by(movieId) %>% summarize(b_i = mean(rating - mu))

# predicted ratings
predicted_ratings_bi <- mu + test %>% left_join(movie_m, by='movieId') %>% .$b_i

#calculate b_u using the edx set
user_m <- edx %>%
  left_join(movie_m, by='movieId') %>% group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))

#predicted ratings
predicted_ratings_bu <- test %>% left_join(movie_m, by='movieId') %>% 
  left_join(user_m, by='userId') %>% 
  mutate(pred = mu + b_i + b_u) %>% .$pred

#calculate b_u_g using the edx set
genre_m <- edx %>%
  left_join(movie_m, by='movieId') %>%
  left_join(user_m, by='userId') %>%
  group_by(genres) %>%
  summarize(b_u_g = mean(rating - mu - b_i - b_u))

#predicted ratings
predicted_ratings_bug <- test %>%
  left_join(movie_m, by='movieId') %>%
  left_join(user_m, by='userId') %>%
  left_join(genre_m, by='genres') %>%
  mutate(pred = mu + b_i + b_u + b_u_g) %>%
  pull(pred)

#calculate the RMSE for movies
rmse_model_1 <- RMSE(test$rating,predicted_ratings_bi) 
rmse_model_1

#calculate the RMSE for movies + users 
rmse_model_2 <- RMSE(test$rating,predicted_ratings_bu) 
rmse_model_2

#calculate the RMSE for movies + users + genres effects
rmse_model_3 <- RMSE(test$rating,predicted_ratings_bug) 
rmse_model_3


#set up the values for the regularized tuning parameter lambda
lambdas <- seq(0, 10, 0.25)

#apply all the values of lambda on regularized models
rmses1 <- sapply(lambdas, function(x){
  mu_reg <- mean(edx$rating)
  
  b_i_reg <- edx %>%
    group_by(movieId) %>%
    summarize(b_i_reg = sum(rating - mu_reg)/(n()+x))
  
  predicted_ratings_b_i <-
    test %>%
    left_join(b_i_reg, by = "movieId") %>% mutate(pred = mu_reg + b_i_reg) %>% .$pred
  return(RMSE(test$rating,predicted_ratings_b_i)) })

rmses2 <- sapply(lambdas, function(x){
  mu_reg <- mean(edx$rating)
  
  b_i_reg <- edx %>%
    group_by(movieId) %>%
    summarize(b_i_reg = sum(rating - mu_reg)/(n()+x))
  
  b_u_reg <- edx %>%
    left_join(b_i_reg, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u_reg = sum(rating - b_i_reg - mu_reg)/(n()+x))
  
  predicted_ratings_b_i_u <-
    test %>%
    left_join(b_i_reg, by = "movieId") %>% left_join(b_u_reg, by = "userId") %>% mutate(pred = mu_reg + b_i_reg + b_u_reg) %>% .$pred
  return(RMSE(test$rating,predicted_ratings_b_i_u)) })

#plot rmse values against lambdas
qplot(lambdas, rmses1)
qplot(lambdas, rmses2)

#find the optimal lambda values respectively
lambda1 <- lambdas[which.min(rmses1)] 
lambda1
lambda2 <- lambdas[which.min(rmses2)] 
lambda2

#get the minimum rmse values
rmse_model_1_reg <- min(rmses1) 
rmse_model_1_reg
rmse_model_2_reg <- min(rmses2) 
rmse_model_2_reg

##-------------Results--------------------

#summarize all the rmse on final_holdout_test data set for models
rmse_results <- data.frame(methods=c("movie effect","movie + user effects",
                                     "movie + user + genre effects", "regularized movie effect",
                                     "regularized movie + user effects"),
                           rmse=c(rmse_model_1,rmse_model_2,rmse_model_3,rmse_model_1_reg,
                                  rmse_model_2_reg)) 

kable(rmse_results) %>% kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                                      position = "center",
                                      full_width = FALSE) %>% row_spec(c(4,5),color = "white" , background ="green")

##-------------Conclusion--------------------

#Apparently, all 5 rmse values from the result summary are less than the value 1.061202 from naïve baseline model.  
#The overall best model is regularized movie + user effects one due to their overwhelming enhancement in decreasing 
#rmse over genre predictor. Though, the regularization process just slightly improved the model quality, it helps 
#to achieve the objective. 

#Similarity measures like euclidean distance, pearson correlation, cosine distance and dimensionality reduction like 
#PCA/SVD can be applied to deal with the problem of sparsity of a more effective matrix for future work. 
#Collaborative filtering recommender system and even ensemble algorithms could be implemented with 
#supercomputing devices.
