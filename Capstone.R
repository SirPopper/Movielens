# Daniel Handojo
# Harvardx Data Science Capstone Project
# Movielens Dataset



##########################################################
# Create edx set, validation set (final hold-out test set)
##########################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(RColorBrewer)) install.packages("RColorBrewer", repos = "http://cran.us.r-project.org")



library(tidyverse)
library(caret)
library(data.table)
library(lubridate)
library(ggplot2)
library(RColorBrewer)
options(digits=5)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")


movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))


movielens <- left_join(ratings, movies, by = "movieId")


#### 1. Introduction ####

# Validation set will be 10% of MovieLens data
set.seed(1) # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]


# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")


# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)


rm(dl, ratings, movies, test_index, temp, movielens, removed)

#quiz
nrow(edx)
ncol(edx)

names(edx)

sum(edx$rating == 0.0)
sum(edx$rating == 3.0)

length(unique(edx$movieId))

length(unique(edx$userId))


#sapply with str_detect to check genres
genres <- c("Drama", "Comedy", "Thriller", "Romance")
sapply(genres, function(g){
  sum(str_detect(edx$genres, g))
})


edx %>% group_by(movieId, title) %>% 
  summarize(count = n()) %>% 
  arrange(desc(count))

edx %>% group_by(rating) %>% 
  summarize(count = n()) %>% 
  arrange(desc(count))

edx %>% group_by(rating) %>% 
  summarize(count = n()) %>% 
  ggplot(aes(x = rating, y = count)) +
  geom_line()


nrow(validation) + nrow(edx)



### 2. Analysis ####

str(edx)

edx$date_rating <- as_datetime(edx$timestamp)
edx$year_rating <- as.integer(year(edx$date))
edx$year_release <- as.numeric(str_sub(edx$title, -5, -2))


#first movie rated 1995
min(edx$year_rating)


#first released movie 1915
min(edx$year_release)


# people are likely to give full ratings instead of half ratings

rating_type <- ifelse((edx$rating == 1 |edx$rating == 2 | edx$rating == 3 | 
                         edx$rating == 4 | edx$rating == 5) ,
                      "whole_rating", 
                      "half_rating") 

edx$rating_type <- rating_type


edx %>% ggplot(aes(x= rating, fill = rating_type)) +
  geom_histogram( binwidth = 0.2) +
  scale_x_continuous(breaks = seq(0.5,5, by = 0.5)) + 
  scale_fill_manual(values =c("half_rating"="red", "whole_rating" ="lightblue3")) + 
  labs(x="rating", y="number of ratings") + 
  ggtitle("People tend to give full star ratings more likely than half star") +
  theme_minimal()


# checking out the release year
# most movies released 1995
edx %>% group_by(year_release) %>% 
  mutate(n = n()) %>% 
  select(year_release, n) %>% 
  slice(1) %>% 
  arrange(desc(n))


#visualize ratings count by year
edx %>% filter(year_release>1970) %>% 
  ggplot(aes(year_release)) +
  geom_histogram() +
  scale_x_continuous(breaks = seq(1970,2010, by = 2)) + 
  labs(x ="year", y="number of ratings") + 
  ggtitle("Ratings count by year") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# checking out genres
# drama, comedy and action are the 3 top movie genres
# realizing that trying to include genres in the matrix factorization 
#would include too much computing power, these is done differently later
genres <- edx %>% separate_rows(genres, sep="\\|") %>%
  group_by(genres) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

genres %>% top_n(10)

genres <- genres %>% mutate(total = sum(count), percentage = count/total)

genres %>% ggplot(aes(reorder(genres, percentage), percentage, fill =percentage)) +
  geom_bar(stat="identity") +
  coord_flip() +
  scale_fill_distiller(palette ="Blues") +
  labs(x="", y="Percentage") +
  ggtitle("Genres by Percentage")


# checking out some relations with time

# newer movies have more ratings
edx %>% group_by(movieId) %>% 
  summarize(n = n(), year = as.character(first(year_release))) %>% 
  qplot(year, n, data = . , geom = "boxplot") +
  coord_trans(y="sqrt")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# movies with the highest amount of ratings per year
highest_rate_titles <- edx %>%  filter(year_release >= 1993) %>% 
  group_by(movieId) %>% 
  summarize(n = n(), years = 2018 - first(year_release),
            title = title[1],
            rating = mean(rating)) %>% 
  mutate(rate = n/years) %>% 
  top_n(25, rate) %>% 
  arrange(desc(rate))

highest_rate_titles %>% top_n(10)


# visualizing the highest amount of ratings of the movies
highest_rate_titles %>% 
  top_n(20) %>% 
  ggplot(aes(x=reorder(title, rate), y = rate)) + 
  geom_bar(stat="identity", fill="lightblue3", color="lightblue4") +
  coord_flip(y=c(0,1400)) + 
  labs(x="", y = "Ratings per year") + 
  geom_text(aes(label = rate), hjust = -0.1, size = 3) + 
  ggtitle("Top 20 movies title by rate") +
  theme_minimal()


# diving into ratings


# distribution of movie ratings
edx %>% group_by(movieId) %>% 
  summarize(n = n()) %>% 
  ggplot(aes(n)) +
  geom_histogram(fill="lightblue3", color = "lightblue4", bins = 10) + 
  scale_x_log10() + 
  labs(x = "Count", y ="MovieID") +
  ggtitle("Number of Movie Ratings")


#distribution of users
edx %>% group_by(userId) %>% 
  summarize(n = n()) %>% 
  ggplot(aes(n)) +
  geom_histogram(fill="lightblue3", color = "lightblue4", bins = 10) + 
  scale_x_log10() + 
  labs(x = "Count", y ="userID") +
  ggtitle("Number of User Ratings")


#userid vs average rating of users
edx %>% group_by(userId) %>% 
  summarize(avg_rating = mean(rating)) %>% 
  ggplot(aes(x=userId, y=avg_rating)) +
  geom_hline(yintercept=mean(edx$rating), color="red") +
  geom_point(alpha= 0.1, color="lightblue4") + 
  ggtitle("UserID vs their average rating")


# skewed ratings, the higher the rating the higher the dispersion of rate vs rating
edx %>%  filter(year_release >= 1993) %>% 
  group_by(movieId) %>% 
  summarize(n = n(), years = 2018 - first(year_release),
            title = title[1],
            rating = mean(rating)) %>% 
  mutate(rate = n/years) %>% 
  ggplot(aes(rating, rate)) +
  geom_point()


# It seems that there is a week effect on the rating
edx %>% mutate(date = round_date(date_rating, unit = "week")) %>%
  group_by(date) %>%
  summarize(rating = mean(rating)) %>%
  ggplot(aes(date, rating)) +
  geom_point() +
  geom_smooth()


# there is a genre effect on rating
edx %>% group_by(genres) %>%
  summarize(n = n(), avg = mean(rating), se = sd(rating)/sqrt(n())) %>%
  filter(n >= 1000) %>% 
  mutate(genres = reorder(genres, avg)) %>%
  ggplot(aes(x = genres, y = avg, ymin = avg - 2*se, ymax = avg + 2*se)) + 
  geom_point() +
  geom_errorbar() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



#### 3. Models ####

set.seed(1)
test_index <- createDataPartition(y = edx$rating, times = 1,
                                  p = 0.2, list = FALSE)
train_set <- edx[-test_index,]
test_set <- edx[test_index,]

test_set <- test_set %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")


#RMSE
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}



#first average model
# y = mu
mu <- mean(train_set$rating)

naive_rmse <- RMSE(mu, test_set$rating)
naive_rmse

rmse_results <- tibble(method = "mu", RMSE=naive_rmse)
rmse_results



#### 3.1 movie effects ####

# calculate average
movie_avgs <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))

qplot(b_i, data = movie_avgs, bins = 10, color = I("black"))


#remember mu = 3.5, thus b_i = 1.5 equals perfect 5 star rating
# predict movie effect 
predicted_ratings <- mu + test_set %>% 
  left_join(movie_avgs, by = "movieId") %>% 
  .$b_i


# calculate and store results
mu_bi_rmse <- RMSE(predicted_ratings, test_set$rating)
mu_bi_rmse

rmse_results <- rmse_results %>% add_row(method = "mu + b_i", RMSE = mu_bi_rmse)
rmse_results



#### 3.2 user effect ####

#check distribution who rated more than 100 movies
train_set %>% 
  group_by(userId) %>% 
  filter(n()>= 100) %>% 
  summarize(b_u = mean(rating)) %>% 
  ggplot(aes(b_u)) + 
  geom_histogram(bins = 30, color = "black")


# create user effect variable
user_avgs <- train_set %>% 
  left_join(movie_avgs, by = "movieId") %>% 
  group_by(userId) %>% 
  summarize(b_u =mean(rating - mu - b_i))


# predict movie effect + user effect
predicted_ratings <- test_set %>% 
  left_join(movie_avgs, by = "movieId") %>% 
  left_join(user_avgs, by ="userId") %>% 
  mutate(pred = mu + b_i + b_u ) %>% 
  .$pred


# calculate and store results
mu_bi_bu_rmse <- RMSE(test_set$rating, predicted_ratings)
mu_bi_bu_rmse

rmse_results <- rmse_results %>% add_row(method ="mu + b_i + b_u", RMSE = mu_bi_bu_rmse)
rmse_results



#### 3.3 week effect ####

#calculate b_t week time effects
time_avgs <- train_set %>% 
  left_join(movie_avgs, by ="movieId") %>% 
  left_join(user_avgs, by ="userId") %>% 
  mutate(date = round_date(as_datetime(timestamp), unit ="week")) %>% 
  group_by(date) %>% 
  summarize(b_t = mean(rating - mu - b_i - b_u))


# predict movie effect + user effect + time effect
train_set_n <- train_set %>% mutate(date = round_date(as_datetime(timestamp), unit="week"))
test_set_n <- test_set %>% mutate(date = round_date(as_datetime(timestamp), unit ="week"))


#predict
predicted_ratings_bt <- test_set_n %>% 
  left_join(movie_avgs, by ="movieId") %>% 
  left_join(user_avgs, by ="userId") %>% 
  left_join(time_avgs, by ="date") %>% 
  mutate(pred = mu + b_i + b_u + b_t) %>% 
  .$pred
  

# calculate and store results
mu_bi_bu_bt_rmse <- RMSE(test_set_n$rating, predicted_ratings_bt)
mu_bi_bu_bt_rmse

rmse_results <- rmse_results %>% add_row(method ="mu + b_i + b_u + b_t", RMSE = mu_bi_bu_bt_rmse)
rmse_results



#### 3.4 genre effect ####
# 18 genres in total

genre_avgs <- train_set_n %>% 
  left_join(movie_avgs, by ="movieId") %>% 
  left_join(user_avgs, by ="userId") %>% 
  left_join(time_avgs, by ="date") %>% 
  group_by(genres) %>% 
  summarize(b_g = mean(rating - mu - b_i - b_u - b_t))


# predict
predicted_ratings <- test_set_n %>% 
  left_join(movie_avgs, by ="movieId") %>% 
  left_join(user_avgs, by ="userId") %>% 
  left_join(time_avgs, by ="date") %>% 
  left_join(genre_avgs, by ="genres") %>% 
  mutate(pred = mu + b_i + b_u + b_t + b_g) %>% 
  .$pred


# calculate and store results
mu_bi_bu_bt_bg_rmse <- RMSE(test_set_n$rating, predicted_ratings)
mu_bi_bu_bt_bg_rmse

rmse_results <- rmse_results %>% add_row(method ="mu + b_i + b_u + b_t + b_g", RMSE = mu_bi_bu_bt_bg_rmse)
rmse_results




#### 3.5 regularization ####

# to improve our results we will use regularization
# we constraint the total variability by penalizing large estimates from small sample sizes

# we will use regularization on the movie effects
test_set %>% 
  left_join(movie_avgs, by ="movieId") %>% 
  mutate(residual = rating - (mu + b_i)) %>% 
  arrange(desc(abs(residual))) %>% 
  select(title, residual) %>% slice(1:40) %>% knitr::kable()

movie_titles <- edx %>% 
  select(movieId, title) %>% 
  distinct()


# movie that received high b_i effects are not well known
movie_avgs %>% left_join(movie_titles, by ="movieId") %>% 
  arrange(desc(b_i)) %>% 
  select(title, b_i) %>% 
  slice(1:10) %>% 
  knitr::kable()


# very low rated movies are also not very well known -> we need to account for that
movie_avgs %>% left_join(movie_titles, by ="movieId") %>% 
  arrange(b_i) %>% 
  select(title, b_i) %>% 
  slice(1:10) %>% 
  knitr::kable()

# we see that those movies rated high have very few ratings
train_set %>% dplyr::count(movieId) %>% 
  left_join(movie_avgs) %>% 
  left_join(movie_titles, by = "movieId") %>% 
  arrange(desc(b_i)) %>% 
  select(title, b_i, n) %>% 
  slice(1:10) %>% 
  knitr::kable()



## regularization of b_i 

lambda <- 5
mu <- mean(train_set$rating)
movie_reg_avgs <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = sum(rating- mu)/(n() + lambda), n_i = n())

data_frame(original = movie_avgs$b_i,
           regularized = movie_reg_avgs$b_i,
           n = movie_reg_avgs$n_i) %>% 
  ggplot(aes(original, regularized, size = sqrt(n))) + 
  geom_point(shape = 1, alpha = 0.5)


# it looks better now
train_set %>% 
  dplyr::count(movieId) %>% 
  left_join(movie_reg_avgs) %>% 
  left_join(movie_titles, by = "movieId") %>% 
  arrange(desc(b_i)) %>% 
  select(title, b_i, n) %>% 
  slice(1:10) %>% 
  knitr::kable()

train_set %>% 
  dplyr::count(movieId) %>% 
  left_join(movie_reg_avgs) %>% 
  left_join(movie_titles, by = "movieId") %>% 
  arrange(b_i) %>% 
  select(title, b_i, n) %>% 
  slice(1:10) %>% 
  knitr::kable()


# Predict
predicted_ratings <- test_set %>% 
  left_join(movie_reg_avgs, by ="movieId") %>% 
  mutate(pred = mu + b_i) %>% 
  .$pred


# calculate and store results
mu_biReg <- RMSE(test_set_n$rating, predicted_ratings)
mu_biReg

rmse_results <- rmse_results %>% add_row(method ="mu + b_iReg", RMSE = mu_biReg)
rmse_results


# check which lambda is the best for regularized movie effect
lambdas <- seq(0, 10, 0.25)
mu <- mean(train_set$rating)

just_the_sum <- train_set %>% 
  group_by(movieId) %>% 
  summarize(s = sum(rating-mu), n_i = n())


rmses <- sapply(lambdas, function(l){
  predicted_ratings <- test_set %>% 
    left_join(just_the_sum, by ="movieId") %>% 
    mutate(b_i = s/(n_i+l)) %>% 
    mutate(pred = mu + b_i) %>% 
    .$pred
  return(RMSE(predicted_ratings, test_set$rating))
})

qplot(lambdas, rmses)
lambdas[which.min(rmses)]




## now regularize b_u
lambda <- 5
user_reg_avgs <- train_set %>% 
  left_join(movie_reg_avgs, by ="movieId") %>% 
  group_by(userId) %>% 
  summarize(b_u = sum(rating - mu - b_i)/(n() + lambda))


# predict with regularized b_i and b_u
predicted_ratings <- test_set %>% 
  left_join(movie_reg_avgs, by ="movieId") %>% 
  left_join(user_reg_avgs, by="userId") %>% 
  mutate(pred = mu + b_i + b_u) %>% 
  .$pred


# calculate and store results
mu_biReg_buReg <- RMSE(test_set_n$rating, predicted_ratings)
mu_biReg_buReg


# we increased the rmse of the test set
rmse_results <- rmse_results %>% add_row(method ="mu + b_iReg + b_uReg", RMSE = mu_biReg_buReg)
rmse_results




# one step seq as 0.25 requires too much computation power 
# -> best lambda is 5
lambdas <- seq(0,10,1)
rmses <- sapply(lambdas, function(l){
  mu <- mean(train_set$rating)
  b_i <- train_set %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  b_u <- train_set %>%
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  predicted_ratings <- test_set %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    .$pred
  return(RMSE(predicted_ratings, test_set$rating))
})

qplot(lambdas, rmses)


# now check model with time effect (lambda 5)
test_set_n <- test_set %>% mutate(date = round_date(as_datetime(timestamp), unit ="week"))


# without genres
predicted_ratings <- test_set_n %>% 
  left_join(movie_reg_avgs, by ="movieId") %>% 
  left_join(user_reg_avgs, by ="userId") %>% 
  left_join(time_avgs, by ="date") %>% 
  mutate(pred = mu + b_i + b_u + b_t + b_g) %>% 
  .$pred


# calculate and store results
mu_biReg_buReg_bt_rmse <- RMSE(test_set_n$rating, predicted_ratings)
mu_biReg_buReg_bt_rmse

rmse_results <- rmse_results %>% add_row(method ="mu + b_iReg + b_uReg + b_t", RMSE = mu_biReg_buReg_bt_rmse)
rmse_results




# with genres
predicted_ratings <- test_set_n %>% 
  left_join(movie_reg_avgs, by ="movieId") %>% 
  left_join(user_reg_avgs, by ="userId") %>% 
  left_join(time_avgs, by ="date") %>% 
  left_join(genre_avgs, by ="genres") %>% 
  mutate(pred = mu + b_i + b_u + b_t + b_g) %>% 
  .$pred


# calculate and store results
mu_biReg_buReg_bt_rmse <- RMSE(test_set_n$rating, predicted_ratings)
mu_biReg_buReg_bt_rmse

rmse_results <- rmse_results %>% add_row(method ="mu + b_iReg + b_uReg + b_t + b_g", RMSE = mu_biReg_buReg_bt_rmse)
rmse_results




#### 4. Validation ####

## check rmse with the validation set
# for doing that we need to train the whole edx dataset as the validation set is a subset
lambda <- 2.5
movie_reg_avgs <- edx %>%
  group_by(movieId) %>%
  summarize(b_i = sum(rating- mu)/(n() + lambda), n_i = n())


lambda <- 5
user_reg_avgs <- edx %>%
  left_join(movie_reg_avgs, by ="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - mu - b_i)/(n() + lambda))


# predict
predicted_ratings <- validation %>% 
  mutate(date = round_date(as_datetime(timestamp), unit ="week")) %>% 
  left_join(movie_reg_avgs, by ="movieId") %>% 
  left_join(user_reg_avgs, by ="userId") %>% 
  left_join(time_avgs, by ="date") %>% 
  left_join(genre_avgs, by ="genres") %>% 
  mutate(pred = mu + b_i + b_u + b_t + b_g) %>% 
  .$pred


# caluclate final RMSE
RMSE(predicted_ratings, validation$rating)

