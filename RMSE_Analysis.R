# Clear environment
rm(list = ls())
# Clear console
cat("\014")
# Clear plots
if(!is.null(dev.list())) dev.off()

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(magrittr)) install.packages("magrittr", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")


library(caret)
library(magrittr)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(lubridate)


newtrialset1 <- read.csv(file = "newtrialset1.csv", head = TRUE, sep="\t")
newtrialset2 <- read.csv(file = "newtrialset2.csv", head = TRUE, sep="\t")
newtrialset3 <- read.csv(file = "newtrialset3.csv", head = TRUE, sep="\t")
newtrialset4 <- read.csv(file = "newtrialset4.csv", head = TRUE, sep="\t")
newtrialset5 <- read.csv(file = "newtrialset5.csv", head = TRUE, sep="\t")



newvalidation1 <- read.csv(file = "newvalidation1.csv", head = TRUE, sep="\t")
newvalidation2 <- read.csv(file = "newvalidation2.csv", head = TRUE, sep="\t")
newvalidation3 <- read.csv(file = "newvalidation3.csv", head = TRUE, sep="\t")

trialset <- rbind(newtrialset1, newtrialset2, newtrialset3, newtrialset4, newtrialset5)
rm(list = c("newtrialset1", "newtrialset2", "newtrialset3", "newtrialset4", "newtrialset5"))

validation <- rbind(newvalidation1, newvalidation2, newvalidation3)
rm(list = c("newvalidation1", "newvalidation2", "newvalidation3"))


high_boxplot_genres_rating <- trialset %>% filter(genres %in% c("Drama", "Comedy", "Comedy|Romance", "Comedy|Drama", "Comedy|Drama|Romance", "Drama|Romance")) %>% select(genres, rating)
str(high_boxplot_genres_rating)
head(high_boxplot_genres_rating)
mean(high_boxplot_genres_rating$rating)
high_boxplot_genres_rating$genres <- factor(high_boxplot_genres_rating$genres, levels = c("Drama", "Comedy", "Comedy|Romance", "Comedy|Drama", "Comedy|Drama|Romance", "Drama|Romance"), labels = c("Dr", "Co", "CoRo", "CoDr", "CoDrRo", "DrRo"))

boxplot(rating ~ genres, data = high_boxplot_genres_rating, xlab = "Genre", ylab = "Rating", main = "Power Genres")

# Clear plots
if(!is.null(dev.list())) dev.off()

low_boxplot_genres_rating <- trialset %>% filter(genres %in% c("Action|Drama|Horror|Sci-Fi", "Action|Romance|Western", "Adventure|Comedy|Drama|Fantasy|Mystery|Sci-Fi", "Adventure|Crime|Horror|Thriller", "Adventure|Fantasy|Film-Noir|Mystery|Sci-Fi", "Adventure|Horror|Romance|Sci-Fi")) %>% select(genres, rating)
str(low_boxplot_genres_rating)
head(low_boxplot_genres_rating)
mean(low_boxplot_genres_rating$rating)
low_boxplot_genres_rating$genres <- factor(low_boxplot_genres_rating$genres, levels = c("Action|Drama|Horror|Sci-Fi", "Action|Romance|Western", "Adventure|Comedy|Drama|Fantasy|Mystery|Sci-Fi", "Adventure|Crime|Horror|Thriller", "Adventure|Fantasy|Film-Noir|Mystery|Sci-Fi"), labels = c("ADHSF", "ARW", "ACDFMSF", "ACHT", "AFMNMSF"))

boxplot(rating ~ genres, data = low_boxplot_genres_rating, xlab = "Genre", ylab = "Rating", main = "Power Genres")
# Clear plots
if(!is.null(dev.list())) dev.off()

RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}


lambdas <- seq(mean(low_boxplot_genres_rating$rating), mean(high_boxplot_genres_rating$rating)+0.5, 0.01)

rm(list = c("high_boxplot_genres_rating", "low_boxplot_genres_rating"))

naive_rmses <- sapply(lambdas, function(l){
  return(RMSE(validation$rating,l))
})

qplot(lambdas, naive_rmses)
# Clear plots
if(!is.null(dev.list())) dev.off()

mu <- lambdas[which.min(naive_rmses)]
mu

rmse_results <- tibble(method = "naive", RMSE = min(naive_rmses))

rm(list = c("lambdas", "naive_rmses"))

movie_avgs <- trialset %>%
  group_by(movieId) %>%
  summarize(b_i = mean(rating - mu))

movieset <- read.csv(file = "movieset.csv", head = TRUE, sep="\t")

predicted_ratings <- mu + movieset %>% left_join(movie_avgs, by='movieId') %>% pull(b_i)

predicted_ratings <- predicted_ratings %>% replace_na(mu)

movie_rmse <- RMSE(predicted_ratings, movieset$rating)
movie_rmse
rmse_results <- rmse_results %>% add_row(method = "movie", RMSE = movie_rmse)

rm(list = c("predicted_ratings", "movieset"))

user_avgs <- trialset %>%
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))

userset <- read.csv(file = "userset.csv", head = TRUE, sep="\t")

predicted_ratings <- userset %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)

predicted_ratings <- predicted_ratings %>% replace_na(mu)

user_rmse <- RMSE(predicted_ratings, userset$rating)
user_rmse
rmse_results <- rmse_results %>% add_row(method = "user", RMSE = user_rmse)

rm(list = c("predicted_ratings", "userset"))

genres_avgs <- trialset %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  group_by(genres) %>%
  summarize(b_g = mean(rating - mu - b_i - b_u))

genresset <- read.csv(file = "genresset.csv", head = TRUE, sep="\t")

predicted_ratings <- genresset %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>% 
  left_join(genres_avgs, by='genres') %>%
  mutate(pred = mu + b_i + b_u + b_g) %>% pull(pred)

predicted_ratings <- predicted_ratings %>% replace_na(mu)

genre_rmse <- RMSE(predicted_ratings, genresset$rating)
genre_rmse
rmse_results <- rmse_results %>% add_row(method = "genre", RMSE = genre_rmse)

rm(list = c("predicted_ratings", "genresset"))

trialset %>% group_by(day_of_week) %>% summarize(total = n(), ave = mean(rating)) %>% ggplot(aes(x=day_of_week, y=ave, fill=day_of_week)) + geom_bar(stat="identity")
# Clear plots
if(!is.null(dev.list())) dev.off()

time_avgs <- trialset %>% 
  left_join(movie_avgs, by='movieId') %>% 
  left_join(user_avgs, by='userId') %>% 
  left_join(genres_avgs, by='genres') %>%
  group_by(day_of_week) %>%
  summarize(b_d = mean(rating - mu - b_i - b_u - b_g))

timeset <- read.csv(file = "timeset.csv", head = TRUE, sep="\t")

predicted_ratings <- timeset %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(genres_avgs, by='genres') %>%
  left_join(time_avgs, by='day_of_week') %>%
  mutate(pred = mu + b_i + b_u + b_g + b_d) %>%pull(pred)

predicted_ratings <- predicted_ratings %>% replace_na(mu)

time_rmse <- RMSE(predicted_ratings, timeset$rating)
time_rmse
rmse_results <- rmse_results %>% add_row(method = "time", RMSE = time_rmse)
rm(list = c("predicted_ratings", "timeset"))

lambdas <- seq(3.5, 5.5, 0.25)
rmses <- sapply(lambdas, function(l){
b_i <- trialset %>% group_by(movieId) %>% summarize(b_i = sum(rating - mu)/(n()+l))

b_u <- trialset %>% left_join(b_i, by="movieId") %>% group_by(userId) %>% 
  summarize(b_u = sum(rating - b_i - mu)/(n()+l))

b_g <- trialset %>% 
  left_join(movie_avgs, by='movieId') %>% left_join(user_avgs, by='userId') %>%
  group_by(genres) %>%
  summarize(b_g = sum(rating - mu - b_i - b_u)/(n()+l))

b_d <- trialset %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(genres_avgs, by='genres') %>%
  group_by(day_of_week) %>%
  summarize(b_d = sum(rating - mu - b_i - b_u - b_g)/(n()+l))

rm(list = c("trialset", "movie_avgs", "user_avgs", "genres_avgs"))

predicted_ratings <- validation %>%
  left_join(b_i, by='movieId') %>%
  left_join(b_u, by='userId') %>%
  left_join(b_g, by='genres') %>%
  left_join(b_d, by='day_of_week') %>%
  mutate(pred = mu + b_i + b_u + b_g + b_d) %>% pull(pred)

predicted_ratings <- predicted_ratings %>% replace_na(mu)

RMSE(predicted_ratings, validation$rating)  
 return(RMSE(predicted_ratings, validation$rating))  
})
rmse_results
rmse_results$method <- factor(rmse_results$method, levels = rmse_results$method[order(rmse_results$RMSE)])
rmse_results %>% ggplot(aes(x=method, y=RMSE)) + geom_point(colour = 'red', size = 2)