rm(list = ls())

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")

library(caret)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)

td = tempdir()
edx = tempfile(tmpdir=td, fileext=".rds")
validation = tempfile(tmpdir=td, fileext=".rds")
download.file("https://www.dropbox.com/s/nspymeso8rmmak1/edx.rds?dl=1", edx)
download.file("https://www.dropbox.com/s/x0s477b0kzxpl6i/validation.rds?dl=1", validation)
edx = readRDS(edx)
validation = readRDS(validation)
unlink(td)

# Trail set of 0.3% of edx for coding and unit testing

set.seed(1) #set.seed(1, sample.kind="Rounding")
trial_index <- createDataPartition(y = edx$rating, times = 1, p = 0.3, list = FALSE)
trialset <- edx[trial_index,]

# Make sure userId and movieId in Trial set are also in edx set
trialset <- trialset %>%
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

rm(list = c("edx", "trial_index"))

trialset<-trialset[,-5]
validation <- validation[,-5]

trialset <- trialset %>% mutate(day_of_week = wday(as_datetime(timestamp), label = TRUE)) 

validation <- validation %>% mutate(day_of_week = wday(as_datetime(timestamp), label = TRUE))

trialset <-trialset[,-4]
validation <- validation[,-4]

nrow(trialset)
trialset1 <- trialset[1:600000, ]
trialset2 <- trialset[600001:1200000, ]
trialset3 <- trialset[1200001:1800000, ]
trialset4 <- trialset[1800001:2400000, ]
trialset5 <- trialset[2400001:nrow(trialset), ]

write.table(trialset1, file = "newtrialset1.csv",
            sep = "\t", row.names = F, append = FALSE, col.names = T)

write.table(trialset2, file = "newtrialset2.csv",
            sep = "\t", row.names = F, append = FALSE, col.names = T)

write.table(trialset3, file = "newtrialset3.csv",
            sep = "\t", row.names = F, append = FALSE, col.names = T)

write.table(trialset4, file = "newtrialset4.csv",
            sep = "\t", row.names = F, append = FALSE, col.names = T)

write.table(trialset5, file = "newtrialset5.csv",
            sep = "\t", row.names = F, append = FALSE, col.names = T)


rm(list = c("trialset1", "trialset2", "trialset3", "trialset4", "trialset5"))

nrow(validation)

validation1 <- validation[1:300000, ]
validation2 <- validation[300001:600000, ]
validation3 <- validation[600001:nrow(validation), ]

write.table(validation1, file = "newvalidation1.csv",
            sep = "\t", row.names = F, append = FALSE, col.names = T)

write.table(validation2, file = "newvalidation2.csv",
            sep = "\t", row.names = F, append = FALSE, col.names = T)

write.table(validation3, file = "newvalidation3.csv",
            sep = "\t", row.names = F, append = FALSE, col.names = T)

rm(list = c("validation", "validation1", "validation2", "validation3"))


# Test data sets for cross validation so that Validation set is used only at the end.
set.seed(1) #set.seed(1, sample.kind="Rounding")
movie_index <- createDataPartition(y = trialset$rating, times = 1, p = 0.25, list = FALSE)
movieset <- trialset[movie_index,]

# Make sure userId and movieId in Trial set are also in edx set
movieset <- movieset %>%
  semi_join(trialset, by = "movieId") %>%
  semi_join(trialset, by = "userId")

rm(list = "movie_index")

set.seed(1) #set.seed(1, sample.kind="Rounding")
user_index <- createDataPartition(y = trialset$rating, times = 1, p = 0.25, list = FALSE)
userset <- trialset[user_index,]

# Make sure userId and movieId in Trial set are also in edx set
userset <- userset %>%
  semi_join(trialset, by = "movieId") %>%
  semi_join(trialset, by = "userId")

rm(list = "user_index")


set.seed(1) #set.seed(1, sample.kind="Rounding")
genres_index <- createDataPartition(y = trialset$rating, times = 1, p = 0.25, list = FALSE)
genresset <- trialset[genres_index,]

# Make sure userId, movieId, and genres in Trial set are also in edx set
genresset <- genresset %>%
  semi_join(trialset, by = "movieId") %>%
  semi_join(trialset, by = "userId") %>%
  semi_join(trialset, by = "genres")

rm(list = "genres_index")

set.seed(1) #set.seed(1, sample.kind="Rounding")
time_index <- createDataPartition(y = trialset$rating, times = 1, p = 0.25, list = FALSE)
timeset <- trialset[time_index,]

# Make sure userId, movieId, and genres in Trial set are also in edx set
timeset <- timeset %>%
  semi_join(trialset, by = "movieId") %>%
  semi_join(trialset, by = "userId") %>%
  semi_join(trialset, by = "day_of_week")

rm(list = "time_index")



write.table(movieset, file = "movieset.csv",
            sep = "\t", row.names = F, append = FALSE, col.names = T)

write.table(userset, file = "userset.csv",
            sep = "\t", row.names = F, append = FALSE, col.names = T)

write.table(genresset, file = "genresset.csv",
            sep = "\t", row.names = F, append = FALSE, col.names = T)

write.table(timeset, file = "timeset.csv",
            sep = "\t", row.names = F, append = FALSE, col.names = T)

rm(list = c("trialset", "movieset", "userset", "genresset", "timeset"))

