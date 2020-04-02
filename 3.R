devtools::install_github("quanteda/quanteda.corpora")
install.packages("quanteda.corpora")
install.packages("quanteda.textmodels")
install.packages("caret")
library(quanteda.corpora)
library(caret)
library(quanteda)
library(caret)
require(quanteda.textmodels)
corp_movies <- movie_corpus
summary(corp_movies, 5)
set.seed(300)
id_train <- sample(1:10662, size = round(0.7*10662) , replace = FALSE)
head(id_train, 10)
corp_movies$id_numeric <- 1:ndoc(corp_movies)
dfmat_training <- corpus_subset(corp_movies, id_numeric %in% id_train) %>%
  dfm(stem = TRUE)
dfmat_test <- corpus_subset(corp_movies, !id_numeric %in% id_train) %>%
  dfm(stem = TRUE)
tmod_nb <- textmodel_nb(dfmat_training, dfmat_training$sentiment)
summary(tmod_nb)
dfmat_matched <- dfm_match(dfmat_test, features = featnames(dfmat_training))
actual_class <- dfmat_matched$sentiment
predicted_class <- predict(tmod_nb, newdata = dfmat_matched)
tab_class <- table(actual_class, predicted_class)
tab_class
install.packages('e1071', dependencies=TRUE)
confusionMatrix(tab_class, mode = "everything")
