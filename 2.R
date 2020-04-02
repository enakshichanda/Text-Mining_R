movie_corpus_dfm
lengths(data_dictionary_LSD2015)
tokens_lookup(tokens(movie_corpus_token), dictionary = data_dictionary_LSD2015[1:2],
              exclusive = FALSE, nested_scope = "dictionary")
dfm_dictionary<- dfm(movie_corpus_token, dictionary = data_dictionary_LSD2015)
dfm_dictionary

data_frame=as.data.frame(dfm(movie_corpus_token, dictionary = data_dictionary_LSD2015[1:2]))
data_frame$sentiment=ifelse(data_frame$negative >= data_frame$positive,"negative","positive")
tab_class <- table(data_frame$sentiment,movie_corpus_dfm@docvars$sentiment)
confusionMatrix(tab_class,mode= "everything")

