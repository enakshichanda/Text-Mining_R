# Fetch and unzip the tar file 
movie_url = "http://www.cs.cornell.edu/people/pabo/movie-review-data/rt-polaritydata.tar.gz"
download.file(movie_url, "rt-polaritydata.tar.gz")
untar("rt-polaritydata.tar.gz")

# Loading to data frames
rt_negative <- data.frame(sentence = readLines("./rt-polaritydata/rt-polarity.neg"),stringsAsFactors = FALSE)
rt_negative['sentiment'] <- "negative"
rt_negative

rt_positive <- data.frame(sentence = readLines("./rt-polaritydata/rt-polarity.pos"),stringsAsFactors = FALSE)
rt_positive['sentiment'] <- "positive"
rt_positive

# Create corpus 
movie_corpus <- corpus(rbind(rt_negative, rt_positive), text_field='sentence')
summary(movie_corpus)
head(movie_corpus)
str(movie_corpus)
dfm(movie_corpus)
#topfeatures(movie_corpus)

positive_corpus = Corpus(VectorSource(rt_positive))
positive_corpus = tm_map(positive_corpus,removePunctuation)
positive_corpus = tm_map(positive_corpus,removeNumbers)
positive_corpus = tm_map(positive_corpus,removeWords,stopwords (kind = "en"))
head(positive_corpus)
positive_corpus = tm_map(positive_corpus,stemDocument)
dt_matrix <- TermDocumentMatrix(positive_corpus)
dt_matrix <- as.matrix(dt_matrix)
sort_data <- sort(rowSums(dt_matrix),decreasing=TRUE)
data_freq <- data.frame(word = names(sort_data),freq=sort_data)
ggplot(data_freq[1:15,], aes(x = reorder(word,-freq), y = freq)) +
  geom_bar(fill = "green", stat = "identity")
wordcloud(words = data_freq$word, freq = data_freq$freq,max.words=50,rot.per=0.70,
          random.order=FALSE,color = brewer.pal(9,"Set1"))

negative_corpus = Corpus(VectorSource(rt_negative))
negative_corpus = tm_map(negative_corpus,removePunctuation)
negative_corpus = tm_map(negative_corpus,removeNumbers)
negative_corpus = tm_map(negative_corpus,removeWords,stopwords (kind = "en"))
negative_corpus = tm_map(negative_corpus,stemDocument)
dt_matrix <- TermDocumentMatrix(negative_corpus)
dt_matrix <- as.matrix(dt_matrix)
sort_data <- sort(rowSums(dt_matrix),decreasing=TRUE)
data_freq <- data.frame(word = names(sort_data),freq=sort_data)
ggplot(data_freq[1:15,], aes(x = reorder(word,-freq), y = freq)) +
  geom_bar(fill = "pink", stat = "identity")
wordcloud(words = data_freq$word, freq = data_freq$freq,max.words=50,rot.per=0.70,
          random.order=FALSE,color = brewer.pal(9,"Set1"))

df_words <- rbind(rt_positive,rt_negative)
mycorpus = Corpus(VectorSource(df_words))
mycorpus = tm_map(mycorpus,removePunctuation)
mycorpus = tm_map(mycorpus,removeNumbers)
mycorpus = tm_map(mycorpus,removeWords,stopwords (kind = "en"))
mycorpus = tm_map(mycorpus,stemDocument)
dt_matrix <- TermDocumentMatrix(mycorpus)
dt_matrix <- as.matrix(dt_matrix)
sort_data <- sort(rowSums(dt_matrix),decreasing=TRUE)
data_freq <- data.frame(word = names(sort_data),freq=sort_data)

wordcloud(words = data_freq$word, freq = data_freq$freq,max.words=50,rot.per=0.70,
          random.order=FALSE,color = brewer.pal(9,"Set1"))

movie_corpus_token <- tokens(movie_corpus,remove_punct = TRUE,remove_numbers = TRUE, remove_symbols = TRUE,remove_separators = TRUE)

#remove the stopwords
movie_corpus_token <- tokens_select(movie_corpus_token,pattern = stopwords('en'),selection = 'remove')
movie_corpus_dfm <- dfm(movie_corpus_token)

movie_corpus_lexical<-textstat_lexdiv(movie_corpus_dfm)
random_lexical_sentence <- movie_corpus_lexical[sample(nrow(movie_corpus_lexical),20,replace = FALSE,prob = NULL),]
plot(random_lexical_sentence$TTR, type = 'l', xaxt = 'n', xlab = NULL, ylab = "TTR")+grid()+axis(1,at=seq_len(nrow(random_lexical_sentence)))

movie_corpus_token_dfm <- dfm(movie_corpus_token)
random_dend <- movie_corpus_token_dfm[sample(nrow(movie_corpus_token_dfm),20,replace = FALSE,prob = NULL),]
dendro_distance <- as.dist(textstat_dist(random_dend))
print(dendro_distance)
hc <- hclust(dendro_distance)
hcd <- as.dendrogram(hc)
nodePar <- list(lab.cex = 0.6, pch = c(NA, 19), 
                cex = 0.7, col = "blue")
plot(hcd, ylab = "Height", nodePar = nodePar, leaflab = "none")
plot(hcd,  type = "rectangle", ylab = "Height", nodePar = nodePar, 
     edgePar = list(col = 2:3, lwd = 2:1))


dd <- dist(scale(random_dend), method = "euclidean")
hc <- hclust(dd, method = "ward.D2")
plot(hcd, labels = NULL, hang = 0.1, 
     main = "Cluster dendrogram", sub = NULL,
     xlab = NULL, ylab = "Height")

textplot_wordcloud(dfm(movie_corpus, remove = stopwords("english"),remove_punct = TRUE, groups = "sentiment"),
                   comparison = TRUE,max_words = 50, color = c("magenta", "blue"))

docvars(movie_corpus, "Comparison") <- 
  factor(ifelse(docvars(movie_corpus, "sentiment") == "positive",
                "positive",
                "negative"))
dfm_sentiment <- dfm(movie_corpus, 
                     groups = "Comparison")
set.seed(200)
#textplot_wordcloud(dfm(dfm_sentiment, comparison = TRUE, max_words = 50))
textplot_wordcloud(dfm_sentiment, remove = stopwords("english"),remove_punct = TRUE, groups = "sentiment",
                   comparison = TRUE,max_words = 50, color = c("magenta", "blue"))

lengths(data_dictionary_LSD2015)
movie_token_sentiment <- tokens_lookup(movie_corpus_token, 
                                       dictionary =  data_dictionary_LSD2015[1:2])
movie_token_sentiment

# Create document-feature matrix from sentiments
dfmat_movie_token_sentiment <- dfm(movie_token_sentiment)
head(movie_token_sentiment)

dfset_movie_token_sentiment <- as.data.frame(dfmat_movie_token_sentiment)
dfset_movie_token_sentiment$Sentiment <- ifelse(dfset_movie_token_sentiment$negative > dfset_movie_token_sentiment$positive,
                                                "negative","positive")
dfset_movie_token_sentiment$Old_Sentiment <-  docvars(dfm_sentiment, "sentiment")


###############################################################################################
# Fetch and unzip the tar file 
movie_url = "http://www.cs.cornell.edu/people/pabo/movie-review-data/rt-polaritydata.tar.gz"
download.file(movie_url, "rt-polaritydata.tar.gz")
untar("rt-polaritydata.tar.gz")

# Loading to data frames
rt_negative <- data.frame(sentence = readLines("./rt-polaritydata/rt-polarity.neg"),stringsAsFactors = FALSE)
rt_negative['sentiment'] <- "negative"
rt_negative

rt_positive <- data.frame(sentence = readLines("./rt-polaritydata/rt-polarity.pos"),stringsAsFactors = FALSE)
rt_positive['sentiment'] <- "positive"
rt_positive

# Create corpus 
movie_corpus <- corpus(rbind(rt_negative, rt_positive), text_field='sentence')
summary(movie_corpus)
head(movie_corpus)
str(movie_corpus)
dfm(movie_corpus)

#convert into tokens
data_corpus_token <- tokens(movie_corpus,remove_punct = TRUE,remove_numbers = TRUE, 
                            split_hyphens= TRUE,remove_symbols = TRUE,
                            remove_separators = TRUE)
#remove the stopwords
data_corpus_token <- tokens_select(data_corpus_token,pattern = stopwords('en'),selection = 'remove')

#transforming it to dfm
data_corpus_dfm <- dfm(data_corpus_token,tolower = TRUE)
ggplot(textstat_frequency(data_corpus_dfm, n = 15),aes(x =  reorder(feature,frequency), y =frequency)) + 
  geom_point() +
  coord_flip() +
  labs(x = "words", y ="Frequency" ) +
  theme_gray()

features_matrix=as.matrix(topfeatures(data_corpus_dfm,n = 15,decreasing = TRUE,
                                      scheme = c("count", "docfreq"),groups = NULL))

textplot_wordcloud(dfm(data_corpus_token, remove = stopwords("english"),remove_punct = TRUE, groups = "sentiment"),
                   comparison = TRUE,max_words = 50, color = c("magenta", "blue"))

data_corpus_ld<-textstat_lexdiv(data_corpus_dfm)
random_lexdiv <- data_corpus_ld[sample(nrow(data_corpus_ld),20,replace = FALSE,prob = NULL),]
plot(random_lexdiv$TTR, type = 'l', xaxt = 'n', xlab = NULL, 
     ylab = "TTR")+grid()+axis(1,at=seq_len(nrow(random_lexdiv)))

random_dendrogram<- data_corpus_dfm[sample(nrow(datacorpus_dfm),20,replace = FALSE,prob = NULL),]
dendro_distance <- as.dist(textstat_dist(random_dendrogram))
print(dendro_distance)
hc <- hclust(dendro_distance)
hcd <- as.dendrogram(hc)
nodePar <- list(lab.cex = 0.6, pch = c(NA, 19), 
                cex = 0.7, col = "blue")
plot(hcd, ylab = "Height", nodePar = nodePar, leaflab = "none")
plot(hcd,  type = "rectangle", ylab = "Height", nodePar = nodePar, 
     edgePar = list(col = 2:3, lwd = 2:1))


