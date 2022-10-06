# Libraries imported
library(ggplot2)
library(dplyr)
library(tidytext)
library(igraph)
library(ggraph)
library(widyr)
library(tidyr)
library(wordcloud)
library(SnowballC)
library(tidyverse)
library(tm)
library(topicmodels)
library(RTextTools)
library(syuzhet)

#Load the dataset containing 14940 twitter messages about demonetisation in india
data <- read.csv("demonetization-tweets.csv", sep = ",", stringsAsFactors = FALSE)
head(data$text)

# Data cleaning
clean_tweets <- function(x) {
  x %>%
    str_remove_all("@[[:alnum:]]+") %>%
    str_remove_all("\\<U[^\\>]*\\>") %>%
    str_remove_all(" ?(f|ht)(tp)(s?)(://)(.*)[.|/](.*)") %>%
    str_remove_all("(^|[^&\\p{L}\\p{M}\\p{Nd}_\u200c\u200d\ua67e\u05be\u05f3\u05f4\u309b\u309c\u30a0\u30fb\u3003\u0f0b\u0f0c\u00b7])(#|\uFF03)(?!\uFE0F|\u20E3)([\\p{L}\\p{M}\\p{Nd}_\u200c\u200d\ua67e\u05be\u05f3\u05f4\u309b\u309c\u30a0\u30fb\u3003\u0f0b\u0f0c\u00b7]*[\\p{L}\\p{M}][\\p{L}\\p{M}\\p{Nd}_\u200c\u200d\ua67e\u05be\u05f3\u05f4\u309b\u309c\u30a0\u30fb\u3003\u0f0b\u0f0c\u00b7]*)") %>%
    str_replace_all("&amp;", "and") %>%
    str_remove_all("[[:punct:]]") %>%
    str_remove_all("^RT:? ") %>%
    str_remove_all("#[[:alnum:]]+") %>%
    str_remove_all("[[:digit:]]+") %>%
    str_replace_all("\\\n", " ") %>%
    str_to_lower() %>%
    str_trim("both")
}

data$ctext <- clean_tweets(data$text)
head(data$ctext)

data_clean <- data %>%
  dplyr::select(ctext) %>%
  unnest_tokens(word, ctext) #Tokenization

# Stopwords data
data("stop_words")
head(stop_words)

# Further data cleaning
cleaned_tweets <- data_clean %>%
  anti_join(stop_words) %>%
  filter(!word %in% tolower(data$screenName)) %>%
  filter(!word == "ed") %>%
  filter(!word == "dear") %>%
  filter(!word == "httpst") %>%
  filter(!word == "https") %>%
  filter(!word == "dont") %>%
  filter(!word == "put") %>%
  filter(!word == "urautelaforever") %>%
  filter(!word == "rs")

head(cleaned_tweets)

# Plot for word count
cleaned_tweets %>%
  dplyr::count(word, sort = TRUE) %>%
  top_n(20) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Count of unique words found in tweets")

# Observing paired words
data_paired <- cleaned_tweets %>%
  dplyr::select(word) %>%
  unnest_tokens(paired_words, word, token = "ngrams", n = 2)

data_paired %>%
  dplyr::count(paired_words, sort = TRUE)

data_separated <- data_paired %>%
  separate(paired_words, c("word1", "word2"), sep = " ")

data_filtered <- data_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# Paired words count
data_words_count <- data_filtered %>%
  dplyr::count(word1, word2, sort = TRUE)

head(data_words_count)

data_words_count %>%
  filter(n >= 110) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_node_point(color = "darkslategray4", size = 3) +
  geom_node_text(aes(label = name), vjust = 1.8, size = 3) +
  labs(title = "Word Network: Tweets on Demonetisation",
       x = "", y = "")

# Topic Modelling (Classifying tweets into specific topics)
cl <- VCorpus(VectorSource(cleaned_tweets))
td <- TermDocumentMatrix(cl, control = list(wordLengths = c(1, Inf)))
dt <- as.DocumentTermMatrix(td)
lda <- LDA(dt, k = 8)
term <- terms(lda, 5)
(term <- apply(term, MARGIN = 2, paste, collapse = ", "))


#Sentiment Analysis
tweets <- as.character(data$ctext)

sent.score <- function(sentences, pos.words, neg.words, .progress='none')
{
  require(plyr)
  require(stringr)
  
  scores <- laply(sentences, function(sentence, pos.words, neg.words)
  {
    
    sentence <- gsub('[[:cntrl:]]', '', sentence)
    sentence <- gsub('(RT|via)((?:\\b\\W*@\\W+)+)', '', sentence)
    sentence <- gsub('http.*','',  sentence)
    sentence <- gsub('https.*','',  sentence)
    sentence <- gsub('@\\w+', '', sentence)
    sentence <- gsub('[[:punct:]]', '', sentence)
    sentence <- gsub('[[:digit:]]', '', sentence)
    sentence <- gsub('http[s]?\\w+', '', sentence)
    sentence <- gsub('[ \t]{2,}', '', sentence)
    sentence <- gsub('^\\s+|\\s+$', '', sentence)
    sentence <- sentence[!is.na(sentence)]
    sentence <- tolower(sentence)
    word.list <- str_split(sentence, '\\s+')
    words <- unlist(word.list)
    
    neg.matches <- match(words, neg.words)
    pos.matches <- match(words, pos.words)
    
    pos.matches <- !is.na(pos.matches)
    neg.matches <- !is.na(neg.matches)
    
    score <- sum(pos.matches) - sum(neg.matches)
    
    return(score)
  }, pos.words, neg.words, .progress=.progress )
  
  scr.df <- data.frame(score=scores, text=sentences)
  return(scr.df)
}

pos <- scan("positive-words.txt", what= "character", comment.char= ";")
neg <- scan("negative-words.txt", what= "character", comment.char= ";")

tweets.analysis <- sent.score(tweets, pos, neg, .progress="none")

# Grouping tweets based on their sentiment score
tweets.analysis$sentiment[tweets.analysis$score == 0] <- "Neutral" 
tweets.analysis$sentiment[tweets.analysis$score < 0] <- "Negative"
tweets.analysis$sentiment[tweets.analysis$score > 0] <- "Positive"

tweets.analysis$sentiment <- factor(tweets.analysis$sentiment)
table(tweets.analysis$score)
mean(tweets.analysis$score)
median(tweets.analysis$score)
summary(tweets.analysis$sentiment)

# Plot based on sentiments towards demonetisation
ggplot(data = tweets.analysis, aes(x = score, fill = sentiment)) + 
  geom_bar() + 
  labs(title = "Sentiment Score Bar Plot", x = "Sentiment Score", y = "Tweet Count") +
  scale_x_continuous(breaks = seq(-6,6,1)) + 
  scale_y_continuous(breaks = seq(0,9000,3000)) + 
  scale_fill_manual(guide = guide_legend("Sentiment"), values = c("#DD0426","#246EB9","#04B430"))

# For word cloud
wc <- cleaned_tweets
wc <- VCorpus(VectorSource(wc))
wc <- tm_map(wc, PlainTextDocument)
wc <- tm_map(wc, removeWords, c("demonetization","demonetisation", stopwords("english")))
wc <- tm_map(wc, stemDocument) #Stemming

# Generates visuals of the word cloud
wordcloud(wc, min.freq = 100,
          max.words=500, rot.per=0.25, 
          colors=brewer.pal(8, "Dark2"), scale = c(2,0.5))

# NRC sentiment analysis (Emotion classification)
wrd.df <- as.vector(data$ctext)
emotion.df <- get_nrc_sentiment(wrd.df)
emotion.df2 <- cbind(data$ctext, emotion.df) 
head(emotion.df2)

# Hypothesis testing (Using Two sample Z-test)

# Majority of positive support to demonitisation was from those who anticipated it.

#Sample data
emotion_sample <- sample_n(emotion.df, 2000)

#Assuming the distribution of the populations to be normal
# Null hypothesis H0: No significant difference between means of anticipation and positive (mu1 = mu2)

# Test statistic Z,
z_two = function(mu1, mu2, sigma1, sigma2, n1, n2){
  zt = (mu1-mu2)/sqrt(sigma1^2/n1+sigma2^2/n2)
  return(zt)
}

#sample means
mu1 <- mean(emotion_sample$anticipation)
mu2 <- mean(emotion_sample$positive)

#sample sizes
n1 <- length(emotion_sample$anticipation)
n2 <- length(emotion_sample$positive)

#sample variances
sigma1_m <- var(emotion_sample$anticipation)
sigma2_m <- var(emotion_sample$positive)

# Calculating value of Z
z2_calu <- z_two(mu1, mu2, sigma1_m, sigma2_m, n1, n2)
print(z2_calu)
z2_calum <- abs(z2_calu)

# Critical value of z for 5% LOS
z_cri_5 = 1.96
print(z_cri_5)

# Decision on H0
if (z2_calum > z_cri_5){
  print ("Reject H0")
  print("Statistically validated")
} else {
  print ("Accept H0")
  print("Statistically validated")
}
