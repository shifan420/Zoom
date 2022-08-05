install.packages("tm")
install.packages("stopwords")
install.packages("twitteR")
install.packages("tidytext")
install.packages("ggplot2")
install.packages("syuzhet")
install.packages("lubridate")
install.packages("dplyr")
install.packages("wordcloud2")
install.packages("Rcpp")
library(tm)
library(twitteR)
library(tidytext)
library(stopwords)
library(ggplot2)
library(syuzhet)
library(lubridate)
library(dplyr)
library(wordcloud2)
library("openssl")
library("httpuv")

##key and token for academic account
bearer_token<-"AAAAAAAAAAAAAAAAAAAAAFosCAEAAAAAEnlD5033GO8%2FUsHvO2Oyhip72Zg%3Dtl2Y9yR2MXCyhlurdLnbSp0Emkmq9FT3iCQNY21Moz2tFJ3jEv"
api_key="eOZzY0p0beA4prviIP9jWOzbL"
api_secret="KNhaPPujjVrYtaQAR8YdipF3aDYGo9Fm6gHAy2ESTwcHl3vmqj"
access_token="1360997359371616259-HaYIexPEqgsspdeELQ6ysCyI85IcUu"
access_token_secret="wWE8RMfQ57fKobawgV8J7y73sLonsOauFWWnYtXmTSCZy"

##launch library##
install.packages("academictwitteR")
library(academictwitteR)
##get tweets
tweets1 <-
  get_all_tweets(
    query = "zoom",
    start_tweets = "2020-11-08T00:00:00Z", ##start time
    end_tweets = "2020-11-09T00:00:00Z", ##end time
    bearer_token = "AAAAAAAAAAAAAAAAAAAAAFosCAEAAAAAEnlD5033GO8%2FUsHvO2Oyhip72Zg%3Dtl2Y9yR2MXCyhlurdLnbSp0Emkmq9FT3iCQNY21Moz2tFJ3jEv",
    n=Inf
  )
tweets2 <-
  get_all_tweets(
    query = "zoom",
    start_tweets = "2020-11-09T00:00:00Z", ##start time
    end_tweets = "2020-11-10T00:00:00Z", ##end time
    bearer_token = "AAAAAAAAAAAAAAAAAAAAAFosCAEAAAAAEnlD5033GO8%2FUsHvO2Oyhip72Zg%3Dtl2Y9yR2MXCyhlurdLnbSp0Emkmq9FT3iCQNY21Moz2tFJ3jEv",
    n=Inf
  )
tweets3 <-
  get_all_tweets(
    query = "zoom",
    start_tweets = "2020-11-10T00:00:00Z", ##start time
    end_tweets = "2020-11-11T00:00:00Z", ##end time
    bearer_token = "AAAAAAAAAAAAAAAAAAAAAFosCAEAAAAAEnlD5033GO8%2FUsHvO2Oyhip72Zg%3Dtl2Y9yR2MXCyhlurdLnbSp0Emkmq9FT3iCQNY21Moz2tFJ3jEv",
    n=Inf
  )
tweets0 <-
  get_all_tweets(
    query = "zoom",
    start_tweets = "2020-11-07T00:00:00Z", ##start time
    end_tweets = "2020-11-08T00:00:00Z", ##end time
    bearer_token = "AAAAAAAAAAAAAAAAAAAAAFosCAEAAAAAEnlD5033GO8%2FUsHvO2Oyhip72Zg%3Dtl2Y9yR2MXCyhlurdLnbSp0Emkmq9FT3iCQNY21Moz2tFJ3jEv",
    n=Inf
  )

tweets1 = as.data.frame(tweets1)
tweets1 = tweets1[tweets1$lang=='en',]
tweets2 = as.data.frame(tweets2)
tweets2 = tweets2[tweets2$lang=='en',]
tweets3 = as.data.frame(tweets3)
tweets3 = tweets3[tweets1$lang=='en',]
tweets0 = as.data.frame(tweets0)
tweets0 = tweets1[tweets0$lang=='en',]


tweets1 = as.data.frame(cbind(tweets1$text, tweets1$created_at, tweets1$id, tweets1$possibly_sensitive))
colnames(tweets1)=c("text","created_at","id","possibly_sensitive")
tweets0 = as.data.frame(cbind(tweets0$text, tweets0$created_at, tweets0$id, tweets0$possibly_sensitive))
colnames(tweets0)=c("text","created_at","id","possibly_sensitive")
tweets2 = as.data.frame(cbind(tweets2$text, tweets2$created_at, tweets2$id, tweets2$possibly_sensitive))
colnames(tweets2)=c("text","created_at","id","possibly_sensitive")
tweets3 = as.data.frame(cbind(tweets3$text, tweets3$created_at, tweets3$id, tweets3$possibly_sensitive))
colnames(tweets3)=c("text","created_at","id","possibly_sensitive")

write.csv(tweets1,"zoom8.csv")
write.csv(tweets2,"zoom9.csv")
write.csv(tweets3,"zoom10.csv")
write.csv(tweets0,"zoom7.csv")


data_clean <- function(tweets) {
  tweets$text <- gsub("@\\w+", "", tweets$text)
  tweets$text <- gsub("#\\w+", "", tweets$text)
  tweets$text  <-  gsub("amp", "", tweets$text) ##ampersand encode broken
  tweets$text <- gsub("https?://.+", "", tweets$text)
  tweets$text <- gsub("\\d+\\w*\\d*", "", tweets$text)
  tweets$text <- gsub("[^\x01-\x7F]", "", tweets$text)
  return(tweets)
}
tweets1 = data_clean(tweets1)
tweets0 = data_clean(tweets0)
tweets2 = data_clean(tweets2)
tweets3 = data_clean(tweets3)

##transfer invalid utf-8 strings
Encoding(tweets0$text) <- "UTF-8"
tweets0$text=iconv(tweets0$text, "UTF-8", "UTF-8",sub='')
##convert to corpus
corpus=Corpus(VectorSource(tweets0$text))
##the following steps are data cleaning based on tm package##
##change to lower case
corpus=tm_map(corpus,tolower)
##remove numbers
corpus = tm_map(corpus, removeNumbers)
##remove Punctuation
corpus = tm_map(corpus, removePunctuation)
##remove stopwords such as your, her, the
##use code stopwords("english") to check the list
corpus = tm_map(corpus, removeWords, stopwords("english"))
##remove stopwords in twitter
corpus = tm_map(corpus, removeWords, c("im","dont","doesnt","didnt","hasnt","havent","hadnt","hes","shes","its","lets"))
##remove URL link
removeUrl=function(x) gsub("http[[:alnum:]]*","",x)
corpus = tm_map(corpus, removeUrl)
##remove additional space
corpus = tm_map(corpus, stripWhitespace)

##remove additional words defined by each case
##!!! this is the place you need adjust according to your case
corpus = tm_map(corpus, removeWords, c("zoom", "Zoom", "zooms","Zooms","join","meeting","call","can","week"))


##count the frequency of words##
## convert to term matrix

dtm = DocumentTermMatrix(corpus)
dtm = removeSparseTerms(dtm, 0.999)
dataset = as.data.frame(as.matrix(dtm))
##sort words by frequency
v = sort(colSums(dataset),decreasing=TRUE)
## data with words and frequency
d = data.frame(word=names(v),freq=v)

##word in the top 20 in terms of frequency
wordfreq=d[1:40,]

##bar plot
ggplot(data=wordfreq, aes(x=reorder(word, freq), y=freq)) +
  geom_bar(stat = 'identity')+coord_flip()+xlab("word")+ylab("count")
##wordcloud
##word with frequency more than 10
wordfreq=d[d$freq>=2000,]
wordcloud2(data=wordfreq,size=0.5,color = "random-light", shape="circle", backgroundColor = "grey")
##wordcloud option: size--word size; color--word color; shape: shape of wordcloud; 
##backgroundColor: color of background

##sentimental analysis
##use nrc lexicon
st=get_nrc_sentiment(tweets0$text)
barplot(colSums(st)/sum(colSums(st)),
        las=2, 
        col=rainbow(10), ##color of each bar
        ylab="share",  ##label for y axis 
        main="share out of all sentiment words", ##main title
        sub="Case of Zoom; Date: 1109", ##sub-title !!this is the place you need to adjust
        cex.sub=0.8, ##font size of sub-title
        cex.names=0.9) ##font size of "sentiment"

##histogram of feeling
feeling=get_sentiment(tweets0$text, method="syuzhet")
head(feeling)
##change feeling to a dataframe with one variable score
feeling=data.frame(score=feeling)
ggplot(feeling, aes(x=score))+ 
  geom_histogram(binwidth=1, color="black", fill="white")+
  ggtitle("Histogram of Feeling in the Post Mentioning Zoom \n Date: Nov 09 ") 
##average feeling
mean(feeling$score)

