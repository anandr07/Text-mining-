library("twitteR")
library("ROAuth")
library("SnowballC")
library("wordcloud")
library("tm")
library("syuzhet")
consumerKey='FXTquJNbgDG2dH81XYVqNZFAb'
consumerSecret='3y0ALNFzJ8JKyxzFd0ba9FWSUpNSWhPisEIZOB6WCTtcGvP6SO'
requestURL='https://api.twitter.com/oauth/request_token'
accessURL='https://api.twitter.com/oauth/access_token'
authURL='https://api.twitter.com/oauth/authorize'
accesstoken <- "529590041-qOXLd769cQEUTbXg3iRqCd33pC1K6xoORrGOMJDh"
accesstokensecret <- "WlqZJwXFQzf64IuojkbKh1jdT5cnSY8U44pqmz6Sc1d4A"
setup_twitter_oauth(consumerKey,consumerSecret,accesstoken,accesstokensecret)
tweets <- searchTwitter("realme",n=500,lang = "en")
tweets
tweetsdf <- twListToDF(tweets)
tweetsdf=unique(tweetsdf)
write.csv(tweetsdf,file = "tweets.txt",row.names = F)
getwd()

#Removing unwanted columns
tweetsdf=tweetsdf[,-c(2:16)]
#Removing similar tweets
tweetsdf=unique(tweetsdf)
View(tweetsdf)

#Cleaning tweets
tweetsdf=gsub("http\\w+","",tweetsdf)
tweetsdf=gsub("&amp","",tweetsdf)
tweetsdf=gsub("@\\w","",tweetsdf)
tweetsdf=gsub("[[:punct:]]","",tweetsdf)
tweetsdf=gsub("[[:digit:]]","",tweetsdf)
tweetsdf=gsub("[ \t]{2,}","",tweetsdf)
tweetsdf=gsub("@\\w+","",tweetsdf)

#Creating corpus and cleaning corpus
corpus_tweets=Corpus(VectorSource(tweetsdf))
corpus_tweets=tm_map(corpus_tweets,tolower)
corpus_tweets=tm_map(corpus_tweets,removePunctuation)
corpus_tweets=tm_map(corpus_tweets,removeWords,c(stopwords("english")))

#WORDCLOUD
wordcloud(corpus_tweets,colors = brewer.pal(8,"Dark2"),random.order = FALSE,rot.per = 0.3,scale = c(4,1),max.words = 150)

#Removing spaces and perform stemming 
corpus_tweets=tm_map(corpus_tweets,stripWhitespace)
corpus_tweets=tm_map(corpus_tweets,stemDocument)

#Creating Doucument term matrix
tweets_DTM=DocumentTermMatrix(corpus_tweets)

#Creating term document matrix and barplot visual
tweets_tdm=TermDocumentMatrix(corpus_tweets)
tdm=as.matrix(tweets_tdm)
w=rowSums(tdm)
w=subset(w,w>=25)
barplot(w,las=2,col=rainbow(50))

tweets_sparse=removeSparseTerms(tweets_DTM,0.995)
tweets_sparse=as.data.frame(as.matrix(tweets_sparse))
colnames(tweets_sparse)=make.names(colnames(tweets_sparse))
sentimatal_value=get_sentiment(tweetsdf)
all_senti_values=ifelse(sentimatal_value<0,"Negative",ifelse(sentimatal_value>0,"Positive","Neutral"))
tweets_sparse$values=all_senti_values
table(tweets_sparse$values)

prop.table(table(tweets_sparse$values))

#Positive feedback given is around 48% and negative feedback is around 17% 

#Generating positive word cloud

pos.words=scan(file.choose(), what="character", comment.char=";")
pos.words=c(pos.words)

#Function for positive word cloud

  makeposwordc = function(a){	
  
  pos.matches = match(colnames(a), pos.words) 		
  
  pos.matches = !is.na(pos.matches)
  
  b1 = apply(a, 2, sum)[pos.matches];	 b1 = as.data.frame(b1);
  
  colnames(b1) = c("freq");
  
  wordcloud(rownames(b1), b1[,1], scale=c(5, 1), colors=rainbow(7),max.words = 15) 
  
  }
#POSITIVE WORD CLOUD
makeposwordc(tweets_DTM)

#Generating negative word cloud

neg.words=scan(file.choose(),what = "character",comment.char=";")
neg.words=c(neg.words)

#Function for negative word cloud

makenegwordc = function(a){	
  
  neg.matches = match(colnames(a), neg.words) 		
  
  neg.matches = !is.na(neg.matches)
  
  b1 = apply(a, 2, sum)[neg.matches];	 b1 = as.data.frame(b1);
  
  colnames(b1) = c("freq");
  
  wordcloud(rownames(b1), b1[,1], scale=c(5, 1), colors=brewer.pal(8,"Dark2"),max.words = 15)  	
  
}	

#NEGATIVE WORD CLOUD
makenegwordc(tweets_DTM)

