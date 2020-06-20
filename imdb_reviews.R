library("stringr")
library("utils")
library("rvest")
library("XML")
library("magrittr")
library("tm")
library("wordcloud")
library("syuzhet")

iurl <-  "https://www.imdb.com/title/tt7286456/reviews?ref_=tt_ov_rt"
imdb_reviews <- NULL
for (i in 1:10){
  murl <- read_html(as.character(paste(iurl,i,sep="")))
  rev <- murl %>%
    html_nodes(".text.show-more__control") %>%
    html_text()
  imdb_reviews <- c(imdb_reviews,rev)
}
write.table(imdb_reviews,"joker.txt",row.name=F)
getwd()

View(imdb_reviews)

#Removing similar reviews
reviews_df=unique(imdb_reviews)
View(reviews_df)

#Cleaning reivews
reviews_df=gsub("http\\w+","",reviews_df)
reviews_df=gsub("&amp","",reviews_df)
reviews_df=gsub("@\\w","",reviews_df)
reviews_df=gsub("[[:punct:]]","",reviews_df)
reviews_df=gsub("[[:digit:]]","",reviews_df)
reviews_df=gsub("[ \t]{2,}","",reviews_df)
reviews_df=gsub("@\\w+","",reviews_df)

#Creating corpus and cleaning corpus
corpus_reviews=Corpus(VectorSource(reviews_df))
corpus_reviews=tm_map(corpus_reviews,tolower)
corpus_reviews=tm_map(corpus_reviews,removePunctuation)
corpus_reviews=tm_map(corpus_reviews,removeWords,c(stopwords("english")))

#WORDCLOUD
wordcloud(corpus_reviews,colors = brewer.pal(8,"Dark2"),random.order = FALSE,rot.per = 0.3,scale = c(4,1),max.words = 150)

#Removing spaces and perform stemming 
corpus_reviews=tm_map(corpus_reviews,stripWhitespace)
corpus_reviews=tm_map(corpus_reviews,stemDocument)

#Creating Doucument term matrix
reviews_DTM=DocumentTermMatrix(corpus_reviews)

#Creating term document matrix 
reviews_tdm=TermDocumentMatrix(corpus_reviews)
tdm=as.matrix(reviews_tdm)

reviews_sparse=removeSparseTerms(reviews_DTM,0.995)
reviews_sparse=as.data.frame(as.matrix(reviews_sparse))
colnames(reviews_sparse)=make.names(colnames(reviews_sparse))
sentimatal_value=get_sentiment(reviews_df)
all_senti_values=ifelse(sentimatal_value<0,"Negative",ifelse(sentimatal_value>0,"Positive","Neutral"))
reviews_sparse$values=all_senti_values
table(reviews_sparse$values)

prop.table(table(reviews_sparse$values))

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
makeposwordc(reviews_DTM)

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
makenegwordc(reviews_DTM)
