### Sentiment Analysis
library(syuzhet)

##Read File
text_df1<-read.csv("Desktop/R and Projects/Amazon review.csv")


##Obtain sentiment score
get_nrc_sentiment('happy')
get_nrc_sentiment('abuse')

##Store it in a new variable
s1<-get_nrc_sentiment('text_df1$Content')


#combine text and sentiment columns

review_sentiment<-cbind(text_df1$Content,s1)

#Graphical Representation of Review Sentiments

barplot(colSums(s1),col = rainbow(10), ylab = 'count', main = 'Amazon Review of airpods 2nd Generation')

