library(twitteR)
library(ROAuth)
#help("ROAuth")
cred <- OAuthFactory$new(consumerKey='5QfAsxc9E5PjgRr0qZMQ33cqi',
                                consumerSecret='iG2P0TdgAXK4r3A9YEc6EL8rZMul1EVrsQMVQPimgm35MlW6xs',
                                requestURL='https://api.twitter.com/oauth/request_token',
                                accessURL='https://api.twitter.com/oauth/access_token',
                                authURL='https://api.twitter.com/oauth/authorize')
                               

save(cred,file = "twitter_authentication.Rdata")
setwd("E:\\all R working directery")
getwd()

############################################
#install.packages("base64enc")
library(base64enc)

#install.packages("httpuv")
library(httpuv)

#install.packages("rtweet")
library(rtweet)


load("twitter_authentication.Rdata")

twitteR::setup_twitter_oauth('5QfAsxc9E5PjgRr0qZMQ33cqi',
                    'iG2P0TdgAXK4r3A9YEc6EL8rZMul1EVrsQMVQPimgm35MlW6xs',
                    '1156831372863913985-031tQ7sjgJExzjBbsCZ7hf6tXoF5A0',
                    'wmhR43JPraXzbFEhTEUQbirTkOa5SW409eRN0hlG4Nqpi')

tweets=twitteR::userTimeline("PawanKalyan",n = 1000)

tweets[1]

length(tweets)
str(tweets)

twetsdf=twitteR::twListToDF(tweets)

head(twetsdf)

write.csv(twetsdf ,"pawanklayan_tweets.cvs")
write.table(twetsdf ,"pawankalyan_tweets.txt")

##################sentiment analysis


person="'pawan kalyan- twitter'"

text=twetsdf$text

library(tm)

s2 <- iconv(text, "UTF-8", "ASCII", sub = "")

s2

x1=Corpus(VectorSource(s2))

removeURL1 <- function(x) gsub("http[[:alnum:]]*", "", x)

removeURL2 <- function(x) gsub("http[[:alnum:][:punct:]]*", "", x)

removeURL3 <- function(x) gsub("http[^[:space:]]*", "", x)



x=tm_map(x1,removeURL1)
x=tm_map(x,removeURL2)
x=tm_map(x,removeURL3)

inspect(x[400:487])
x=tm_map(x,tolower)
x=tm_map(x,removeNumbers)
x=tm_map(x,removePunctuation)
x=tm_map(x,removeWords,stopwords("en"))
x=tm_map(x,stripWhitespace)
inspect(x[400:487])

write.table(x,file = "GK.csv")
class(x)
dataframe <- data.frame(text=sapply(x, identity), 
                        stringsAsFactors=F)


tdm=TermDocumentMatrix(x)
tdm=as.matrix(tdm)

w=rowSums(tdm)
head(w)
tail(w)

w_sub=subset(w,w>5)
head(w_sub)
w_sub

sort(w_sub)

nqwrd=c('now','pawan','sir','kalyan','put'," anandaaquagasleak","jsp",'amp','jspstandswithtitlivictims','every','office','telugu','goi','gopala' )          
nqwrd

x=tm_map(x,removeWords,nqwrd)

tdm=TermDocumentMatrix(x)
tdm=as.matrix(tdm)

w=rowSums(tdm)
head(w)
tail(w)

w_sub=subset(w,w>5)
head(w_sub)
w_sub

sort(w_sub)


##### asume prepossing is completed if want remove some words we can add words to "nqwrd" list
### word cloud visualization

library(wordcloud)
library(wordcloud2)

help(wordcloud)
windows()
wordcloud(words = names(w_sub),freq = w_sub,colors = rainbow(70)) # morethen 5 times repeted words
wordcloud(words = names(w),freq = w,colors = rainbow(70)) # total words


library(wordcloud2)

#wordcloud2(data, size = 1, minSize = 0, gridSize =  0,
#           fontFamily = 'Segoe UI', fontWeight = 'bold',
#           color = 'random-dark', backgroundColor = "white",
#           minRotation = -pi/4, maxRotation = pi/4, shuffle = TRUE,
#           rotateRatio = 0.4, shape = 'circle', ellipticity = 0.65,
#           widgetsize = NULL, figPath = NULL, hoverFunction = NULL)
#Arguments
#data	
#A data frame including word and freq in each column

#size	
#Font size, default is 1. The larger size means the bigger word.

help("wordcloud2")


w2=data.frame(sort(names(w)),sort(w))
w2

colnames(w2)=c("words","freqence")
w2
windows()
wordcloud2(data=w2,size=2,color = rainbow(70))

w_sub2=data.frame(names(w_sub),w_sub)
colnames(w_sub2)=c("words","freq")
wordcloud2(w_sub2,size=1,color = rainbow(70))

#letter cloud
windows()
letterCloud(w2,word = "s",col=rainbow(50))


####### sentimental analysis

data=dataframe$text

library(syuzhet)

all_sentences=get_sentences(data)

all_sentences[1:5]

## using bing framme we cam meaure entire 
bing_sentment=get_sentiment(all_sentences,method = "bing")
sum(bing_sentment)
max(bing_sentment)
min(bing_sentment)
mean(bing_sentment)
summary(bing_sentment)
postive_sentince_number=which.max(bing_sentment)
Negate_sentience_number=which.min(bing_sentment)
positive_sentince=all_sentences[postive_sentince_number]
positive_sentince
negitive_sentience=all_sentences[Negate_sentience_number]
negitive_sentience

plot(bing_sentment,main = "plot Trajectory",type = "l",xlab = "bing_semtimens",ylab = "values of bing sentimnet")
abline(h=0,col="red")


### using nrc 


emotions=get_nrc_sentiment(all_sentences)
head(emotions)


#anger
angger_senences=which(emotions$anger>0)
all_sentences[angger_senences]
s1=length(angger_senences)


# antipation
antipation_senences=which(emotions$anticipation>0)
all_sentences[antipation_senences]
s2=length(antipation_senences)

#
disgust_senences=which(emotions$disgust>0)
all_sentences[disgust_senences]
s3=length(disgust_senences)



fear_senences=which(emotions$fear>0)
all_sentences[fear_senences]
s4=length(fear_senences)

joy_senences=which(emotions$joy>0)
all_sentences[joy_senences]
s5=length(joy_senences)


sadness_senences=which(emotions$sadness>0)
all_sentences[sadness_senences]
s6=length(sadness_senences)


surprise_senences=which(emotions$surprise>0)
all_sentences[surprise_senences]
s7=length(surprise_senences)

trust_senences=which(emotions$trust>0)
all_sentences[trust_senences]
s8=length(trust_senences)



#### plottionhg and visuvalix=zation
colSums(emotions)
df=colSums(data.frame(emotions[1:8]))
df

windows()

barplot(sort(df),col = 1:8,horiz = T,xlab = "persentage of emotion",ylab = "emotions",main = " pawankalyan twitter-analysis")

print("pawan kalyan twitter sentmen analysis is completed")






















