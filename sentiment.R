View(total)


##  Sentiment Analysis
###  Applying Sentiment Dictionary to DTM
Read lexicon as dictionary to define positive and negative words.
```{r}
lexicon = readRDS("lexicon.rds")
pos_words = lexicon$word1[lexicon$priorpolarity == "positive"]
neg_words = lexicon$word1[lexicon$priorpolarity == "negative"]
```
Read tweets texts, and apply the sentiment dictionary to create matrix of positive and negative words.
```{r}
uatext=total[c("text")]
uadtm = create_matrix(uatext, language="english",removePunctuation=T, stemWords=F)
total$npos = row_sums(uadtm[, colnames(uadtm) %in% pos_words])
total$nneg = row_sums(uadtm[, colnames(uadtm) %in% neg_words])
```
Calculate the sentiment scores.

total$sent = (total$npos - total$nneg) / (total$npos + total$nneg)
total$sent[is.na(total$sent)] = 0
write.csv(total, file = "totaltweets.csv")
saveRDS(total, file="totaltweets.rds")
save(total,file="totaltweets.rda")

total$date = as.Date(total$created)
sentiment = data.frame(aggregate(sent~date,total,sum))
ggplot(sentiment,aes(date,sent))+geom_point()+ geom_line()

ggplot(df, aes(x, y)) + 
  geom_point() + 
  scale_x_datetime(breaks =df$x , labels = format(df$x, "%Y-%m-%d")) 


library(ggplot2)
ggplot(total,aes(date,sent)) + geom_line()+scale_x_date()


dict = dictionary(list(pos=pos_words, neg=neg_words))
dfm2 = dfm(paste(total$text), dictionary=dict)
head(dfm2)
```


Convert it to a data.frame and combine it with the tweets, according to the sentiment classification and retweetcount. 
```{r}
d2 = as.data.frame(dfm2)
total = cbind(total, d2)
tweetsent=subset(total,select=c("id","screenName","text","retweetCount","pos","neg","sent"))
tweetsent$gold = as.factor(ifelse(tweetsent$retweetCount > 1000, "pos", "neg"))
tweetsent$pred = ifelse(tweetsent$sent > 0, "pos", "neg")
mt = table(tweetsent$gold, tweetsent$pred)
mt
```