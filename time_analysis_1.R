####时间分析
library(plyr)
nyt_17_articles = readRDS("nyt_17_articles.rds")
text_from_nyt = nyt_17_articles$text_list_nyt
nyt_token = tokenize(as.character(text_from_nyt),removePunct = T)
ul1 = lapply(nyt_token, ldply)

for(i in seq(1)){
  data_frame = data.frame(ul1[[i]])
  data_frame["article"] = i
  #data_frame
}

total = c()
for(i in seq(1:17)){
  data_frame = data.frame(ul1[[i]])
  data_frame["article"] = i
  total = rbind(total,data_frame)
}
total

nyt_17_articles["article"] = rownames(nyt_17_articles) 
dtm = dtm.create(total$article, total$V1)

s = c(13,27,15,11,22,11,12,14,12,11,26,11,11,11,13,13,10)
nyt_17_articles$time_number = s
a = nyt_17_articles$time_nyt

wordfreqs = dtm.to.df(dtm)
wordfreqs = merge(nyt_17_articles,wordfreqs,by.x="article",by.y="doc")
mmode <- function(v){uniqv <- unique(v); uniqv[which.max(tabulate(match(v, uniqv)))]}
dates = aggregate(wordfreqs["time_number"], by=wordfreqs["term"], FUN=mmode)

#cmp = corpora.compare(dtm, select.rows = obama)
cmp = arrange(merge(cmp, dates), -termfreq)
cmp
length(cmp$term)
with(head(cmp, 150), plotWords(x = time_number,words=term,wordfreq=termfreq,random.y =T,col=col,scale=1))
#######





