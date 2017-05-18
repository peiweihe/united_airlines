###SCRAPING NEWS
library(rvest)
library(httr)
library(stringr)

user_agent = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_11_1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/57.0.2987.98 Safari/537.36"

nyt_news = as.vector(readLines("list_nyt_urls.txt"))
wp_news = as.vector(readLines("list_wp_urls.txt"))
bbc_news = as.vector(readLines("list_bbc_urls_1.txt"))

#nyt_news
text_list_nyt = c()
time_nyt = c()
title_nyt = c()
length(nyt_news)

for(url in nyt_news){
  r = GET(url,user_agent(user_agent))
  text = iconv(list(r$content))
  text_tree = read_html(text)
  text_raw = text_tree %>% html_nodes(css=".story-content") %>% html_text(" ")
  time = text_tree %>% html_node(css=".dateline") %>% html_text(" ")
  title = text_tree %>% html_node(css=".headline") %>% html_text(" ")
  text_clean = gsub("[\r\n]","", text_raw)
  article = paste(text_clean,collapse = "\n")
  text_list_nyt = c(text_list_nyt,article)
  time_nyt = c(time,time_nyt)
  title_nyt = c(title,title_nyt)
}

nyt_17_articles = data.frame(text_list_nyt,title_nyt,time_nyt)
saveRDS(nyt_17_articles,file = "nyt_17_articles.rds") 

readRDS("nyt_17_articles.rds")
help(saveRDS)

text_list_wp = c()
title_wp = c()
time_wp = c()
for(url in wp_news){
  r = GET(url,user_agent(user_agent))
  text = iconv(list(r$content))
  text_tree = read_html(text)
  text_raw = text_tree%>% html_node(css = ".article-body") %>% html_text(" ")
  title = text_tree %>% html_node(xpath ="//*[@id='topper-headline-wrapper']/h1") %>% html_text(" ")
  time = text_tree %>% html_node(css=".pb-timestamp") %>% html_text(" ")
  print(time)
  print(title)
  text_clean = gsub("[\r\n]", "", text_raw)
  text_list_wp = c(text_list_wp,text_clean)
  title_wp = c(title,title_wp)
  time_wp = c(time,time_wp)
  #time_nyt = c(time,time_nyt)
  #title_nyt = c(title,title_nyt)
  #text_list = c(text_list,text_clean)
  #text_url = str_extract(text_clean,"https:.+.\\/")
}
bbc_news

text_list_bbc = c()
title_bbc = c()
time_bbc = c()
for(url in bbc_news[0:100]){
  r = GET(url,user_agent(user_agent))
  text = iconv(list(r$content))
  text_tree = read_html(text)
  text_raw = text_tree%>% html_node(css = ".story-body__inner") %>% html_text(" ")
  title = text_tree %>% html_node(css = ".story-body__h1") %>% html_text(" ")
  time = text_tree %>% html_node(css = ".date--v2") %>% html_text(" ")
  text_clean = gsub("[\r\n]", "", text_raw)
  article = paste(text_clean,collapse = "\n")
  text_list_bbc = c(text_list_bbc,article)
  title_bbc = c(title,title_bbc)
  time_bbc = c(time,time_bbc)
  #time_nyt = c(time,time_nyt)
  #title_nyt = c(title,title_nyt)
  #text_list = c(text_list,text_clean)
  #text_url = str_extract(text_clean,"https:.+.\\/")
}

bbc_100_articles = data.frame(text_list_bbc,title_bbc,time_bbc)
saveRDS(bbc_100_articles,file = "bbc_100_articles.rds") 

library(quanteda)
library(plyr)
library(corpustools)

text_from_nyt = nyt_17_articles$text_list_nyt
text_from_wp = wp_114_articles$text_list_wp
text_from_bbc = bbc_100_articles$text_list_bbc

convert_dtm = function(dataset){
  dataset = as.character(dataset)
  corpus = corpus(dataset)
  dtm_text = dfm(corpus,stem=F,removePunct=T)
  dtm_text = dfm_select(dtm_text,c(stopwords("english"),"will","media","new","one","function","mr","bbcdotcom.adverts","passenger","said","united","airlines","airline","flight","passengers","bbcdotcom.adverts.slotasync","image","copyright","caption","international","window.bbcdotcom"),selection = c("remove"))
  return(dtm_text)
}

dtm_text_nyt = convert_dtm(text_from_nyt)
dtm_text_wp = convert_dtm(text_from_wp)
dtm_text_bbc = convert_dtm(text_from_bbc)

library(wordcloud)
library(RColorBrewer)
plot(dtm_text_nyt, max.words = 100, colors = brewer.pal(9, "Reds")[5:9], scale = c(4, .5))
topfeatures(dtm_text_nyt)

plot(dtm_text_wp,max.words = 100, colors = brewer.pal(9, "Reds")[5:9], scale = c(4, .5))
topfeatures(dtm_text_wp)

plot(dtm_text_bbc,max.words = 100, colors = brewer.pal(9, "Reds")[5:9], scale = c(4, .5))
topfeatures(dtm_text_bbc)

cmp = corpustools::corpora.compare(dtm_text_wp,dtm_text_bbc)
cmp = cmp[order(cmp$over, decreasing = F), ]
head(cmp,100)

h = rescale(log(cmp$over), c(1, .6666))
s = rescale(sqrt(cmp$chi), c(.25,1))
cmp$col = hsv(h, s, .33 + .67*s)

cmp = arrange(cmp, -termfreq)
with(head(cmp, 50), plotWords(x=log(over), words=term, wordfreq=termfreq, random.y = T, col=col, scale=2))


text(-2, 0, "BBC", srt=90, col="red", cex=2)
text(2, 0, "WP", srt=90, col="blue", cex=2)
title(xlab="Overrepresentation")
#华盛顿邮报和BBC的对比

lexicon = readRDS("lexicon.rds")
dict = dictionary(list(pos=pos_words, neg=neg_words))
corpus_nyt = corpus(as.character(text_from_nyt))
corpus_wp = corpus(as.character(text_from_wp))
corpus_bbc = corpus(as.character(text_from_wp))

dtm_nyt = dfm(corpus_nyt,stem=F,removePunct=T,dictionary=dict)
dtm_wp = dfm(corpus_wp,stem=F,removePunct=T,dictionary=dict)
dtm_bbc = dfm(corpus_wp,stem=F,removePunct=T,dictionary=dict)

head(dtm_wp,100)
head(dtm_nyt,100)
head(dtm_bbc,100)

###对西方媒体的初步分析
####1:大多数用词上，都比较客观，不用太多带感情的词。
####2:华盛顿邮报注重报道个人的情况，比如说联航的CEO以及这个被打的医生。
####3.BBC注重本国，把这个事情报道为一个外国新闻。并且经常提及美联航之前发生过的事情。
####4.媒体的用词都颇为中立，看不出太大的感情偏向。