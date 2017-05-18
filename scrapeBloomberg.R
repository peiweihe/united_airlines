library(httr)
library(rvest)
retry = function(fun, attempts=5, ...){
  for (attempt in 1:5) {
    result = try(fun(...))
    if (class(result) != "try-error") return(result)
    message("Attempt ", attempt, " failed, sleeping 1 second")
    Sys.sleep(10)
  }
    # try one last time, let error pass if it still fails
    fun(...)
}

  
get_page <- function(path, ...) {
    r = retry(GET, url=path, query=list(...))
    #text = iconv(list(r$content), from="gbk", to="utf-8", sub="")
    read_html(r$content)
    #read_html(r,encoding='gbk')
 }
  

t = "https://www.bloomberg.com/search?query=ibm&page=1"
library(stringr)
library(lubridate)
Get_date<-function(date){
  pdate = str_trim(date)
  return (mdy(pdate))
}
read.url<-function(t){
  webs <- get_page(t)
  url<- webs %>% 
    html_nodes("div.search-result-story__container") %>%
    html_nodes("h1 > a") %>%
    html_attr("href")
  time<- webs %>%
    html_nodes("div.search-result-story__container") %>%
    html_nodes("div.search-result-story__metadata > span > time") %>%
    html_text()
  date = lapply(time, Get_date)
  m =data.frame(cbind(url,date))
}

#创建List爬搜索到的页面设置页数
topics<-c()
for(i in 1:2){
  topics<-c(topics,paste("https://www.bloomberg.com/search?query=ibm&page=",i,sep = ""))
}

#爬url
ibm<-do.call(rbind,lapply(topics,read.url))
View(ibm)


#下面是爬正文的代码也分装成函数了，但我还没有改
get_text <- function(path, ...) {
  url = paste0("http://guba.sina.com.cn", path)
  r = retry(GET, url=url, query=list(...))
  read_html(r, encoding='gbk')
}

get_comment<-function(url){
  message(url)
  comment<- get_text(url) %>% html_nodes( '#thread_content') %>% html_text()
}

comments = do.call(rbind,lapply(Huayi_Bro$url,get_comment))
HuaYi = cbind(Huayi_Bro_a,comments)