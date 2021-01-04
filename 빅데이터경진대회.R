##### R코드 목차#####
#본문 2.1.1 9행~
#본문 2.1.2 159행~
#본문 2.2.1 275행~
#본문 2.2.2 296행~
#본문 2.3   307행~


#본문 2.1 part
##### 각 신문사 단어출현 비교(날짜별) 파일 만들기 #####
### 한겨레 ###
urls <- NULL # 날짜 변경하며 날짜별 파일로 각각 저장 후 rbind로 합치기
basic_url <- "http://search.hani.co.kr/Search?command=query&keyword=%EC%84%B8%EC%9B%94%ED%98%B8&media=news&submedia=&sort=d&period=all&datefrom=2014.04.16&dateto=2014.04.16&pageseq="
for(x in 0:9){
  urls[x+1] <- paste0(basic_url,x) 
}

links<-NULL
for(url in urls){
  html <- read_html(url)
  links <- c(links,html %>% html_nodes('.search-result-list') %>% html_nodes('a') %>% html_attr('href') %>% unique())
}
links<-paste0('http:',links)

txts <- NULL
for(link in links){
  html <- read_html(link)
  txts <- c(txts, html %>% html_nodes('.a-left') %>% html_nodes('.text') %>% html_text())
}

txt_pre <- gsub('[\r\n\t]','',txts) 
txt_pre <- gsub('[a-z]','',txt_pre) 
txt_pre <- gsub('[A-Z]','',txt_pre) 
txt_pre <- gsub('\\s+',' ',txt_pre) 
txt_pre <- gsub('[[:cntrl:]]','',txt_pre) 
txt_pre <- gsub('[[:punct:]]','',txt_pre)
txt_pre <- gsub('\\d+',' ',txt_pre) 

write.csv(txt_pre, "txt_pre0416_h.csv", quote = F) # 여기까지 날짜별 파일로 각각 저장

test123 <- data.frame()
for(i in 16:30){
  ja <- paste("txt_pre04",i,"_h.csv",sep="") #t
  txt_text <- read.csv(ja,header=T,stringsAsFactors = F)[,2]
  
  user_dic <- data.frame(term=c("세월호, 사고 , 참사"),tag='ncn')
  buildDictionary(ext_dic = 'sejong', user_dic = user_dic)
  exNouns <- function(x){paste(extractNoun(x),collapse=" ")} 
  news_nouns <- sapply(txt_text, exNouns)
  newsCorpus <- Corpus(VectorSource(news_nouns))
  TDM <- TermDocumentMatrix(newsCorpus, control = list(wordLengths = c(4,16)))
  tdm.df <- as.data.frame(as.matrix(TDM))
  wordResult <- sort(rowSums(tdm.df),decreasing = T)
  myNames <- names(wordResult)
  df <- data.frame(word=myNames, freq=wordResult)

  ta <- paste('201404',i,sep="") #t
  date <- c(rep(ta, each=length(df$word)))
  df04 <- cbind(date,df)
  test123 <- rbind(test123,df04) #t
}

write.csv(test123,"통합일빈도수_h.csv",row.names=F,quote=F)

a <- subset(test123, freq>=4)
write.csv(a,"통합일빈도수(4이상)_h.csv",row.names=F,quote=F)

### 조선일보 ###
basic_url <- 'https://search.daum.net/search?w=news&enc=utf8&cluster=y&cluster_page=1&q=%EC%84%B8%EC%9B%94%ED%98%B8&sd=20140430000000&ed=20140430235959&period=u&cpname=%EC%A1%B0%EC%84%A0%EC%9D%BC%EB%B3%B4&cp=16d4PV266g2j-N3GYq&DA=PGD&p='

urls <- NULL
for (x in 1:10) {
  urls[x] = paste0(basic_url,x)
}

links <- NULL
for(url in urls){
  html <- read_html(url)
  links <- c(links, html %>% html_nodes('.coll_cont') %>% html_nodes('a') %>% html_attr('href') %>% unique())
}

links <- links[-grep("f=o",links)]
links <- links[-grep("w=tot",links)]
links <- links[-grep("together",links)]
links <- links[-grep("v.daum",links)]
links <- links[-grep("java",links)]
links <- links[-grep("news",links)]
links <- unique(links)

txts <- NULL
for (link in links){
  html <- read_html(link)
  txts <- c(txts,html %>% html_nodes('.news_view') %>% html_text())
}

txt_pre <- gsub("[\n]",'', txts)
txt_pre <- gsub("[[:punct:]]",'', txt_pre)
txt_pre <- gsub("[[:cntrl:]]",'', txt_pre)
txt_pre <- gsub("[a-z]+",'', txt_pre)
txt_pre <- gsub("[A-Z]+",'', txt_pre)
txt_pre <- gsub('\\s+',' ', txt_pre)
txt_pre <- gsub('\\d+','',txt_pre);length(txt_pre)
write.csv(txt_pre, "txt_pre0430_j.csv", quote = F) # 여기까지 날짜별 파일로 각각 저장

txt_pre_m4_j <- data.frame()
for(i in 16:30){
  ja <- paste("txt_pre04",i,"_j.csv",sep="") #t
  txt_text <- read.csv(ja,header=T,stringsAsFactors = F)[,2]
  
  
  user_dic <- data.frame(term=c("세월호, 사고 , 참사"),tag='ncn')
  buildDictionary(ext_dic = 'sejong', user_dic = user_dic)
  
  exNouns <- function(x){paste(extractNoun(x),collapse=" ")} 
  news_nouns <- sapply(txt_text, exNouns)
  newsCorpus <- Corpus(VectorSource(news_nouns)) 
  TDM <- TermDocumentMatrix(newsCorpus, control = list(wordLengths = c(4,16)))
  tdm.df <- as.data.frame(as.matrix(TDM))
  wordResult <- sort(rowSums(tdm.df),decreasing = T)
  myNames <- names(wordResult)
  df <- data.frame(word=myNames, freq=wordResult)
  
  ta <- paste('201404',i,sep="") #t
  date <- c(rep(ta, each=length(df$word)))
  df04 <- cbind(date,df)
  txt_pre_m4_j <- rbind(txt_pre_m4_j,df04) #t
}

write.csv(txt_pre_m4_j,"통합일빈도수_j.csv",row.names=F,quote=F)
a <- subset(txt_pre_m4_j, freq>=4)
write.csv(a, "통합일빈도수(4이상)_j.csv",row.names=F, quote=F)


##### 16~20일 신문사별 단어 시각화(통합일빈도수 파일이용) #####

freqbyj416 <- read.csv("통합일빈도수(4이상)_j.csv")
freqbyh416 <- read.csv("통합일빈도수(4이상)_h.csv")

freqbyj416 <- subset(freqbyj416, date==20140416)
freqbyh416 <- subset(freqbyh416, date==20140416)

type <- c(rep("조선일보", each=length(freqbyj416)))
freqbyj416 <- cbind(type, freqbyj416)

type <- c(rep("한겨레", each=length(freqbyh416)))
freqbyh416 <- cbind(type, freqbyh416)

freqbyjh416 <- rbind(freqbyj416,freqbyh416)
freqbyjh416sorted <- freqbyjh416[c(1:21,272:293),] #상위 20개씩
freqbyjh416sorted[22,3] <- "세월호"  #세월을 세월호로 변경
freqbyjh416sorted <- freqbyjh416sorted[-30,] #어조사 없애기

ggplot(freqbyjh416sorted, aes(label=word, size=freq, color=factor(type), angle_group=type)) + 
  geom_text_wordcloud_area(eccentricity=1.2) +
  scale_size_area(max_size=30) +
  theme_minimal()


##### 각 신문사 종합 단어 출현 비교(전체비교) 파일 만들기 #####
### 한겨레 (16~30 전체 종합 단어 파일) ###
urls<-NULL
for(k in 16:30){
  basic_url <- paste("http://search.hani.co.kr/Search?command=query&keyword=%EC%84%B8%EC%9B%94%ED%98%B8&media=news&submedia=&sort=d&period=all&datefrom=2014.04.",k,"&dateto=2014.04.",k,"&pageseq=",sep="")
  t <- k-16
  for(x in 0:9){
    urls[(10*t)+x+1] <- paste0(basic_url,x) 
  }
}

links<-NULL
for(url in urls){
  html <- read_html(url)
  links <- c(links,html %>% html_nodes('.search-result-list') %>% html_nodes('a') %>% html_attr('href') %>% unique())
}
links<-paste0('http:',links)

txts <- NULL
for(link in links){
  html <- read_html(link)
  txts <- c(txts, html %>% html_nodes('.a-left') %>% html_nodes('.text') %>% html_text())
}

txt_pre <- gsub('[\r\n\t]','',txts) 
txt_pre <- gsub('[a-z]','',txt_pre) 
txt_pre <- gsub('[A-Z]','',txt_pre) 
txt_pre <- gsub('\\s+',' ',txt_pre) 
txt_pre <- gsub('[[:cntrl:]]','',txt_pre) 
txt_pre <- gsub('[[:punct:]]','',txt_pre) 
txt_pre <- gsub('\\d+',' ',txt_pre)

write.csv(txt_pre, "all_pre_h.csv",  quote = F) #사전저장
txt_text <- read.csv('all_pre_h.csv',header=T,stringsAsFactors = F)[,2]

user_dic <- data.frame(term=c("세월호, 사고 , 참사"),tag='ncn') 
buildDictionary(ext_dic = 'sejong', user_dic = user_dic)

exNouns <- function(x){paste(extractNoun(x),collapse=" ")} 
news_nouns <- sapply(txt_text, exNouns)
newsCorpus <- Corpus(VectorSource(news_nouns)) 
TDM <- TermDocumentMatrix(newsCorpus, control = list(wordLengths = c(4,16)))
tdm.df <- as.data.frame(as.matrix(TDM))
wordResult <- sort(rowSums(tdm.df),decreasing = T)
myNames <- names(wordResult)
df <- data.frame(word=myNames, freq=wordResult)

write.csv(df, "all_h.csv",row.names=F, quote = F)
all_70_h <- df[1:70,]
write.csv(all_70_j,"all_70_j.csv",row.names=F, quote = F)


### 조선일보 (16~30 전체 종합 단어 파일) ###
urls<-NULL
for(k in 16:30){
  basic_url <- paste("https://search.daum.net/search?w=news&enc=utf8&cluster=y&cluster_page=1&q=%EC%84%B8%EC%9B%94%ED%98%B8&sd=201404",k,"000000&ed=201404",k,"235959&period=u&cpname=%EC%A1%B0%EC%84%A0%EC%9D%BC%EB%B3%B4&cp=16d4PV266g2j-N3GYq&DA=PGD&p=",sep="")
  t <- k-16
  for(x in 1:10){
    urls[(10*t)+x] <- paste0(basic_url,x) 
  }
}

links <- NULL
for(url in urls){
  html <- read_html(url)
  links <- c(links, html %>% html_nodes('.coll_cont') %>% html_nodes('a') %>% html_attr('href') %>% unique())
}

links <- links[-grep("f=o",links)];length(links)
links <- links[-grep("w=tot",links)];length(links)
links <- links[-grep("together",links)];length(links)
links <- links[-grep("v.daum",links)];length(links)
links <- links[-grep("java",links)];length(links)
links <- links[-grep("news",links)];length(links)
links <- unique(links);length(links)

txts <- NULL
for (link in links){
  html <- read_html(link)
  txts <- c(txts,html %>% html_nodes('.news_view') %>% html_text())
}

txt_pre <- gsub("[\n]",'', txts)
txt_pre <- gsub("[[:punct:]]",'', txt_pre)
txt_pre <- gsub("[[:cntrl:]]",'', txt_pre)
txt_pre <- gsub("[a-z]+",'', txt_pre)
txt_pre <- gsub("[A-Z]+",'', txt_pre)
txt_pre <- gsub('\\s+',' ', txt_pre)
txt_pre <- gsub('\\d+','',txt_pre);length(txt_pre)

write.csv(txt_pre, "all_pre_j.csv", quote = F) #사전저장
txt_text <- read.csv('all_pre_j.csv',header=T,stringsAsFactors = F)[,2]

user_dic <- data.frame(term=c("세월호, 사고 , 참사"),tag='ncn') 
buildDictionary(ext_dic = 'sejong', user_dic = user_dic)
exNouns <- function(x){paste(extractNoun(x),collapse=" ")} 
news_nouns <- sapply(txt_text, exNouns)
newsCorpus <- Corpus(VectorSource(news_nouns)) 
TDM <- TermDocumentMatrix(newsCorpus, control = list(wordLengths = c(4,16)))
tdm.df <- as.data.frame(as.matrix(TDM))
wordResult <- sort(rowSums(tdm.df),decreasing = T)
myNames <- names(wordResult)
df <- data.frame(word=myNames, freq=wordResult)

write.csv(df, "all_j.csv",row.names=F, quote = F)
all_70_j <- df[1:70,]
write.csv(all_70_j,"all_70_j.csv",row.names=F, quote = F)


##### 각 신문사 종합 단어 시각화(all_70 파일이용) #####
#wordcloud2 패키지 이용
wordcloud2(all_70_h, size=1.8)
wordcloud2(all_70_j, size=1.8)



#본문 2.2 part
##### 주요 선별 단어 신문사별 출현빈도 비교(날짜별) ##### 

data_word <- read.csv('wordselect.csv') # 기존 파일에서 추출하여 만든 파일

data_word[1:10,] %>% ggplot(aes(date, freq))+geom_line(aes(group=news, colour=news))+
  theme_light()+ggtitle("각 신문사 [정부] 출현 빈도수 시간별 변화")+
  theme(plot.title = element_text(hjust=0.5,size=20))

data_word[11:20,] %>% ggplot(aes(date, freq))+geom_line(aes(group=news, colour=news))+
  theme_light()+ggtitle("각 신문사 [대통령] 출현 빈도수 시간별 변화")+
  theme(plot.title = element_text(hjust=0.5,size=20))

data_word[21:30,] %>% ggplot(aes(date, freq))+geom_line(aes(group=news, colour=news))+
  theme_light()+ggtitle("각 신문사 [참사] 출현 빈도수 시간별 변화")+
  theme(plot.title = element_text(hjust=0.5,size=20))

data_word[31:40,] %>% ggplot(aes(date, freq))+geom_line(aes(group=news, colour=news))+
  theme_light()+ggtitle("각 신문사 [책임] 출현 빈도수 시간별 변화")+
  theme(plot.title = element_text(hjust=0.5,size=20))

##### 주요 선별 단어 신문사별 출현빈도 비교(전체) ##### 

graphofall <- matrix( c(912,401,743,528,552,461,523,443,34,403,464,172,223,123,207,84),ncol=8,nrow=2) # 기존 파일에서 추출하여 만든 행렬
rownames(graphofall) <- c("한겨레", "조선")
colnames(graphofall) <- c("정부","대통령","참사","사고","기부","책임","박근혜","청와대")
barplot(graphofall,beside=T,col = rep(c('lavender','mistyrose'),4), ylim=c(0,1000), ylab="frequency", xlab='word',cex.names = 2,)
title(main="신문사 별 주요단어 출현 빈도 비교", font.main=2,cex.main=3,font.main=1)
legend(15,800,c("한겨레","조선일보"),
       cex=2, fill=rep(c('lavender','mistyrose'),4))


#part2.3
##### 연관어 분석 #####
### 한겨레 ###
urls<-NULL
for(k in 16:30){
  basic_url <- paste("http://search.hani.co.kr/Search?command=query&keyword=%EC%84%B8%EC%9B%94%ED%98%B8&media=news&submedia=&sort=d&period=all&datefrom=2014.04.",k,"&dateto=2014.04.",k,"&pageseq=",sep="")
  t <- k-16
  for(x in 0:9){
    urls[(10*t)+x+1] <- paste0(basic_url,x) 
  }
}

links<-NULL
for(url in urls){
  html <- read_html(url)
  links <- c(links,html %>% html_nodes('.search-result-list') %>% html_nodes('a') %>% html_attr('href') %>% unique())
}
links<-paste0('http:',links)
head(links)
length(links)

txts <- NULL
for(link in links){
  html <- read_html(link)
  txts <- c(txts, html %>% html_nodes('.a-left') %>% html_nodes('.text') %>% html_text())
}

txt_pre <- gsub("[\t\r]",'', txts)
txt_pre_over_100 <- gsub("[[:punct:]]",'', txt_pre_over_100)
txt_pre_over_100 <- gsub("[[:cntrl:]]",'', txt_pre_over_100)
txt_pre <- gsub("[a-z]+",'', txt_pre)
txt_pre <- gsub("[A-Z]+",'', txt_pre)
txt_pre <- gsub('\\s+',' ', txt_pre)
txt_pre <- gsub('\\d+','',txt_pre);length(txt_pre)

txt_pre <- str_split(txt_pre,"\n")
txt_pre <- unlist(txt_pre)

loftxt = nchar(txt_pre)

txt_pre_over_100 <- txt_pre[which(loftxt>=100)]

tran <- Map(extractNoun, txt_pre_over_100)
tran <- unique(tran)
tran <- sapply(tran, unique)
tran <- sapply(tran, function(x) {Filter(function(y)
{nchar(y) <= 4 && nchar(y) > 1 && is.hangul(y)},x)})
tran <- Filter(function(x) {length(x) >=2}, tran)

names(tran) <- paste("Tr", 1:length(tran), sep="")
wordtran <- as(tran,"transactions")
ares <- apriori(wordtran, parameter = list(supp=0.013, conf=0.7))

rules <- labels(ares, ruleSep=" ")
rules <- sapply(rules, strsplit, " ", USE.NAMES = F)
rulemat <- do.call("rbind", rules)
ruleg <- graph.edgelist(rulemat, directed = F)

plot.igraph(ruleg, vertex.label=V(ruleg)$name, vertex.label.cex=1.15, vertex.size=25,
            vertex.color="#f1c40f",vertex.frame.color="white",
            vertex.label.font=1,vertex.label.color='black',
            edge.color='darkgrey', edge.width=2)

### 조선일보 ###
urls<-NULL
for(k in 16:30){
  basic_url <- paste("https://search.daum.net/search?w=news&enc=utf8&cluster=y&cluster_page=1&q=%EC%84%B8%EC%9B%94%ED%98%B8&sd=201404",k,"000000&ed=201404",k,"235959&period=u&cpname=%EC%A1%B0%EC%84%A0%EC%9D%BC%EB%B3%B4&cp=16d4PV266g2j-N3GYq&DA=PGD&p=",sep="")
  t <- k-16
  for(x in 1:10){
    urls[(10*t)+x] <- paste0(basic_url,x) 
  }
}

links <- NULL
for(url in urls){
  html <- read_html(url)
  links <- c(links, html %>% html_nodes('.coll_cont') %>% html_nodes('a') %>% html_attr('href') %>% unique())
}

links <- links[-grep("f=o",links)];length(links)
links <- links[-grep("w=tot",links)];length(links)
links <- links[-grep("together",links)];length(links)
links <- links[-grep("v.daum",links)];length(links)
links <- links[-grep("java",links)];length(links)
links <- links[-grep("news",links)];length(links)
links <- unique(links);length(links)

txts <- NULL
for (link in links){
  html <- read_html(link)
  txts <- c(txts,html %>% html_nodes('.news_view') %>% html_text())
}
head(txts)
length(txts)

txt_pre <- gsub("[\n]",'', txts)            #전처리 과정에서의 오류 해결을 위해 변수명 혼용 
txt_pre_over_100 <- gsub("[[:punct:]]",'', txt_pre_over_100)
txt_pre_over_100 <- gsub("[[:cntrl:]]",'', txt_pre_over_100)
txt_pre <- gsub("[a-z]+",'', txts)
txt_pre <- gsub("[A-Z]+",'', txt_pre)
txt_pre_over_100 <- gsub('\\s+',' ', txt_pre_over_100)
txt_pre <- gsub('\\d+','',txt_pre);length(txt_pre)

txt_pre <- str_split(txt_pre,"\n")
txt_pre <- unlist(txt_pre)
loftxt = nchar(txt_pre)

txt_pre_over_100 <- txt_pre[which(loftxt>=100)]

useNIADic()
tran <- Map(extractNoun,txt_pre_over_100)
tran <- unique(tran)
tran <- sapply(tran, unique)
tran <- sapply(tran, function(x) {Filter(function(y)
  
{nchar(y) <= 4 && nchar(y) > 1 && is.hangul(y)},x)})

tran <- Filter(function(x) {length(x) >=2}, tran)

names(tran) <- paste("Tr", 1:length(tran), sep="")
wordtran <- as(tran, "transactions")

ares <- apriori(wordtran, parameter = list(supp=0.04, conf=0.1))

rules <- labels(ares, ruleSep=" ")
rules <- sapply(rules, strsplit, " ", USE.NAMES = F)
rulemat <- do.call("rbind", rules)
ruleg <- graph.edgelist(rulemat, directed = F)

plot.igraph(ruleg, vertex.label=V(ruleg)$name, vertex.label.cex=1.1, vertex.size=23,
            vertex.color="#f1c40f",vertex.frame.color="white",
            vertex.label.font=1,vertex.label.color='black',
            edge.color='darkgrey', edge.width=2)

#노란리본
wordcloud2(all_70_j, figPath ="ribon.png",backgroundColor = 'black', color = '#f1c40f',size=1.8)