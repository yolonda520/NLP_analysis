library(textreadr)
library(dplyr)
library(dplyr)
library(stringr)
library(tidytext)
library(ggplot2)
library(tidyverse)
library(reshape2)

serve <- read_document(file = "C:/Users/Huangyu3/Documents/Text Analyisis/NLP_team6.txt")

a <- 30 #how many observations to you have
b <- 6 #how many variables do you have
my_df <- as.data.frame(matrix(nrow=a, ncol=b))

for(z in 1:b){
  for(i in 1:a){
    my_df[i,z]<- serve[i*b+z-b]
  }#closing z loop
}
names(my_df) <- c("name", "Q1", "Q2","Q3","Q4","Q5")
#View(my_df)
cust_stop <- data_frame(
  word=c("Q1","Q2","Q3","Q4","Q5","q2","q1","q3","q4","q5","NA"),
  lexicon=rep("custom",each=11) 
)

#Create the bigram
frequencies_tokens_nostop_Q1 <- data_frame(name=my_df$name,text=my_df$Q1) %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2) %>%  
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word1 %in% cust_stop$word) %>%
  filter(!word2 %in% cust_stop$word) %>%
  unite(bigram, word1, word2, sep=" ") %>%
  count(bigram,sort=TRUE) 




frequencies_tokens_nostop_Q2 <-data_frame(name=my_df$name, text=my_df$Q2) %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2) %>%  
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word1 %in% cust_stop$word) %>%
  filter(!word2 %in% cust_stop$word) %>%
  unite(bigram, word1, word2, sep=" ") %>%
  top_n(20) %>%
  count(bigram,sort=TRUE) 

frequencies_tokens_nostop_Q3 <- data_frame(name=my_df$name, text=my_df$Q3) %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2) %>%  
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word1 %in% cust_stop$word) %>%
  filter(!word2 %in% cust_stop$word) %>%
  unite(bigram, word1, word2, sep=" ") %>%
  top_n(20) %>%
  count(bigram,sort=TRUE)  

frequencies_tokens_nostop_Q4 <- data_frame(name=my_df$name, text=my_df$Q4) %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2) %>%  
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word1 %in% cust_stop$word) %>%
  filter(!word2 %in% cust_stop$word) %>%
  unite(bigram, word1, word2, sep=" ") %>%
  top_n(20) %>%
  count(bigram,sort=TRUE) 

frequencies_tokens_nostop_Q5 <-data_frame(name=my_df$name, text=my_df$Q5) %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2) %>%  
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word1 %in% cust_stop$word) %>%
  filter(!word2 %in% cust_stop$word) %>%
  unite(bigram, word1, word2, sep=" ") %>%
  count(bigram,sort=TRUE) 

df <- bind_rows(
  mutate(frequencies_tokens_nostop_Q1,question="Q1"),
  mutate(frequencies_tokens_nostop_Q2,question="Q2"),
  mutate(frequencies_tokens_nostop_Q3,question="Q3"),
  mutate(frequencies_tokens_nostop_Q4,question="Q4"),
  mutate(frequencies_tokens_nostop_Q5,question="Q5")
)
#View(df)
book_words <- df %>%
  bind_tf_idf(bigram, question, n) %>%
  arrange(desc(tf_idf)) %>%
  mutate(bigram=factor(bigram, levels=rev(unique(bigram)))) %>% #rev() 一个向量逆转
  group_by(question) %>%
  top_n(2) %>%
  ungroup %>%
  ggplot(aes(bigram, tf_idf, fill=question))+
  geom_col(show.legend=FALSE)+
  labs(x=NULL, y="tf-idf")+
  facet_wrap(~question, ncol=2, scales="free")+
  coord_flip()
book_words


# create word cloud
library(textreadr)
library(dplyr)
library(dplyr)
library(stringr)
library(tidytext)
library(ggplot2)
library(tidyverse)
library(reshape2)
library(wordcloud)


frequencie_tokens_nostop_Q1 <- my_df$Q1 %>%
  substr(start=4 , stop = 10000) %>%
  data_frame(name=my_df$name, text=my_df$Q1) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>% 
  anti_join(cust_stop) %>%
  count(word, sort=TRUE) 


frequencie_tokens_nostop_Q2 <- my_df$Q2 %>%
  substr(start=4 , stop = 10000) %>%
  data_frame(name=my_df$name, text=my_df$Q2) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>% 
  anti_join(cust_stop) %>%
  count(word, sort=TRUE)

frequencie_tokens_nostop_Q3 <- my_df$Q3 %>%
  substr(start=4 , stop = 10000) %>%
  data_frame(name=my_df$name, text=my_df$Q3) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>% 
  anti_join(cust_stop) %>%
  count(word, sort=TRUE) 

frequencie_tokens_nostop_Q4 <- my_df$Q4 %>%
  substr(start=4 , stop = 10000) %>%
  data_frame(name=my_df$name, text=my_df$Q4) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>% 
  anti_join(cust_stop) %>%
  count(word, sort=TRUE) 

frequencie_tokens_nostop_Q5 <- my_df$Q5 %>%
  substr(start=4 , stop = 10000) %>%
  data_frame(name=my_df$name, text=my_df$Q5) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>% 
  anti_join(cust_stop) %>%
  count(word, sort=TRUE) 

df <- bind_rows(
  mutate(frequencie_tokens_nostop_Q1,question="Q1"),
  mutate(frequencie_tokens_nostop_Q2,question="Q2"),
  mutate(frequencie_tokens_nostop_Q3,question="Q3"),
  mutate(frequencie_tokens_nostop_Q4,question="Q4"),
  mutate(frequencie_tokens_nostop_Q5,question="Q5")
) 

words <- df %>%
  bind_tf_idf(word, question, n) 
tail(words)

wordcloud <- words %>%
  with(wordcloud(word, n, max.words = 100,
                 # colors = c("black", "red","blue","grey","pink","yellow","green"),
                 fixed.asp=TRUE))  



words %>%
  arrange(desc(tf_idf))

  words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word=factor(word, levels=rev(unique(word)))) %>% #rev() 一个向量逆转
  group_by(question) %>%
  top_n(5) %>%
  ungroup %>%
  ggplot(aes(word, tf_idf, fill=question))+
  geom_col(show.legend=FALSE)+
  labs(x=NULL, y="tf-idf")+
  facet_wrap(~question, ncol=2, scales="free")+
  coord_flip()

  
  # create sentiment
  library(textreadr)
  library(dplyr)
  library(dplyr)
  library(stringr)
  library(tidytext)
  library(ggplot2)
  library(tidyverse)
  library(reshape2)
  library(tidyr)
  library(wordcloud)
 
  afinn <- get_sentiments("afinn")
  nrc <- get_sentiments("nrc")
  bing <- get_sentiments("bing")
  
  frequencies_tokens_Q1 <- my_df$Q1 %>%
    substr(start=4 , stop = 10000) %>%
    data_frame(name=my_df$name, text=my_df$Q1) %>%
    unnest_tokens(word, text) %>%
    anti_join(stop_words) %>% 
    anti_join(cust_stop) %>%
    count(word, sort=TRUE) %>%
    mutate(word = reorder(word,n)) %>%
    ggplot(aes(word, n))+
    geom_col()+
    xlab(NULL)+
    coord_flip()
  print(frequencies_tokens_Q1)
  
  frequencies_tokens_Q2 <- my_df$Q2 %>%
    substr(start=4 , stop = 10000) %>%
    data_frame(name=my_df$name, text=my_df$Q2) %>%
    unnest_tokens(word, text) %>%
    anti_join(stop_words) %>% 
    anti_join(cust_stop) %>%
    count(word, sort=TRUE) %>%
    inner_join(get_sentiments("nrc")) %>%
    count(word, sentiment, sort=TRUE) %>%
    acast(word ~sentiment, value.var="n", fill=0) %>%
    comparison.cloud(colors = c("black", "red","blue","grey","pink","yellow","green"),
                     max.words=100,
                     scale = c(1,1),
                     fixed.asp=TRUE,   #True将长宽比例固定
                     title.size=1
    )
  
  
  frequencies_tokens_Q3 <- my_df$Q3 %>%
    substr(start=4 , stop = 10000) %>%
    data_frame(name=my_df$name, text=my_df$Q3) %>%
    unnest_tokens(word, text) %>%
    anti_join(stop_words) %>% 
    anti_join(cust_stop) %>%
    count(word, sort=TRUE) %>%
    inner_join(get_sentiments("nrc")) %>%
    count(word, sentiment, sort=TRUE) %>%
    acast(word ~sentiment, value.var="n", fill=0) %>%
    comparison.cloud(colors = c("black", "red","blue","grey","pink","yellow","green"),
                     max.words=100,
                     scale = c(1,1),
                     fixed.asp=TRUE,   #True将长宽比例固定
                     title.size=1
    )
  
  frequencies_tokens_Q4 <- my_df$Q4 %>%
    substr(start=4 , stop = 10000) %>%
    data_frame(name=my_df$name, text=my_df$Q4) %>%
    unnest_tokens(word, text) %>%
    anti_join(stop_words) %>% 
    anti_join(cust_stop) %>%
    count(word, sort=TRUE) %>%
    mutate(word = reorder(word,n)) %>%
    top_n(20) %>%
    ggplot(aes(word, n))+
    geom_col()+
    xlab(NULL)+
    coord_flip()
  print(frequencies_tokens_Q4)
  
  frequencies_tokens_Q5 <- my_df$Q5 %>%
    substr(start=4 , stop = 10000) %>%
    data_frame(name=my_df$name, text=my_df$Q5) %>%
    unnest_tokens(word, text) %>%
    anti_join(stop_words) %>% 
    anti_join(cust_stop) %>%
    count(word, sort=TRUE) %>%
    inner_join(get_sentiments("bing")) %>%
    count(word, sentiment, sort=TRUE) %>%
    acast(word ~sentiment, value.var="n", fill=0) %>%
    comparison.cloud(colors = c("black", "red","blue","grey","pink","yellow","green"),
                     max.words=100,
                     scale = c(1,1),
                     fixed.asp=TRUE,   #True将长宽比例固定
                     title.size=1
    )
  
  
  
#Naive Bayes model  
  library(quanteda)
  library(RColorBrewer)
  library(ggplot2)
  library(textreadr)
  library(dplyr)
  library(dplyr)
  library(stringr)
  library(tidytext)
  library(ggplot2)
  library(tidyverse)
  library(reshape2)
  library(topicmodels) 
  library(twitteR)
  library(tm)
  
 
  df <- unite(my_df, "text", Q5 ,sep = "")
  #Q1, Q2, Q3,
  #View(df)
  df$binary <-c(0,1,0,1,0,0,0,1,1,1,0,0,0,0,0,1,1,0,1,0,0,0,0,1,0,0,0,0,0,0)  
  
  
  
  #we need to convert the VCorpus from the previous point to
  #a regular corpus using the corpus() function.
  head(df)
  twitter_corpus <- corpus(df$text) #creating the corpus on the $text var
  msg.dfm <- dfm(twitter_corpus, tolower = TRUE) #generating document 
  msg.dfm <- dfm_trim(msg.dfm, min_termfreq = 3, min_docfreq = 0)
  msg.dfm <- dfm_weight(msg.dfm)
  
  head(msg.dfm)
  #let's split the docs into training and testing data
  msg.dfm.train<-msg.dfm[1:22,]
  msg.dfm.test<-msg.dfm[23:30,]
  
  NB_classifier <- textmodel_nb(msg.dfm.train, c(0,1,0,1,0,0,0,1,1,1,0,0,0,0,0,1,1,0,1,0,0,0))
  NB_classifier
  summary(NB_classifier)
  
  # predicting the testing data
  pred <- predict(NB_classifier, msg.dfm.test)
  pred

  
# LDA model
  library(textreadr)
  library(dplyr)
  library(dplyr)
  library(stringr)
  library(tidytext)
  library(ggplot2)
  library(tidyverse)
  library(reshape2)
  library(topicmodels) 
  
  
  
    Q1 <- my_df$Q1 %>%
    substr(start=4 , stop = 10000) %>%
    data_frame(name=my_df$name, text=my_df$Q1) %>%
    unnest_tokens(word, text) %>%
    anti_join(stop_words) %>% 
    anti_join(cust_stop) %>%
    count(word, sort=TRUE) 
  
  
    Q2 <- my_df$Q2 %>%
    substr(start=4 , stop = 10000) %>%
    data_frame(name=my_df$name, text=my_df$Q2) %>%
    unnest_tokens(word, text) %>%
    anti_join(stop_words) %>% 
    anti_join(cust_stop) %>%
    count(word, sort=TRUE)
  
    Q3 <- my_df$Q3 %>%
    substr(start=4 , stop = 10000) %>%
    data_frame(name=my_df$name, text=my_df$Q3) %>%
    unnest_tokens(word, text) %>%
    anti_join(stop_words) %>% 
    anti_join(cust_stop) %>%
    count(word, sort=TRUE) 
  
    Q4 <- my_df$Q4 %>%
    substr(start=4 , stop = 10000) %>%
    data_frame(name=my_df$name, text=my_df$Q4) %>%
    unnest_tokens(word, text) %>%
    anti_join(stop_words) %>% 
    anti_join(cust_stop) %>%
    count(word, sort=TRUE) 
  
    Q5 <- my_df$Q5 %>%
    substr(start=4 , stop = 10000) %>%
    data_frame(name=my_df$name, text=my_df$Q5) %>%
    unnest_tokens(word, text) %>%
    anti_join(stop_words) %>% 
    anti_join(cust_stop) %>%
    count(word, sort=TRUE) 
  
  df <- bind_rows(
    mutate(Q1,question="Q1"),
    mutate(Q2,question="Q2"),
    mutate(Q3,question="Q3"),
    mutate(Q4,question="Q4"),
    mutate(Q5,question="Q5")
  ) 
  
  df_dtm <- df %>%
    cast_dtm(question,word,n)
  df_LDA  <- LDA(df_dtm, k=2, control=list(seed=123)) 
  
  library(tidytext)
  
  df_topics <- tidy(df_LDA, matrix="beta") 
  df_topics
  library(ggplot2)
  library(dplyr)
  
  top_terms <- df_topics %>%
    group_by(topic) %>%
    top_n(10, beta) %>%
    ungroup() %>%
    arrange(topic, -beta) 
  top_terms
  
  
  top_terms %>%
    mutate(term=reorder(term, beta))%>%
    ggplot(aes(term, beta, fill = factor(topic))) +
    geom_col(show.legend=FALSE) +
    facet_wrap(~topic, scales = "free") +
    coord_flip() 
  
  
  beta_spread <- df_topics %>%
    mutate(topic=paste0("topic", topic)) %>%
    spread(topic, beta) %>%
    filter(topic1>.001 | topic2 >0.001) %>%
    mutate(log_rate = log2(topic2/topic1)) %>%  
    arrange(desc(log_rate))
  
  beta_spread
  
  my_gamma <- tidy(df_LDA,matrix="gamma")
  my_gamma
  
