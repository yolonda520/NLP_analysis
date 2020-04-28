library(textreadr)
library(dplyr)
library(dplyr)
library(stringr)
library(tidytext)
library(ggplot2)
library(tidyverse)
library(reshape2)

serve <- read_document(file = "NLP_team6.txt")

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




######################################################




# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
source("combine_NLP.R")

shinyServer(function(input,output){
   output$mtcars <- renderTable({
    mtcars[,c("mpg",input$ngear)]
   })
   output$summ <-renderPrint({
     summary(mtcars[,c("mpg",input$ngear)])
    })
    output$plot <- renderPlot({
       with(mtcars,boxplot(mpg~gear))
    })
   
    output$plot4tab4 <- renderPlot({
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
      
    })
    
    output$text4 <- renderText(
    print("Yoyoyoyoyoyoyoyoyoyoyoyoyoy
            yoyoyoyoyoyoyoyoooooooooo
            yooooooooooooooooooooooooo
            yoooooooooooooooooooooooooooYoyoyoyoyoyoyoyoyoyoyoyoyoy
            yoyoyoyoyoyoyoyoooooooooo
            yooooooooooooooooooooooooo
            yooooooooooooooooooooooooooo"))
  })
      
    # output$downloadData <- downloadHandler(  #to download button
    #   filename = function(){
    #     paste("mtcars","csv",sep=".")
    #    }
    #    ,
    #     content = function(file){
    #    write.csv(mtcars(),file)
    #    }
    # )
    
    # output$downloadPlot <- downloadHandler(  #to download button
    #   filename = function(){
    #     paste("mtcars~plot","png",sep=".")
    #   }
    #   ,
    #   content = function(file){
    #     png(file)
    #     with(mtcars,boxplot(mpg~gear))  #location of the plot that we create in code above
    #     dev.off()
    #   }
    # )
    
    # output$histogram <- renderPlot({ 
    #     hist(faithful$eruptions,breaks = input$bins)#create this code after create box plot output histogram
    # })
    # output$approvedSales <- renderInfoBox({
    #   infoBox("Approval Sales","1,000,000",icon = icon("chart-line"))
    # })
    # output$itemRequested <- renderValueBox({
    #   valueBox(15*300,"Item Requested by Employees",icon =icon("fire"), color = "red")
    # })
    
    # output$plot4tab4 <- renderPlot({
    #   df %>%
    #     bind_tf_idf(bigram, question, n) %>%
    #     arrange(desc(tf_idf)) %>%
    #     mutate(bigram=factor(bigram, levels=rev(unique(bigram)))) %>% #rev() 一个向量逆转
    #     group_by(question) %>%
    #     top_n(2) %>%
    #     ungroup %>%
    #     ggplot(aes(bigram, tf_idf, fill=question))+
    #     geom_col(show.legend=FALSE)+
    #     labs(x=NULL, y="tf-idf")+
    #     facet_wrap(~question, ncol=2, scales="free")+
    #     coord_flip()
    # })
    
        # output$downloadPlot <- downloadHandler(  #to download button
        # filename = function(){
        # paste("plot","png",sep=".")},
        # content = function(file){
        # png(file)
        # with(nlp,boxplot(value~word))  #location of the plot that we create in code above
        # dev.off()})




