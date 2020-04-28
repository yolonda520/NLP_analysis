library(textreadr)
library(tidyverse)
library(tidytext)
library(dplyr)
library(textdata)
library(igraph)
library(ggraph)
library(stringr)
library(ggplot2)
data(stop_words)

# Loading survey(doc)
setwd("C:/Users/qinyi/Desktop/Data Text/NLP")
survey_text <- read_docx(file="Survey_Dating App.docx")
#View(survey_text)

# Creating data frame
survey_df <- as.data.frame(matrix(nrow=42, ncol=5))
for(z in 1:5){
  for(i in 1:42){
    survey_df[i,z]<- survey_text[i*5+z-5]
  }
}
#View(survey_df)


# Splitting 5 questions
q1_df <- data_frame(line=42, text=survey_df$V1)
# q1_df2  <- data_frame(text=survey_df$V1)
#View(q1_df)
q2_df <- data_frame(line=42, text=survey_df$V2)
q3_df <- data_frame(line=42, text=survey_df$V3)
q4_df <- data_frame(line=42, text=survey_df$V4)
q5_df <- data_frame(line=42, text=survey_df$V5)

# Custom stopwords
# 1. Please, describe yourself in three sentences.
custom_stopwords01 <- data_frame(
  word=c("lot","person","likes","love","describe","3","people","I","am","like","i"),
  lexicon=rep("custom", each=11)
)
# 2. Describe your future or actual partner and where would you 
# like to go or where did you go for your first date?
custom_stopwords02 <- data_frame(
  word=c("partner","future","uh","person","date","day","meet","met","um","actual","hard","relationship","dream"),
  lexicon=rep("custom", each=13)
)
# 3. How would you describe your dream relationship?
custom_stopwords03 <- data_frame(
  word=c("relationship","dream","partner","yeah"),
  lexicon=rep("custom", each=4)
)
# 4. What is your criteria when looking for a partner?
custom_stopwords04 <- data_frame(
  word=c("criteria","partner","person","don't","guys","women","relationship","dream"),
  lexicon=rep("custom", each=8)
)
# 5. Would you be willing to try a new APP?
custom_stopwords05 <- data_frame(
  word=c("app","dating","apps","yeah","life","stand","feel","people","a"),
  lexicon=rep("custom", each=9)
)


# Tokenizing
q1_tokenized <- q1_df %>%
  unnest_tokens(word,text) %>%
  anti_join(stop_words) %>%
  anti_join(custom_stopwords01) %>%
  count(word, sort = T)
#View(q1_tokenized)
# q1_tokenized2 <- q1_df2 %>%
#   unnest_tokens(word,text) %>%
#   anti_join(stop_words) %>%
#   count(word, sort = T)
# View(q1_tokenized2)
q2_tokenized <- q2_df %>%
  unnest_tokens(word,text) %>%
  anti_join(stop_words) %>%
  anti_join(custom_stopwords02) %>%
  count(word, sort = T)
q3_tokenized <- q3_df %>%
  unnest_tokens(word,text) %>%
  anti_join(stop_words) %>%
  anti_join(custom_stopwords03) %>%
  count(word, sort = T)
q4_tokenized <- q4_df %>%
  unnest_tokens(word,text) %>%
  anti_join(stop_words) %>%
  anti_join(custom_stopwords04) %>%
  count(word, sort = T)
q5_tokenized <- q5_df %>%
  unnest_tokens(word,text) %>%
  anti_join(stop_words) %>%
  anti_join(custom_stopwords05) %>%
  count(word, sort = T)

# Creating my indexes for question
survey_questions <- bind_rows(
  mutate(q1_tokenized, question="Question 1"),
  mutate(q2_tokenized, question="Question 2"),
  mutate(q3_tokenized, question="Question 3"),
  mutate(q4_tokenized, question="Question 4"),
  mutate(q5_tokenized, question="Question 5"),
)
#View(survey_questions)



# Chart 01 : Total words per question
total_words_perq <- survey_questions %>%
  group_by(question) %>%
  summarize(total=sum(n))

frequency_totalwords <- total_words_perq %>%
  ggplot(aes(question,total,fill=question))+
  geom_col(show.legend = FALSE) 
frequency_totalwords 


########################
#Dashboard coding

shinyServer(function(input,output){
  
  #tab1 - problem statement
  
  output$text1 <- renderText(
    print("Yoyoyoyoyoyoyoyoyoyoyoyoyoy
          yoyoyoyoyoyoyoyoooooooooo
          yooooooooooooooooooooooooo
          yooooooooooooooooooooooooooo")
  )
  
  #tab2 - insight 1
  #frequency1
  
  output$plot4tab2 <- renderPlot({
   # replace wiht the right plot
    
    # total_words_perq <- survey_questions %>%
    #   group_by(question) %>%
    #   summarize(total=sum(n))
    # frequency_totalwords <- total_words_perq %>%
    #   ggplot(aes(question,total,fill=question))+
    #   geom_col(show.legend = FALSE) 
    # plot(frequency_totalwords) 
    
  })
  
  output$text2 <- renderText(
    print("Yoyoyoyoyoyoyoyoyoyoyoyoyoy
          yoyoyoyoyoyoyoyoooooooooo
          yooooooooooooooooooooooooo
          yooooooooooooooooooooooooo
          yooooooooooooooooooooooooooo")
  )
  
  #tab3 - insight2
  # Chart 02 : Top 5 (by n) words per question
  
  output$plot4tab3 <- renderPlot({
    
    survey_questions_frequency <- survey_questions %>%
      group_by(question) %>%
      top_n(5,n) %>%
      ungroup() %>%
      mutate(word = reorder(word,n)) %>%
      ggplot(aes(word,n, fill=question))+
      geom_col(show.legend = FALSE) +
      facet_wrap(~question, scales = "free_y")+
      coord_flip()
    plot(survey_questions_frequency)
    
  })
  
  output$text3 <- renderText(
    print("Yoyoyoyoyoyoyoyoyoyoyoyoyoy
          yoyoyoyoyoyoyoyoooooooooo
          yooooooooooooooooooooooooo
          yoyoyoyoyoyoyoyoooooooooo
          yooooooooooooooooooooooooo
          yooooooooooooooooooooooooooo")
  )
  
    #tab4 - insight3
    #chart - TBD
    
    output$plot4tab4 <- renderPlot({
      
      survey_questions_frequency <- survey_questions %>%
        group_by(question) %>%
        top_n(5,n) %>%
        ungroup() %>%
        mutate(word = reorder(word,n)) %>%
        ggplot(aes(word,n, fill=question))+
        geom_col(show.legend = FALSE) +
        facet_wrap(~question, scales = "free_y")+
        coord_flip()
      plot(survey_questions_frequency)
      
    })
    
    output$text4 <- renderText(
      print("Yoyoyoyoyoyoyoyoyoyoyoyoyoy
          yoyoyoyoyoyoyoyoooooooooo
          yooooooooooooooooooooooooo
          yoooooooooooooooooooooooooooYoyoyoyoyoyoyoyoyoyoyoyoyoy
          yoyoyoyoyoyoyoyoooooooooo
          yooooooooooooooooooooooooo
          yooooooooooooooooooooooooooo"))
      
      #tab5 - recommendations
      
      output$text5 <- renderText(
        print("Yoyoyoyoyoyoyoyoyoyoyoyoyoy
          yoyoyoyoyoyoyoyoooooooooo
          yooooooooooooooooooooooooo
          yoooooooooooooooooooooooooooYoyoyoyoyoyoyoyoyoyoyoyoyoy
          yoyoyoyoyoyoyoyoooooooooo
          yooooooooooooooooooooooooo
          yoooooooooooooooooooooooooooYoyoyoyoyoyoyoyoyoyoyoyoyoy
          yoyoyoyoyoyoyoyoooooooooo
          yooooooooooooooooooooooooo
          yooooooooooooooooooooooooooo"))
  
})

 






# source("~/Documents/MSBA/Text mining/Boss_shiny/NLP_bigrams.R")
# output$tf_idf <- renderPlot({
# df %>%
#   bind_tf_idf(bigram, question, n) %>%
#   arrange(desc(tf_idf)) %>%
#   mutate(bigram=factor(bigram, levels=rev(unique(bigram)))) %>% #rev() 一个向量逆转
#   group_by(question) %>%
#   top_n(3) %>%
#   ungroup %>%
#   ggplot(aes(bigram, tf_idf, fill=question))+
#   geom_col(show.legend=FALSE)+
#   labs(x=NULL, y="tf-idf")+
#   facet_wrap(~question, ncol=2, scales="free")+
#   coord_flip()
# })










