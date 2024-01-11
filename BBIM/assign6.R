## ----setup, include=FALSE----------------------------------------------------------------------------
knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)



## ----echo = TRUE-------------------------------------------------------------------------------------
## Load required packages ----
if(!require(pacman)){
        install.packages('pacman')
}

p_load(tidyverse, tm, textclean, 
       gt, janitor, ggthemes,
       kableExtra, SnowballC,
       RColorBrewer, wordcloud)

theme_set(ggthemes::theme_clean())
options(digits = 3)
options(scipen = 999)


## ----------------------------------------------------------------------------------------------------
## Load the data ----
chats <- read_csv('stack.csv') %>% 
        clean_names()


## ----------------------------------------------------------------------------------------------------
## Topics ----
counts <- chats %>% 
        count(topic, 
              sort = TRUE, 
              name = "Count")



## ----------------------------------------------------------------------------------------------------
## Prevalent topics table ----
counts %>% 
        set_names(names(.) %>% str_to_upper()) %>% 
        mutate(Prop = COUNT / nrow(chats)) %>% 
        gt(caption = "Prevalence of Topics")
        


## ----------------------------------------------------------------------------------------------------
## Prevalent topics ----
counts %>% 
        mutate(topic = fct_reorder(topic, Count)) %>% 
        ggplot(mapping = aes(x = topic, y = Count)) + 
        geom_col() + 
        labs(x = "", title = "Prevalence of Topics")


## ----------------------------------------------------------------------------------------------------
## Chats body overview ----
chats %>% 
        pull(body) %>% 
        head(5)


## ----------------------------------------------------------------------------------------------------
## text cleaning function using text clean ----
text_cleaner <- function(text){
        library(textclean)
        library(tidyverse)
        text %>% 
                replace_contraction() %>% 
                replace_date(replacement = "") %>% 
                replace_email() %>% 
                replace_emoji() %>% 
                replace_emoticon() %>% 
                replace_hash() %>% 
                replace_html() %>% 
                replace_internet_slang() %>% 
                replace_white() %>% 
                replace_number(remove = TRUE) %>% 
                replace_tag() %>% 
                replace_url() %>% 
                replace_word_elongation()
}


## ----------------------------------------------------------------------------------------------------
## Clean facebook data ----
clean_fb <- chats %>% 
        filter(topic == "Facebook") %>% 
        select(body) %>% 
        text_cleaner()


## ----------------------------------------------------------------------------------------------------
## Clean security data ----
clean_sec <- chats %>% 
        filter(topic == "Security") %>% 
        select(body) %>% 
        text_cleaner()


## ----------------------------------------------------------------------------------------------------
## Create a document term matrix for facebook topic ----
fb_dtm <- clean_fb %>% 
        VectorSource() %>% 
        Corpus() %>% 
        tm_map(removeWords, stopwords('english')) %>% 
        tm_map(removeNumbers) %>% 
        tm_map(removePunctuation) %>% 
        tm_map(tolower) %>% 
        tm_map(stripWhitespace) %>% 
        tm_map(stemDocument) %>% 
        TermDocumentMatrix()
        


## ----------------------------------------------------------------------------------------------------
## Create a document term matrix for security topic ----
sec_dtm <- clean_sec %>% 
        VectorSource() %>% 
        Corpus() %>% 
        tm_map(removeWords, stopwords('english')) %>% 
        tm_map(removeNumbers) %>% 
        tm_map(removePunctuation) %>% 
        tm_map(tolower) %>% 
        tm_map(stripWhitespace) %>% 
        tm_map(stemDocument) %>% 
        TermDocumentMatrix()


## ----------------------------------------------------------------------------------------------------
## Documents and term counts in facebook topic ----
fb_dtm


## ----------------------------------------------------------------------------------------------------
## Documents and term counts in security topic ----
sec_dtm


## ----------------------------------------------------------------------------------------------------
## Popular terms- facebook ----
fb_dtm %>% 
        findFreqTerms(lowfreq = 400) %>% 
        head(30)


## ----------------------------------------------------------------------------------------------------
## Popular terms- security ----
sec_dtm %>% 
        findFreqTerms(lowfreq = 400) %>% 
        head(30)


## ----fig.cap="WordCloud for the Facebook Topic"------------------------------------------------------
## Wordcloud- facebook ----
fb_df <- fb_dtm %>% 
        as.matrix() %>% 
        data.frame() %>% 
        set_names("Count") %>% 
        mutate(Term = row.names(.)) %>% 
        arrange(desc(Count))

## Word cloud for facebook topic ----
wordcloud(
        words = fb_df$Term,
        freq = fb_df$Count,
         min.freq=1, 
         max.words = 50, 
         random.order = FALSE, 
         colors=brewer.pal(7, "Set2")) 


## ----fig.cap="WordCloud for the Security Topic"------------------------------------------------------
## Wordcloud- security ----
sec_df <- sec_dtm %>% 
        as.matrix() %>% 
        data.frame() %>% 
        set_names("Count") %>% 
        mutate(Term = row.names(.)) %>% 
        arrange(desc(Count))

## Word cloud for security topic ----
wordcloud(
        words = sec_df$Term,
        freq = sec_df$Count,
         min.freq=1, 
         max.words = 50, 
         random.order = FALSE, 
         colors=brewer.pal(7, "Set2")) 

