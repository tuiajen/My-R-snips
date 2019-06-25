# grad survey analysis

#https://www.datacamp.com/community/tutorials/sentiment-analysis-R

library(plyr)
library(dplyr) #Data manipulation (also included in the tidyverse package)
library(tidytext) #Text mining
library(tidyr) #Spread, separate, unite, text mining (also included in the tidyverse package)
library(widyr) #Use for pairwise correlation

#Visualizations!
library(ggplot2) #Visualizations (also included in the tidyverse package)
library(ggrepel) #`geom_label_repel`
library(gridExtra) #`grid.arrange()` for multi-graphs
library(knitr) #Create nicely formatted output tables
library(kableExtra) #Create nicely formatted output tables
library(formattable) #For the color_tile function
library(circlize) #Visualizations - chord diagram
library(memery) #Memes - images with plots
library(magick) #Memes - images with plots (image_read)
library(yarrr)  #Pirate plot
library(radarchart) #Visualizations
library(igraph) #ngram network diagrams
library(ggraph) #ngram network diagrams
library(wordcloud2)

#define as you see fit this was an example and so I'm using it 
undesirable_words <- c("prince", "chorus", "repeat", "lyrics", 
                       "theres", "bridge", "fe0f", "yeah", "baby", 
                       "alright", "wanna", "gonna", "chorus", "verse", 
                       "whoa", "gotta", "make", "miscellaneous", "2", 
                       "4", "ooh", "uurh", "pheromone", "poompoom", "3121", 
                       "matic", " ai ", " ca ", " la ", "hey", " na ", 
                       " da ", " uh ", " tin ", "  ll", "transcription",
                       "repeats")

#Define some colors to use throughout
my_colors <- c("#E69F00", "#56B4E9", "#009E73", "#CC79A7", "#D55E00", "#D65E00")

#Customize ggplot2's default theme settings
#This tutorial doesn't actually pass any parameters, but you may use it again in future tutorials so it's nice to have the options
theme_jen <- function(aticks = element_blank(),
                         pgminor = element_blank(),
                         lt = element_blank(),
                         lp = "none")
{
  theme(plot.title = element_text(hjust = 0.5), #Center the title
       # axis.ticks = aticks, #Set axis ticks to on or off
        panel.grid.minor = pgminor, #Turn the minor grid lines on or off
        legend.title = lt, #Turn the legend title on or off
        legend.position = lp) #Turn the legend on or off
}

#Customize the text tables for consistency using HTML formatting
my_kable_styling <- function(dat, caption) {
  kable(dat, "html", escape = FALSE, caption = caption) %>%
    kable_styling(bootstrap_options = c("striped", "condensed", "bordered"),
                  full_width = FALSE)
}

grad_data <- read.csv('grad_B89.csv', stringsAsFactors = FALSE)#, row.names = 1)


grad_data = within(grad_data, {
  ach_obj2 = ifelse(grad_data$ach_obj == "Strongly Agree", 5,
      ifelse(grad_data$ach_obj == "Agree", 4,
             ifelse(grad_data$ach_obj == "Neutral", 3,
                    ifelse(grad_data$ach_obj == "Disagree", 2,
                           ifelse(grad_data$ach_obj == "Strongly Disagree", 1, 0)))))
  
})

fix.contractions <- function(doc) {
  # "won't" is a special case as it does not expand to "wo not"
  doc <- gsub("won't", "will not", doc)
  doc <- gsub("can't", "can not", doc)
  doc <- gsub("n't", " not", doc)
  doc <- gsub("'ll", " will", doc)
  doc <- gsub("'re", " are", doc)
  doc <- gsub("'ve", " have", doc)
  doc <- gsub("'m", " am", doc)
  doc <- gsub("'d", " would", doc)
  # 's could be 'is' or could be possessive: it has no expansion
  doc <- gsub("'s", "", doc)
  return(doc)
}

# fix (expand) contractions
grad_data$comment <- sapply(grad_data$comment, fix.contractions)

#Step 2 remove special characters
# function to remove special characters
removeSpecialChars <- function(x) gsub("[^a-zA-Z0-9 ]", " ", x)
# remove special characters
grad_data$comment <- sapply(grad_data$comment, removeSpecialChars)

#set it to lowercase
# convert everything to lower case
grad_data$comment <- sapply(grad_data$comment, tolower)

glimpse(grad_data) #Transposed version of `print()`
sum(grad_data$comment != "")

#undesirable words already done
grad_tidy <- grad_data %>%
  unnest_tokens(word, comment) %>% #Break the lyrics into individual words
 # filter(!word %in% undesirable_words) %>% #Remove undesirables
  filter(!nchar(word) < 3) %>% #Words like "ah" or "oo" used in music
  anti_join(stop_words) #Data provided by the tidytext package

glimpse(grad_tidy) #From `dplyr`, better than `str()`.




#sentiments
new_sentiments <- sentiments %>% #From the tidytext package
  filter(lexicon != "loughran") %>% #Remove the finance lexicon
  mutate( sentiment = ifelse(lexicon == "AFINN" & score >= 0, "positive",
                             ifelse(lexicon == "AFINN" & score < 0,
                                    "negative", sentiment))) %>%
  group_by(lexicon) %>%
  mutate(words_in_lexicon = n_distinct(word)) %>%
  ungroup()

step_1 <- grad_data %>%
  group_by(ach_obj) %>% 
  summarise(my_cnt_no_record=n())

count_words_by_ID <- grad_tidy %>%
    group_by(myid, ach_obj) %>% 
    summarise(my_cnt=n()) 
    
#this gets the number of records with comments in original form
step_2 <- count_words_by_ID %>%
    group_by(ach_obj) %>%
    summarise(my_cnt_no_records_w_comm=n()) 

step_3 <- count_words_by_ID %>%
  group_by(ach_obj) %>%
  summarise(my_cnt_no_words = sum(my_cnt))
 
stat_table <- join_all(list(step_1,step_2,step_3), by='ach_obj', type='inner')

word_per_person <- stat_table %>%
  mutate(per_comm_pers = my_cnt_no_words/my_cnt_no_records_w_comm)%>%
  mutate(per_all_pers = my_cnt_no_words/my_cnt_no_record)
word_per_person

new_sentiments %>%
  group_by(lexicon, sentiment, words_in_lexicon) %>%
  summarise(distinct_words = n_distinct(word)) %>%
  ungroup() %>%
  spread(sentiment, distinct_words) %>%
  mutate(lexicon = color_tile("lightblue", "lightblue")(lexicon),
         words_in_lexicon = color_bar("lightpink")(words_in_lexicon)) %>%
  my_kable_styling(caption = "Word Counts Per Lexicon")

#common match
grad_tidy %>%
  mutate(words_in_comments = n_distinct(word)) %>%
  inner_join(new_sentiments) %>%
  group_by(lexicon, words_in_comments, words_in_lexicon) %>%
  summarise(lex_match_words = n_distinct(word)) %>%
  ungroup() %>%
  mutate(total_match_words = sum(lex_match_words), #Not used but good to have
         match_ratio = lex_match_words / words_in_comments) %>%
  select(lexicon, lex_match_words,  words_in_comments, match_ratio) %>%
  mutate(lex_match_words = color_bar("lightpink")(lex_match_words),
         lexicon = color_tile("lightgreen", "lightgreen")(lexicon)) %>%
  my_kable_styling(caption = "comments Found In Lexicons")



#research lexicon work; stemming lemmatization, word replacement for better match
#what is the difference between lexicon based sentiment and classifier based sentiment

#create some sentiment
grad_bing <- grad_tidy %>%
  inner_join(get_sentiments("bing"))
grad_nrc <- grad_tidy %>%
  inner_join(get_sentiments("nrc"))
grad_nrc_sub <- grad_tidy %>%
  inner_join(get_sentiments("nrc")) %>%
  filter(!sentiment %in% c("positive", "negative"))

#overall sentiment
nrc_plot <- grad_nrc %>%
  group_by(sentiment) %>%
  summarise(word_count = n()) %>%
  ungroup() %>%
  mutate(sentiment = reorder(sentiment, word_count)) %>%
  #Use `fill = -word_count` to make the larger bars darker
  ggplot(aes(sentiment, word_count, fill = -word_count)) +
  geom_col() +
  guides(fill = FALSE) + #Turn off the legend
  theme_jen() +
  labs(x = NULL, y = "Word Count") +
  scale_y_continuous(limits = c(0, 1300)) + #Hard code the axis limit
  ggtitle("Grad NRC Sentiment") +
  coord_flip()

nrc_plot #DO THIS
#numbers to go with above
grad_nrc %>% 
   group_by(sentiment) %>% 
    summarise(n=n())%>%
   arrange(desc(n))

#How many words are positive and negative
bing_plot <- grad_bing %>%
  group_by(sentiment) %>%
  summarise(word_count = n()) %>%
  ungroup() %>%
  mutate(sentiment = reorder(sentiment, word_count)) %>%
  ggplot(aes(sentiment, word_count, fill = sentiment)) +
  geom_col() +
  guides(fill = FALSE) +
  theme_jen() +
  labs(x = NULL, y = "Word Count") +
  scale_y_continuous(limits = c(0, 1300)) +
  ggtitle("Grad Bing Sentiment") +
  coord_flip()
bing_plot


#mood ring #DO THIS
grid.col = c("1" = my_colors[1], "2" = my_colors[2], "3" = my_colors[3], "4" = my_colors[4], "5" = my_colors[5], "anger" = "grey", "anticipation" = "grey", "disgust" = "grey", "fear" = "grey", "joy" = "grey", "sadness" = "grey", "surprise" = "grey", "trust" = "grey")

decade_mood <-  grad_nrc %>%
  filter(ach_obj != "NA" & !sentiment %in% c("positive", "negative")) %>%
  count(sentiment, ach_obj) %>%
  group_by(ach_obj, sentiment) %>%
  summarise(sentiment_sum = sum(n)) %>%
  ungroup()

circos.clear()
#Set the gap size
circos.par(gap.after = c(rep(5, length(unique(decade_mood[[1]])) - 1), 15,
                         rep(5, length(unique(decade_mood[[2]])) - 1), 15))
chordDiagram(decade_mood, grid.col = grid.col, transparency = .2)
title("Relationship Between Mood and Achieved Objective")

#this is good
#another chart
grad_nrc_sub %>%
  filter(ach_obj %in% c("Agree", "Disagree", "Neutral",
                     "Strongly Agree", "Strongly Disagree")) %>%
  count(ach_obj, sentiment) %>%
  mutate(sentiment = reorder(sentiment, n), ach_obj = reorder(ach_obj, n)) %>%
  ggplot(aes(sentiment, n, fill = sentiment)) +
  geom_col() +
 facet_wrap(~ach_obj, labeller = label_both) +
  theme_jen() +
  theme(panel.grid.major.x = element_blank())+
      #  axis.text.x = element_blank()) +
  labs(x = NULL, y = NULL) +
  ggtitle("NRC Sentiment Grad Analysis") +
  coord_flip()


#good chart maybe - all words aren't included
#Word sentiment 
plot_words_2 <- grad_nrc %>%
  filter(ach_obj == "Disagree") %>%
  group_by(sentiment) %>%
  count(word, sort = TRUE) %>%
  arrange(desc(n)) %>%
  slice(seq_len(10)) %>%
  ungroup()
#Same comments as previous graph
plot_words_2 %>%
  ggplot(aes(word, 1, label = word, fill = sentiment )) +
  geom_point(color = "transparent") +
  geom_label_repel(force = 1,nudge_y = .5,  
                   direction = "y",
                   box.padding = 0.05,
                   segment.color = "transparent",
                   size = 3) +
  facet_grid(~sentiment) +
  theme_jen() +
  theme(axis.text.y = element_blank(), axis.text.x = element_blank(),
        axis.title.x = element_text(size = 6),
        panel.grid = element_blank(), panel.background = element_blank(),
        panel.border = element_rect("lightgray", fill = NA),
        strip.text.x = element_text(size = 9)) +
  xlab(NULL) + ylab(NULL) +
  ggtitle("2 NRC Sentiment") +
  coord_flip()

#This is a good chart 
#bigrams per objective
grad_bigrams <- grad_data %>%
  unnest_tokens(bigram, comment, token = "ngrams", n = 2)

bigrams_separated <- grad_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word1 %in% undesirable_words) %>%
  filter(!word2 %in% undesirable_words)
#Because there is so much repetition in music, also filter out the cases where the two words are the same
bigram_decade <- bigrams_filtered %>%
  filter(word1 != word2) %>%
  filter(ach_obj != "NA") %>%
  unite(bigram, word1, word2, sep = " ") %>%
  inner_join(grad_data) %>%
  count(bigram, ach_obj, sort = TRUE) %>%
  group_by(ach_obj) %>%
  slice(seq_len(7)) %>%
  ungroup() %>%
  arrange(ach_obj, n) %>%
  mutate(row = row_number())

## Joining, by = c("song", "year", "album", "peak", "us_pop", "us_rnb", "decade", "chart_level", "charted")

bigram_decade %>%
  ggplot(aes(row, n, fill = ach_obj)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ach_obj, scales = "free_y") +
  xlab(NULL) + ylab(NULL) +
  scale_x_continuous(  # This handles replacement of row
    breaks = bigram_decade$row, # Notice need to reuse data frame
    labels = bigram_decade$bigram) +
  theme_jen() +
  theme(panel.grid.major.x = element_blank()) +
  ggtitle("Bigrams Per objective") +
  coord_flip()

grad_nrc_sub_2 <- grad_tidy %>%
  inner_join(get_sentiments("nrc")) %>%
  filter(!sentiment %in% c("positive", "negative"))

grad_avg_nrc <- grad_nrc_sub_2 %>%
  group_by(sentiment) %>%
  summarise(rating=mean(ach_obj2, na.rm=TRUE))

grad_avg_nrc

grad_avg_bing <- grad_bing %>%
  group_by(sentiment) %>%
  summarise(rating=mean(ach_obj2, na.rm=TRUE))
grad_avg_bing

grad_tidy %>%  
      group_by(word) %>%
      summarise(n=n()) %>%
      arrange(desc(n))

#I don't feel this is informative
pwc <- grad_tidy %>%
  filter(n() >= 7) %>%  #High counts
  pairwise_count(word, ach_obj, sort = TRUE) %>%
  filter(item1 %in% c("spscc", "program", "staff", "classes")) %>%
  group_by(item1) %>%
  slice(seq_len(7)) %>%
  ungroup() %>%
  mutate(row = -row_number()) #Descending order
pwc %>%
  ggplot(aes(row, n, fill = item1)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~item1, scales = "free") +
  scale_x_continuous(  #This handles replacement of row
    breaks = pwc$row, #Notice need to reuse data frame
    labels = pwc$item2) +
  theme_jen() + theme(panel.grid.major.x = element_blank()) +
  xlab(NULL) + ylab(NULL) +
  ggtitle("Pairwise Counts") +
  coord_flip()

#popular words by objective rating
popular_words <- grad_tidy %>% 
  group_by(ach_obj) %>%
  count(word, ach_obj, sort = TRUE) %>%
  slice(seq_len(8)) %>%
  ungroup() %>%
  arrange(ach_obj,n) %>%
  mutate(row = row_number()) 

popular_words %>%
  ggplot(aes(row, n, fill = ach_obj)) +
  geom_col(show.legend = NULL) +
  labs(x = NULL, y = "Objective Count") +
  ggtitle("Popular Words by Achieved Objective") + 
  theme_jen() +  
  facet_wrap(~ach_obj, scales = "free") +
  scale_x_continuous(  # This handles replacement of row 
    breaks = popular_words$row, # notice need to reuse data frame
    labels = popular_words$word) +
  coord_flip()

#important words
popular_tfidf_words <- grad_data %>%
  unnest_tokens(word, comment) %>%
  distinct() %>%
  filter(!word %in% undesirable_words) %>%
  anti_join(stop_words) %>%
  filter(nchar(word) > 3) %>%
  count(ach_obj, word, sort = TRUE) %>%
  ungroup() %>%
  bind_tf_idf(word, ach_obj, n)

head(popular_tfidf_words)


#important words
top_popular_tfidf_words <- popular_tfidf_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  group_by(ach_obj) %>% 
  slice(seq_len(8)) %>%
  ungroup() %>%
  arrange(ach_obj, tf_idf) %>%
  mutate(row = row_number())

top_popular_tfidf_words %>%
  ggplot(aes(x = row, tf_idf, 
             fill = ach_obj)) +
  geom_col(show.legend = NULL) +
  labs(x = NULL, y = "TF-IDF") + 
  ggtitle("Important Words using TF-IDF by Achievement Obj") +
  theme_jen() +  
  facet_wrap(~ach_obj, ncol = 3, scales = "free") +
  scale_x_continuous(  # This handles replacement of row 
    breaks = top_popular_tfidf_words$row, # notice need to reuse data frame
    labels = top_popular_tfidf_words$word) +
  coord_flip()

grad_words_counts <- grad_nrc %>%
  count(word, sort = TRUE) 

wordcloud2(grad_words_counts[1:50, ], size = .5)
