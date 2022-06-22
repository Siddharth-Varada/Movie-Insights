library(rvest)
library(dplyr)
library(stringr)
library(writexl)
library(readxl)
library(tidytext)
library(tidyr)
library(ggplot2)

pre_movies <- data.frame()
post_movies <- data.frame()

for(page_result1 in seq(from = 1, to = 7351, by = 50)){
  pre_page <- "https://www.imdb.com/search/title/?title_type=feature&release_date=2018-01-01,2019-12-31&countries=us&sort=alpha,asc&start="
  pre <- read_html(paste0(pre_page, page_result1, "&ref_=adv_nxt"))
  movies1 <- pre %>% html_elements("div.lister-item")
  pre_titles <- movies1 %>% html_element(".lister-item-header a") %>% html_text()
  pre_year <- movies1 %>% html_element(".text-muted.unbold") %>% html_text()
  pre_genre <- movies1 %>% html_element(".genre") %>% html_text()
  pre_runtime <- movies1 %>% html_element(".runtime") %>% html_text()
  pre_rating <- movies1 %>% html_element(".ratings-imdb-rating strong") %>% html_text()
  pre_synopsis <- movies1 %>% html_element(".ratings-bar+ .text-muted") %>% html_text()
  pre_cast <- movies1 %>% html_element(".text-muted+ p") %>% html_text()
  pre_votes <- movies1 %>% html_element(".sort-num_votes-visible span:nth-child(2)") %>% html_text()
  pre_gross <- movies1 %>% html_element(".ghost~ .text-muted+ span") %>% html_text()
  pre_link <- movies1 %>% html_element(".ghost~ .text-muted+ span") %>% html_text()
  pre_movies <- rbind(pre_movies, data.frame(pre_titles, pre_year, pre_genre, pre_runtime, pre_rating, pre_synopsis, pre_cast, pre_votes, pre_gross))
}

for(page_result2 in seq(from = 1, to = 7851, by = 50)){
  post_page <- "https://www.imdb.com/search/title/?title_type=feature&release_date=2020-01-01,2021-12-31&countries=us&sort=alpha,asc&start="
  post <- read_html(paste0(post_page, page_result2, "&ref_=adv_nxt"))
  movies2 <- post %>% html_elements("div.lister-item")
  post_titles <- movies2 %>% html_element(".lister-item-header a") %>% html_text()
  post_year <- movies2 %>% html_element(".text-muted.unbold") %>% html_text()
  post_genre <- movies2 %>% html_element(".genre") %>% html_text()
  post_runtime <- movies2 %>% html_element(".runtime") %>% html_text()
  post_rating <- movies2 %>% html_element(".ratings-imdb-rating strong") %>% html_text()
  post_synopsis <- movies2 %>% html_element(".ratings-bar+ .text-muted") %>% html_text()
  post_cast <- movies2 %>% html_element(".text-muted+ p") %>% html_text()  
  post_votes <- movies2 %>% html_element(".sort-num_votes-visible span:nth-child(2)") %>% html_text()
  post_gross <- movies2 %>% html_element(".ghost~ .text-muted+ span") %>% html_text()
  post_movies <- rbind(post_movies, data.frame(post_titles, post_year, post_genre, post_runtime, post_rating, post_synopsis, post_cast, post_votes, post_gross))
}

colnames(pre_movies) <- c("title", "year", "genre", "runtime", "rating",  "summary", "cast", "votes", "gross")
colnames(post_movies) <- c("title", "year", "genre", "runtime", "rating",  "summary", "cast", "votes", "gross")

pre_movies$year <- as.character(gsub("(", "", pre_movies$year, fixed=TRUE))
pre_movies$year <- as.character(gsub(")", "", pre_movies$year, fixed=TRUE))
pre_movies$year <- as.numeric(pre_movies$year)
pre_movies$genre <- as.character(gsub("\n", "", pre_movies$genre, fixed=TRUE))
pre_movies$runtime <- as.character(gsub(" min", "", pre_movies$runtime, fixed=TRUE))
pre_movies$runtime <- as.numeric(pre_movies$runtime)
pre_movies$rating <- as.numeric(pre_movies$rating)
pre_movies$summary <- as.character(gsub("\n        Add a Plot\n", " ", pre_movies$summary))
pre_movies$summary <- as.character(gsub("See full summary", "", pre_movies$summary, fixed = TRUE))
pre_movies$summary <- as.character(gsub("»\n", "", pre_movies$summary, fixed = TRUE))
pre_movies$summary <- as.character(gsub("...", "", pre_movies$summary, fixed = TRUE))
pre_movies$summary <- as.character(gsub("\n", "", pre_movies$summary, fixed = TRUE))
pre_movies$summary <- as.character(gsub("  ", "", pre_movies$summary, fixed = TRUE))
pre_movies$votes <- as.character(gsub(",", "", pre_movies$votes, fixed=TRUE))
pre_movies$votes <- as.numeric(pre_movies$votes)
pre_movies$director <- as.character(gsub("\\|.*", "", pre_movies$cast)) 
pre_movies$director <- as.character(gsub("\n    Director:\n", "", pre_movies$director, fixed = TRUE))
pre_movies$director <- as.character(gsub("\n    Directors:\n", "", pre_movies$director, fixed = TRUE))
pre_movies$director <- as.character(gsub("\n", "", pre_movies$director, fixed = TRUE))
pre_movies$actor <- as.character(gsub(".*\\|", "", pre_movies$cast)) 
pre_movies$actor <- as.character(gsub(" \n    Star:\n", "", pre_movies$actor, fixed = TRUE))
pre_movies$actor <- as.character(gsub(" \n    Stars:\n", "", pre_movies$actor, fixed = TRUE))
pre_movies$actor <- as.character(gsub("\n", "", pre_movies$actor, fixed = TRUE))

post_movies$year <- as.character(gsub("(", "", post_movies$year, fixed=TRUE))
post_movies$year <- as.character(gsub(")", "", post_movies$year, fixed=TRUE))
post_movies$year <- as.numeric(post_movies$year)
post_movies$genre <- as.character(gsub("\n", "", post_movies$genre, fixed=TRUE))
post_movies$runtime <- as.character(gsub(" min", "", post_movies$runtime, fixed=TRUE))
post_movies$runtime <- as.numeric(post_movies$runtime)
post_movies$rating <- as.numeric(post_movies$rating)
post_movies$summary <- as.character(gsub("\n", "", post_movies$summary, fixed = TRUE))
post_movies$summary <- as.character(gsub("See full summary", "", post_movies$summary, fixed = TRUE))
post_movies$summary <- as.character(gsub("»\n", "", post_movies$summary, fixed = TRUE))
post_movies$summary <- as.character(gsub("...", "", post_movies$summary, fixed = TRUE))
post_movies$summary <- as.character(gsub("  ", "", post_movies$summary, fixed = TRUE))
post_movies$votes <- as.character(gsub(",", "", post_movies$votes, fixed=TRUE))
post_movies$votes <- as.numeric(post_movies$votes)
post_movies$director <- as.character(gsub("\\|.*", "", post_movies$cast)) 
post_movies$director <- as.character(gsub("\n    Director:\n", "", post_movies$director, fixed = TRUE))
post_movies$director <- as.character(gsub("\n    Directors:\n", "", post_movies$director, fixed = TRUE))
post_movies$director <- as.character(gsub("\n", "", post_movies$director, fixed = TRUE))
post_movies$actor <- as.character(gsub(".*\\|", "", post_movies$cast)) 
post_movies$actor <- as.character(gsub(" \n    Star:\n", "", post_movies$actor, fixed = TRUE))
post_movies$actor <- as.character(gsub(" \n    Stars:\n", "", post_movies$actor, fixed = TRUE))
post_movies$actor <- as.character(gsub("\n", "", post_movies$actor, fixed = TRUE))


for(i in 1:nrow(pre_movies))
{
  if(grepl("Director", pre_movies$actor[i], ignore.case=TRUE)==TRUE)
  {
    pre_movies$actor[i] <- ""
  }
  else if(grepl("Directors", pre_movies$actor[i], ignore.case=TRUE)==TRUE)
  {
    pre_movies$actor[i] <- ""
  }
  else
  {
    pre_movies$actor[i] <- pre_movies$actor[i]
  }
}

for(i in 1:nrow(post_movies))
{
  if(grepl("Director", post_movies$actor[i], ignore.case=TRUE)==TRUE)
  {
    post_movies$actor[i] <- ""
  }
  else if(grepl("Directors", post_movies$actor[i], ignore.case=TRUE)==TRUE)
  {
    post_movies$actor[i] <- ""
  }
  else
  {
    post_movies$actor[i] <- post_movies$actor[i]
  }
}

for(i in 1:nrow(pre_movies))
{
  if(grepl("Star", pre_movies$director[i], ignore.case=TRUE)==TRUE)
  {
    pre_movies$director[i] <- ""
  }
  else if(grepl("Stars", pre_movies$director[i], ignore.case=TRUE)==TRUE)
  {
    pre_movies$director[i] <- ""
  }
  else
  {
    pre_movies$director[i] <- pre_movies$director[i]
  }
}

for(i in 1:nrow(post_movies))
{
  if(grepl("Star", post_movies$director[i], ignore.case=TRUE)==TRUE)
  {
    post_movies$director[i] <- ""
  }
  else if(grepl("Stars", post_movies$director[i], ignore.case=TRUE)==TRUE)
  {
    post_movies$director[i] <- ""
  }
  else
  {
    post_movies$director[i] <- post_movies$director[i]
  }
}

pre_movies$actor <- as.character(gsub("   ", "", pre_movies$actor, fixed = TRUE))
post_movies$actor <- as.character(gsub("   ", "", post_movies$actor, fixed = TRUE))
pre_movies$director <- as.character(gsub("   ", "", pre_movies$director, fixed = TRUE))
post_movies$director <- as.character(gsub("   ", "", post_movies$director, fixed = TRUE))

writexl::write_xlsx(pre_movies, "C:/Users/Siddharth Varada/OneDrive - Hult Students/Project/final//pre_pandemic.xlsx")
writexl::write_xlsx(post_movies, "C:/Users/Siddharth Varada/OneDrive - Hult Students/Project/final//post_pandemic.xlsx")
prepan_movies <- read_excel("C:/Users/Siddharth Varada/OneDrive - Hult Students/Project/final//pre_pandemic.xlsx")
postpan_movies <- read_excel("C:/Users/Siddharth Varada/OneDrive - Hult Students/Project/final///post_pandemic.xlsx")

#pre_summary_blob <- paste(prepan_movies[,6], sep=" ", collapse = " ")
#post_summary_blob <- paste(postpan_movies[,6], sep=" ", collapse = " ")
#pre_director_blob <- paste(prepan_movies[,10], sep=" ", collapse = " ")
#post_director_blob <- paste(postpan_movies[,10], sep=" ", collapse = " ")
#pre_actor_blob <- paste(prepan_movies[,11], sep=" ", collapse = " ")
#post_actor_blob <- paste(postpan_movies[,11], sep=" ", collapse = " ")

all_movies <- rbind(prepan_movies, postpan_movies)
#analysis_blob <- data.frame(pre_summary_blob, post_summary_blob, pre_director_blob, post_director_blob, pre_actor_blob, post_actor_blob)
head(all_movies)
#############################################
###### N-grams and tokenizing ###############
#############################################

prepan_bigrams <- prepan_movies %>%
  #filter(genre == "Comedy, Romance") %>%
  unnest_tokens(bigram, summary, token = "ngrams", n=2)

prepan_bigrams #We want to see the bigrams (words that appear together, "pairs")

prepan_bigrams %>%
  count(bigram, sort = TRUE) #this has many stop words, need to remove them 

#to remove stop words from the bigram data, we need to use the separate function:
prepan_separated <- prepan_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

prepan_filtered <- prepan_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

#creating the new bigram, "no-stop-words":
prepan_counts <- prepan_filtered %>%
  count(word1, word2, sort = TRUE)
#want to see the new bigrams
prepan_counts

###########################################################
###### We can also apply the tf_idf framework  ############
########### on our bigram and quadro-gram #################
###########################################################

prepan_united <- prepan_filtered %>%
  unite(bigram, word1, word2, sep=" ") #we need to unite what we split in the previous section

prepan_tf_idf <- prepan_united %>%
  count(year, bigram) %>%
  bind_tf_idf(bigram, year, n) %>%
  arrange(desc(tf_idf))

prepan_tf_idf

library(igraph)
prepan_graph <- prepan_counts %>%
  filter(n>10) %>%
  graph_from_data_frame()

prepan_graph

#install.packages("ggraph")
library(ggraph)
ggraph(prepan_graph, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =1, hjust=1)

postpan_bigrams <- postpan_movies %>%
  #filter(genre == "Comedy, Romance") %>%
  unnest_tokens(bigram, summary, token = "ngrams", n=2)

postpan_bigrams #We want to see the bigrams (words that appear together, "pairs")

postpan_bigrams %>%
  count(bigram, sort = TRUE) #this has many stop words, need to remove them 

#to remove stop words from the bigram data, we need to use the separate function:
postpan_separated <- postpan_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

postpan_filtered <- postpan_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

#creating the new bigram, "no-stop-words":
postpan_counts <- postpan_filtered %>%
  count(word1, word2, sort = TRUE)
#want to see the new bigrams
postpan_counts

###########################################################
###### We can also apply the tf_idf framework  ############
########### on our bigram and quadro-gram #################
###########################################################

postpan_united <- postpan_filtered %>%
  unite(bigram, word1, word2, sep=" ") #we need to unite what we split in the previous section

postpan_tf_idf <- postpan_united %>%
  count(year, bigram) %>%
  bind_tf_idf(bigram, year, n) %>%
  arrange(desc(tf_idf))

postpan_tf_idf

library(igraph)
postpan_graph <- postpan_counts %>%
  filter(n>10) %>%
  graph_from_data_frame()

postpan_graph

#install.packages("ggraph")
library(ggraph)
ggraph(postpan_graph, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =1, hjust=1)


#Correlation between runtime and rating
cor.test(prepan_movies$rating, prepan_movies$runtime, use = "complete.obs", method="pearson")
cor.test(postpan_movies$rating, postpan_movies$runtime, use = "complete.obs", method="pearson")

movies_runtime <- data.frame()
movies_runtime <- all_movies[complete.cases(all_movies$runtime),]

short_bigrams <- movies_runtime %>%
  filter(runtime <= 90) %>%
  unnest_tokens(bigram, summary, token = "ngrams", n=2)

short_bigrams #We want to see the bigrams (words that appear together, "pairs")

short_bigrams %>%
  count(bigram, sort = TRUE) #this has many stop words, need to remove them 

#to remove stop words from the bigram data, we need to use the separate function:
short_separated <- short_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

short_filtered <- short_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

#creating the new bigram, "no-stop-words":
short_counts <- short_filtered %>%
  count(word1, word2, sort = TRUE)
#want to see the new bigrams
short_counts

###########################################################
###### We can also apply the tf_idf framework  ############
########### on our bigram and quadro-gram #################
###########################################################

short_united <- short_filtered %>%
  unite(bigram, word1, word2, sep=" ") #we need to unite what we split in the previous section

short_tf_idf <- short_united %>%
  count(year, bigram) %>%
  bind_tf_idf(bigram, year, n) %>%
  arrange(desc(tf_idf))

short_tf_idf

######################################################
####### VISUALISING A BIGRAM NETWORK #################
######################################################
library(igraph)
short_graph <- short_counts %>%
  filter(n>10) %>%
  graph_from_data_frame()

short_graph

#install.packages("ggraph")
library(ggraph)
ggraph(short_graph, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =1, hjust=1)

###########################################################
###########################################################
###########################################################
long_bigrams <- movies_runtime %>%
  filter(runtime > 90) %>%
  unnest_tokens(bigram, summary, token = "ngrams", n=2)

long_bigrams #We want to see the bigrams (words that appear together, "pairs")

long_bigrams %>%
  count(bigram, sort = TRUE) #this has many stop words, need to remove them 

#to remove stop words from the bigram data, we need to use the separate function:
long_separated <- long_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

long_filtered <- long_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

#creating the new bigram, "no-stop-words":
long_counts <- long_filtered %>%
  count(word1, word2, sort = TRUE)
#want to see the new bigrams
long_counts

###########################################################
###### We can also apply the tf_idf framework  ############
########### on our bigram and quadro-gram #################
###########################################################

long_united <- long_filtered %>%
  unite(bigram, word1, word2, sep=" ") #we need to unite what we split in the previous section

long_tf_idf <- long_united %>%
  count(year, bigram) %>%
  bind_tf_idf(bigram, year, n) %>%
  arrange(desc(tf_idf))

long_tf_idf

######################################################
####### VISUALISING A BIGRAM NETWORK #################
######################################################
library(igraph)
long_graph <- long_counts %>%
  filter(n>10) %>%
  graph_from_data_frame()

long_graph

#install.packages("ggraph")
library(ggraph)
ggraph(long_graph, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =1, hjust=1)

movies_rating <- data.frame()
movies_rating <- all_movies[complete.cases(all_movies$rating),]
#low_movies <- data.frame()
#high_movies <- data.frame()
#high_movies <- movies_rating[which(movies_rating$rating > 8),] 
#low_movies <- movies_rating[which(movies_rating$rating < 3),] 

low_bigrams <- movies_rating %>%
  filter(rating < 3) %>%
  unnest_tokens(bigram, summary, token = "ngrams", n=2)

low_bigrams #We want to see the bigrams (words that appear together, "pairs")

low_bigrams %>%
  count(bigram, sort = TRUE) #this has many stop words, need to remove them 

#to remove stop words from the bigram data, we need to use the separate function:
low_separated <- low_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

low_filtered <- low_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

#creating the new bigram, "no-stop-words":
low_counts <- low_filtered %>%
  count(word1, word2, sort = TRUE)
#want to see the new bigrams
low_counts

###########################################################
###### We can also apply the tf_idf framework  ############
########### on our bigram and quadro-gram #################
###########################################################

low_united <- low_filtered %>%
  unite(bigram, word1, word2, sep=" ") #we need to unite what we split in the previous section

low_tf_idf <- low_united %>%
  count(year, bigram) %>%
  bind_tf_idf(bigram, year, n) %>%
  arrange(desc(tf_idf))

low_tf_idf

######################################################
####### VISUALISING A BIGRAM NETWORK #################
######################################################
library(igraph)
low_graph <- low_counts %>%
  filter(n>2) %>%
  graph_from_data_frame()

low_graph

#install.packages("ggraph")
library(ggraph)
ggraph(low_graph, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =1, hjust=1)

high_bigrams <- movies_rating %>%
  filter(rating > 8) %>%
  unnest_tokens(bigram, summary, token = "ngrams", n=2)

high_bigrams #We want to see the bigrams (words that appear together, "pairs")

high_bigrams %>%
  count(bigram, sort = TRUE) #this has many stop words, need to remove them 

#to remove stop words from the bigram data, we need to use the separate function:
high_separated <- high_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

high_filtered <- high_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

#creating the new bigram, "no-stop-words":
high_counts <- high_filtered %>%
  count(word1, word2, sort = TRUE)
#want to see the new bigrams
high_counts

###########################################################
###### We can also apply the tf_idf framework  ############
########### on our bigram and quadro-gram #################
###########################################################

high_united <- high_filtered %>%
  unite(bigram, word1, word2, sep=" ") #we need to unite what we split in the previous section

high_tf_idf <- high_united %>%
  count(year, bigram) %>%
  bind_tf_idf(bigram, year, n) %>%
  arrange(desc(tf_idf))

high_tf_idf

######################################################
####### VISUALISING A BIGRAM NETWORK #################
######################################################
library(igraph)
high_graph <- high_counts %>%
  filter(n>2) %>%
  graph_from_data_frame()

high_graph

#install.packages("ggraph")
library(ggraph)
ggraph(high_graph, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =1, hjust=1)

#index1 <- sample(1:nrow(prepan_movies), size=0.8*nrow(prepan_movies))
#index2 <- sample(1:nrow(postpan_movies), size=0.8*nrow(postpan_movies))

#prepan_train <- prepan_movies[index1,]
#prepan_test <- prepan_movies[-index1,]
#postpan_train <- postpan_movies[index2,]
#postpan_test <- postpan_movies[-index2,]

## building predictive linear regression
#prepan_movies$genre1 <- prepan_movies$genre[which(prepan_movies$genre == "Comedy" || prepan_movies$genre == "Comedy, Romance" || prepan_movies$genre == "Drama"),]
options(max.print = nrow(postpan_movies))
my_linear1 <- lm(formula=rating~genre, data=prepan_movies)
summary(my_linear1)
#ggplot(data = prepan_movies, aes(x=rating,y=genre))+geom_point()
#abline(lm(formula=rating~genre, data=prepan_movies))

options(max.print = nrow(postpan_movies))
my_linear2 <- lm(formula=rating~genre, data=postpan_movies)
summary(my_linear2)
sum1 <- 0
sum2 <- 0
for(i in 1:nrow(prepan_movies))
{
  if(is.na(prepan_movies$genre[i]))
  {
    sum1 <- sum1
  }
  else if(prepan_movies$genre[i] == "Comedy, Romance")
  {
    sum1 <- sum1+1
  }
  else
  {
    sum1 <- sum1
  }
}

for(i in 1:nrow(postpan_movies))
{
  if(is.na(postpan_movies$genre[i]))
  {
    sum2 <- sum2
  }
  else if(postpan_movies$genre[i] == "Comedy, Romance")
  {
    sum2 <- sum2+1
  }
  else
  {
    sum2 <- sum2
  }
}

#############################################
###### N-grams and tokenizing ###############
#############################################

prepan_actors <- prepan_movies %>%
  #filter(genre == "Comedy, Romance") %>%
  unnest_tokens(bigram2, actor, token = "ngrams", n=2)

prepan_actors #We want to see the bigrams (words that appear together, "pairs")

prepan_actors %>%
  count(bigram2, sort = TRUE) #this has many stop words, need to remove them 

#############################################
###### N-grams and tokenizing ###############
#############################################

postpan_actors <- postpan_movies %>%
  #filter(genre == "Comedy, Romance") %>%
  unnest_tokens(bigram3, actor, token = "ngrams", n=2)

postpan_actors #We want to see the bigrams (words that appear together, "pairs")

postpan_actors %>%
  count(bigram3, sort = TRUE) #this has many stop words, need to remove them 

#############################################
###### N-grams and tokenizing ###############
#############################################

prepan_directors <- prepan_movies %>%
  #filter(genre == "Comedy, Romance") %>%
  unnest_tokens(bigram4, director, token = "ngrams", n=2)

prepan_directors #We want to see the bigrams (words that appear together, "pairs")

prepan_directors %>%
  count(bigram4, sort = TRUE) #this has many stop words, need to remove them 

#############################################
###### N-grams and tokenizing ###############
#############################################

postpan_directors <- postpan_movies %>%
  #filter(genre == "Comedy, Romance") %>%
  unnest_tokens(bigram5, director, token = "ngrams", n=2)

postpan_directors #We want to see the bigrams (words that appear together, "pairs")

postpan_directors %>%
  count(bigram5, sort = TRUE) #this has many stop words, need to remove them 


prepan_comedy <- prepan_movies %>%
  filter(genre == "Comedy")
prepan_romcom <- prepan_movies %>%
  filter(genre == "Comedy, Romance")
prepan_drama <- prepan_movies %>%
  filter(genre == "Drama")
postpan_comedy <- postpan_movies %>%
  filter(genre == "Comedy")
postpan_romcom <- postpan_movies %>%
  filter(genre == "Comedy, Romance")
postpan_drama <- postpan_movies %>%
  filter(genre == "Drama")
prepan_romance <- prepan_movies %>%
  filter(genre=="Romance")
postpan_romance <- postpan_movies %>%
  filter(genre=="Romance")
prepan_scifi <- prepan_movies %>%
  filter(genre=="Sci-Fi")
postpan_scifi <- postpan_movies %>%
  filter(genre=="Sci-Fi")