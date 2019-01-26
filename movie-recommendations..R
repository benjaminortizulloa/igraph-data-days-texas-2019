library(tidyverse)
library(igraph)

movies <- read_csv('ml-latest-small/movies.csv')
ratings <- read_csv('ml-latest-small/ratings.csv')

#people -[like]-> movies
g <- ratings %>%
  left_join(movies) %>%
  select(userId, title, rating) %>%
  graph_from_data_frame()

person_of_interest <- '1'

p1 <- g %>%
  ego(nodes = person_of_interest) %>%
  .[[1]] %>%
  {g - V(g)[!name %in% .$name]}

#all movies person of interest likes
p1_movies <- head_of(p1, E(p1)) %>%
  unique %>%
  .$name

#Who are similar people?
p2 <- g %>% 
  {
    #how similar are nodes to our person of interest?
    simscore <- similarity(., mode = 'out')[V(.)$name == person_of_interest];
    V(.)$simscore <- simscore;
    .;
  } %>%
  #remove person of interest
  {. - V(.)[name == person_of_interest]} %>%
  #remove movies the person of interest already saw
  {. - V(.)[name %in% p1_movies]} %>%
  #remove people who are not similar to person of interest
  {
    raters <- tail_of(., E(.)) %>% unique
    q3 <- quantile(raters$simscore, .95)

    allnames <- ego(. , nodes = V(.)[simscore > q3]) %>%
      unlist %>%
      names %>%
      unique

    . - V(.)[!name %in% allnames]
  } %>%
  #Within this group, how s
  {
    #total rating / total connections
    rate_score <- strength(., weights = E(.)$rating)/degree(.);
    V(.)$rate_score <- rate_score;
    V(.)$degree <- degree(.)
    . - tail_of(., E(.));
  }

p2 %>% 
  as_data_frame('vertices') %>% 
  filter(degree > 5) %>% 
  arrange(desc(rate_score)) %>%
  head(10)

#let's get top movies 
p1_top <- p1 %>%
  {
    q3 <- quantile(E(.)$rating, .75);
    . - E(.)[rating < q3]
  } %>%
  head_of(E(.)) %>%
  .$name

recommend_movies <- function(g, person_of_interest){
  person_of_interest <- '1'
  
  p1 <- g %>%
    ego(nodes = person_of_interest) %>%
    .[[1]] %>%
    {g - V(g)[!name %in% .$name]}
  
  #all movies person of interest likes
  p1_movies <- head_of(p1, E(p1)) %>%
    unique %>%
    .$name
  
  #Who are similar people?
  p2 <- g %>% 
  {
    #how similar are nodes to our person of interest?
    simscore <- similarity(., mode = 'out')[V(.)$name == person_of_interest];
    V(.)$simscore <- simscore;
    .;
  } %>%
    #remove person of interest
  {. - V(.)[name == person_of_interest]} %>%
    #remove movies the person of interest already saw
  {. - V(.)[name %in% p1_movies]} %>%
    #remove people who are not similar to person of interest
  {
    raters <- tail_of(., E(.)) %>% unique
    q3 <- quantile(raters$simscore, .95)
    
    allnames <- ego(. , nodes = V(.)[simscore > q3]) %>%
      unlist %>%
      names %>%
      unique
    
    . - V(.)[!name %in% allnames]
  } %>%
    #Within this group, how s
  {
    #total rating / total connections
    rate_score <- strength(., weights = E(.)$rating)/degree(.);
    V(.)$rate_score <- rate_score;
    V(.)$degree <- degree(.)
    . - tail_of(., E(.));
  }
  
  p2 %>% 
    as_data_frame('vertices') %>% 
    filter(degree > 5) %>% 
    arrange(desc(rate_score)) 
}

p1_test <-recommend_movies((g - E(g)[person_of_interest %--% p1_top]), person_of_interest)

p1_top[p1_top %in% head(p1_test$name, 20)]
