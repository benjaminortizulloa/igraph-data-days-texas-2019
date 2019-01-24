library(tidyverse)
library(igraph)

tweets <- read_csv('russian-troll-tweets/tweets.csv')
users <- read_csv('russian-troll-tweets/users.csv')

tweet_hashtag_edges <- tweets %>% 
  filter(hashtags != '[]') %>%
  select(user_key, hashtags) %>%
  mutate(hashtags = map(hashtags, jsonlite::fromJSON)) %>%
  unnest() %>%
  count(user_key, hashtags, sort = T) %>%
  rename(weight = n) %>%
  mutate(type = 'used_hashtag',
         user_key = paste0('user_', user_key),
         hashtags = paste0('hashtag_', hashtags)) 

tweet_hashtag_nodes <- bind_rows(
  tibble(
    label = tweet_hashtag_edges$user_key %>% unique(),
    type = 'user',
    color = 'purple',
    size = 3
  ),
  tibble(
    label = tweet_hashtag_edges$hashtags %>% unique(),
    type = 'hashtag',
    color = 'lightgrey',
    size = 3
  )
)

tweet_hashtag_g <- tweet_hashtag_edges %>%
  graph_from_data_frame(vertices = tweet_hashtag_nodes)

tweet_hashtag_g

summary(E(tweet_hashtag_g)$weight)

tweet_hashtag_g_filtered <- tweet_hashtag_g %>%
  {. - E(.)[weight <= 10]} %>%
  {. - V(.)[degree(.) == 0]}

plot(tweet_hashtag_g_filtered, 
     vertex.label = '',
     edge.arrow.mode = '-')

tweet_hashtag_g_filtered_components <- components(tweet_hashtag_g_filtered)

V(tweet_hashtag_g_filtered)$membership <- tweet_hashtag_g_filtered_components$membership

tweet_hashtag_g_filtered %>%
  {V(.)$type <- V(.)$type == 'user'; .} %>%
  igraph::bipartite_projection() %>%
  {
    . <- map(., function(x){
      V(x)$component <- components(x)$membership;
      return(x)
    })
    hashtag_g <<- .$proj1;
    user_g <<- .$proj2;
  }

hashtag_g_component <<- hashtag_g %>%
  {
    comps <- V(.)$component %>%
      unique()
    g <- .
    map(comps, function(i){
      g %>%
        {. - V(g)[component != i]}
    })
  }

map(hashtag_g_component, function(g){str_replace(V(g)$name, '^[^_]+_', '')})

hashtag_g_component[[1]] %>%
  spinglass.community()
