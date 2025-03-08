library(tidyverse)
library(igraph)

tweets <- read_csv('russian-troll-tweets/tweets.csv')
users <- read_csv('russian-troll-tweets/users.csv')

tweet_hashtag <- tweets %>% 
  filter(hashtags != '[]') %>%
  select(user_key, hashtags, text) %>%
  mutate(hashtags = map(hashtags, jsonlite::fromJSON)) %>%
  unnest() 

tweet_hashtag_edges <- tweet_hashtag %>%
  select(-text) %>%
  count(user_key, hashtags, sort = T) %>%
  rename(weight = n) %>%
  mutate(type = 'used_hashtag',
         user_key = paste0('user_', user_key),
         hashtags = paste0('hashtag_', hashtags)
         ) 

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
  {. - E(.)[weight <= 5]} %>%
  {. - V(.)[degree(.) == 0]}

plot(tweet_hashtag_g_filtered, 
     vertex.label = '',
     edge.arrow.mode = '-')

tweet_hashtag_g_filtered_components <- components(tweet_hashtag_g_filtered)

names(tweet_hashtag_g_filtered_components)

tweet_hashtag_g_filtered_components %>% 
  groups() %>%
  map(head, n = 10)

# V(tweet_hashtag_g_filtered)$membership <- tweet_hashtag_g_filtered_components$membership

tweet_hashtag_g_filtered %>%
  {
    V(.)$type <- V(.)$type == 'user'; 
    V(.)$degree <- degree(.);
    V(.)$weighted_degree <- strength(.);
    .
  } %>%
  igraph::bipartite_projection() %>%
  {
    . <- map(., function(x){
      V(x)$component <- components(x)$membership;
      return(x)
    })
    hashtag_g <<- .$proj1;
    user_g <<- .$proj2;
  }

hashtag_communities_wt <- walktrap.community(hashtag_g)

hashtag_communities_wt %>% groups %>% map(function(grp){str_replace(grp, '^[^_]+_', '') %>% head(10)} )

tweet_hashtag %>%
  # filter(hashtags == 'Texit')
  # filter(hashtags == 'BTP')
  filter(hashtags == 'PodernFamily')

V(hashtag_g)$membership <- hashtag_communities_wt$membership

vertex_clr_palette <- rainbow(hashtag_communities_wt %>% groups %>% length())

hashtag_g %>%
  plot(
    vertex.color = map_chr(V(.)$membership, function(x){vertex_clr_palette[x]}),
    vertex.label = ''
  )

# hashtag_g_component <<- hashtag_g %>%
# {
#   comps <- V(.)$component %>%
#     unique()
#   g <- .
#   map(comps, function(i){
#     g %>%
#     {. - V(g)[component != i]}
#   })
# } 

# map(hashtag_g_component, function(g){str_replace(V(g)$name, '^[^_]+_', '') %>% head(10)})



# largest_hashtag_component <- hashtag_g_component[[1]]
# 
# set.seed(321)
# largest_hashtag_component_communities <- spinglass.community(largest_hashtag_component)
# 
# largest_hashtag_component_communities %>% groups

# tweet_hashtag <- tweets %>% 
#   filter(hashtags != '[]') %>%
#   select(user_key, hashtags, text) %>%
#   mutate(hashtags = map(hashtags, jsonlite::fromJSON)) %>%
#   unnest() 


# largest_hashtag_component_communities$membership

hashtag_community_summary <- hashtag_g %>%
  # {V(.)$community <- largest_hashtag_component_communities$membership; .} %>%
  {
    E(.)$tailCommunity <- tail_of(., E(.))$membership;
    E(.)$headCommunity <- head_of(., E(.))$membership;
    .
  }

hashtag_community_summary <- hashtag_community_summary %>%
  as_data_frame('both') %>%
  {
    el <- .$edges %>%
      filter(tailCommunity != headCommunity) %>%
      #1 - 6 is same as 6 - 1
      mutate(
        tail = map2_dbl(tailCommunity, headCommunity, min) %>% round %>% as.character(),
        head = map2_dbl(tailCommunity, headCommunity, max) %>% round %>% as.character()
      ) %>%
      group_by(tail, head) %>%
      nest() %>%
      mutate(data = map(data, function(x){
        top_hashes <- x %>%
          arrange(desc(weight)) %>%
          head(3) %>%
          {paste(.$from, .$to, sep = ' | ', collapse = '\n')} %>%
          str_replace_all('hashtag_', '')
        
        tibble(
          top_hashes,
          sum_weight = sum(x$weight),
          ave_weight = mean(x$weight),
          count = nrow(x)
        )
      })) %>% 
      unnest()
    
    nl <- .$vertices %>%
      mutate(membership = as.character(membership)) %>%
      group_by(membership) %>%
      nest() %>%
      mutate(top_hashes = map_chr(data, function(x){
        x %>%
          head %>%
          .$name %>%
          paste(collapse = '\n') %>%
          str_replace_all('hashtag_', '')
      }))
    
    set.seed(4321)
    graph_from_data_frame(el, F, nl) %>%
      {
        l <- layout_nicely(.)
        V(.)$x <- l[,1]
        V(.)$y <- l[,2]
        .
      }
  } 
hashtag_community_summary %>% 
  # remove.edge.attribute('weight') %>%
  plot(vertex.color = map_chr(V(.)$name, function(x){vertex_clr_palette[as.numeric(x)]}))

png('img/twitter_trolls_grouped_text.png', 800, 600)
set.seed(1)
hashtag_community_summary %>%
  plot(
    vertex.label = V(.)$top_hashes,
    vertex.label.cex = .75,
    vertex.shape = 'none',
    main = 'Twitter Troll Groups: Hashtags Common to Group') 
dev.off()

png('img/twitter_trolls_grouped_connection_text.png', 800, 600)
set.seed(1)
hashtag_community_summary %>%
  {. - V(.)[degree(.) == 0]} %>%
  plot(
    edge.label = E(.)$top_hashes,
    edge.label.cex = .7,
    vertex.size = 5,
    main = "Twitter Troll Groups: Hashtags Connecting two Groups"
  )
dev.off()

#=======================================#
#---- Text Analysis---------------------#
#=======================================#

library(tidytext)
library(stm)

tidy_tweets <- tweet_hashtag %>%
  select(-hashtags) %>%
  distinct() %>%
  mutate(text = str_replace_all(text, "&#x27;|&quot;|&#x2F;", "'"), ## weird encoding
         text = str_replace_all(text, "<a(.*?)>", " "),             ## links 
         text = str_replace_all(text, "&gt;|&lt;|&amp;", " "),      ## html yuck
         text = str_replace_all(text, "&#[:digit:]+;", " "),        ## html yuck
         text = str_remove_all(text, "<[^>]*>"),                    ## mmmmm, more html yuck
         text = str_remove_all(text, 'http\\S+')) %>%
  filter(!is.na(text)) %>%
  unnest_tokens(word, text, token = "tweets") %>%
  anti_join(get_stopwords()) %>%
  filter(!str_detect(word, "[0-9]+")) %>%
  filter(word != 'rt')%>%
  count(user_key, word, sort = T)

tweets %>% 
  # filter(user_key == 'ameliebaldwin') %>% 
  filter(user_key == 'newspeakdaily') %>%
  select(text) %>%
  filter(!str_detect(text, '^RT'))

# tweets_sparse <- tidy_tweets %>%
#   cast_sparse(user_key, word, n)
# 
# topic_model <- stm(tweets_sparse, K = ,
#                    verbose = FALSE, init.type = "Spectral")

topic_model <- read_rds('created_data/twitter_troll_tweets_users.rds')

topic_model

td_beta <- tidy(topic_model)

png('img/twitter_trolls_topic_model_beta_users.png', 800, 600)
td_beta %>%
  filter(term != 'rt') %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  mutate(topic = paste0("Topic ", topic)) %>%
  ggplot(aes(term, beta, fill = as.factor(topic))) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free_y") +
  coord_flip() +
  labs(x = NULL, y = expression(beta),
       title = "Grouping of Users: Highest word probabilities for each topic",
       subtitle = "Different words are associated with different topics")
dev.off()

td_gamma <- tidy(topic_model, matrix = "gamma",                    
                 document_names = rownames(tweets_sparse)) %>%
  arrange(document) 

categorize_users <- td_gamma %>%
  group_by(document) %>%
  top_n(1, gamma)

tidy_hash <- tweet_hashtag %>%
  select(-text) %>%
  count(user_key, hashtags, sort = T)

tweets_sparse_hash <- tidy_hash %>%
  cast_sparse(user_key, hashtags, n)

topic_model_hash <- stm(tweets_sparse_hash, K = 6,
                        verbose = FALSE, init.type = "Spectral")

write_rds(topic_model_hash, 'created_data/twitter_troll_tweets_user_hash.rds')

topic_model_hash

td_beta_hash <- tidy(topic_model_hash)

png('img/twitter_trolls_topic_model_beta_user_hash.png', 800, 600)
td_beta_hash %>%
  filter(term != 'rt') %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  mutate(topic = paste0("Topic ", topic)) %>%
  ggplot(aes(term, beta, fill = as.factor(topic))) +
  geom_col(alpha = 0.8, show.legend = FALSE, color = 'black') +
  facet_wrap(~ topic, scales = "free_y") +
  coord_flip() +
  labs(x = NULL, y = expression(beta),
       title = "Grouping of Hashtags: Highest word probabilities for each topic",
       subtitle = "Different words are associated with different topics") +
  scale_fill_brewer(type = 'qual') +
  theme_bw()
dev.off()

td_gamma_hash <- tidy(topic_model_hash, matrix = "gamma",
                      document_names = rownames(tweets_sparse_hash)) %>%
  arrange(document)

categorize_user <- td_gamma_hash %>%
  group_by(document) %>%
  top_n(1, gamma) %>%
  mutate(color = map_chr(topic, function(x){scales::brewer_pal('qual')(6)[x]})) %>%
  rename(name = document)

colored_user_g <- user_g %>% 
  as_data_frame('both') %>%
  {
    el <- .$edges %>%
      mutate(
        from = str_remove(from, '^user_'),
        to = str_remove(to, '^user_')
      ) 
    
    nl <- .$vertices %>%
      remove_rownames() %>%
      select(-color) %>%
      mutate(
        name = str_remove(name, '^user_')
      ) %>%
      left_join(categorize_user) 
    
    graph_from_data_frame(el, F, nl)
  }

plot(colored_user_g, vertex.label = '')

# tidy_tweets_hash <- tweet_hashtag %>%
#   select(-user_key) %>%
#   distinct() %>%
#   mutate(text = str_replace_all(text, "&#x27;|&quot;|&#x2F;", "'"), ## weird encoding
#          text = str_replace_all(text, "<a(.*?)>", " "),             ## links 
#          text = str_replace_all(text, "&gt;|&lt;|&amp;", " "),      ## html yuck
#          text = str_replace_all(text, "&#[:digit:]+;", " "),        ## html yuck
#          text = str_remove_all(text, "<[^>]*>"),                    ## mmmmm, more html yuck
#          text = str_remove_all(text, 'http\\S+')) %>%
#   filter(!is.na(text)) %>%
#   unnest_tokens(word, text, token = "tweets") %>%
#   anti_join(get_stopwords()) %>%
#   filter(!str_detect(word, "[0-9]+")) %>%
#   count(hashtags, word, sort = T) %>%
#   filter(!str_detect(word, '^#|^rt$'))
# 
# tweets_sparse_hash <- tidy_tweets_hash %>%
#   cast_sparse(hashtags, word, n)
# 
# topic_model_hash <- stm(tweets_sparse_hash, K = 6,
#                    verbose = FALSE, init.type = "Spectral")
# 
# write_rds(topic_model_hash, 'created_data/twitter_troll_tweets_hash.rds')
# 
# topic_model_hash
# 
# td_beta_hash <- tidy(topic_model_hash)

# png('img/twitter_trolls_topic_model_beta_hash.png', 800, 600)
# td_beta_hash %>%
#   filter(term != 'rt') %>%
#   group_by(topic) %>%
#   top_n(10, beta) %>%
#   ungroup() %>%
#   mutate(topic = paste0("Topic ", topic)) %>%
#   ggplot(aes(term, beta, fill = as.factor(topic))) +
#   geom_col(alpha = 0.8, show.legend = FALSE) +
#   facet_wrap(~ topic, scales = "free_y") +
#   coord_flip() +
#   labs(x = NULL, y = expression(beta),
#        title = "Grouping of Hashtags: Highest word probabilities for each topic",
#        subtitle = "Different words are associated with different topics")
# dev.off()
# 
# td_gamma_hash <- tidy(topic_model_hash, matrix = "gamma",
#                  document_names = rownames(tweets_sparse_hash)) %>%
#   arrange(document)
# 
# categorize_hash <- td_gamma_hash %>%
#   group_by(document) %>%
#   top_n(1, gamma)

