library(ggplot2)
library(dplyr)
library(dplyr)
library(tidyr)
library(DT)
library(reshape)
library(wordcloud)
library(tidytext)
library(stringr)
library(doParallel)
nrCores = detectCores()
cl <- makeCluster(nrCores)
registerDoParallel(cl)
clusterEvalQ(cl,library(RPostgreSQL))
clusterEvalQ(cl,library(DBI))

sapply(dbListConnections(drv), dbDisconnect)

clusterEvalQ(cl, {
  library(RPostgreSQL)
  drv<-dbDriver("PostgreSQL")
  dsn_database = "ingxoxo"            # e.g. "compose"
  dsn_hostname = "localhost"    # e.g.: "aws-us-east-1-portal.4.dblayer.com"
  dsn_port = "5432"                 # e.g. 11101 
  dsn_uid = "postgres"
  dsn_pwd = "" # pass
  con<-dbConnect(drv, user=dns_uid, password=dsn_pwd, dbname=sdn_database,host=dsn_hostname)
  dbGetQuery(con, "with all_main as (
                     SELECT
             t.created_at,
             t.id AS topic_id,
             t.user_id topic_creater,
             u.username,
             t.title topic_title,
             t.category_id,
             p.raw reply_raw,
             p.user_id post_reply_user_id,
             n.username  post_reply_username,
             y.gender,
             
             p.id,
             COUNT(p.id) AS reply_count,
             sum(p.reads) as reply_reads,
             sum(p.avg_time) as avg_time_reply,
             sum(p.like_count) reply_count_total,
             sum(typing_duration_msecs) post_reply_duration,
             sum(drafts_saved) post_reply_drafts_saved,
             sum(yearly_posts_count) yearly_posts_count,
             sum(yearly_views_count) yearly_views_count_post,
             sum(yearly_likes_count) yearly_likes_count_post,
             sum(daily_posts_count) daily_posts_count,
             sum(daily_likes_count) daily_likes_count
             
             FROM topics t
             JOIN posts p ON t.id = p.topic_id
             INNER JOIN users u ON
             t.user_id=u.id
             INNER JOIN users n
             ON t.user_id=n.id
             LEFT JOIN google_user_infos as y
             on y.user_id=p.user_id
             INNER JOIN post_stats ps
             ON ps.post_id=p.id
             LEFT JOIN top_topics tp
             ON tp.topic_id=t.id
             
             
             WHERE t.category_id = (SELECT id FROM
             categories WHERE name = 'Phases & Intermolecular forces')
             and
             t.archetype = 'regular'
             AND t.user_id > 0
             GROUP BY 1,2,3,4,5,6,7,8,9,10,11
             ORDER BY 2,5
             # COUNT(p.id) DESC, t.score DESC),
             mentions AS (
             SELECT target_topic_id, target_post_id, created_at
             FROM user_actions
             WHERE action_type = 7 -- mentions
             
             ), replies AS (
             SELECT target_topic_id, user_id,action_type, MAX(created_at) created_at
             FROM user_actions
             WHERE action_type = 5 -- replies
             
             GROUP BY 1,2,3  
             ), 
             replies_no_response as (
             SELECT DATE(m.created_at) mentioned_at,r.user_id reply_user_id, t.id as topic_id, target_post_id post_id
             FROM mentions m
             JOIN replies r ON r.target_topic_id = m.target_topic_id
             JOIN topics t ON t.id = m.target_topic_id
             WHERE m.created_at > r.created_at
             AND t.deleted_at IS NULL
             AND NOT t.archived
             AND NOT t.closed
             ORDER BY m.created_at DESC)
             select aa.*,
             bb.mentioned_at, bb.topic_id as topic_mentioned_no_response,bb.post_id as mentioned_post_no_response    
             FROM  all_main as aa
             LEFT JOIN replies_no_response as bb
             ON bb.topic_id=aa.topic_id
             AND bb.post_id=aa.id
             AND bb.reply_user_id=aa.post_reply_user_id")
  
  
})

sapply(dbListConnections(drv), dbDisconnect)


# Text Analysis
dat_inqxoxo<-read.csv("Ingxoxo_downloadSheet1.csv", stringsAsFactors = FALSE)
dat_try<- dat_inqxoxo %>% select(raw)
tidy_ftp  <-  dat_inqxoxo %>%
  unnest_tokens(word, raw)
tidy_ftp 
data(stop_words)
tidy_ftp <- tidy_ftp %>%
  anti_join(stop_words) 
# count(word, sort = TRUE) 

tidy_ftp %>%
  count(word, sort = TRUE) %>%
  filter(n > 5 ) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()


# Word Cloud
dat_try<- dat_inqxoxo %>% select(raw)
tidy_ftp  <-  dat_inqxoxo %>%
  unnest_tokens(word, raw)
 data(stop_words)
 tidy_ftp <- tidy_ftp %>%
  anti_join(stop_words) 
  count(word, sort = TRUE) 

tidy_ftp %>%
  count(word, sort = TRUE) %>%
  filter(n > 5 ) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

wordcloud(tidy_ftp,max.words = 200,random.color = TRUE,random.order=FALSE)


tidy_dat <- tidyr::gather(dat_try, key, word) %>% select(word)
tidy_dat$word %>% length()
unique(tidy_dat$word) %>% length()

tokens <- tidy_dat %>% 
  unnest_tokens(word, word) %>% 
  dplyr::count(word, sort = TRUE) %>% 
  ungroup()
tokens


  data("stop_words")
tokens_clean <- tokens %>%
  anti_join(stop_words)

nums <- tokens_clean  %>% filter(str_detect(word, "^[0-9]")) %>% select(word) %>% unique()

tokens_clean <- tokens_clean %>% 
  anti_join(nums, by = "word")


# remove unique stop words that snuck in there
uni_sw <- data.frame(word = c("al","figure","i.e", "l3"))

tokens_clean <- tokens_clean %>% 
  anti_join(uni_sw, by = "word")


# define a nice color palette
pal <- brewer.pal(8,"Dark2")

# plot the 50 most common words
tokens_clean %>% 
  with(wordcloud(word, n, random.order = FALSE, max.words = 50, colors=pal))


# Level Analysis

dat_inqxoxo_summary<-read.csv("Ingxoxo_downloadSheet1.csv")


data<- select(dat_inqxoxo_summary,
       title,
       post_admin,
       post_number,
       posts_count,
       Taxonomy_level,
       like_count,
       participant_count,
       reply_count
) %>%
  group_by(title,post_admin,post_number,Taxonomy_level) %>%
  summarise(cont_taxonomy=count(Taxonomy_level),
           like_count = sum(like_count),
            reply_count = sum( reply_count),
            posts_count = sum( posts_count)
            
            
            ) %>%
  as.data.frame()

data %>%
  DT::datatable()



data_raw<- select(dat_inqxoxo_summary,
              title,
              post_admin,
              post_number,
              posts_count,
              Taxonomy_level,
              like_count,
              participant_count,
              reply_count, 
              raw
) %>%
  group_by(title,post_admin,post_number,raw,Taxonomy_level) %>%
  summarise(like_count = sum(like_count),
            reply_count = sum( reply_count),
            posts_count = sum( posts_count)
            
            
  ) %>%
  as.data.frame()

data_raw %>%
  DT::datatable()




