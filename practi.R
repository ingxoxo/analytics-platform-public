

library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(DT)
library(reshape)
library(wordcloud)
library(tidytext)
library(RPostgreSQL)
library(plotlyGeoAssets)
library(stringr)
library(wordcloud2)
library(stopwords)
library(tidyverse)
library(wordcloud)
library(tm)
library(RColorBrewer)
library(igraph)
library(radarchart)
library(gridExtra)
library(circlize)
library(formattable)



dsn_database = "ingxoxo_test"            # e.g. "compose"
dsn_hostname = "localhost"    # e.g.: "aws-us-east-1-portal.4.dblayer.com"
dsn_port = ""                 # e.g. 11101 
dsn_uid = ""        
dsn_pwd = "" 

tryCatch({
  sapply(dbListConnections(drv), dbDisconnect)
  drv <- dbDriver("PostgreSQL")
  print("Connecting to database")
  conn <- dbConnect(drv, 
                    dbname = dsn_database,
                    host = dsn_hostname, 
                    port = dsn_port,
                    user = dsn_uid, 
                    password = dsn_pwd)
  print("Connected!")
  
},
error=function(cond) {
  print("Unable to connect to database.")
})



participation <- dbGetQuery(conn, "
  SELECT  CASE WHEN admin='true' THEN 'administrator'
                                      WHEN admin='false' AND moderator='false' THEN 'Student' 
                                      WHEN admin='false' AND moderator='true' THEN 'Moderator'
                                      END platform_users, COUNT(*) Total FROM users
                                      WHERE id>1
                                      GROUP BY 1
                                      
                                      
                                      ")



data_out_2<-dbGetQuery(conn, 
                                 "
                                 WITH platform_users AS  (SELECT DISTINCT id, CASE WHEN admin='true' THEN 'administrator'
                                 WHEN admin='false' AND moderator='false' THEN 'Student' 
                                 WHEN admin='false' AND moderator='true' THEN 'Moderator'
                                 END platform_usertype
                                 FROM users
                                 WHERE id>1), all_main AS (
                                 SELECT
                                 t.created_at,
                                 t.id AS topic_id,
                                 t.user_id title_creator,
                                 u.username topic_creator,
                                 t.title topic_title,
                                 t.category_id,
                                 p.raw reply_raw,
                                 p.id post_reply_id,
                                 p.user_id post_reply_user_id,
                                 n.username  post_reply_username,
                                 y.gender,
                                 
                                 p.id,
                                 COUNT(p.id) AS reply_count,
                                 sum(p.reads) AS reply_reads,
                                 sum(p.avg_time) AS avg_time_reply,
                                 sum(p.like_count) reply_count_total,
                                 sum(typing_duration_msecs) post_reply_duration,
                                 sum(drafts_saved) post_reply_drafts_saved,
                                 sum(yearly_posts_count) yearly_posts_count,
                                 sum(yearly_views_count) yearly_views_count_post,
                                 sum(yearly_likes_count) yearly_likes_count_post,
                                 sum(daily_posts_count) daily_posts_count,
                                 sum(daily_likes_count) daily_likes_count,
                                 sum(p.word_count) post_word_count,
                                 sum(t.views) topic_views
                                 
                                 FROM topics t
                                 JOIN posts p ON t.id = p.topic_id
                                 INNER JOIN users u ON
                                 t.user_id=u.id
                                 INNER JOIN users n
                                 ON t.user_id=n.id
                                 LEFT JOIN google_user_infos AS y
                                 ON y.user_id=p.user_id
                                 INNER JOIN post_stats ps
                                 ON ps.post_id=p.id
                                 LEFT JOIN top_topics tp
                                 ON tp.topic_id=t.id
                                 
                                 
                                 WHERE t.category_id = (SELECT id FROM
                                 categories WHERE name = 'Phases & Intermolecular forces')
                                 AND
                                 t.archetype = 'regular'
                                 AND t.user_id > 0
                                 AND p.deleted_at IS NULL
                                 GROUP BY 1,2,3,4,5,6,7,8,9,10,11,12
                                 ORDER BY 2 DESC,8 DESC
                                 
                                 ),
                                 mentions AS (
                                 SELECT target_topic_id, target_post_id, created_at
                                 FROM user_actions
                                 WHERE action_type = 7 -- mentions
                                 
                                 ),replies AS (
                                 SELECT target_topic_id,target_post_id, user_id,acting_user_id,action_type, a.created_at topic_created_at,b.username,b.admin,b.moderator,
                                 CASE WHEN b.admin='true' THEN 'administrator'
                                 WHEN b.admin='false' AND b.moderator='false' THEN 'Student' 
                                 WHEN b.admin='false' AND b.moderator='true' THEN 'Moderator'
                                 ELSE 'postcreator'
                                 END post_usertype
                                 
                                 FROM user_actions a 
                                 LEFT JOIN users b ON 
                                 b.id=a.user_id
                                 WHERE action_type = 5 
                                 
                                 GROUP BY 1,2,3,4,5,6,7,8,9 ), analy AS (
                                 SELECT * FROM 
                                 all_main a
                                 LEFT JOIN replies b
                                 ON b.target_topic_id=a.topic_id
                                 AND a.id=b.target_post_id
                                 AND b.user_id=a.post_reply_user_id
                                 
                                 WHERE
                                 --   a.topic_id=124 AND 
                                 a.reply_raw!= '(post withdrawn by author, will be automatically deleted IN 24 hours unless flagged)'
                                 ORDER BY 2 ASC,8 ASC)
                                 SELECT  a.platform_usertype,b.* 
                                 FROM platform_users a
                                 INNER JOIN analy b 
                                 ON a.id=b.title_creator
                                 ORDER BY 3 ASC,9 ASC;
                                 
                                 ")
  






sapply(dbListConnections(drv), dbDisconnect)
dbGetQuery(conn, 
           "
           SELECT  CASE WHEN admin='true' THEN 'administrator'
           WHEN admin='false' AND moderator='false' THEN 'Student' 
           WHEN admin='false' AND moderator='true' THEN 'Moderator'
           END platform_users, COUNT(*) Total 
           FROM 
            users
           WHERE id>1
           GROUP BY ")







tryCatch({
  sapply(dbListConnections(drv), dbDisconnect)
  drv <- dbDriver("PostgreSQL")
  print("Connecting to database")
  conn <- dbConnect(drv, 
                    dbname = dsn_database,
                    host = dsn_hostname, 
                    port = dsn_port,
                    user = dsn_uid, 
                    password = dsn_pwd)
  print("Connected!")
  
})







drv <- dbDriver("PostgreSQL")

con <- dbConnect(drv, 
                 dbname = "ingxoxo_test",
                 user = "sidneytirivavi",
                 password = "",
                 host = "localhost",
                 port = "5432" )

sapply(dbListConnections(drv), dbDisconnect)








  drv <- dbDriver("PostgreSQL")
  con <- dbConnect(drv, user="sidneytirivavi", password="", dbname="ingxoxo_test",host="localhost")
  queryResult <- dbGetQuery(con, "with all_main as (
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
  dbDisconnect(con)
  return(queryResult)
}
stopCluster(cl)



   austen_books() %>%
  unnest_tokens(word, text) %>%
  count(book, word, sort = TRUE)

total_words <- book_words %>% 
  group_by(book) %>% 
  summarize(total = sum(n))

book_words <- left_join(book_words, total_words)

book_words
