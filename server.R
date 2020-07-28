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


dsn_database = "ingxoxo"            # e.g. "compose"
dsn_hostname = "localhost"    # e.g.: "aws-us-east-1-portal.4.dblayer.com"
dsn_port = "5432"                 # e.g. 11101 
dsn_uid = "postgres"
dsn_pwd = "" # pass

tryCatch({
  drv <- dbDriver("PostgreSQL")
  sapply(dbListConnections(drv), dbDisconnect)
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

shinyServer(
  function(input,output,session) {
        # can be a bit challenging to install RPostgreSQL, if using MAC install on terminal use: conda install -c conda-forge r-rpostgresql
    
    
    #Filters
    
    output$topic<-renderUI(pickerInput(
      inputId = "topic_select",
      label = "Select Topic Title",
      choices = unique(data_out_2()$topic_title),
      selected=unique(data_out_2()$topic_title),
      options = list(
        `actions-box` = TRUE,
        size = 10,
        `selected-text-format` = "count > 3"
      ),
      multiple = TRUE
    ))
    
    
    
    
    #End Filters
    
   
    participation <- reactive({dbGetQuery(conn, "
  SELECT  CASE WHEN admin='true' THEN 'administrator'
                    WHEN admin='false' AND moderator='false' THEN 'Student' 
                                     WHEN admin='false' AND moderator='true' THEN 'Moderator'
                                     END platform_users, COUNT(*) Total FROM users
                                     WHERE id>1
                                     GROUP BY 1
                               
")})
    
    
data_out_2<-reactive({dbGetQuery(conn, 
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
    -- y.gender,
    
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
    -- LEFT JOIN google_user_infos AS y
    -- ON y.user_id=p.user_id
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
    GROUP BY 1,2,3,4,5,6,7,8,9,10,11
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
    
  }
    
    
  )



df_products_upload <- reactive({
  inFile <- input$target_upload
  if (is.null(inFile))
    return(NULL)
  df <- read.csv(inFile$datapath, header = TRUE,sep = input$separator)
  return(df)
})






output$all_data_upload<-DT::renderDataTable( datatable(
  df_products_upload()%>%
    as.data.frame(),
  #  click_lag()
  # all_data_lag()  ,
  extensions = 'Buttons',
  class = "display nowrap compact", # style
  filter = "top", # location of column filters,
  options = list(
    dom = 'tpB',
    scrollX = TRUE,
    lengthMenu = list(c(5, 15, -1), c('5', '15', 'All')),
    pageLength = 15,
    buttons = list(
      
      list(
        extend = "collection",
        text = 'Show All',
        action = DT::JS("function ( e, dt, node, config ) {
                        dt.page.len(-1);
                        dt.ajax.reload();}")
        
        ),list(
          extend = "collection",
          text = 'Show Less',
          action = DT::JS("function ( e, dt, node, config ) {
                          dt.page.len(10);
                          dt.ajax.reload();}")
          )
          )
        )
      )
  )





  participation_1<-reactive({dbGetQuery(conn, 
                                   "
                     SELECT  CASE WHEN admin='true' THEN 'administrator'
                    WHEN admin='false' AND moderator='false' THEN 'Student' 
                                   WHEN admin='false' AND moderator='true' THEN 'Moderator'
                                   END platform_users, COUNT(*) Total FROM users
                                   WHERE id>1
                                   GROUP BY 1,2 ")})
  
   
  
  
  
   output$plot_participation<-renderPlot(
     {
       
       ggplot(participation(), aes(x = "", y=total, fill = platform_users)) + 
         geom_bar(width = 1, stat = "identity") +
         theme(axis.line = element_blank(), 
               plot.title = element_text(hjust=0.5)) + 
         labs(fill="platform_users", 
              x=NULL, 
              y=NULL, 
              title="Pie Chart of class", 
              caption="Source: mpg") + coord_polar(theta = "y", start=0) +
         geom_text(aes(x=1, y = cumsum(total) - total/2, label=total))
       
      }
   )
   
    
   
   
   
   output$plot_participation_2<-renderPlot(
     {
       
       ggplot(participation(), aes(x=platform_users,y=Total)) + 
         geom_bar(stat="identity", width=.5, fill="tomato3") + 
         labs(title="Participants") + 
         theme(axis.text.x = element_text(angle=65, vjust=0.6))
       
       }
   )
   
   
  
topic_created<-reactive({
  select(data_out_2(),
           platform_usertype,
           topic_title,
           topic_id)%>%
    group_by(platform_usertype,topic_title)%>%
  summarise(total_topics=n_distinct(topic_id))%>% 
   mutate(freq = total_topics/sum(total_topics))%>%
  as.data.frame()
})  
  
  

  
   

  output$plot_topic_1<-renderPlot(
    {
      
      ggplot(topic_created()%>%
               group_by(platform_usertype)%>%
               summarise(freq=sum(freq))%>%
               mutate(Perc_Topic_Contribution=(freq/sum(freq))*100 ), aes(x=platform_usertype, y=Perc_Topic_Contribution)) + 
        geom_bar(stat="identity", width=.5, fill="tomato3") + 
        labs(title="Topic Created % Contribution by User Group") + 
        theme(axis.text.x = element_text(angle=65, vjust=0.6))
      
    }
  )
  
  
  output$topic_creat<-DT::renderDataTable( datatable(
    topic_created()%>%
      select(platform_usertype,topic_title),
    #  click_lag()
    # all_data_lag()  ,
    extensions = 'Buttons',
    class = "display nowrap compact", # style
    filter = "top", # location of column filters,
    options = list(
      dom = 'tpB',
      scrollX = TRUE,
      lengthMenu = list(c(5, 15, -1), c('5', '15', 'All')),
      pageLength = 15,
      buttons = list(
        
        list(
          extend = "collection",
          text = 'Show All',
          action = DT::JS("function ( e, dt, node, config ) {
                          dt.page.len(-1);
                          dt.ajax.reload();}")
          
          ),list(
            extend = "collection",
            text = 'Show Less',
            action = DT::JS("function ( e, dt, node, config ) {
                            dt.page.len(10);
                            dt.ajax.reload();}")
            
            )
          )
        )
      )
    )
  
   
  
   
   
   
    output$all_data_analysis<-DT::renderDataTable( datatable(
      data_ingxoxo(),
      #  click_lag()
      # all_data_lag()  ,
      extensions = 'Buttons',
      class = "display nowrap compact", # style
      filter = "top", # location of column filters,
      options = list(
        dom = 'tpB',
        scrollX = TRUE,
        lengthMenu = list(c(5, 15, -1), c('5', '15', 'All')),
        pageLength = 15,
        buttons = list(
          
          list(
            extend = "collection",
            text = 'Show All',
            action = DT::JS("function ( e, dt, node, config ) {
                            dt.page.len(-1);
                            dt.ajax.reload();}")
            
            ),list(
              extend = "collection",
              text = 'Show Less',
              action = DT::JS("function ( e, dt, node, config ) {
                              dt.page.len(10);
                              dt.ajax.reload();}")
              )
            )
          )
        )
      )
    
    
    
    output$all_data_analysis_2<-DT::renderDataTable( datatable(
      data_out_2(),
      #  click_lag()
      # all_data_lag()  ,
      extensions = 'Buttons',
      class = "display nowrap compact", # style
      filter = "top", # location of column filters,
      options = list(
        dom = 'tpB',
        scrollX = TRUE,
        lengthMenu = list(c(5, 15, -1), c('5', '15', 'All')),
        pageLength = 15,
        buttons = list(
          
          list(
            extend = "collection",
            text = 'Show All',
            action = DT::JS("function ( e, dt, node, config ) {
                            dt.page.len(-1);
                            dt.ajax.reload();}")
            
            ),list(
              extend = "collection",
              text = 'Show Less',
              action = DT::JS("function ( e, dt, node, config ) {
                              dt.page.len(10);
                              dt.ajax.reload();}")
              
              )
            )
          )
        )
      )
    
    
    # text analysis Title
    
   
    
    
   text_analysis<-reactive(
     {
        quest<-c("?","how","what","which","explain","more","?")
        data_out_2()%>%
         select(platform_usertype,topic_title)%>%
         data.frame()%>%
         group_by(platform_usertype,topic_title)%>%
         unnest_tokens(topic_title,topic_title)%>%
         filter(topic_title %in% quest)%>%
         count(platform_usertype,topic_title, sort = TRUE)
         
       
       
       
     #  total_words <- tidy_ftp%>% 
       #  group_by(platform_usertype)%>% 
      #   summarize(total = sum(n))
     #  total_words
      # book_words <- left_join(book_words, total_words)
      # book_words%>%
      #   as.data.frame()
       
       
     }
   ) 
    
   
   text_analysis_1<-reactive(
     {
       data_sub_1<-data_out_2()%>%
         select(platform_usertype,topic_title)%>%
         data.frame()%>%
         group_by(platform_usertype,topic_title)%>%
         unnest_tokens(topic_title,topic_title) %>%
         mutate(word=topic_title)
       
       sub_dat<-select(data_sub_1,
                       word)%>%
         group_by(word)
                       
       sub_dat_1 <- sub_dat %>%
          count(word) %>%
        arrange(desc(n))
       
          data("stop_words")
          sub_dat_1<-sub_dat_1 %>%
           anti_join(stop_words) 
         
          sub_dat_1 %>%
            count(word) %>%
            arrange(desc(n))
         
          
          sub_dat_1<-sub_dat_1[-grep("\\b\\d+\\b", sub_dat_1$word),]
          sub_dat_1 
          
          
     }
   )
   
   
   
   
   
   
   
   
   
   
   output$word_cloud <- renderPlot({
   text_analysis_1() %>% 
       with(wordcloud(word, n, 
                      random.order = FALSE, 
                      max.words = 200, 
                      colors=brewer.pal(8, "Dark2")))
   })
   
   
   
   
   
   output$plot_no_stop_word<-renderPlot(
     {
       data_out_2()%>%
         select(platform_usertype,title_creator,topic_title)%>%
         data.frame()%>%
         mutate(title_creator=as.factor(title_creator))%>%
         group_by(platform_usertype,title_creator,topic_title)%>%
         unnest_tokens(topic_title,topic_title)%>%
         count(platform_usertype,topic_title, sort = TRUE)%>%
         ggplot(aes(topic_title, n )) +
         geom_col() + facet_grid(~platform_usertype,scales = "free") +
         xlab(NULL) + 
         coord_flip()+theme(axis.text.x = element_text(angle=90, vjust=0.5, size=10)) 
         
       
     }
   )
   
   
   
   
  # End Text Analysis
   
   
   
   # Word Cloud 
   
   output$text_1<-DT::renderDataTable( datatable(
     text_analysis_1(),
     #  click_lag()
     # all_data_lag()  ,
     extensions = 'Buttons',
     class = "display nowrap compact", # style
     filter = "top", # location of column filters,
     options = list(
       dom = 'tpB',
       scrollX = TRUE,
       lengthMenu = list(c(5, 15, -1), c('5', '15', 'All')),
       pageLength = 15,
       buttons = list(
         
         list(
           extend = "collection",
           text = 'Show All',
           action = DT::JS("function ( e, dt, node, config ) {
                           dt.page.len(-1);
                           dt.ajax.reload();}")
           
           ),list(
             extend = "collection",
             text = 'Show Less',
             action = DT::JS("function ( e, dt, node, config ) {
                             dt.page.len(10);
                             dt.ajax.reload();}")
             
             )
           )
         )
       )
     )
   
   
   
   
    

   
   
   
   
   
   
   
   
   output$text_title<-DT::renderDataTable( datatable(
     text_analysis(),
     #  click_lag()
     # all_data_lag()  ,
     extensions = 'Buttons',
     class = "display nowrap compact", # style
     filter = "top", # location of column filters,
     options = list(
       dom = 'tpB',
       scrollX = TRUE,
       lengthMenu = list(c(5, 15, -1), c('5', '15', 'All')),
       pageLength = 15,
       buttons = list(
         
         list(
           extend = "collection",
           text = 'Show All',
           action = DT::JS("function ( e, dt, node, config ) {
                           dt.page.len(-1);
                           dt.ajax.reload();}")
           
           ),list(
             extend = "collection",
             text = 'Show Less',
             action = DT::JS("function ( e, dt, node, config ) {
                             dt.page.len(10);
                             dt.ajax.reload();}")
             
             )
           )
         )
       )
     )
   
    
   # Sentiment Analysis TEXT
   
   
   text_sentiment<-reactive(
     {
       undesirable_words <- c("prince", "chorus", "repeat", "lyrics",
                              "theres", "bridge", "fe0f", "yeah", "baby",
                              "alright", "wanna", "gonna", "chorus", "verse",
                              "whoa", "gotta", "make", "miscellaneous", "2",
                              "4", "ooh", "uurh", "pheromone", "poompoom", "3121",
                              "matic", " ai ", " ca ", " la ", "hey", " na ",
                              " da ", " uh ", " tin ", "  ll", "transcription",
                              "repeats", "la", "da", "uh", "ah")
       
       data_sub_1<-data_out_2()%>%
         select(platform_usertype,topic_title)%>%
         data.frame()%>%
         group_by(platform_usertype,topic_title)%>%
         unnest_tokens(topic_title,topic_title) %>%
         mutate(word=topic_title)
       
       sub_dat<-select(data_sub_1,
                       word)%>%
         group_by(word)
       
       sub_dat_1 <- sub_dat %>%
         count(word) %>%
         arrange(desc(n))
       
       data("stop_words")
       sub_dat_1<-sub_dat_1 %>%
         anti_join(stop_words) 
       
       sub_dat_1 %>%
         count(word) %>%
         arrange(desc(n))
       
       sub_dat_1<-sub_dat_1[-grep("\\b\\d+\\b", sub_dat_1$word),]
       
       sub_dat_1
       
       #
     
       
       
       
     }
   )
   
   
   
    # Sentiment Analysis
   
  sentiment_analysis<- reactive({
  
    data_sub_1<-data_out_2()%>%
      select(platform_usertype,topic_title)%>%
      data.frame()%>%
      group_by(platform_usertype,topic_title)%>%
      unnest_tokens(topic_title,topic_title) %>%
      mutate(word=topic_title)%>%
     # anti_join(stop_words) %>% 
      inner_join(get_sentiments("bing")) %>% 
      group_by(sentiment, platform_usertype) %>% 
      count(word) 
      
    
    
    
    
    }
     )
   
   
  output$text_sentimentsummary<-DT::renderDataTable( datatable(
    sentiment_analysis(),
    #  click_lag()
    # all_data_lag()  ,
    extensions = 'Buttons',
    class = "display nowrap compact", # style
    filter = "top", # location of column filters,
    options = list(
      dom = 'tpB',
      scrollX = TRUE,
      lengthMenu = list(c(5, 15, -1), c('5', '15', 'All')),
      pageLength = 15,
      buttons = list(
        
        list(
          extend = "collection",
          text = 'Show All',
          action = DT::JS("function ( e, dt, node, config ) {
                          dt.page.len(-1);
                          dt.ajax.reload();}")
          
          ),list(
            extend = "collection",
            text = 'Show Less',
            action = DT::JS("function ( e, dt, node, config ) {
                            dt.page.len(10);
                            dt.ajax.reload();}")
            
            )
          )
        )
      )
    )
  
  # END Topic Analysis
  ## Posts Analysis by Topic
  
  
  output$topic_question<-DT::renderDataTable( datatable(
    data_out_2()%>%
      filter(is.na(acting_user_id))
    ,
    #  click_lag()
    # all_data_lag()  ,
    extensions = 'Buttons',
    class = "display nowrap compact", # style
    filter = "top", # location of column filters,
    options = list(
      dom = 'tpB',
      scrollX = TRUE,
      lengthMenu = list(c(5, 15, -1), c('5', '15', 'All')),
      pageLength = 15,
      buttons = list(
        
        list(
          extend = "collection",
          text = 'Show All',
          action = DT::JS("function ( e, dt, node, config ) {
                          dt.page.len(-1);
                          dt.ajax.reload();}")
          
          ),list(
            extend = "collection",
            text = 'Show Less',
            action = DT::JS("function ( e, dt, node, config ) {
                            dt.page.len(10);
                            dt.ajax.reload();}")
            
            )
          )
        )
      )
    )
  
  
  
  
  data_question<-reactive({
    data_out_2()%>%
      filter(is.na(acting_user_id))%>%
    
      select(platform_usertype,
        topic_title,
             reply_raw,
             id,
             reply_count,
             reply_reads,
             avg_time_reply,
             reply_count_total,
             post_reply_duration,
             post_reply_drafts_saved,
             yearly_posts_count,
             yearly_views_count_post,
             yearly_likes_count_post,
             daily_posts_count,
             daily_likes_count)})
    
    
    
  data_question_responses<-reactive({
    data_out_2()%>%
      filter(!is.na(acting_user_id))%>%
      filter(!is.na( reply_raw))%>%
      filter(id!=394)%>%
    #  filter(is.na(acting_user_id))%>%
      select(platform_usertype,
             topic_title,
             reply_raw,
             id,
             reply_count,
             reply_reads,
             avg_time_reply,
             reply_count_total,
             post_reply_duration,
             post_reply_drafts_saved,
             yearly_posts_count,
             yearly_views_count_post,
             yearly_likes_count_post,
             daily_posts_count,
             daily_likes_count)})
  
        
       
  output$data_question_response_table<-DT::renderDataTable( datatable(
    data_question_responses()%>%
      select(
        platform_usertype,
        topic_title,
        reply_raw,
        id)%>%
      as.data.frame(),
    #  click_lag()
    # all_data_lag()  ,
    extensions = 'Buttons',
    class = "display nowrap compact", # style
    filter = "top", # location of column filters,
    options = list(
      dom = 'tpB',
      scrollX = TRUE,
      lengthMenu = list(c(5, 15, -1), c('5', '15', 'All')),
      pageLength = 15,
      buttons = list(
        
        list(
          extend = "collection",
          text = 'Show All',
          action = DT::JS("function ( e, dt, node, config ) {
                          dt.page.len(-1);
                          dt.ajax.reload();}")
          
          ),list(
            extend = "collection",
            text = 'Show Less',
            action = DT::JS("function ( e, dt, node, config ) {
                            dt.page.len(10);
                            dt.ajax.reload();}")
            
            )
          )
        )
      )
    )
  
 
  
  
  
  
  
  
  
  output$plot_topic_question_views<-renderPlot(
    {
    ggplot(data_question()%>%
        pivot_longer(
      c(
        reply_reads,
        # avg_time_reply,
        reply_count_total,
        # post_reply_duration,
        # post_reply_drafts_saved,
        yearly_posts_count,
        yearly_views_count_post,
        yearly_likes_count_post
      ),
      names_to = "Metric", values_to = "ConverImpress"
    )%>%
      as.data.frame()
    ,
           aes(x=topic_title,y=ConverImpress,fill=Metric)
           )+geom_bar(stat="identity", position="stack",width=.5)+facet_wrap(. ~ platform_usertype)+coord_flip()
      
    }
  )
  
  
 output$target_post_text_question<-reactive(
   {
     data_out_2()%>%
       select(
         platform_usertype,
         topic_title,
         reply_raw,
         id)%>%
       as.data.frame()
   
   }
   
 )
  
  

  
 output$text_summary_target_post<-DT::renderDataTable( datatable(
   data_out_2()%>%
     select(
       platform_usertype,
       topic_title,
       reply_raw,
       id)%>%
     as.data.frame(),
   #  click_lag()
   # all_data_lag()  ,
   extensions = 'Buttons',
   class = "display nowrap compact", # style
   filter = "top", # location of column filters,
   options = list(
     dom = 'tpB',
     scrollX = TRUE,
     lengthMenu = list(c(5, 15, -1), c('5', '15', 'All')),
     pageLength = 15,
     buttons = list(
       
       list(
         extend = "collection",
         text = 'Show All',
         action = DT::JS("function ( e, dt, node, config ) {
                         dt.page.len(-1);
                         dt.ajax.reload();}")
         
         ),list(
           extend = "collection",
           text = 'Show Less',
           action = DT::JS("function ( e, dt, node, config ) {
                           dt.page.len(10);
                           dt.ajax.reload();}")
           
           )
         )
       )
     )
   )
  
  
 
  
  
 
 text_analysis_topic_question<-reactive(
   {
     data_sub_1<-data_question()%>%
       select(platform_usertype,reply_raw)%>%
       data.frame()%>%
       group_by(platform_usertype,reply_raw)%>%
       unnest_tokens(reply_raw,reply_raw) %>%
       mutate(word=reply_raw)
     
     sub_dat<-select(data_sub_1,
                     word)%>%
       group_by(word)
     
     sub_dat_1 <- sub_dat %>%
       count(word) %>%
       arrange(desc(n))
     
     data("stop_words")
     sub_dat_1<-sub_dat_1 %>%
       anti_join(stop_words) 
     
     sub_dat_1 %>%
       count(word) %>%
       arrange(desc(n))%>%
       top_n(100)
     
     
     sub_dat_1<-sub_dat_1[-grep("\\b\\d+\\b", sub_dat_1$word),]
     sub_dat_1 
     
     
   }
 )
 
 
 output$plot_question_posts_text<-renderPlot(
   {
     data_question()%>%
       select(platform_usertype,topic_title,reply_raw)%>%
       filter(topic_title %in% input$topic_select)%>%
       data.frame()%>%
       mutate(topic_title=as.factor(topic_title))%>%
       group_by(platform_usertype,topic_title,reply_raw)%>%
       unnest_tokens(reply_raw,reply_raw)%>%
      # filter(reply_raw %in% stop_words$word)%>%              
       count(platform_usertype,reply_raw, sort = TRUE)%>%
       ggplot(aes(reply_raw, n,fill=topic_title )) +
       geom_col() + facet_wrap(~platform_usertype,scales = "free") +
       xlab(NULL) + 
       coord_flip()+theme(axis.text.x = element_text(angle=90, vjust=0.5, size=10)) 
     
     
   }
 )
 
 
  output$word_cloud_question_topic <- renderPlot({
   text_analysis_topic_question()%>% 
     with(wordcloud(word, n, 
                    random.order = FALSE, 
                    max.words = 300, 
                    colors=brewer.pal(8, "Dark2")))
 })
 
 
 
  
  output$plot_question_posts_text_nostop_word<-renderPlot(
    {
      text_analysis_topic_question()%>%
      ggplot(aes(word, n )) +
        geom_col()  +
        xlab(NULL) + 
        coord_flip()+theme(axis.text.x = element_text(angle=90, vjust=0.5, size=10)) 
      
      
    }
  )  
  
  
  
  
  
  # Analysis Response 
  
  text_analysis_topic_responses<-reactive(
    {
      data_sub_1<- data_question_responses()%>%
        select(platform_usertype,reply_raw,topic_title)%>%
        filter(topic_title %in% input$topic_select)%>%
        data.frame()%>%
        group_by(platform_usertype,reply_raw)%>%
        unnest_tokens(reply_raw,reply_raw) %>%
        mutate(word=reply_raw)
      
      sub_dat<-select(data_sub_1,
                      word)%>%
        group_by(word)
      
      sub_dat_1 <- sub_dat %>%
        count(word) %>%
        arrange(desc(n))
      
      data("stop_words")
      sub_dat_1<-sub_dat_1 %>%
        anti_join(stop_words) 
      
      sub_dat_1 %>%
        count(word) %>%
        arrange(desc(n))
        #top_n(100)
      
      
      sub_dat_1<-sub_dat_1[-grep("\\b\\d+\\b", sub_dat_1$word),]
      sub_dat_1 
      
      
    }
  )
  
  
  output$word_cloud_response_topic <- renderPlot({
    text_analysis_topic_responses()%>% 
      with(wordcloud(word, n, 
                     random.order = FALSE, 
                     max.words = 300, 
                     colors=brewer.pal(8, "Dark2")))
  })
  
  
  
  output$word_cloud_response_topic_by_topic <- renderPlot({
    text_analysis_topic_responses()%>%
      with(wordcloud(word, n, 
                     random.order = FALSE, 
                     max.words = 300, 
                     colors=brewer.pal(8, "Dark2")))
    
    
    
    
    
  })
  
   
    
  })
