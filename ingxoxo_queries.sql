WITH all_main AS (
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
                     sum(p.reads) AS reply_reads,
                     sum(p.avg_time) AS avg_time_reply,
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
                     LEFT JOIN users u ON
                     t.user_id=u.id
                     LEFT JOIN users n
                     ON t.user_id=n.id
                     LEFT JOIN google_user_infos AS y
                     ON y.user_id=p.user_id
                     LEFT JOIN post_stats ps
                     ON ps.post_id=p.id
                     LEFT JOIN top_topics tp
                     ON tp.topic_id=t.id
                     
                     
                     WHERE t.category_id = (SELECT id FROM
                     categories WHERE name = 'Phases & Intermolecular forces')
                     AND
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
                      replies_no_response AS (
                     SELECT DATE(m.created_at) mentioned_at,r.user_id reply_user_id, t.id AS topic_id, target_post_id post_id
                     FROM mentions m
                     JOIN replies r ON r.target_topic_id = m.target_topic_id
                     JOIN topics t ON t.id = m.target_topic_id
                     WHERE m.created_at > r.created_at
                     AND t.deleted_at IS NULL
                     AND NOT t.archived
                     AND NOT t.closed
                     ORDER BY m.created_at DESC)
                     SELECT aa.*,bb.mentioned_at, bb.topic_id AS topic_mentioned_no_response,bb.post_id AS mentioned_post_no_response    FROM  all_main AS aa
                     LEFT JOIN replies_no_response AS bb
                     ON bb.topic_id=aa.topic_id
                     AND bb.post_id=aa.id
                     AND bb.reply_user_id=aa.post_reply_user_id;

                     SELECT target_topic_id,target_post_id ,user_id,action_type,b.username post_reply_user,b.admin
                     FROM user_actions a
                     INNER JOIN 
                     users b 
                     ON a.user_id=b.id
                     WHERE action_type = 5 -- replies
                     AND target_topic_id=124
                     
                      ;
SELECT * FROM topic_users 
WHERE topic_id=124
;