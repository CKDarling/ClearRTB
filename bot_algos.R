setwd("~/Desktop/ClearRTB/Clear_Project/datasets")

library(tidyverse)
library(dplyr)
library(knitr)
library(C50)
library(caret)
library(e1071)
install.packages("rmdformats")
install.packages('kableExtra')
library(partykit)
library(corrplot) #correlogram
library(ggplot2)

# Iteration 1 ----
bot_tests_BIG<-read.csv('bot_tests_BIG.csv')
bot_tests_BIG<-bot_tests_BIG%>%
  mutate(classification = ifelse(bot_score == 100, 'Human',
                          ifelse(bot_score == 0, 'Bot',"Unsure"))
  )%>%
  select(-c(id,uuid,time_record,bot_score))


# Decision Tree 1----
set.seed(47)
rows<-createDataPartition(bot_tests_BIG$classification,p=.8,list=F)
train<-bot_tests_BIG[rows,]
test<-bot_tests_BIG[-rows,]

set.seed(47)

c5<-train(y= factor(train$classification),
          x= dplyr::select(train, -classification),
          method="C5.0Tree")


c5_predictions <- predict(c5, newdata = test)

confusionMatrix(c5_predictions,factor(test$classification))

# Remove 'Unsure' variables

# Iteration 2 ----

bot_tests<-read.csv("bot_tests.csv")
bot_tests<-bot_tests%>%
  mutate(classification = ifelse(bot_score == 100, 'Human',
                                 ifelse(bot_score == 0, 'Bot','Unsure'))
  )%>%
  select(-c(id,uuid,time_record,bot_score))
# Time Record may be valuable in further exploration

bot_tests<-bot_tests%>%
  filter(classification != 'Human')

bot_tests$classification<-factor(bot_tests$classification)

sub_bot_tests<-bot_tests[1:50000,]

sub_bot_tests<-bot_tests[1:50000,]%>%
  filter(classification != 'Human')

sub_bot_tests$classification<-factor(sub_bot_tests$classification)

bot_tests_BIG<-read.csv('bot_tests_BIG.csv')

bot_tests_BIG<-bot_tests_BIG%>%
  mutate(classification = ifelse(bot_score == 100, 'Human',
                                 ifelse(bot_score == 0, 'Bot',"Unsure"))
  )%>%
  select(-c(id,uuid,time_record,bot_score))

bot_tests_BIG<-bot_tests_BIG%>%
  filter(classification != 'Human')


bot_viz <- plyr::count(bot_tests_BIG, c('phantom_ua', 'phantom_properties', 'phantom_etsl',
                                 'phantom_language', 'phantom_websocket', 'mq_screen',
                                 'phantom_overflow','phantom_window_height','headchr_ua',
                                 'webdriver','headchr_chrome_obj','headchr_permissions',
                                 'headchr_plugins','headchr_iframe','chr_debug_tools',
                                 'selenium_driver','chr_battery','chr_memory','transparent_pixel',
                                 'sequentum','video_codecs','classification'))



# Decision Tree 2 ----
set.seed(47)
rows<-createDataPartition(bot_tests_BIG$classification,p=.6,list=F)
train<-bot_tests_BIG[rows,]
test<-bot_tests_BIG[-rows,]

set.seed(47)

c5<-train(y= factor(train$classification),
          x= dplyr::select(train, -classification),
          method="C5.0Tree")


c5_predictions <- predict(c5, newdata = test)

confusionMatrix(c5_predictions,factor(test$classification))

summary(c5)

# Visualizations ----
# Display the most common failing variables


ggplot(bot_viz, aes(x=headchr_plugins, y=freq))+
       geom_bar(stat = "identity", color ="skyblue",fill='skyblue',
       position = position_dodge())+
       facet_grid(~classification)+
       labs(x='HEADCHR_PLUGINS',y='Frequency',title = 'Frequency of a Key Variable')

ggplot(bot_viz, aes(x=headchr_iframe, y=freq))+
       geom_bar(stat = "identity", color ="skyblue",fill='skyblue',
       position = position_dodge(0.9))+
       facet_grid(~classification)+
       labs(x='HEADCHR_IFRAME',y='Frequency',title = 'Frequency of a Key Variable')

ggplot(bot_viz, aes(x=chr_memory, y=freq))+
       geom_bar(stat = "identity", color ="skyblue",fill='skyblue',
       position = position_dodge(0.9))+
       facet_grid(~classification)+
       labs(x='CHR_MEMORY',y='Frequency',title = 'Frequency of a Key Variable')

ggplot(bot_viz, aes(x=phantom_etsl, y=freq))+
       geom_bar(stat = "identity", color ="skyblue",fill='skyblue',
       position = position_dodge(0.9))+
       facet_grid(~classification)+
       labs(x='PHANTOM_ETSL',y='Frequency',title = 'Frequency of a Key Variable')

ggplot(bot_viz, aes(x=video_codecs, y=freq))+
       geom_bar(stat = "identity",  color ="skyblue",fill='skyblue',
       position = position_dodge(2))+
       facet_grid(~classification)+
       labs(x='VIDEO_CODECS',y='Frequency',title = 'Frequency of a Key Variable')


# Second Round of Bot Score Exploration ----
# Variables of interest include, headcrh_plugins, headchr_iframe, chr_memory,
# phantom_etsl, and video_codecs
# Another curious varible is the in_view_time from the unique_ad_logs table
# headchr_permissions is the most misclassified variable for mobile devices

ua_and_bots <-read.csv('ua_strings_and_bot_attributes.csv')
ua_and_bots_mobile <-read.csv('ua_strings_and_bot_attributes_MOBILE.csv')

ua_and_bots<-ua_and_bots%>%
         mutate(classification = ifelse(bot_score == 100, 'Human',
                                 ifelse(bot_score == 0, 'Bot',"Unsure")),
                ua_length = nchar(as.character(user_agent_string)))

ua_and_bots_mobile<-ua_and_bots_mobile%>%
         mutate(classification = ifelse(bot_score == 100, 'Human',
                                 ifelse(bot_score == 0, 'Bot',"Unsure")),
                ua_length = nchar(as.character(user_agent_string)))

ua_and_bots <- ua_and_bots%>%
                select(user_agent_string,ua_length,in_view_time,bot_score,classification,everything())
ua_and_bots_mobile <- ua_and_bots_mobile%>%
                select(user_agent_string,ua_length,in_view_time,bot_score,classification,everything())


ua_bot_viz <- plyr::count(ua_and_bots, c('user_agent_string','bot_score',
                                        'phantom_ua', 'phantom_properties', 'phantom_etsl',
                                        'phantom_language', 'phantom_websocket', 'mq_screen',
                                        'phantom_overflow','phantom_window_height','headchr_ua',
                                        'webdriver','headchr_chrome_obj','headchr_permissions',
                                        'headchr_plugins','headchr_iframe','chr_debug_tools',
                                        'selenium_driver','chr_battery','chr_memory','transparent_pixel',
                                        'sequentum','video_codecs','classification'))

ua_and_bots<-ua_and_bots%>%
            mutate(minutes_viewed = in_view_time/60)

ggplot(ua_and_bots, aes(minutes_viewed))+
      geom_histogram(bins=30)


ggplot(ua_and_bots, aes(ua_and_bots$in_view_time)) + 
  geom_histogram(col="black", 
                 fill="skyblue") + 
  labs(title="Seconds ad was in View") +
  labs(x="in_view_time", y="Count") + 
  xlim(c(0,500)) + 
  ylim(c(0,5000))+
  facet_grid(~classification)

ggplot(ua_and_bots, aes(ua_and_bots$minutes_viewed)) + 
  geom_histogram(col="black", 
                 fill="skyblue") + 
  labs(title="Seconds ad was in View") +
  labs(x="minutes_viewed", y="Count") + 
  xlim(c(0,50)) + 
  ylim(c(0,5000))+
  facet_grid(~classification)


# Understanding Misclassifying Variables Within Grouped User_Agent_Strings ----
gbuas<-read.csv('group_by_uas.csv')
gbuas<-gbuas%>%
  mutate(classification = ifelse(bot_score == 100, 'Human',
                                 ifelse(bot_score == 0, 'Bot',"Unsure")),
         ua_length = nchar(as.character(user_agent_string)))

gbuas <- gbuas%>%
  select(user_agent_string,ua_length,uas_count,in_view_time,bot_score,classification,everything())

# Bot Proportions ----
prop.table(table(gbuas$classification)) 
# Bot: 88% Unsure: 9% Human 2%
prop.table(table(gbuas$phantom_etsl)) # 3% bots
prop.table(table(gbuas$headchr_plugins)) # 92% Generally unsure
prop.table(table(gbuas$chr_memory)) # 57% bots
prop.table(table(gbuas$headchr_iframe)) # 72% bots
prop.table(table(gbuas$mq_screen)) # 19% bots
prop.table(table(gbuas$headchr_chrome_obj)) # 43% bots
prop.table(table(gbuas$headchr_permissions)) # 10% bots

# YOU NEED TO CHECK OUT THE IN_TIME_VIEW VARIABLE MORE

# Rename the important ua_strings to a more readable format

# GBUAS Visuals ----
gbuas_large <-gbuas%>%
  filter(uas_count > 80000)

gbuas_large$bot_score<-factor(gbuas_large$bot_score)
gbuas_large$classification<-factor(gbuas_large$classification)
gbuas_large$user_agent_string<-factor(gbuas_large$user_agent_string)


gbuas_large<-gbuas_large%>%
              mutate(id = rownames(gbuas_large))%>%
              select(user_agent_string,id,everything())

gbuas_focus<-gbuas_large%>%
  select(user_agent_string,id,uas_count,classification,phantom_etsl,
         headchr_plugins,headchr_chrome_obj,headchr_permissions,
         chr_memory,mq_screen)


ggplot(gbuas_large,aes(headchr_iframe, id))+
  geom_point(col="green",
             size=3)+
  labs(x=" iframe Score",
       y='User Agent Strings',
       title="Understanding Misclassifying Variables") +
  facet_wrap(~classification)


col<-names(gbuas_focus[,5:10])

for(i in col) {

  ua_p<-ggplot(gbuas_focus,aes(x=i,y=id))+
    geom_point(col="green",
               size=3)+
    labs(y='User Agent Strings',
         title="Understanding Misclassifying Variables") +
    facet_wrap(~classification)
  
  print(ua_p)
}





