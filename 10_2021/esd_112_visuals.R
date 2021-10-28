# WASTEM Graphs and OSPI API 
library(RSocrata)
library(httr)
library(jsonlite)
library(tidyverse)
library(hrbrthemes)
library(stringr)
library(scales)

options(stringsAsFactors = FALSE)
source('tokens.R')

url <- 'https://data.wa.gov/resource/fwse-4pny.json'

wa_kids <- read.socrata(url)

# ESD 112 
# WAKIDS scores across all domains 
# compare everyone to:
# 1. Low Income 
# 2. ELL
# Compare 2019-20 to 2012 scores and changes across 
names(wa_kids)
head(wa_kids$domain)

# Domains
wa_kids %>%
  group_by(domain) %>%
  summarise(n = n())

sub_domain <- c(
  'Cognitive',
  'Literacy',
  'Math',
  'Physical',
  'SocialEmotional',
  'Language'
)

# groups 
wa_kids %>%
  group_by(studentgrouptype) %>%
  summarise(n = n())

groups <- c(
  'All Students',
  'English Language Learners',
  'Non-English Language Learners',
  'Low-Income',
  'Non-Low Income'
)

# Column subset for visuals 
names(wa_kids)

columns <- c(
  'schoolyear',                   
  'organizationlevel',            
  'esdname',                     
  'districtname',                 
  'schoolname',
  'studentgroup',
  'domain',
  'preparedfork_numerator',       
  'preparedfork_denominator',    
  'preparedfork_percent',
  'Match'
)

# ESD 112 
wa_kids$Match <- grepl(pattern = "(112)$", wa_kids$esdname)

# Subset data frame to show only TRUE matches
esd_112_df <- wa_kids %>%
  select(all_of(columns)) %>%
  filter(Match == TRUE, 
         studentgroup %in% groups,
         domain %in% sub_domain) %>%
  mutate(across(c(preparedfork_numerator,       
                  preparedfork_denominator,    
                  preparedfork_percent),
                as.numeric))

# Create graph sowing the three groups of interest for most recent year
glimpse(esd_112_df)

esd_df <- esd_112_df %>%
  filter(schoolyear == '2019-20')  %>% 
  group_by(studentgroup, domain) %>%
  summarise(numm = sum(preparedfork_numerator, na.rm = T),
            dem = sum(preparedfork_denominator, na.rm = T)) %>%
  mutate(percent = (numm/dem))


esd_df %>%
  filter(studentgroup == 'All Students') %>%
ggplot(., aes(y=percent, x=domain, label = scales::percent(percent))) + 
  geom_bar(stat="identity", fill="steelblue") +
  ggtitle("2019-20 Percent of Students Entering Kindergarten\nWith Skills Expected of Age") +
  theme_ipsum() +
  xlab("") + 
  ylab("") +
  theme(axis.text.x=element_text(size=15)) + 
  geom_text(position = position_dodge(width = .9),    # move to center of bars
            vjust = -0.5,    # nudge above top of bar
            size = 5) + 
  scale_y_continuous(labels = scales::percent)
  
ggsave('All_students.png',  units = "in", width = 10, height = 7)

colors <- c('#99d594','#3288bd')

esd_df %>%
  filter(studentgroup %in% c('Non-English Language Learners', 'English Language Learners')) %>%
  ggplot(., aes(y=percent, x=domain, fill=studentgroup, label = scales::percent(percent, accuracy = 0.01L))) + 
    geom_bar(position="dodge", stat="identity") +
  ggtitle("2019-20 Percent of Students Entering Kindergarten\nWith Skills Expected of Age",
          subtitle = 'English Language Learners Comparison') +
  theme_ipsum() +
  xlab("") + 
  geom_text(position = position_dodge(width = .9),    # move to center of bars
            vjust = -0.5,    # nudge above top of bar
            size = 4) + 
  scale_y_continuous(labels = scales::percent) +
  theme(legend.position="bottom",
        legend.title = element_blank(),
        legend.text = element_text(size=12),
        axis.title.y=element_blank(),
        axis.text.x=element_text(size=15)) +
  scale_fill_manual(values=colors)

ggsave('ELL.jpg', units = "in", width = 10, height = 7)

## Low income 
esd_df %>%
  filter(studentgroup %in% c('Low-Income', 'Non-Low Income')) %>%
  ggplot(., aes(y=percent, x=domain, fill=studentgroup, label = scales::percent(percent, accuracy = 0.01L))) + 
  geom_bar(position="dodge", stat="identity") +
  ggtitle("2019-20 Percent of Students Entering Kindergarten\nWith Skills Expected of Age",
          subtitle = 'Low Income Comparison') +
  theme_ipsum() +
  xlab("") + 
  geom_text(position = position_dodge(width = .9),    # move to center of bars
            vjust = -0.5,    # nudge above top of bar
            size = 4) + 
  scale_y_continuous(labels = scales::percent) +
  theme(legend.position="bottom",
        legend.title = element_blank(),
        legend.text = element_text(size=12),
        axis.title.y=element_blank(),
        axis.text.x=element_text(size=15)) +
  scale_fill_manual(values=colors)


ggsave('low_income.jpg', units = "in", width = 10, height = 7)

# Trend lines and gaps 
esd_df_trend <- esd_112_df %>%
  mutate(schoolyear=recode(schoolyear, 
                    `2011-12`=2012,
                    `2012-13`=2013,
                    `2013-14`=2014,
                    `2014-15`=2015,
                    `2015-16`=2016,
                    `2016-17`=2017,
                    `2017-18`=2018,
                    `2018-19`=2019,
                    `2019-20`=2020)) %>%
  group_by(schoolyear, studentgroup, domain) %>%
  summarise(numm = sum(preparedfork_numerator, na.rm = T),
            dem = sum(preparedfork_denominator, na.rm = T)) %>%
  mutate(percent = (numm/dem))

esd_df_trend %>%
  filter(
         domain == 'Math',
         studentgroup %in% c('English Language Learners', 'Non-English Language Learners')) %>%
  ggplot( aes(x=schoolyear, y=percent, group=studentgroup, color=studentgroup)) +
  geom_line() +
  ggtitle("title") +
  theme_ipsum() +
  ylab("")
# state_test <- wa_kids %>%
#   filter(schoolyear == '2019-20',
#          esdname == 'State Total',
#          studentgroup %in% groups,
#          domain %in% sub_domain) %>%
#   mutate(across(c(preparedfork_numerator,       
#                   preparedfork_denominator,    
#                   preparedfork_percent),
#                 as.numeric)) %>% 
#   group_by(studentgroup, domain) %>%
#   summarise(numm = sum(preparedfork_numerator, na.rm = T),
#             dem = sum(preparedfork_denominator, na.rm = T)) %>%
#   mutate(percent = (numm/dem)*100)
