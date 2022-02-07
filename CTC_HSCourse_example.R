rm(list=ls())
#install.packages(c())
library(RODBC)
library(dplyr)
library(ggplot2)
library(tidyr)
library(knitr)
library(scales)
library(grid)

#source("C:/Users/jhernandez/Google Drive/CCER/Credentials")

db <- odbcDriverConnect('driver={SQL Server};
                        server=SQLDB-DEV-01;
                        database=SandBox;
                        trusted_connection=true')

df <- sqlFetch(db,"dbo.CTCHSCourses2")

close(DataHub)
rm(password,username)
#######################################
#Check cohorts
  SSID_unique <- df %>%
  group_by(SSID,Cohort) %>%
  summarise(total = n()) %>%
  group_by(Cohort) %>%
  summarise(total2 = n())
#######################################
table(df$GradeLevelWhenCourseTaken)
names(df)

df2012C <- df %>%
           filter(GradeLevelWhenCourseTaken == 12)#ContentAreaName=='Miscellaneous'

#ccd_2010_15[grep("\\<distance\\>",ccd_2010_15$notschool),]$notschool = "1"

df2012C$Area <- tolower(df2012C$CourseTitle)

##Math###
df2012C[grep("calc",df2012C$Area),]$Area = "math"
df2012C[grep("clg alg",df2012C$Area),]$Area = "math"
df2012C[grep("college alg",df2012C$Area),]$Area = "math"
df2012C[grep("comp netwrk",df2012C$Area),]$Area = "math"
df2012C[grep("math",df2012C$Area),]$Area = "math"
df2012C[grep("alg",df2012C$Area),]$Area = "math"
df2012C[grep("geom",df2012C$Area),]$Area = "math"
df2012C[grep("integrated",df2012C$Area),]$Area = "math"
df2012C[grep("ap computer science",df2012C$Area),]$Area = "math"
df2012C[grep("ap comp sci",df2012C$Area),]$Area = "math"
df2012C[grep("trig",df2012C$Area),]$Area = "math"
##English##
df2012C[grep("\\<ell\\>",df2012C$Area),]$Area = "ell_course"
df2012C[grep("\\<eld\\>",df2012C$Area),]$Area = "ell_course"
df2012C[grep("ell_",df2012C$Area),]$Area = "ell_course"

df2012C[grep("eng 12",df2012C$Area),]$Area = "english"
df2012C[grep("eng sr",df2012C$Area),]$Area = "english"
df2012C[grep("literature/composition 4",df2012C$Area),]$Area = "english"
df2012C[grep("college writing",df2012C$Area),]$Area = "english"
df2012C[grep("lang art 12",df2012C$Area),]$Area = "english"
df2012C[grep("engl",df2012C$Area),]$Area = "english"
df2012C[grep("ap eng",df2012C$Area),]$Area = "english"
df2012C[grep("ap lit",df2012C$Area),]$Area = "english"
df2012C[grep("amer lit",df2012C$Area),]$Area = "english"
df2012C[grep("coll writ",df2012C$Area),]$Area = "english"

##Science##
df2012C[grep("bio",df2012C$Area),]$Area = "science" 
df2012C[grep("anat/physio",df2012C$Area),]$Area = "science"
df2012C[grep("astro",df2012C$Area),]$Area = "science"
df2012C[grep("chem",df2012C$Area),]$Area = "science"
df2012C[grep("physics",df2012C$Area),]$Area = "science"
df2012C[grep("geology",df2012C$Area),]$Area = "science"
df2012C[grep("environ science",df2012C$Area),]$Area = "science"
df2012C[grep("physiology",df2012C$Area),]$Area = "science"
df2012C[grep("environmental science",df2012C$Area),]$Area = "science"
df2012C[grep("ap envir",df2012C$Area),]$Area = "science"
df2012C[grep("advscience",df2012C$Area),]$Area = "science"
df2012C[grep("envscience",df2012C$Area),]$Area = "science"
df2012C[grep("\\<science lab\\>",df2012C$Area),]$Area = "science"
df2012C[grep("lab science",df2012C$Area),]$Area = "science"
df2012C[grep("env_sci",df2012C$Area),]$Area = "science"

##Social Sciences
df2012C[grep("pol",df2012C$Area),]$Area = "ss"  
df2012C[grep("hist",df2012C$Area),]$Area = "ss"
df2012C[grep("gov",df2012C$Area),]$Area = "ss"
df2012C[grep("soc",df2012C$Area),]$Area = "ss"
df2012C[grep("civics",df2012C$Area),]$Area = "ss"
df2012C[grep("psych",df2012C$Area),]$Area = "ss"
df2012C[grep("geography",df2012C$Area),]$Area = "ss"
df2012C[grep("humanities",df2012C$Area),]$Area = "ss"
df2012C[grep("philosophy",df2012C$Area),]$Area = "ss"
df2012C[grep("econ",df2012C$Area),]$Area = "ss"
#df2012C[grep("ap macro",df2012C$Area),]$Area = "ss"
#df2012C[grep("ap micro",df2012C$Area),]$Area = "ss"
df2012C[grep("ap am gvt",df2012C$Area),]$Area = "ss"

##Arts
df2012C[grep("art",df2012C$Area),]$Area = "art"
df2012C[grep("drama",df2012C$Area),]$Area = "art"
df2012C[grep("draw",df2012C$Area),]$Area = "art"
df2012C[grep("paint",df2012C$Area),]$Area = "art"
df2012C[grep("acting",df2012C$Area),]$Area = "art"
df2012C[grep("pottery",df2012C$Area),]$Area = "art"
df2012C[grep("sculp",df2012C$Area),]$Area = "art"
#df2012C[grep("sclpt",df2012C$Area),]$Area = "art"
df2012C[grep("ceramics",df2012C$Area),]$Area = "art"

##F-Langueage
df2012C[grep("foreign lang",df2012C$Area),]$Area = "flang"
df2012C[grep("french",df2012C$Area),]$Area = "flang"
df2012C[grep("spanish",df2012C$Area),]$Area = "flang"
df2012C[grep("japanese",df2012C$Area),]$Area = "flang"
df2012C[grep("german",df2012C$Area),]$Area = "flang"
df2012C[grep("chinese",df2012C$Area),]$Area = "flang"
df2012C[grep("world lang",df2012C$Area),]$Area = "flang"
df2012C[grep("latin",df2012C$Area),]$Area = "flang"

##Other (might not count towards cadrs)
df2012C[grep("statistics",df2012C$Area),]$Area = "other"
df2012C[grep("business",df2012C$Area),]$Area = "other"


dfarea <- df2012C %>%
  mutate(Area2 = ifelse(Area == "math", "mth",
                        ifelse(Area == "english", "eng",
                               ifelse(Area == "science", "sci",
                                      ifelse(Area == "ss", "ss",
                                             ifelse(Area == "art", "art",
                                                    ifelse(Area == "flang", "wlang",
                                                           ifelse(Area == "other", "other",
                                                                  ifelse(Area == "ell_course","ell_course","other")))))
                                      ))))
