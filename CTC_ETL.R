library(RODBC)
library(tidyverse)
library(knitr)
library(scales)
library(grid)
library(reshape2)
library(ggplot2)
library(ggthemes)
percForm <- function(x){
  return(percent(round(x,3)))
}


#df <- readRDS(file="ctc.Rda")
db <- odbcDriverConnect('driver={SQL Server};
                        server=SQLDB-DEV-01;
                        database=SandBox;
                        trusted_connection=true')

df <- sqlFetch(db,"dbo.CTC_Ind_Conc")
close(db)

df <- df %>%
  mutate(concent =ifelse(is.na(CreditTot),0,1),
         Noconcent = ifelse(is.na(CreditTot),1,0))

cohort <- df %>% 
  filter(LastRMRAcademicYear %in% c(2012))
#################
#FT RS, 
dRace <- cohort %>%
  group_by(LastRMRAcademicYear,FederalEthRaceRollupName) %>%
  summarize(num=length(SSID)) %>%
  group_by(LastRMRAcademicYear) %>%
  mutate(total=sum(num),
         perc=num/total,
         indicator = "Overall") %>%
  rename(subgroup=FederalEthRaceRollupName)
##################################################
####  Create the HS visual
##################################################
#GEt HS course information

db <- odbcDriverConnect('driver={SQL Server};
                        server=SQLDB-DEV-01;
                        database=SandBox;
                        trusted_connection=true')

df2 <- sqlFetch(db,"dbo.CTCHSCourses3")

close(db)


table(df2$GradeLevelWhenCourseTaken)


df3 <- df2 %>%
  filter(Cohort==2012) %>%
  mutate(AdvMath2 = ifelse(AdvMath=='advmath',1,0)) %>%
  group_by(SSID) %>%
  summarise(IBFlag = max(InternationalBaccalaureateFlag),           
            CollHSFlag = max(CollegeattheHighSchoolFlag),                 
            HonorsFlag = max(HonorsFlag),                                 
            APFlag = max(AdvancedPlacementFlag),                      
            RSFlag = max(RunningStartFlag),                           
            CADRS = max(CollegeAcademicDistributionRequirementsFlag),
            CambFlag = max(CambridgeProgramFlag),                       
            OnlineFlag = max(OnlineFlag),                                 
            dPassedFlag = max(dPassedFlag),                                                                
            AdvMath = max(AdvMath2),                                    
            passadvmath = max(passadvmath),                                
            passAP = max(passAP),                                     
            passIB = max(passIB))



###########
ctc_out <- left_join(df,df3, by=c("SSID"))
###############

###############
names(cohort)
cohort <- ctc_out %>% 
  filter(LastRMRAcademicYear %in% c(2012))

cohort <- cohort %>%
  mutate(Npassadv = ifelse(passadvmath==0,1,0),
         NpassAP = ifelse(passAP==0,1,0),
         NpassIB = ifelse(passIB==0,1,0))

cohortFinAid <- cohort %>% filter(EverFinAid==1)
cohortNotFinAid <- cohort %>% filter(EverFinAid==0)
cohortFT <- cohort %>% filter(FullTimeAvg==1)
cohortPT <- cohort %>% filter(PartTimeAvg==1)
cohortCE <- cohort %>% filter(ContEnrolled==1)
cohortNotCE <- cohort %>% filter(NotContEnrolled==1)
cohortPreCol <- cohort %>% filter(PreCollegeAnyTime==1)
cohortCollegeReady <- cohort %>% filter(CollegeReadyOrUnknown==1)
cohortRS <- cohort %>% filter(EverRS==1)
cohortNotRS <- cohort %>% filter(NeverRS==1)
cohortELL <- cohort %>% filter(ELLFlag==1)
cohortNotELL <- cohort %>% filter(ELLFlag==0)
#################

dems <- bind_rows(
     list( 
         #cohort %>%
        #     group_by(LastRMRAcademicYear) %>%
         #    summarize(num=length(SSID)) %>%
        #     group_by(LastRMRAcademicYear) %>%
        #     mutate(total=sum(num),
        #                           perc=num/total) %>%
        #     mutate(indicator="Total Students"),
         
           cohort %>%
             filter(!(LastRMRDistrictName %in% c("Kent School District","Seattle Public Schools"))) %>%
             group_by(LastRMRAcademicYear,FRPLFlag) %>%
             summarize(num=length(SSID)) %>%
             group_by(LastRMRAcademicYear) %>%
             mutate(total=sum(num),
                                   perc=num/total) %>%
             filter(FRPLFlag==1) %>%
             select(-FRPLFlag) %>%
             mutate(indicator = "low-income"),
         
           cohort %>%
             group_by(LastRMRAcademicYear,FullTimeAvg) %>%
             summarize(num=length(SSID)) %>%
             group_by(LastRMRAcademicYear) %>%
             mutate(total=sum(num),
                     perc=num/total) %>%
             filter(FullTimeAvg==1) %>%
             select(-FullTimeAvg) %>%
             mutate(indicator="full-time"),
         
           cohort %>%
             group_by(LastRMRAcademicYear,MoveOnToCollegeMath) %>%
             summarize(num=length(SSID)) %>%
             group_by(LastRMRAcademicYear) %>%
             mutate(total=sum(num),
                    perc=num/total) %>%
             filter(MoveOnToCollegeMath==1) %>%
             select(-MoveOnToCollegeMath) %>%
             mutate(indicator="math college-ready"),
         
           cohort %>%
             group_by(LastRMRAcademicYear,EverRS) %>%
             summarize(num=length(SSID)) %>%
             group_by(LastRMRAcademicYear) %>%
             mutate(total=sum(num),
                                   perc=num/total) %>%
             filter(EverRS==1) %>%
             select(-EverRS)%>%
             mutate(indicator="running-start"),
         
           cohort %>%
             group_by(LastRMRAcademicYear,concent) %>%
             summarize(num=length(SSID)) %>%
             group_by(LastRMRAcademicYear) %>%
             mutate(total=sum(num),
                                   perc=num/total) %>%
             filter(concent==1) %>%
             select(-concent)%>%
             mutate(indicator="concentrator"),
         
         cohort %>%
           group_by(LastRMRAcademicYear,creds30OneYr) %>%
           summarize(num=length(SSID)) %>%
           group_by(LastRMRAcademicYear) %>%
           mutate(total=sum(num),
                  perc=num/total) %>%
           filter(creds30OneYr==1) %>%
           select(-creds30OneYr)%>%
           mutate(indicator="Complete30"),
         
         cohort %>%
           group_by(LastRMRAcademicYear,RetainedTwoYr) %>%
           summarize(num=length(SSID)) %>%
           group_by(LastRMRAcademicYear) %>%
           mutate(total=sum(num),
                  perc=num/total) %>%
           filter(RetainedTwoYr==1) %>%
           select(-RetainedTwoYr)%>%
           mutate(indicator="Retained")
       )
)
  
     dems <- dems %>%
         mutate(subgroup = "GenTotal") %>%
         select(LastRMRAcademicYear,subgroup,num,total,perc,indicator)
     

     
     
#     write.csv(dems, file="overall_ctcdems.csv")
     
################

raceDem <- bind_rows(
       list( 
        cohort %>%
           group_by(LastRMRAcademicYear,FederalEthRaceRollupName) %>%
           summarize(num=length(SSID)) %>%
           mutate(total=sum(num),
                  perc=num/total) %>%
           mutate(indicator = "Total Students") %>%
           rename(subgroup=FederalEthRaceRollupName),
        
        cohort %>%
          filter(!(LastRMRDistrictName %in% c("Kent School District","Seattle Public Schools"))) %>%
          group_by(LastRMRAcademicYear,FRPLFlag,FederalEthRaceRollupName) %>%
          summarize(num=length(SSID)) %>%
          #group_by(LastRMRAcademicYear) %>%
          mutate(total=sum(num),
                 perc=num/total) %>%
          filter(FRPLFlag==1) %>%
          ungroup() %>%
          select(-FRPLFlag) %>%
          mutate(indicator = "low-income") %>%
          rename(subgroup=FederalEthRaceRollupName),
       
        cohort %>%
           group_by(LastRMRAcademicYear,FullTimeAvg,FederalEthRaceRollupName) %>%
           summarize(num=length(SSID)) %>%
          # group_by(LastRMRAcademicYear,FederalEthRaceRollupName) %>%
           mutate(total=sum(num),
                  perc=num/total) %>%
           filter(FullTimeAvg==1) %>%
           ungroup() %>%
           select(-FullTimeAvg) %>%
           mutate(indicator = "full-time") %>%
           rename(subgroup=FederalEthRaceRollupName),
         
       cohort %>%
         group_by(LastRMRAcademicYear,MoveOnToCollegeMath,FederalEthRaceRollupName) %>%
         summarize(num=length(SSID)) %>%
         # group_by(LastRMRAcademicYear,FederalEthRaceRollupName) %>%
         mutate(total=sum(num),
                perc=num/total) %>%
         filter(MoveOnToCollegeMath==1) %>%
         ungroup() %>%
         select(-MoveOnToCollegeMath) %>%
         mutate(indicator = "math college-ready") %>%
         rename(subgroup=FederalEthRaceRollupName),  
       
       cohort %>%
           group_by(LastRMRAcademicYear,EverRS,FederalEthRaceRollupName) %>%
           summarize(num=length(SSID)) %>%
           #group_by(LastRMRAcademicYear,FederalEthRaceRollupName) %>%
           mutate(total=sum(num),
                  perc=num/total) %>%
           filter(EverRS==1) %>%
           ungroup() %>%
           select(-EverRS) %>%
           mutate(indicator = "running-start") %>%
           rename(subgroup=FederalEthRaceRollupName),
       
        cohort %>%
         group_by(LastRMRAcademicYear,concent,FederalEthRaceRollupName) %>%
         summarize(num=length(SSID)) %>%
         #group_by(LastRMRAcademicYear,FederalEthRaceRollupName) %>%
         mutate(total=sum(num),
                perc=num/total) %>%
         filter(concent==1) %>%
         ungroup() %>%
         select(-concent) %>%
         mutate(indicator = "concentrator") %>%
         rename(subgroup=FederalEthRaceRollupName),
       
       cohort %>%
         group_by(LastRMRAcademicYear,creds30OneYr,FederalEthRaceRollupName) %>%
         summarize(num=length(SSID)) %>%
         #group_by(LastRMRAcademicYear,FederalEthRaceRollupName) %>%
         mutate(total=sum(num),
                perc=num/total) %>%
         filter(creds30OneYr==1) %>%
         ungroup() %>%
         select(-creds30OneYr) %>%
         mutate(indicator = "Complete30") %>%
         rename(subgroup=FederalEthRaceRollupName),
       
       cohort %>%
         group_by(LastRMRAcademicYear,RetainedTwoYr,FederalEthRaceRollupName) %>%
         summarize(num=length(SSID)) %>%
         #group_by(LastRMRAcademicYear,FederalEthRaceRollupName) %>%
         mutate(total=sum(num),
                perc=num/total) %>%
         filter(RetainedTwoYr==1) %>%
         ungroup() %>%
         select(-RetainedTwoYr) %>%
         mutate(indicator = "Retained") %>%
         rename(subgroup=FederalEthRaceRollupName)
         
       )
     )
     
     