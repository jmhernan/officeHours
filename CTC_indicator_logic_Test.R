# Create Flattened Student-Level CTC Indicators Table and Save to DataHub #

## load packages
# aggregation and reshaping tool package
library(tidyverse)
# sql package
library(RODBC)

#### load data ####
# connect to Wayfinder - pulls CTC completions and transcript records (already saved to working directory, loaded below)
db <- odbcDriverConnect('driver={SQL Server};
                        server=SQLDB-DEV-01;
                        database=Wayfinder;
                        trusted_connection=true')
# completions don't have InRMR flag, so joining on meta.schools 
compl <- sqlQuery(db, " 
                  SELECT a.*
                  FROM [Final].[CTC_Quarter_Completions] a
                  INNER JOIN Meta.K12_School sch 
                  ON a.K12_SchoolCode=sch.BuildingID and a.K12_DistrictCode=sch.DistrictCode and a.K12_AcademicYear=sch.AcademicYear
                  WHERE sch.dRoadMapRegionFlag=1
                  ")

cohort <- sqlQuery(db, "
                   SELECT *
                   FROM [Final].[CTC_QuarterTranscriptAnalysis] 
                   WHERE InRMR=1
                   ")

# close connection to Wayfinder
close(db)


#### quick fixes ####
#rename completion category
levels(compl$CompletionCategory)[levels(compl$CompletionCategory)=="General Studies or Associates, not Transfer"] <- "Workforce and Applied Associates"
#filter out bad subgroups
cohort <- cohort %>% filter(FederalEthRaceRollupName != "Not Provided" & CollegeName != "Seattle Voc Institute")
#fix bad FRPL data
cohort$FRPLFlag[cohort$LastRMRDistrictName %in% c("Kent School District","Seattle Public Schools")] <- NA

#### subset CTC records to Cohort of Interest ####
#1 students must have enrolled in an quarter Fall-Spring after high school graduation (quarter 2:4),
#2 also requiring that they enroll in the academic year immediately after their high school grad academic year
#3 every record must show enrollment in at least 1 credit (no non-credit bearing enrollments counted)
#4 students must have graduated from high school
cohort <- cohort %>% filter(SSID %in% SSID[AcademicQuarterCode %in% 2:4 #1
                                           & AcademicYear == LastRMRAcademicYear+1 #2
                                           & CreditsEnrolled > 0 #3
                                           & RMRGraduateFlag==1 #4
                                           ])

# filter completion table SSIDs to cohort SSIDs
compl <- compl %>% filter(SSID %in% unique(cohort$SSID))



#### calculated fields start ####
# How many quarters after graduation did student first enroll in a CTC? (negative value for dually-enrolled students)
# used in determining if a student ever dual-enrolled (below), and convenient for add-hoc queries
cohort <- cohort %>% mutate(QAGofFirstEnroll = QuartersAfterGraduation - QuartersAfterFirstEnrollment + 1)

# What were a students enrollment characteristics during their first enrolled quarter after high school (not including summer quarter) - 
# first academic quarter, college, full-time status
cohort1<-cohort %>%
  filter(AcademicQuarterCode != 1 & AcademicYear == LastRMRAcademicYear + 1) %>% # filters summer quarter
  arrange(SSID,AcademicQuarterCode) %>% 
  group_by(SSID) %>% 
  filter(rank(AcademicYear,ties.method = "first")==1) %>% 
  ungroup() %>% 
  select(SSID,AcademicYear,AcademicQuarterCode,CollegeName,FullTimeFlag) %>%
  rename(FirstYear=AcademicYear,
         FirstQ=AcademicQuarterCode,
         FirstCollegeAfterHS=CollegeName,
         FirstFullStatus=FullTimeFlag
         ) %>%
  mutate(FirstYearQuarter=FirstYear+(FirstQ/10))

# Similar to above in that this gets first quarter, but now regardless if summer or not 
# (because we include summer quarters in credit indicators)
# These fields are prefaced by "Real" to indicate they are the true value (in that they include any first enrollments in summer quarters)
cohort1Real<-cohort %>%
  filter(AcademicYear == LastRMRAcademicYear + 1) %>% # not filtering out summer quarter
  arrange(SSID,AcademicQuarterCode) %>% 
  group_by(SSID) %>% 
  filter(rank(AcademicYear,ties.method = "first")==1) %>% 
  ungroup() %>% 
  select(SSID,AcademicYear,AcademicQuarterCode) %>%
  rename(RealFirstYear=AcademicYear,
         RealFirstQ=AcademicQuarterCode
  ) %>%
  mutate(RealFirstYearQuarter=RealFirstYear+(RealFirstQ/10))

# join these first-quarter variables to master cohort and compl data frames
cohort <- left_join(cohort,cohort1,"SSID")
  cohort <- left_join(cohort,cohort1Real,"SSID")
compl <- left_join(compl,cohort1,"SSID")
  compl <- left_join(compl,cohort1Real,"SSID")

# create a field that combines year and quarter into unique year+quarter values
cohort <- cohort %>% mutate(YearQuarter = AcademicYear + (AcademicQuarterCode/10))
compl <- compl %>% mutate(YearQuarter = AcademicYear + (AcademicQuarterCode/10))

# OneYearMark: student's YearQuarter indicating has been enrolled for 1 academic year since direct enrolling
# summer enrollment NOT counted as 1st quarter
cohort <- cohort %>% mutate(OneYearMark = LastRMRAcademicYear + 2 +(FirstQ/10))
# summer enrollment IS counted as 1st quarter 
cohort <- cohort %>% mutate(RealOneYearMark = LastRMRAcademicYear + 2 +(RealFirstQ/10))

# TwoYearMark: student's YearQuarter indicating has been enrolled for 2 academic year since direct enrolling
# summer enrollment NOT counted as 1st quarter 
cohort <- cohort %>% mutate(TwoYearMark = LastRMRAcademicYear + 3 +(FirstQ/10))
# summer enrollment IS counted as 1st quarter 
cohort <- cohort %>% mutate(RealTwoYearMark = LastRMRAcademicYear + 3 +(RealFirstQ/10))



#### create flags for student characteristics ####

### dual enrollment ###

# Ever Running Start?
cohort <- cohort %>% mutate(EverRS = ifelse(SSID %in% SSID[RunningStartStatusCode==1],
                                            1,
                                            0)
                            )
# Never Running Start?
cohort <- cohort %>% mutate(NeverRS = ifelse(SSID %in% SSID[EverRS==0],
                                            1,
                                            0)
)

# Ever Other Dual-Enroll?
cohort <- cohort %>% mutate(EverOtherDualEnroll = ifelse(!(SSID %in% SSID[RunningStartStatusCode==1]) 
                                                         & SSID %in% SSID[QAGofFirstEnroll <= 0],
                                                         1,
                                                         0)
                            )

### course taking ###

# Ever took math at a CTC?
cohort <- cohort %>% group_by(SSID) %>% mutate(
  EverTakeMath = ifelse(SSID %in% SSID[AcademicYear >= LastRMRAcademicYear+1
                                       & MathFlag==1],
                        1,
                        0)
) %>% ungroup()

# Took math in first year at CTC?
cohort <- cohort %>% group_by(SSID) %>% mutate(
  EverTakeMathFirstYear = ifelse(SSID %in% SSID[AcademicYear >= LastRMRAcademicYear+1
                                       & YearQuarter < RealOneYearMark
                                       & MathFlag==1],
                        1,
                        0)
) %>% ungroup()


### income stats ###

# Non-FRPL flag (FRPL already exists. We use need-based financial aid for most income questions.)
cohort <- cohort %>% mutate(NonFRPLFlag = ifelse(FRPLFlag!=1,
                                                         1,
                                                         0)
)

# Student ever recieves need based financial aid (not during high school)?
cohort <- cohort %>% mutate(
  EverFinAid = ifelse(SSID %in% cohort$SSID[cohort$AcademicYear > LastRMRAcademicYear
                                            & cohort$FinancialAidNeedBasedFlag==1], 
                      1,
                      0)
)

# Student Never recieves need based financial aid (not during high school)?
cohort <- cohort %>% mutate(
  NeverFinAid = ifelse(SSID %in% cohort$SSID[cohort$AcademicYear > LastRMRAcademicYear
                                             & cohort$FinancialAidNeedBasedFlag==1], 
                       0,
                       1)
)


# for reference:
#                         CompletionCategory            CompletionCategoryID
# 1                        HS or GED Completion                    1
# 2                           Short Certificate                    2
# 3                            Long Certificate                    3
# 4 General Studies or Associates, not Transfer                    4
# 5                         Academic Associates                    5
# 6                         Workforce Bachelors                    6
# 7                                       Other                    7


#### 3 Year Flags ####
# these calculated values strictly depend on the specified timeframe, which is 3 years
# after graduating high school in our current analysis regime.

YEARS<-4    # years can be changed in case we want to look at different outcome timeframes


# Attended only 1 college after high school flag (useful when need to filter out students attending multiple schools)
cohort <- cohort %>% group_by(SSID) %>% mutate(
  ThreeYrOneCollege = ifelse(length(unique(CollegeName[AcademicYear >= LastRMRAcademicYear+1
                                                       & AcademicYear <= LastRMRAcademicYear+YEARS]))==1,
                      1,
                      0)
) %>% ungroup()


# count of # of colleges attended after high school
cohort <- cohort %>% group_by(SSID) %>% mutate(
  ThreeYrNumberColleges = length(unique(CollegeName[AcademicYear >= LastRMRAcademicYear+1
                                                       & AcademicYear <= LastRMRAcademicYear+YEARS]))
) %>% ungroup()


# 3-year completions (see completion category reference guide above)
cohort <- cohort %>% mutate(
  ThreeYrAwardAny = ifelse(SSID %in% compl$SSID[compl$CompletionCategoryID > 1 & compl$AcademicYear <= compl$K12_AcademicYear + YEARS], 
                       1,
                       0)
  )


#### 3-year Transfer ####


## STEP 1: pull in NSC dataset and add CTC academic year and month derived columns
# connect to Wayfinder
datahub <- odbcDriverConnect('driver={SQL Server};
                              server=SQLDB-DEV-01;
                              database=DataHub;
                              trusted_connection=true')


Mnsc <- sqlFetch(datahub,"Final.NSC")

QD <- sqlFetch(datahub,"meta.QuarterDates")
str(QD)
close(datahub)

# filter to CTC SSIDs
Mnsc <- Mnsc %>% filter(SSID %in% unique(cohort$SSID))

# load CTC academic year dates
#QD<-read.csv("Quarter Dates/Meta.CTC_QuarterDates.csv")
QD$EarliestEnd<-as.Date(QD$EarliestEnd)
QD$LatestStart<-as.Date(QD$LatestStart)

# format dates fields into date format
Mnsc$EnrollmentEnd<-as.Date(Mnsc$EnrollmentEnd)
Mnsc$EnrollmentBegin<-as.Date(Mnsc$EnrollmentBegin)
Mnsc$GraduationDate<-as.Date(Mnsc$GraduationDate)
# calculate month and day of enrollment. If a graduation record, use Graduation Date, else use Enrollment Begin date.
Mnsc$enrollM<-as.numeric(format(Mnsc$EnrollmentBegin,format="%m"))
Mnsc$enrollM[Mnsc$Graduated=="Y"]<-as.numeric(format(Mnsc$GraduationDate[Mnsc$Graduated=="Y"],format="%m"))
Mnsc$enrollD<-as.numeric(format(Mnsc$EnrollmentBegin,format="%d"))
Mnsc$enrollD[Mnsc$Graduated=="Y"]<-as.numeric(format(Mnsc$GraduationDate[Mnsc$Graduated=="Y"],format="%d"))

# add academic year based on quarter dates
# if enrollment END or graduation date is > latest start date for academic year, and < latest end date, assign appropriate academic year
x=as.Date('2010-03-19')
addYear <- function(x){
  year<-QD$Year[QD$LatestStart <= x & QD$LatestEnd >= x]
  ifelse(!is.na(year),return(year),return(NA))
}
str(QD$LatestStart)

year<-QD$Year[QD$LatestStart <= as.Date('2010-03-19') & QD$LatestEnd >= as.Date('2010-03-19')]

addYear(as.Date('2010-03-19'))

# add academic year column to Mnsc
Mnsc$nscYear<-unlist(
  lapply(
    ifelse(Mnsc$Graduated=="N",Mnsc$EnrollmentEnd, Mnsc$GraduationDate),
    function(x) addYear(x)
  )
)


## STEP 2: create ctc table containing most recent transcript record for each student
# get most recent ctc records, excluding summer quarter (this assumption could be revisited)
cohortmax <- cohort %>% 
  filter(AcademicQuarterCode != 1) %>% 
  arrange(desc(AcademicQuarterCode)) %>%
  group_by(SSID) %>%
  filter(rank(-AcademicYear, ties.method="first")==1)

## STEP 3: join max ctc record to modifified nsc table
ctc.nsc <- inner_join(cohortmax,Mnsc,"SSID")

## STEP 4: determine whether each student is considered to have transferred out of CTCs and create table of transfer students (ctc.nscT)
# transfers criteria subsetting
#   -no CTC records 3rd year
#   -nsc records after final CTC year and before 3 years after HSG (gets earliest nsc record after leaving CTCs within 3 yrs of HSG)
#   -nsc enrollments in summer do not count
#   (do not count NSC enrollments beginning in June and July- enrollM based on field EnrollmentBegin)
#   -institution transferred to is designated as 4-year
ctc.nscT <- ctc.nsc %>%
  filter(AcademicYear >= LastRMRAcademicYear+1 & AcademicYear <= LastRMRAcademicYear + (YEARS-1)
                  & nscYear > AcademicYear
                  & nscYear <= LastRMRAcademicYear + YEARS
                  & !(enrollM %in% 6:7)
                  & v2year4year == "4" # Consider changing this condition, or determining 4-year and 2-year transfers as separate outcomes
  ) %>%
  arrange(EnrollmentBegin,desc(EnrollmentEnd)) %>% # sorting so that rank gives latest enrollment end as second sorting criteria
  group_by(SSID) %>%
  filter(rank(EnrollmentBegin, ties.method="first")==1) # when tied, gets first enrollment with latest Enrollment End (i.e. longest enrollment)


# 3-year Transfer flag (by checking whether student is included in the transfers table)
cohort <- cohort %>% mutate(
  ThreeYrTransfer = ifelse(SSID %in% ctc.nscT$SSID,
                           1,
                           0)
)

# 3-year Any Award OR Transfer flag
cohort <- cohort %>% mutate(
  ThreeYrAwardOrTransfer = ifelse(ThreeYrAwardAny == 1 | ThreeYrTransfer == 1,
                           1,
                           0)
)

# 3-year Any Award AND Transfer flag
cohort <- cohort %>% mutate(
  ThreeYrAwardTransfer = ifelse(SSID %in% ctc.nscT$SSID
                                  & SSID %in% compl$SSID[compl$CompletionCategoryID > 1 & compl$AcademicYear <= compl$K12_AcademicYear + YEARS], 
                                  1,
                                  0)
)

# 3-year Any Award No Transfer flag
cohort <- cohort %>% mutate(
  ThreeYrAwardNoTransfer = ifelse(!(SSID %in% ctc.nscT$SSID) # "!" = not, so student did NOT transfer
                                & SSID %in% compl$SSID[compl$CompletionCategoryID > 1 & compl$AcademicYear <= compl$K12_AcademicYear + YEARS], 
                                1,
                                0)
)

# 3-year Transfer with No Award flag
cohort <- cohort %>% mutate(
  ThreeYrTransferNoAward = ifelse(SSID %in% ctc.nscT$SSID
                      & !(SSID %in% compl$SSID[compl$CompletionCategoryID > 1 & compl$AcademicYear <= compl$K12_AcademicYear + YEARS]), 
                      1,
                      0)
)

# 3-year Associate
cohort <- cohort %>% mutate(
  ThreeYrAssociates = ifelse(SSID %in% compl$SSID[compl$CompletionCategoryID == 5 & compl$AcademicYear <= compl$K12_AcademicYear + YEARS]
                             & !(SSID %in% compl$SSID[compl$CompletionCategoryID > 5]), 
                           1,
                           0)
)

# 3-year Associate Transfer
cohort <- cohort %>% mutate(
  ThreeYrAssociatesTransfer = ifelse(SSID %in% compl$SSID[compl$CompletionCategoryID == 5 & compl$AcademicYear <= compl$K12_AcademicYear + YEARS]
                             & !(SSID %in% compl$SSID[compl$CompletionCategoryID > 5])
                             & SSID %in% ctc.nscT$SSID, 
                             1,
                             0)
)

# 3-year Associate No Transfer
cohort <- cohort %>% mutate(
  ThreeYrAssociatesNoTransfer = ifelse(SSID %in% compl$SSID[compl$CompletionCategoryID == 5 & compl$AcademicYear <= compl$K12_AcademicYear + YEARS]
                             & !(SSID %in% compl$SSID[compl$CompletionCategoryID > 5])
                             & !(SSID %in% ctc.nscT$SSID), 
                             1,
                             0)
)

# 3-year Workforce and Applied Associates
cohort <- cohort %>% mutate(
  ThreeYrWorkforce = ifelse(SSID %in% compl$SSID[compl$CompletionCategoryID == 4 & compl$AcademicYear <= compl$K12_AcademicYear + YEARS]
                            & !(SSID %in% compl$SSID[compl$CompletionCategoryID > 4]), 
                             1,
                             0)
)

# 3-year Workforce and Applied Associates Transfer
cohort <- cohort %>% mutate(
  ThreeYrWorkforceTransfer = ifelse(SSID %in% compl$SSID[compl$CompletionCategoryID == 4 & compl$AcademicYear <= compl$K12_AcademicYear + YEARS]
                                     & !(SSID %in% compl$SSID[compl$CompletionCategoryID > 4])
                                     & SSID %in% ctc.nscT$SSID, 
                                     1,
                                     0)
)

# 3-year Workforce and Applied Associates No Transfer
cohort <- cohort %>% mutate(
  ThreeYrWorkforceNoTransfer = ifelse(SSID %in% compl$SSID[compl$CompletionCategoryID == 4 & compl$AcademicYear <= compl$K12_AcademicYear + YEARS]
                                       & !(SSID %in% compl$SSID[compl$CompletionCategoryID > 4])
                                       & !(SSID %in% ctc.nscT$SSID), 
                                       1,
                                       0)
)

# 3-year long certificate
cohort <- cohort %>% mutate(
  ThreeYrLongCert = ifelse(SSID %in% compl$SSID[compl$CompletionCategoryID == 3 & compl$AcademicYear <= compl$K12_AcademicYear + YEARS]
                           & !(SSID %in% compl$SSID[compl$CompletionCategoryID > 3]), 
                             1,
                             0)
)

# 3-year long certificate Transfer
cohort <- cohort %>% mutate(
  ThreeYrLongCertTransfer = ifelse(SSID %in% compl$SSID[compl$CompletionCategoryID == 3 & compl$AcademicYear <= compl$K12_AcademicYear + YEARS]
                           & !(SSID %in% compl$SSID[compl$CompletionCategoryID > 3])
                           & SSID %in% ctc.nscT$SSID, 
                           1,
                           0)
)

# 3-year long certificate No Transfer
cohort <- cohort %>% mutate(
  ThreeYrLongCertNoTransfer = ifelse(SSID %in% compl$SSID[compl$CompletionCategoryID == 3 & compl$AcademicYear <= compl$K12_AcademicYear + YEARS]
                                   & !(SSID %in% compl$SSID[compl$CompletionCategoryID > 3])
                                   & !(SSID %in% ctc.nscT$SSID), 
                                   1,
                                   0)
)

# 3-year short certificate
cohort <- cohort %>% mutate(
  ThreeYrShortCert = ifelse(SSID %in% compl$SSID[compl$CompletionCategoryID == 2 & compl$AcademicYear <= compl$K12_AcademicYear + YEARS]
                            & !(SSID %in% compl$SSID[compl$CompletionCategoryID > 2]), 
                             1,
                             0)
)

# 3-year short certificate Transfer
cohort <- cohort %>% mutate(
  ThreeYrShortCertTransfer = ifelse(SSID %in% compl$SSID[compl$CompletionCategoryID == 2 & compl$AcademicYear <= compl$K12_AcademicYear + YEARS]
                            & !(SSID %in% compl$SSID[compl$CompletionCategoryID > 2])
                            & SSID %in% ctc.nscT$SSID, 
                            1,
                            0)
)

# 3-year short certificate No Transfer
cohort <- cohort %>% mutate(
  ThreeYrShortCertNoTransfer = ifelse(SSID %in% compl$SSID[compl$CompletionCategoryID == 2 & compl$AcademicYear <= compl$K12_AcademicYear + YEARS]
                                    & !(SSID %in% compl$SSID[compl$CompletionCategoryID > 2])
                                    & !(SSID %in% ctc.nscT$SSID), 
                                    1,
                                    0)
)

# 3-year No Award, No Transfer
cohort <- cohort %>% mutate(
  ThreeYrNoAwardNoTransfer = ifelse(!(SSID %in% compl$SSID[compl$CompletionCategoryID > 1 & compl$AcademicYear <= compl$K12_AcademicYear + YEARS])
                                    & !(SSID %in% ctc.nscT$SSID), 
                           1,
                           0)
)


### Determine Total Accumulated Credits after 3 years ###

# create mini credits table (used to create flags with credit parameters later)
creds45 <- cohort %>% 
  filter(AcademicYear >= LastRMRAcademicYear+1 & AcademicYear <= LastRMRAcademicYear+YEARS) %>%
  group_by(SSID) %>% 
  summarize(creds = sum(CreditsEarned[CollegeLevelFlag==1])) %>%
  filter(creds >= 45) %>%
  select(SSID)

credsLess45 <- cohort %>% 
  filter(AcademicYear >= LastRMRAcademicYear+1 & AcademicYear <= LastRMRAcademicYear+YEARS) %>%
  group_by(SSID) %>%
  summarize(creds = sum(CreditsEarned[CollegeLevelFlag==1])) %>%
  filter(creds < 45) %>%
  select(SSID)

#### Detailed 3-year non-completion Outcome Categories ####

# 3-year No Award No Transfer Still Enrolled 
cohort <- cohort %>% mutate(
  ThreeYrStillEnrolled= ifelse(!(SSID %in% compl$SSID[compl$CompletionCategoryID > 1 & compl$AcademicYear <= compl$K12_AcademicYear + YEARS])
                               & !(SSID %in% ctc.nscT$SSID)
                               & SSID %in% cohort$SSID[cohort$AcademicYear==cohort$LastRMRAcademicYear+YEARS],
                               1,
                               0)
)

# 3-year No Award No Transfer Still Enrolled >= 45 college credits 
cohort <- cohort %>% mutate(
  ThreeYrStillEnrolled45Creds= ifelse(!(SSID %in% compl$SSID[compl$CompletionCategoryID > 1 & compl$AcademicYear <= compl$K12_AcademicYear + YEARS])
                                    & !(SSID %in% ctc.nscT$SSID)
                                    & SSID %in% cohort$SSID[cohort$AcademicYear==cohort$LastRMRAcademicYear+YEARS]
                                    & SSID %in% creds45$SSID, 
                                    1,
                                    0)
)



# 3-year No Award No Transfer Still Enrolled < 45 college credits 
cohort <- cohort %>% mutate(
  ThreeYrStillEnrolledLess45Creds= ifelse(!(SSID %in% compl$SSID[compl$CompletionCategoryID > 1 & compl$AcademicYear <= compl$K12_AcademicYear + YEARS])
                                      & !(SSID %in% ctc.nscT$SSID)
                                      & SSID %in% cohort$SSID[cohort$AcademicYear==cohort$LastRMRAcademicYear+YEARS]
                                      & SSID %in% credsLess45$SSID, 
                                      1,
                                      0)
)

# 3-year No Award No Transfer Left Postsecondary
cohort <- cohort %>% mutate(
  ThreeYrLeft= ifelse(!(SSID %in% compl$SSID[compl$CompletionCategoryID > 1 & compl$AcademicYear <= compl$K12_AcademicYear + YEARS])
                               & !(SSID %in% ctc.nscT$SSID)
                               & !(SSID %in% cohort$SSID[cohort$AcademicYear==cohort$LastRMRAcademicYear+YEARS]), 
                               1,
                               0)
)

# 3-year No Award No Transfer Left Postsecondary >= 45 credits
cohort <- cohort %>% mutate(
  ThreeYrLeft45Creds= ifelse(!(SSID %in% compl$SSID[compl$CompletionCategoryID > 1 & compl$AcademicYear <= compl$K12_AcademicYear + YEARS])
                      & !(SSID %in% ctc.nscT$SSID)
                      & !(SSID %in% cohort$SSID[cohort$AcademicYear==cohort$LastRMRAcademicYear+YEARS])
                      & SSID %in% creds45$SSID, 
                      1,
                      0)
)

# 3-year No Award No Transfer Left Postsecondary < 45 credits
cohort <- cohort %>% mutate(
  ThreeYrLeftLess45Creds= ifelse(!(SSID %in% compl$SSID[compl$CompletionCategoryID > 1 & compl$AcademicYear <= compl$K12_AcademicYear + YEARS])
                             & !(SSID %in% ctc.nscT$SSID)
                             & !(SSID %in% cohort$SSID[cohort$AcademicYear==cohort$LastRMRAcademicYear+YEARS])
                             & SSID %in% credsLess45$SSID, 
                             1,
                             0)
)


#### Pre-College Courses ####

# Pre-college Math course in 1st year flag
### RealOneYearMark is used to accomodate students first enrolling in summer quarter 
cohort <- cohort %>% mutate(
  PreCollegeMathFirstYear = ifelse(SSID %in% SSID[AcademicYear >= LastRMRAcademicYear + 1
                                             & YearQuarter < RealOneYearMark
                                             & CollegeLevelFlag==0
                                             & MathFlag==1],
                              1,
                              0)
)

cohort <- cohort %>% mutate(
  CollegeMathTwoYear2 = ifelse(SSID %in% SSID[AcademicYear >= LastRMRAcademicYear + 1
                                                  & YearQuarter < RealTwoYearMark
                                                  & CollegeLevelFlag==1
                                                  & MathFlag==1
                                                  & CreditsEarned > 0 ],
                                   1,
                                   0)
)

# Pre-college English course in 1st year flag
cohort <- cohort %>% mutate(
  PreCollegeEnglishFirstYear = ifelse(SSID %in% SSID[AcademicYear >= LastRMRAcademicYear + 1
                                                 & YearQuarter < RealOneYearMark
                                                 & CollegeLevelFlag==0
                                                 & EnglishFlag==1],
                                  1,
                                  0)
)

# Pre-college Math or English course in 1st year flag
cohort <- cohort %>% mutate(
  PreCollegeAnyFirstYear = ifelse(SSID %in% SSID[AcademicYear >= LastRMRAcademicYear + 1
                                             & YearQuarter < RealOneYearMark
                                             & CollegeLevelFlag==0
                                             & (EnglishFlag==1 | MathFlag==1)],
                                 1,
                                 0)
)

# Pre-college Math any time
cohort <- cohort %>% mutate(
  PreCollegeMathAnyTime = ifelse(SSID %in% SSID[AcademicYear >= LastRMRAcademicYear + 1
                                            & CollegeLevelFlag==0
                                            & MathFlag==1],
                             1,
                             0)
)

# Pre-college English course any time
cohort <- cohort %>% mutate(
  PreCollegeEnglishAnyTime = ifelse(SSID %in% SSID[AcademicYear >= LastRMRAcademicYear + 1
                                            & CollegeLevelFlag==0
                                            & EnglishFlag==1],
                             1,
                             0)
)

# Pre-college Math or English course any time
cohort <- cohort %>% mutate(
  PreCollegeAnyTime = ifelse(SSID %in% SSID[AcademicYear >= LastRMRAcademicYear + 1
                                                 & CollegeLevelFlag==0
                                                 & (EnglishFlag==1 | MathFlag==1)],
                                  1,
                                  0)
)

# College-Ready or Unknown (no Pre-College courses taken)
cohort <- cohort %>% mutate(
  CollegeReadyOrUnknown = ifelse(SSID %in% SSID[PreCollegeAnyTime==0],
                             1,
                             0)
)


#### Enrollment Status Calculations ####

# number of quarters enrolled
cohort <- cohort %>%
  group_by(SSID) %>% 
  mutate(ThreeYrNumQuarters=length(unique(
    AcademicYear[AcademicQuarterCode != 1 & AcademicYear >= LastRMRAcademicYear+1 & AcademicYear <= LastRMRAcademicYear+YEARS] + 
      AcademicQuarterCode[AcademicQuarterCode !=1 & AcademicYear >= LastRMRAcademicYear+1 & AcademicYear <= LastRMRAcademicYear+YEARS]/10
  ))) %>%
  ungroup()

# Last enrollment
cohortmax <- cohort %>% 
  filter(AcademicQuarterCode != 1 & AcademicYear >= LastRMRAcademicYear+1 & AcademicYear <= LastRMRAcademicYear+YEARS) %>% 
  arrange(desc(AcademicYear),desc(AcademicQuarterCode)) %>%
  group_by(SSID) %>%
  filter(rank(-AcademicYear, ties.method="first")==1) %>%
  summarize(MaxYear=AcademicYear,MaxQuarter=AcademicQuarterCode)

cohort <- left_join(cohort,cohortmax,by = "SSID")

# Percentage of Quarters Enrolled
cohort <- cohort %>% mutate(PercEnrolled=ThreeYrNumQuarters/
                              ((MaxYear-FirstYear)*3 + (MaxQuarter-FirstQ + 1))
)

# Continuously Enrolled flag
cohort <- cohort %>% mutate(ContEnrolled=ifelse(PercEnrolled == 1,
                                                1,
                                                0)
)

# not continuously enrolled flag
cohort <- cohort %>% mutate(NotContEnrolled=ifelse(ContEnrolled == 0,
                                                1,
                                                0)
)

### Full-time Part-time ###

# average credits enrolled per quarter
cohort <- cohort %>%
  group_by(SSID) %>% 
  mutate(ThreeYrCredsQ=sum(CreditsEnrolled[AcademicQuarterCode != 1 # & CollegeLevelFlag==1 (no longer used - was a big mistake in first CTC report)
                                         & AcademicYear >= LastRMRAcademicYear+1 & AcademicYear <= LastRMRAcademicYear+YEARS])/length(unique(
    AcademicYear[AcademicQuarterCode != 1 & AcademicYear >= LastRMRAcademicYear+1 & AcademicYear <= LastRMRAcademicYear+YEARS] + 
      AcademicQuarterCode[AcademicQuarterCode !=1 & AcademicYear >= LastRMRAcademicYear+1 & AcademicYear <= LastRMRAcademicYear+YEARS]/10
  ))) %>%
  ungroup()

# Full Time on average
cohort <- cohort %>% mutate(
  FullTimeAvg = ifelse(ThreeYrCredsQ >= 12,
                                  1,
                                  0)
)

# Part-Time on average
cohort <- cohort %>% mutate(
  PartTimeAvg = ifelse(ThreeYrCredsQ < 12,
                       1,
                       0)
)

#### Fields used to filter Completers and Transfers from metrics where completers and transfers are not valid part of sample ####

# completion within 1 full academic year from start (used to filter out completers from metrics)
cohort <- cohort %>% mutate(
  complOneFullYr = ifelse(SSID %in% compl$SSID[compl$CompletionCategoryID > 1 
                                               & compl$YearQuarter >= compl$RealFirstYearQuarter # makes sure completions in first summer quarter counted
                                               & compl$YearQuarter <= compl$FirstYearQuarter + 1],
                          1,
                          0)
)

# completion 2 full academic years from start (used to filter out completers from metrics)
cohort <- cohort %>% mutate(
  complTwoFullYr = ifelse(SSID %in% compl$SSID[compl$CompletionCategoryID > 1 
                                               & compl$YearQuarter >= compl$RealFirstYearQuarter # makes sure completions in first summer quarter counted
                                               & compl$YearQuarter < compl$FirstYearQuarter + 2],
                          1,
                          0)
)

# transfer by 2nd academic year from start (used to filter out transfers from metrics)
cohort <- cohort %>% mutate(
  transfer2ndYr = ifelse(SSID %in% ctc.nscT$SSID[ctc.nscT$nscYear == ctc.nscT$LastRMRAcademicYear + 2],
                          1,
                          0)
)

#### Progress Indicators ####

# Move on to College-Level Math
# must enroll in, and receive credit in, pre-college math in their 1st year
# must also enroll in college-level math during 1st or/and 2nd year
# does not exclude students not returning for 2nd year - still want to
# hold these students accountable for reaching college-level math
cohort <- cohort %>% mutate(
  MoveOnToCollegeMath = ifelse(SSID %in% SSID[YearQuarter < RealOneYearMark
                                                  & CollegeLevelFlag==0
                                                  & MathFlag==1
                                                  & CreditsEarned > 0]
                               & SSID %in% SSID[YearQuarter < RealTwoYearMark
                                                & CollegeLevelFlag==1
                                                & MathFlag==1],
                                   1,
                                   0)
)
################
cohort <- cohort %>% mutate(
  CollegeMathFirstTwo = ifelse(SSID %in% SSID[YearQuarter < RealOneYearMark
                                              & CollegeLevelFlag==0
                                              & MathFlag==1
                                              & CreditsEarned > 0]
                               & SSID %in% SSID[YearQuarter < RealTwoYearMark
                                                & CollegeLevelFlag==1
                                                & MathFlag==1
                                                & CreditsEarned > 0],
                               1,
                               0)
)


# Retention to 2nd Year: students returning for second academic year (enroll Fall quarter of second year)
## need to also satisfy condition 1) did not transfer out by second year 2) did not complete 1st year
# does not matter when first enroll. Exploratory analysis confirms that largest enrollment drop happens between spring
# of first year and fall of second year
cohort <- cohort %>% mutate(
  RetainedTwoYr = ifelse(SSID %in% SSID[YearQuarter==(LastRMRAcademicYear + 2.2)]
                         | SSID %in% SSID[complOneFullYr==1 | transfer2ndYr==1],
                               1,
                               0)
)

# persistence to 2nd year
cohort <- cohort %>% mutate(
  PersistTwoYr = ifelse(SSID %in% SSID[YearQuarter >= OneYearMark & YearQuarter <= TwoYearMark],
                         1,
                         0)
)


# create mini credits tables for use in flagging students into different credit accumulation categories
creds15OneYr <- cohort %>% 
  filter(YearQuarter >= RealFirstYearQuarter & YearQuarter < RealOneYearMark) %>%
  group_by(SSID) %>% 
  summarize(creds = sum(CreditsEarned[CollegeLevelFlag==1])) %>%
  filter(creds >= 15) %>%
  select(SSID)

creds30OneYr <- cohort %>% 
  filter(YearQuarter >= RealFirstYearQuarter & YearQuarter < RealOneYearMark) %>%
  group_by(SSID) %>% 
  summarize(creds = sum(CreditsEarned[CollegeLevelFlag==1])) %>%
  filter(creds >= 30) %>%
  select(SSID)

creds45TwoYr <- cohort %>% 
  filter(YearQuarter >= RealFirstYearQuarter & YearQuarter < RealTwoYearMark) %>%
  group_by(SSID) %>% 
  summarize(creds = sum(CreditsEarned[CollegeLevelFlag==1])) %>%
  filter(creds >= 45) %>%
  select(SSID)



# 15 credits first year 
cohort <- cohort %>% mutate(
  creds15OneYr = ifelse(SSID %in% creds15OneYr$SSID, 
                                      1,
                                      0)
)

# 30 credits first year 
cohort <- cohort %>% mutate(
  creds30OneYr = ifelse(SSID %in% creds30OneYr$SSID, 
                        1,
                        0)
)

# 45 credits second year 
cohort <- cohort %>% mutate(
  creds45TwoYr = ifelse(SSID %in% creds45TwoYr$SSID, 
                        1,
                        0)
)


#### Completion Types ####

#### Completions types analysis ####

# highest completion category and degree title in 3 years
complH <- compl %>%
  filter(CompletionCategoryID > 1 &
         AcademicYear >= K12_AcademicYear + 1 
         & AcademicYear <= K12_AcademicYear + 3) %>%
  arrange(-AcademicYear, -AcademicQuarterCode) %>%
  group_by(SSID) %>%
  filter(rank(-CompletionCategoryID,ties.method = "first")==1) %>%
  ungroup() %>%
  select(SSID,CompletionCategory,DegreeTitle) %>%
  rename(HighestCompletion = CompletionCategory,
         HighestDegreeTitle = DegreeTitle)

cohort <- left_join(cohort,complH,by="SSID")




#### Flatten indicators table ####
# 1 record per student
FlatCohort <- cohort %>%
  select(
    SSID,
    Gender,
    FederalEthRaceRollupName,
    FRPLFlag,
    NonFRPLFlag,
    ELLFlag,
    LastRMRAcademicYear,
    LastRMRGradeLevelSortOrder,
    LastRMRDistrictName,
    LastRMRDistrictCode,
    LastRMRSchoolName,
    LastRMRSchoolCode,
    FirstCollegeAfterHS,
    QAGofFirstEnroll,
    FirstQ,
    FirstYearQuarter,
    FirstFullStatus,
    EverRS,
    NeverRS,
    EverTakeMath,
    EverTakeMathFirstYear,
    EverOtherDualEnroll,
    ThreeYrOneCollege,
    ThreeYrNumberColleges,
    PreCollegeMathFirstYear,
    PreCollegeEnglishFirstYear,
    PreCollegeAnyFirstYear,
    PreCollegeMathAnyTime,
    PreCollegeEnglishAnyTime,
    PreCollegeAnyTime,
    CollegeReadyOrUnknown,
    ThreeYrNumQuarters,
    ThreeYrCredsQ,
    FullTimeAvg,
    PartTimeAvg,
    MoveOnToCollegeMath,
    creds15OneYr,
    creds30OneYr,
    creds45TwoYr,
    PercEnrolled,
    ContEnrolled,
    NotContEnrolled,
    EverFinAid,
    NeverFinAid,
    HighestCompletion,
    HighestDegreeTitle,
    CollegeMathTwoYear2,
    CollegeMathFirstTwo
    ) %>%
  unique()

saveRDS(FlatCohort,"n_ind.rda")

#### save flattened indicators table to DataHub ####
datahub <-odbcConnect("DataHub", uid=Sys.getenv("SQLUser"), pwd=Sys.getenv("SQLDevPass"))
sqlQuery(datahub," IF OBJECT_ID('dbo.CTC_Indicators', 'U') IS NOT NULL
         DROP TABLE dbo.CTC_Indicators")
sqlSave(datahub, FlatCohort, tablename = "CTC_Indicators",rownames = F,fast = T)
close(datahub)

##################


