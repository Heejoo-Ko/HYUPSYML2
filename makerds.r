library(readxl)
library(data.table)
library(magrittr)
library(labelled)
library(haven)
a<-as.data.table(read_excel("REAP-BD_20200917_to DrPark.xlsx",sheet=1,skip=5))

b<-data.table()

#outcome---------------------------------
b$Carbarmazepine <- ifelse(is.na(a$mcarbx),0,1)
b$Valproic <- ifelse(is.na(a$mvalpx),0,1)
b$Lamotrigine <- ifelse(is.na(a$mlamox),0,1)
b$Lithium <- ifelse(is.na(a$mlithx),0,1)
b$Lithium_and_Carbamazepine<-(b$Lithium&b$Carbarmazepine) %>% as.numeric
b$Lithium_and_Valproic<-(b$Lithium&b$Valproic) %>% as.numeric
b$Lithium_and_Lamotrigine<-(b$Lithium&b$Lamotrigine) %>% as.numeric

ad<-a[,209:232]
ad[,antidepressants:=rowSums(is.na(ad))!=24,]
b$antidepressants<-ad$antidepressants %>% as.numeric

b$ECT_Before_1_year <- ifelse(is.na(a$ECT_Before_1_year),0,ifelse(a$ECT_Before_1_year=="Yes",1,0))
b$BST_Before_1_year <- ifelse(is.na(a$BST_Before_1_year),0,ifelse(a$BST_Before_1_year=="Yes",1,0))

af<-names(b)
b[,(af):=lapply(.SD,function(x){
  factor(x, labels=c("No","Yes"))
}),.SDcols=af]

b$Region_code_area <- ifelse(a$Country %in% c("china","hongkong","Japan","korea","taiwan"),"eastern asia",
                             ifelse(a$Country %in% c("bangladesh","india","pakistan","srilanka"),"southeastern asia",
                                    ifelse(a$Country %in% c("indonesia","malaysia","myanmar","singapore","thailand","vietnam"),"southern asia",NA)))
b$Incode_code_area <- ifelse(a$Country %in% c("hongkong","Japan","korea","singapore","taiwan"),"high income",
                            ifelse(a$Country %in% c("china","malaysia","thailand"),"upper middle income",
                                   ifelse(a$Country %in% c("bangladesh","india","indonesia","myanmar","pakistan","srilanka","vietnam"),"lower middle income",NA)))

b$Patient <- ifelse(a$Patient=="N/A",NA,a$Patient)

b$Sex <- a$Sex %>% as.factor

#-172<=Age<=4848 이어서 0~120아니면 NA함
b$Age <- ifelse(a$Age>=0 & a$Age<=120, a$Age, NA)
#-999<=Weight<=999 이어서 30~180아니면 NA함
b$Weight <- ifelse(a$Weight>30 & a$Weight<=180, a$Weight, NA)
#72<=Height<=1999 이어서 0~280아니면 NA함
b$Height <- ifelse(a$Height<280,a$Height,NA)
b$BMI <- b$Weight/(b$Height/100 * b$Height/100)

b$Duration_BP<-ifelse(a$Duration %in% c("6 months or less","Less than 6 months"),"Less than 6 months",
                   ifelse(a$Duration %in% c("6 - 12 months","6 to 12 months"),"6 - 12 months",
                          ifelse(a$Duration %in% c("1 to 5 years","1 year - Less than 5 years"),"1 year - Less than 5 years",
                                 ifelse(a$Duration %in% c("5 years - Less than 10 years","6 to 10 years"),"5 years - Less than 10 years",
                                        ifelse(a$Duration %in% c("10 to 20 years","10years - Less than 20 years"),"10years - Less than 20 years",
                                               ifelse(a$Duration %in% c("More than 20 years","over 20 years"),"over 20 years",NA))))))

b$Duration_Untreated <- ifelse(a$DUI %in% c("6 months or less","Less than 6 months"),"Less than 6 months",
                               ifelse(a$DUI %in% c("6 - 12 months","6 to 12 months"),"6 - 12 months",
                                      ifelse(a$DUI %in% c("1 to 5 years","1 year - Less than 5 years"),"1 year - Less than 5 years",
                                             ifelse(a$DUI %in% c("More than 5 years","over 5 years"),"More than 5 years",NA))))

b$Employed <- ifelse(a$Financial_01 %in% c("checked","Yes"),"Yes","No")

b$BD_subclass <- ifelse(a$Diagnosis_R_C=="No information",NA,a$Diagnosis_R_C) %>% as.factor
b$BD_subclass <- factor(b$BD_subclass, labels = c("Current episode hypomanic",
                                 "Current episode manic without psychotic symptoms", 
                                 "Current episode with psychotic symptoms",
                                 "Current episode mild or moderate depression",
                                 "Current episode severe without psychotic symptoms",
                                 "Current episode severe depression with psychotic symptoms", 
                                 "Current episode mixed",
                                 "Currently in remission",
                                 "Other", 
                                 "Unspecified"))

b$Duration_current_episode <-ifelse(a$Episode %in% c("Less than 1 months","one month or less"),"Less than 1 months",
                                    ifelse(a$Episode %in% c("1 - 3 months","1 to 3 months"),"1 - 3 months",
                                           ifelse(a$Episode %in% c("3 - 6 months","3 to 6 months"),"3 - 6 months",
                                                  ifelse(a$Episode %in% c("6 - 12 months","6 to 12 months"),"6 - 12 months",
                                                         ifelse(a$Episode %in% c("More than 1 years","over 1 year"),"More than 1 years",NA)))))

b$Suicide_Lifetime <- a$Suicide_Lifetime

b$Suicide_Past_1_year <- ifelse(is.na(a$Suicide_Past_1_year) & is.na(a$Suicide_Past_1_month),NA,
                                ifelse(a$Suicide_Past_1_year=="Yes" | a$Suicide_Past_1_month=="Yes","Yes","No"))

b$RCAD_Last_1_year <- a$RCAD_Last_1_year
b$RCAD_Lifetime <- a$RCAD_Lifetime

b$Seasonality_Last_1_year <- a$Seasonality_Last_1_year
b$Seasonality_Lifetime <- a$Seasonality_Lifetime

b$illness_course_pattern <- ifelse(grepl("MDI",a$illness_course_pattern),"MDI",
                                   ifelse(grepl("DMI",a$illness_course_pattern),"DMI",NA))

af<-b[,!c("Age","Height","Weight","BMI")] %>% names
b[,(af):=lapply(.SD,as.factor),.SDcols=af]

saveRDS(b, file = "REAP-BD-20220307.rds")
