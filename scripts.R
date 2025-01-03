library(tidyverse)
library(readxl)
install.packages("dplyr")
library(dplyr)
install.packages("cardx")
library(cardx)
install.packages("broom.helpers")
library(broom.helpers)


data<-read_excel("raw_data/AMR_KAP_Data.xlsx")

sf28 <- data |>
  select(12:39)

colnames(sf28) <- paste0("Q",1:28)

#antibiotic knowledge 
antibiotic_knowledge <-sf28 |>
  select(Q1:Q12) |>
  mutate(across(Q1:Q12,~case_when(
    . =="Don't Know"~50 ,
    . =="Yes"~100 ,
    . == "No" ~50,
    TRUE~NA_real_
  )))|>
  rowwise() |>
  mutate(antibiotic_knowledge = mean (c_across(Q1:Q12),na.rm=TRUE))


# attitude
attitude<-sf28 |>
  select(Q13:Q22) |>
  mutate(across(Q13:Q22,~case_when(
    . =="Neutral"~50 ,
    . =="Agree"~100 ,
    . == "Disagree" ~50,
    TRUE~NA_real_
  )))|>
  rowwise() |>
  mutate( attitude= mean (c_across(Q13:Q22),na.rm=TRUE))



# practice
practice<-sf28 |>
  select(Q23:Q28) |>
  mutate(across(Q23:Q28,~case_when(
    . =="Yes"~100 ,
    . == "No" ~0,
    TRUE~NA_real_
  ))) |>
  rowwise() |>
  mutate( practice= mean (c_across(Q23:Q28),na.rm=TRUE))

#Combined data
demographics <- data |>
  select(1:11)

sources_of_information<- data |>
  select(41:49) 

sf_domains <- cbind(demographics,antibiotic_knowledge,attitude,practice,sources_of_information)
sf_domains <-sf_domains |>
  select(antibiotic_knowledge,attitude,practice)|>
  mutate(kap_score=rowMeans(cbind(antibiotic_knowledge,attitude,practice),na.rm=TRUE))

kap_data<-cbind(demographics,sf_domains,sources_of_information)

#export data
write.csv(kap_data,"clean_data/kap_clean.csv",row.names=FALSE)



#load package
library(tidyverse)
library(gtsummary)
library(gt)

library(xlsx)

#load data
data <- read.csv("clean_data/kap_clean.csv")


write.xlsx(kap_data,"clean_data/kap_clean.xlsx",row.names=FALSE)

data <- readxl::read_excel("clean_data/kap_clean.xlsx")



#table 1: Demographic characteristics of study participants
data |>
  select(1:11)|>
   tbl_summary()|>
  as_gt()|>
  gtsave("tables/Table10.docx")


#table 2: Major sources of information about antibiotic parents 
data |>
  select(16:24)|>
  tbl_summary()|>
  as_gt()|>
  gtsave("tables/Table2.docx")


#table 3: Level of knowledge, attitudes, and practices towards antibiotic resistance among parents with school-going children 

data |>
  select(25:37:45)
tbl_summary()|>
  as_gt()|>
  gtsave("tables/Table3.docx")


#table 4: Factors associated with the level of knowledge among parents of school-going children
data |>
  select(all_of(demographics))|>
  tbl_uvregression(
    method = lm,
    y=demographics
  )
test <- t.test(data$kap_score~data$Parent.s.age..years.)

data|>
  select(1:11,antibiotic_knowledge)|>
  tbl_uvregression(
  method=lm,
  y=antibiotic_knowledge
  )|>
  as_gt()|>
 gtsave("tables/Table4.docx")

#Table 5 Factors associated with the level of attitudes towards antibiotic resistance among parents of school-going children

data|>
  select(1:11,attitude)|>
  tbl_uvregression(
    method=lm,
    y=attitude
  )|>
  as_gt()|>
  gtsave("tables/Table5.docx")


#Table 6 Factors associated with the level of practices regarding antibiotic resistance among parents of school-going children
data|>
  select(1:11,practice)|>
  tbl_uvregression(
    method=lm,
    y=practice
  )|>
  as_gt()|>
  gtsave("tables/Table6.docx")



