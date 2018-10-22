---
title: "Psychometrics Gatekeeper Prelim Results"
output:
  pdf_document: default
  html_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
################
Data cleaning
################
```{r}
library(foreign)
library(nnet)
library(ggplot2)
library(prettyR)
library(nlme)
library(prettyR)
library(descr)
library(Amelia)
library(mitools)
library(BaylorEdPsych)
library(openxlsx)
library(lavaan)
library(lavaan)
library(psych)
library(semTools)
library(dplyr)
library(ltm)
library(prettyR)
library(semTools)
library(GPArotation)
library(lavaan)
library(psych)
library(semTools)
library(dplyr)
library(ltm)
library(lordif)
library(Amelia)
library(plyr)
library(DescTools)
library(MissMech)
library(robustlmm)
library(jtools)
library(lmtest)
library(lmerTest)
library(MuMIn)
library(HLMdiag)
library(Hmisc)
library(stargazer)
library(effsize)
setwd("P:/Evaluation/TN Lives Count_Writing/3_Target1_SUICClinicalTrainingComparison/3_Data & Analyses")
datPre = read.csv("Pre.csv", header = FALSE, row.names = NULL)

datPre = t(datPre)
write.csv(datPre, "datPre.csv", row.names = FALSE)
datPre = read.csv("datPre.csv", header = TRUE)
head(datPre)
dim(datPre)

datPre = datPre[,c(1, 3, 7:18, 21:35, 38:45, 49:72, 78:80, 83, 85, 94)]
datPre = data.frame(datPre)
head(datPre)

colnames(datPre) = c("ID", "Treatment", "Sec1Qa", "Sec1Qb", "Sec1Qc", "Sec1Qd", "Sec1Qe", "Sec1Qf", "Sec1Qg", "Sec1Qh", "Sec1Qi", "Sec1Qj", "Sec1Qk", "Sec1Ql", "Sec2Qa", "Sec2Qb", "Sec2Qc", "Sec2Qd", "Sec2Qe", "Sec2Qf", "Sec2Qg",  "Sec2Qh", "Sec2Qi", "Sec2Qj", "Sec2Qk", "Sec2Ql", "Sec2Qm", "Sec2Qn", "Sec2Qo", "Sec3Qa", "Sec3Qb", "Sec3Qc", "Sec3Qd", "Sec3Qe", "Sec3Qf", "Sec3Qg", "Sec3Qh", "Sec4QaA", "Sec4QaB", "Sec4QbA", "Sec4QbB", "Sec4QcA", "Sec4QcB", "Sec4QdA", "Sec4QdB", "Sec4QeA", "Sec4QeB", "Sec4QfA","Sec4QfB", "Sec4QgA", "Sec4QgB", "Sec4QhA", "Sec4QhB", "Sec4QiA", "Sec4QiB", "Sec4QjA", "Sec4QjB", "Sec4QkA", "Sec4QkB", "Sec4QlA", "Sec4QlB","Age", "Gender", "Eth", "Race", "Edu", "Clinical_Staff")
head(datPre)
### Get rif of first row once you figure out which variables to keep
datPre = datPre[-1,]
datPre = data.frame(datPre)
head(datPre)

#Only retain clincial staff 1
datPre = subset(datPre, Clinical_Staff == 1)
head(datPre)
dim(datPre)

setwd("P:/Evaluation/TN Lives Count_Writing/3_Target1_SUICClinicalTrainingComparison/3_Data & Analyses")
datPost = read.csv("Post.csv", header = FALSE, row.names = NULL)

datPost = t(datPost)
write.csv(datPost, "datPost.csv", row.names = FALSE)
datPost = read.csv("datPost.csv", header = TRUE)
head(datPost)

datPost = datPost[,c(3, 5, 15:26, 29:43, 46:53, 57:80)]

datPost = data.frame(datPost)

colnames(datPost) = c("ID", "Treatment", "Sec1Qa", "Sec1Qb", "Sec1Qc", "Sec1Qd", "Sec1Qe", "Sec1Qf", "Sec1Qg", "Sec1Qh", "Sec1Qi", "Sec1Qj", "Sec1Qk", "Sec1Ql", "Sec2Qa", "Sec2Qb", "Sec2Qc", "Sec2Qd", "Sec2Qe", "Sec2Qf", "Sec2Qg",  "Sec2Qh", "Sec2Qi", "Sec2Qj", "Sec2Qk", "Sec2Ql", "Sec2Qm", "Sec2Qn", "Sec2Qo", "Sec3Qa", "Sec3Qb", "Sec3Qc", "Sec3Qd", "Sec3Qe", "Sec3Qf", "Sec3Qg", "Sec3Qh", "Sec4QaA", "Sec4QaB", "Sec4QbA", "Sec4QbB", "Sec4QcA", "Sec4QcB", "Sec4QdA", "Sec4QdB", "Sec4QeA", "Sec4QeB", "Sec4QfA","Sec4QfB", "Sec4QgA", "Sec4QgB", "Sec4QhA", "Sec4QhB", "Sec4QiA", "Sec4QiB", "Sec4QjA", "Sec4QjB", "Sec4QkA", "Sec4QkB", "Sec4QlA", "Sec4QlB")
head(datPost)

# making treatment null, because it does not change and it will make so it doesn't repeat later 
datPost$ID = as.factor(datPost$ID)
datPost = datPost[-1,]
datPost$Treatment = NULL
datPost = data.frame(datPost)

dim(datPre)
dim(datPost)


write.csv(datPre, "datPre.csv", row.names = FALSE)
datPre = read.csv("datPre.csv", header = TRUE)

write.csv(datPost, "datPost.csv", row.names = FALSE)
datPost = read.csv("datPost.csv", header = TRUE)

#Should not have ID one, because they are the wrong code and not in datPre
datPrePost = merge(datPre, datPost, by = "ID",  all.x = TRUE, sort = TRUE)



dat3month = read.csv("3month.csv", header  = TRUE)
head(dat3month)
dat3month = dat3month[c(7, 11:22, 23:69)]
dim(datPost)
head(datPost)

# Now rename everything 
colnames(dat3month) = c("ID", "Sec1Qa", "Sec1Qb", "Sec1Qc", "Sec1Qd", "Sec1Qe", "Sec1Qf", "Sec1Qg", "Sec1Qh", "Sec1Qi", "Sec1Qj", "Sec1Qk", "Sec1Ql", "Sec2Qa", "Sec2Qb", "Sec2Qc", "Sec2Qd", "Sec2Qe", "Sec2Qf", "Sec2Qg",  "Sec2Qh", "Sec2Qi", "Sec2Qj", "Sec2Qk", "Sec2Ql", "Sec2Qm", "Sec2Qn", "Sec2Qo", "Sec3Qa", "Sec3Qb", "Sec3Qc", "Sec3Qd", "Sec3Qe", "Sec3Qf", "Sec3Qg", "Sec3Qh", "Sec4QaA", "Sec4QaB", "Sec4QbA", "Sec4QbB", "Sec4QcA", "Sec4QcB", "Sec4QdA", "Sec4QdB", "Sec4QeA", "Sec4QeB", "Sec4QfA","Sec4QfB", "Sec4QgA", "Sec4QgB", "Sec4QhA", "Sec4QhB", "Sec4QiA", "Sec4QiB", "Sec4QjA", "Sec4QjB", "Sec4QkA", "Sec4QkB", "Sec4QlA", "Sec4QlB")


datPrePost3month = merge(datPrePost, dat3month, by = "ID", all.x = TRUE, sort = TRUE)

head(datPrePost3month)


### Now make long format
### These variables are not included: 							

datPrePost3month = reshape(datPrePost3month, varying  = list(c("Sec1Qa.x", "Sec1Qa.y", "Sec1Qa"), c("Sec1Qb.x", "Sec1Qb.y", "Sec1Qb"), c("Sec1Qc.x", "Sec1Qc.y", "Sec1Qc"), c("Sec1Qd.x", "Sec1Qd.y", "Sec1Qd"), c("Sec1Qe.x", "Sec1Qe.y", "Sec1Qe"), c("Sec1Qf.x", "Sec1Qf.y", "Sec1Qf"), c("Sec1Qg.x", "Sec1Qg.y", "Sec1Qg"), c("Sec1Qh.x", "Sec1Qh.y", "Sec1Qh"), c("Sec1Qi.x", "Sec1Qi.y", "Sec1Qi"), c("Sec1Qj.x", "Sec1Qj.y", "Sec1Qj"), c("Sec1Qk.x", "Sec1Qk.y", "Sec1Qk"), c("Sec1Ql.x", "Sec1Ql.y", "Sec1Ql"), c("Sec2Qa.x", "Sec2Qa.y", "Sec2Qa"), c("Sec2Qb.x", "Sec2Qb.y", "Sec2Qb"), c("Sec2Qc.x", "Sec2Qc.y", "Sec2Qc"), c("Sec2Qd.x", "Sec2Qd.y", "Sec2Qd"), c("Sec2Qe.x", "Sec2Qe.y", "Sec2Qe"), c("Sec2Qf.x", "Sec2Qf.y", "Sec2Qf"), c("Sec2Qg.x", "Sec2Qg.y", "Sec2Qg"), c("Sec2Qh.x", "Sec2Qh.y", "Sec2Qh"), c("Sec2Qi.x", "Sec2Qi.y", "Sec2Qi"), c("Sec2Qj.x", "Sec2Qj.y", "Sec2Qj"), c("Sec2Qk.x", "Sec2Qk.y", "Sec2Qk"), c("Sec2Ql.x", "Sec2Ql.y", "Sec2Ql"), c("Sec2Qm.x", "Sec2Qm.y", "Sec2Qm"), c("Sec2Qn.x", "Sec2Qn.y", "Sec2Qn"), c("Sec2Qo.x", "Sec2Qo.y", "Sec2Qo"), c("Sec3Qa.x", "Sec3Qa.y","Sec3Qa"), c("Sec3Qb.x", "Sec3Qb.y", "Sec3Qb"), c("Sec3Qc.x", "Sec3Qc.y", "Sec3Qc"), c("Sec3Qd.x", "Sec3Qd.y", "Sec3Qd"), c("Sec3Qe.x", "Sec3Qe.y", "Sec3Qe"), c("Sec3Qf.x", "Sec3Qf.y", "Sec3Qf"), c("Sec3Qg.x", "Sec3Qg.y", "Sec3Qg"), c("Sec3Qh.x", "Sec3Qh.y", "Sec3Qh"), c("Sec4QaA.x", "Sec4QaA.y", "Sec4QaA"), c("Sec4QaB.x", "Sec4QaB.y", "Sec4QaB"), c("Sec4QbA.x", "Sec4QbA.y", "Sec4QbA"), c("Sec4QbB.x", "Sec4QbB.y", "Sec4QbB"), c("Sec4QcA.x", "Sec4QcA.y", "Sec4QcA"), c("Sec4QcB.x", "Sec4QcB.y", "Sec4QcB"), c("Sec4QdA.x", "Sec4QdA.y", "Sec4QdA"), c("Sec4QdB.x", "Sec4QdB.y", "Sec4QdB"), c("Sec4QeA.x", "Sec4QeA.y", "Sec4QeA"), c("Sec4QeB.x", "Sec4QeB.y", "Sec4QeB"), c("Sec4QfA.x", "Sec4QfA.y", "Sec4QfA"), c("Sec4QfB.x", "Sec4QfB.y", "Sec4QfB"), c("Sec4QgA.x", "Sec4QgA.y", "Sec4QgA"), c("Sec4QgB.x", "Sec4QgB.y", "Sec4QgB"), c("Sec4QhA.x", "Sec4QhA.y", "Sec4QhA"), c("Sec4QhB.x", "Sec4QhB.y", "Sec4QhB"), c("Sec4QiA.x", "Sec4QiA.y", "Sec4QiA"), c("Sec4QiB.x", "Sec4QiB.y", "Sec4QiB"), c("Sec4QjA.x", "Sec4QjA.y", "Sec4QjA"), c("Sec4QjB.x", "Sec4QjB.y", "Sec4QjB"), c("Sec4QkA.x", "Sec4QkA.y", "Sec4QkA"), c("Sec4QkB.x", "Sec4QkB.y", "Sec4QkB"), c("Sec4QlA.x", "Sec4QlA.y", "Sec4QlA"), c("Sec4QlB.x", "Sec4QlB.y", "Sec4QlB")), direction = "long", times =c(0,1,2))

head(datPrePost3month)


write.csv(datPrePost3month, "datPrePost3month.csv", row.names = FALSE)
datPrePost3month = read.csv("datPrePost3month.csv", header = TRUE)




describe.factor(datPrePost3month$Sec1Qf.x)
datPrePost3month$Sec1Qf.x = as.factor(datPrePost3month$Sec1Qf.x)
describe.factor(datPrePost3month$Sec1Qf.x)
datPrePost3month$Sec1Qf.x = ifelse(datPrePost3month$Sec1Qf.x== 5, NA, datPrePost3month$Sec1Qf.x)
describe.factor(datPrePost3month$Sec1Qf.x)
datPrePost3month$Sec1Qf.x = ifelse(datPrePost3month$Sec1Qf.x == 2,1,0)
describe.factor(datPrePost3month$Sec1Qf.x)

describe.factor(datPrePost3month$Sec1Qi.x)
datPrePost3month$Sec1Qi.x = ifelse(datPrePost3month$Sec1Qi.x == 5, NA, datPrePost3month$Sec1Qi.x)
datPrePost3month$Sec1Qi.x = ifelse(datPrePost3month$Sec1Qi.x == 9, NA, datPrePost3month$Sec1Qi.x)
describe.factor(datPrePost3month$Sec1Qi.x)



describe.factor(datPrePost3month$Sec1Qg.x)
datPrePost3month$Sec1Qg.x = ifelse(datPrePost3month$Sec1Qg.x == 3, NA, datPrePost3month$Sec1Qg.x)
describe.factor(datPrePost3month$Sec1Qg.x)



describe.factor(datPrePost3month$Sec1Qh.x)
datPrePost3month$Sec1Qh.x = ifelse(datPrePost3month$Sec1Qh.x == 4, NA, datPrePost3month$Sec1Qh.x)
describe.factor(datPrePost3month$Sec1Qh.x)

describe.factor(datPrePost3month$Sec1Qk.x) 
datPrePost3month$Sec1Qk.x =ifelse(datPrePost3month$Sec1Qk.x == 5, NA, datPrePost3month$Sec1Qk.x)
describe.factor(datPrePost3month$Sec1Qk.x)


describe.factor(datPrePost3month$Sec2Qf.x)
datPrePost3month$Sec2Qf.x = ifelse(datPrePost3month$Sec2Qf.x == 0, NA, datPrePost3month$Sec2Qf.x)
describe.factor(datPrePost3month$Sec2Qf.x)



describe.factor(datPrePost3month$Sec1Ql.x)
datPrePost3month$Sec1Ql.x = ifelse(datPrePost3month$Sec1Ql.x > 1, NA, datPrePost3month$Sec1Ql.x)
describe.factor(datPrePost3month$Sec1Ql.x)


describe.factor(datPrePost3month$Sec2Qo.x)
datPrePost3month$Sec2Qo.x = ifelse(datPrePost3month$Sec2Qo.x == 0, NA, datPrePost3month$Sec2Qo.x)
describe.factor(datPrePost3month$Sec2Qo.x)


# 5 = -3; 4 = -2, 3 = -1, 6=0, 7=1,  8 = 2, 1 = NA, NA = NA, 9 = 3
describe.factor(datPrePost3month$Sec4QfA.x)
datPrePost3month$Sec4QfA.x = ifelse(datPrePost3month$Sec4QfA.x == " ", NA, ifelse(datPrePost3month$Sec4QfA.x == "-", NA, datPrePost3month$Sec4QfA.x))
describe.factor(datPrePost3month$Sec4QfA.x)

datPrePost3month$Sec4QfA.x = ifelse(datPrePost3month$Sec4QfA.x == 5, -3, ifelse(datPrePost3month$Sec4QfA.x  == 4, -2, ifelse(datPrePost3month$Sec4QfA.x == 3, -1, ifelse(datPrePost3month$Sec4QfA.x == 6, 0, ifelse(datPrePost3month$Sec4QfA.x == 7, 1, ifelse(datPrePost3month$Sec4QfA.x == 8, 2, ifelse(datPrePost3month$Sec4QfA.x == 1, NA, ifelse(datPrePost3month$Sec4QfA.x == 9, 3, datPrePost3month$Sec4QfA.x ))))))))
describe.factor(datPrePost3month$Sec4QfA.x)

describe.factor(datPrePost3month$Sec4QfB.x)
datPrePost3month$Sec4QfB.x = ifelse(datPrePost3month$Sec4QfB.x == -23, NA, datPrePost3month$Sec4QfB.x)
describe.factor(datPrePost3month$Sec4QfB.x)

describe.factor(datPrePost3month$Sec4QgB.x)
datPrePost3month$Sec4QgB.x = ifelse(datPrePost3month$Sec4QgB.x == -11, NA, datPrePost3month$Sec4QgB.x)
describe.factor(datPrePost3month$Sec4QgB.x)


describe.factor(datPrePost3month$Sec4QhA.x)
datPrePost3month$Sec4QhA.x = ifelse(datPrePost3month$Sec4QhA.x == -4, NA, datPrePost3month$Sec4QhA.x)
describe.factor(datPrePost3month$Sec4QhA.x)

describe.factor(datPrePost3month$Sec4QeB.x)
datPrePost3month$Sec4QeB.x = ifelse(datPrePost3month$Sec4QeB.x == -32, NA, ifelse(datPrePost3month$Sec4QeB.x == -4, NA, datPrePost3month$Sec4QeB.x))
describe.factor(datPrePost3month$Sec4QeB.x)

describe.factor(datPrePost3month$Sec2Qh.x)
datPrePost3month$Sec2Qh.x = ifelse(datPrePost3month$Sec2Qh.x == 56, NA, datPrePost3month$Sec2Qh.x)
describe.factor(datPrePost3month$Sec2Qh.x)


datPrePost3month$Sec4QaA.x =  datPrePost3month$Sec4QaA.x--2.71
datPrePost3month$Sec4QaB.x =  datPrePost3month$Sec4QaB.x- 1.86 

datPrePost3month$Sec4QbA.x =  datPrePost3month$Sec4QbA.x--2.71
datPrePost3month$Sec4QbB.x =  datPrePost3month$Sec4QbB.x- 1.86 

datPrePost3month$Sec4QcA.x =  datPrePost3month$Sec4QcA.x--2.14
datPrePost3month$Sec4QcB.x =  datPrePost3month$Sec4QcB.x-2.14 

datPrePost3month$Sec4QdA.x =  datPrePost3month$Sec4QdA.x-1.29 
datPrePost3month$Sec4QdB.x =  datPrePost3month$Sec4QdB.x--2.71

datPrePost3month$Sec4QeA.x =  datPrePost3month$Sec4QeA.x-2.43 
datPrePost3month$Sec4QeB.x =  datPrePost3month$Sec4QeB.x--2.71 

datPrePost3month$Sec4QfA.x =  datPrePost3month$Sec4QfA.x--2 
datPrePost3month$Sec4QfB.x =  datPrePost3month$Sec4QfB.x-2.57


datPrePost3month$Sec4QgA.x =  datPrePost3month$Sec4QgA.x-2  
datPrePost3month$Sec4QgB.x =  datPrePost3month$Sec4QgB.x--1.29 

datPrePost3month$Sec4QhA.x =  datPrePost3month$Sec4QhA.x--2.29 
datPrePost3month$Sec4QhB =   datPrePost3month$Sec4QhB.x-2.14

datPrePost3month$Sec4QiA.x =  datPrePost3month$Sec4QiA.x--1.29 
datPrePost3month$Sec4QiB.x =  datPrePost3month$Sec4QiB.x-1.29  

datPrePost3month$Sec4QjA.x =  datPrePost3month$Sec4QjA.x-2.29 
datPrePost3month$Sec4QjB.x =  datPrePost3month$Sec4QjB.x--2.43  

datPrePost3month$Sec4QkA.x =  datPrePost3month$Sec4QkA.x--2.42  
datPrePost3month$Sec4QkB.x =  datPrePost3month$Sec4QkB.x-2.43 

datPrePost3month$Sec4QlA.x =  datPrePost3month$Sec4QlA.x-2.00 
datPrePost3month$Sec4QlB.x =  datPrePost3month$Sec4QlB.x-3.00 


#Now we are getting total scores for the three measures use apply and create a smaller data set.

#Then put that data set back together call it analysis at the end.


#Now we saying that if there is a missing value for any of the resposnes that the total score will be NA.  This is probably better than skipping missing values, because it could be the case that if you have only one response to one item, then that would be the total score, which would not be accurate.  

#Need to get rid of time, before the sum of the variables to be summed, because that will mess up the math and we needed time from the earlier analysis for the CFA to get only the baseline data.

#Then created dicotmoized variables
#Gender: Males = 1, Female = 0 no
##Race: White =1, other racial identity
#Edu: Bachelors or lower = 1, higher than Bachelors = 0

## Now I am getting rid of any data that is missing more than 70% of data
head(datPrePost3month)
datPrePost3monthComplete = data.frame(is.na(datPrePost3month))
datPrePost3monthComplete$NAs = apply(datPrePost3monthComplete, 1, sum)
datPrePost3monthComplete$NAs = datPrePost3monthComplete$NAs / dim(datPrePost3monthComplete)[2]
describe.factor(datPrePost3monthComplete$NAs)
datPrePost3monthComplete$NAs = ifelse(datPrePost3monthComplete$NAs >= .7, 0,1)


# I need to grab the NAs variable put it into the full data set, then create two data sets and dim them one with all the data and one without all the data, then use na.omit later
datPrePost3month$NAs = datPrePost3monthComplete$NAs 
datPrePost3monthComplete = subset(datPrePost3month, NAs == 1)
dim(datPrePost3monthComplete)[1]/dim(datPrePost3month)[1]
datPrePost3month = datPrePost3monthComplete
datPrePost3month$NAs = NULL


head(datPrePost3month)

datPrePost3monthSec1 = datPrePost3month[,c(9,10:21)]
head(datPrePost3monthSec1)
datPrePost3monthSec1Base = subset(datPrePost3monthSec1, time == 0)
describe.factor(datPrePost3monthSec1Base$time)
datPrePost3monthSec1Base$time = NULL

datPrePost3monthSec1Base = data.frame(datPrePost3monthSec1Base)
write.csv(datPrePost3monthSec1Base, "datPrePost3monthSec1Base.csv", row.names = FALSE)
datPrePost3monthSec1Base = read.csv("datPrePost3monthSec1Base.csv", header = TRUE)

head(datPrePost3month)
datPrePost3monthSec2 = datPrePost3month[,c(9, 22:36)]
head(datPrePost3monthSec2)


datPrePost3monthSec2Base = subset(datPrePost3monthSec2, time == 0)
describe.factor(datPrePost3monthSec2Base$time)
datPrePost3monthSec2Base$time = NULL

head(datPrePost3monthSec2Base)

datPrePost3monthSec2Base = data.frame(datPrePost3monthSec2Base)
write.csv(datPrePost3monthSec2Base, "datPrePost3monthSec2Base.csv", row.names = FALSE)
datPrePost3monthSec2Base = read.csv("datPrePost3monthSec2Base.csv", header = TRUE)


head(datPrePost3month)
datPrePost3monthSec3 = datPrePost3month[,c(9, 37:44)]
### Need to get reverse scoring for A,C,E,G
head(datPrePost3monthSec3)
summary(datPrePost3monthSec3)

datPrePost3monthSec3$Sec3Qa.x = ifelse(datPrePost3monthSec3$Sec3Qa.x == 1, 5, ifelse(datPrePost3monthSec3$Sec3Qa.x == 2,4, ifelse(datPrePost3monthSec3$Sec3Qa.x == 3,3, ifelse(datPrePost3monthSec3$Sec3Qa.x == 4,2, ifelse(datPrePost3monthSec3$Sec3Qa.x == 5,1,datPrePost3monthSec3$Sec3Qa.x)))))
describe.factor(datPrePost3monthSec3$Sec3Qa.x)


datPrePost3monthSec3$Sec3Qc.x = ifelse(datPrePost3monthSec3$Sec3Qc.x == 1, 5, ifelse(datPrePost3monthSec3$Sec3Qc.x == 2,4, ifelse(datPrePost3monthSec3$Sec3Qc.x == 3,3, ifelse(datPrePost3monthSec3$Sec3Qc.x == 4,2, ifelse(datPrePost3monthSec3$Sec3Qc.x == 5,1,datPrePost3monthSec3$Sec3Qc.x)))))
describe.factor(datPrePost3monthSec3$Sec3Qc.x)

datPrePost3monthSec3$Sec3Qd.x = ifelse(datPrePost3monthSec3$Sec3Qd.x == 1, 5, ifelse(datPrePost3monthSec3$Sec3Qd.x == 2,4, ifelse(datPrePost3monthSec3$Sec3Qd.x == 3,3, ifelse(datPrePost3monthSec3$Sec3Qd.x == 4,2, ifelse(datPrePost3monthSec3$Sec3Qd.x == 5,1,datPrePost3monthSec3$Sec3Qd.x)))))
describe.factor(datPrePost3monthSec3$Sec3Qd.x)


datPrePost3monthSec3$Sec3Qg.x = ifelse(datPrePost3monthSec3$Sec3Qg.x == 1, 5, ifelse(datPrePost3monthSec3$Sec3Qg.x == 2,4, ifelse(datPrePost3monthSec3$Sec3Qg.x == 3,3, ifelse(datPrePost3monthSec3$Sec3Qg.x == 4,2, ifelse(datPrePost3monthSec3$Sec3Qg.x == 5,1,datPrePost3monthSec3$Sec3Qg.x)))))
describe.factor(datPrePost3monthSec3$Sec3Qg.x)


datPrePost3monthSec3Base = subset(datPrePost3monthSec3, time == 0)
describe.factor(datPrePost3monthSec3Base$time)
datPrePost3monthSec3Base$time = NULL

datPrePost3monthSec3Base = data.frame(datPrePost3monthSec3Base)
write.csv(datPrePost3monthSec3Base, "datPrePost3monthSec3Base.csv", row.names = FALSE)
datPrePost3monthSec3Base = read.csv("datPrePost3monthSec3Base.csv", header = TRUE)
head(datPrePost3monthSec3Base)

head(datPrePost3month)
datPrePost3monthSec4 = datPrePost3month[,c(9, 45:68)]
head(datPrePost3monthSec4)

datPrePost3monthSec4Base = subset(datPrePost3monthSec4, time == 0)
describe.factor(datPrePost3monthSec4Base$time)
datPrePost3monthSec4Base$time = NULL




datPrePost3monthSec1$time = NULL
datPrePost3monthSec2$time = NULL
datPrePost3monthSec3$time = NULL
datPrePost3monthSec4$time = NULL

write.csv(datPrePost3monthSec1, "datPrePost3monthSec1.csv", row.names = FALSE)
datPrePost3monthSec1 = read.csv("datPrePost3monthSec1.csv", header = TRUE)

write.csv(datPrePost3monthSec2, "datPrePost3monthSec2.csv", row.names = FALSE)
datPrePost3monthSec2 = read.csv("datPrePost3monthSec2.csv", header = TRUE)

write.csv(datPrePost3monthSec3, "datPrePost3monthSec3.csv", row.names = FALSE)
datPrePost3monthSec3 = read.csv("datPrePost3monthSec3.csv", header = TRUE)

write.csv(datPrePost3monthSec4, "datPrePost3monthSec4.csv", row.names = FALSE)
datPrePost3monthSec4 = read.csv("datPrePost3monthSec4.csv", header = TRUE)

write.csv(datPrePost3monthSec1, "datPrePost3monthSec1.csv", row.names = FALSE)

#Here for break up of section three
#Factor 1 Sec3Qa.x +  + Sec3Qc.x + Sec3Qd.x +Sec3Qg.x
#Factor 2 Sec3Qe.x + Sec3Qf.x + Sec3Qb.x 
datPrePost3monthSec3F1 = data.frame(Sec3Qa.x = datPrePost3monthSec3$Sec3Qa.x, Sec3Qc.x = datPrePost3monthSec3$Sec3Qc.x, Sec3Qd.x = datPrePost3monthSec3$Sec3Qd.x, Sec3Qg.x = datPrePost3monthSec3$Sec3Qg.x)

datPrePost3monthSec3F2 = data.frame(Sec3Qe.x = datPrePost3monthSec3$Sec3Qe.x, Sec3Qf.x = datPrePost3monthSec3$Sec3Qf.x, Sec3Qb.x = datPrePost3monthSec3$Sec3Qb.x)



sum(is.na(datPrePost3monthSec1))
Sec1Total = rowSums(datPrePost3monthSec1)
Sec2Total = rowSums(datPrePost3monthSec2)
Sec3TotalF1 = rowSums(datPrePost3monthSec3F1)
Sec3TotalF2 = rowSums(datPrePost3monthSec3F2)
Sec4Total = rowSums(datPrePost3monthSec4)

datPrePost3monthAnalysisJen = data.frame(ID = datPrePost3month$ID, Treatment = datPrePost3month$Treatment, Age =  datPrePost3month$Age, Gender = datPrePost3month$Gender, Race = datPrePost3month$Race, Edu = datPrePost3month$Edu, Time = datPrePost3month$time,Sec1Total =  Sec1Total, Sec2Total = Sec2Total,Sec3TotalF1 = Sec3TotalF1, Sec3TotalF2 = Sec3TotalF2, Sec4Total = Sec4Total, datPrePost3monthSec1, datPrePost3monthSec2, datPrePost3monthSec3, datPrePost3monthSec4)

write.csv(datPrePost3monthAnalysisJen, "GatekeeperData.csv", row.names = FALSE)


datPrePost3monthAnalysis = data.frame(ID = datPrePost3month$ID, Treatment = datPrePost3month$Treatment, Age =  datPrePost3month$Age, Gender = datPrePost3month$Gender, Race = datPrePost3month$Race, Edu = datPrePost3month$Edu, Time = datPrePost3month$time,Sec1Total =  Sec1Total, Sec2Total = Sec2Total, Sec3TotalF1 = Sec3TotalF1, Sec3TotalF2 = Sec3TotalF2, Sec4Total = Sec4Total)

# No casese non female or male gender
describe.factor(datPrePost3monthAnalysis$Gender)
datPrePost3monthAnalysis$Gender = ifelse(datPrePost3monthAnalysis$Gender == 1,1,0)
datPrePost3monthAnalysis$Race = ifelse(datPrePost3monthAnalysis$Race == 5, 0, 1)

describe.factor(datPrePost3monthAnalysis$Edu)


datPrePost3monthAnalysis$Edu = ifelse(datPrePost3monthAnalysis$Edu < 6, 1, 0)

#datPrePost3monthAnalysisComplete = subset(datPrePost3monthAnalysis, Treatment == 1 | Treatment == 2)

# Getting the data ready
write.csv(datPrePost3monthAnalysis, "datPrePost3monthAnalysis.csv", row.names = FALSE)
datPrePost3monthAnalysis = read.csv("datPrePost3monthAnalysis.csv", header = TRUE)

# Maybe make sure time and treatment are treated as factors
datPrePost3monthAnalysis$Treatment = as.factor(datPrePost3monthAnalysis$Treatment)
datPrePost3monthAnalysis$Gender = as.factor(datPrePost3monthAnalysis$Gender)
datPrePost3monthAnalysis$Race = as.factor(datPrePost3monthAnalysis$Race)
datPrePost3monthAnalysis$Edu = as.factor(datPrePost3monthAnalysis$Edu)

head(datPrePost3monthAnalysis)
dim(datPrePost3monthAnalysis)
```
Assess missing values for prePost3month and prePost
```{r}
sum(is.na(datPrePost3monthAnalysis))
datPrePost3monthAnalysisComplete = na.omit(datPrePost3monthAnalysis)
dim(datPrePost3monthAnalysis)[1]
dim(datPrePost3monthAnalysisComplete)[1]

1-(dim(datPrePost3monthAnalysisComplete)[1] / dim(datPrePost3monthAnalysis)[1])

# Missing values for prePost
write.csv(datPrePost3monthAnalysis, "datPrePost3monthAnalysis.csv", row.names = FALSE)
datPrePost3monthAnalysis = read.csv("datPrePost3monthAnalysis.csv", header = TRUE)
datPrePostAnalysis = subset(datPrePost3monthAnalysis, Time  == 0 | Time == 1)
write.csv(datPrePostAnalysis, "datPrePostAnalysis.csv", row.names = FALSE)
datPrePostAnalysis = read.csv("datPrePostAnalysis.csv", header = TRUE)
describe.factor(datPrePostAnalysis$Time)

## Ok no need to impute for pre and post
datPrePostAnalysisComplete = na.omit(datPrePostAnalysis)
1-(dim(datPrePostAnalysisComplete)[1]/dim(datPrePostAnalysis)[1])
TestMCARNormality(datPrePostAnalysis)

## Missing values for pre, post, and 3month
datPrePost3monthAnalysisComplete = na.omit(datPrePost3monthAnalysis)
1-(dim(datPrePost3monthAnalysisComplete)[1]/dim(datPrePost3monthAnalysis)[1])
write.csv(datPrePost3monthAnalysis, "datPrePost3monthAnalysis.csv", row.names = FALSE)
datPrePost3monthAnalysis = read.csv("datPrePost3monthAnalysis.csv", header = TRUE)

TestMCARNormality(datPrePost3monthAnalysis)
```
Get descriptives for each time point
```{r}
datPrePost3monthAnalysisBase = subset(datPrePost3monthAnalysis, Time == 0)
describe(datPrePost3monthAnalysisBase)
round(sd(datPrePost3monthAnalysisBase$Age, na.rm  =TRUE),2)
round(sd(datPrePost3monthAnalysisBase$Sec2Total, na.rm  =TRUE),2)
round(sd(datPrePost3monthAnalysisBase$Sec3TotalF1, na.rm  =TRUE),2)
round(sd(datPrePost3monthAnalysisBase$Sec3TotalF2, na.rm  =TRUE),2)
round(sd(datPrePost3monthAnalysisBase$Sec4Total, na.rm  =TRUE),2)


datPrePost3monthAnalysisPost = subset(datPrePost3monthAnalysis, Time == 1)
describe(datPrePost3monthAnalysisPost)

datPrePost3monthAnalysisPostTest = na.omit(subset(datPrePost3monthAnalysis, Time == 1))
describe(datPrePost3monthAnalysisPostTest)

round(sd(datPrePost3monthAnalysisPost$Age, na.rm  =TRUE),2)
round(sd(datPrePost3monthAnalysisPost$Sec2Total, na.rm  =TRUE),2)
round(sd(datPrePost3monthAnalysisPost$Sec3TotalF1, na.rm  =TRUE),2)
round(sd(datPrePost3monthAnalysisPost$Sec3TotalF2, na.rm  =TRUE),2)
round(sd(datPrePost3monthAnalysisPost$Sec4Total, na.rm  =TRUE),2)

datPre3month3monthAnalysis3monthTest = na.omit(subset(datPrePost3monthAnalysis, Time == 2))
describe(datPre3month3monthAnalysis3monthTest)

datPre3month3monthAnalysis3month= subset(datPrePost3monthAnalysis, Time == 2)
describe(datPre3month3monthAnalysis3month)
round(sd(datPre3month3monthAnalysis3month$Age, na.rm  =TRUE),2)
round(sd(datPre3month3monthAnalysis3month$Sec2Total, na.rm  =TRUE),2)
round(sd(datPre3month3monthAnalysis3month$Sec3TotalF1, na.rm  =TRUE),2)
round(sd(datPre3month3monthAnalysis3month$Sec3TotalF2, na.rm  =TRUE),2)
round(sd(datPre3month3monthAnalysis3month$Sec4Total, na.rm  =TRUE),2)

```



Now generate missing data for varibles
There are 18 data points so six people with treatment for three month follow-up but no treatment ID for pre or post. 
Deleting them for now, but will check in later on this

If data is still missing that means that there is zero data for the response

Getting rid of missing values after imputation, because if there are still missing values that means that the entire data set is empty
```{r}
head(datPrePost3monthAnalysis)
summary(datPrePost3monthAnalysis$ID)

# Getting rid of NA's for treatment
datPrePost3monthAnalysis = subset(datPrePost3monthAnalysis, Treatment == 1 | Treatment == 2 | Treatment == 3)
describe.factor(datPrePost3monthAnalysis$Treatment)
m = 10
head(datPrePost3monthAnalysis)

datPrePost3monthAnalysisImpute = amelia(m = m, datPrePost3monthAnalysis, noms = c("Gender", "Race", "Edu"), idvars = c("ID", "Treatment"), ts = "Time")

compare.density(datPrePost3monthAnalysisImpute, var = "Sec1Total")
compare.density(datPrePost3monthAnalysisImpute, var = "Sec2Total")
#compare.density(datPrePost3monthAnalysisImpute, var = "Sec3Total")
compare.density(datPrePost3monthAnalysisImpute, var = "Sec4Total")
summary(datPrePost3monthAnalysisImpute)
datAnalysisAll = lapply(1:m, function(x){datPrePost3monthAnalysisImpute$imputations[[x]]})

datAnalysisAllComplete = NULL
for(i in 1:m){
 datAnalysisAllComplete[[i]] = na.omit(datAnalysisAll[[i]])
}


```
Now get desciptives for base
```{r}
datAnalysisAllDes = lapply(1:m, function(x){subset(datAnalysisAllComplete[[x]], Time == 0)})

mean.out = NULL
for(i in 1:m) {
  mean.out[[i]] = apply(datAnalysisAllDes[[i]], 2, mean)
  mean.out = data.frame(mean.out)
}
mean.out

descFun = function(x){
  x = data.frame(t(x))
}
mean.out = descFun(mean.out)
mean.out

# now get sds
sd.out = NULL
for(i in 1:m) {
  sd.out[[i]] = apply(datAnalysisAllDes[[i]], 2, sd)
  sd.out = data.frame(sd.out)
}
sd.out = descFun(sd.out)
sd.out
mean.sd.out= mi.meld(mean.out, sd.out)
mean.sd.out

```
Now get descriptives for post
```{r}
datAnalysisAllDes = lapply(1:m, function(x){subset(datAnalysisAllComplete[[x]], Time == 1)})

mean.out = NULL
for(i in 1:m) {
  mean.out[[i]] = apply(datAnalysisAllDes[[i]], 2, mean)
  mean.out = data.frame(mean.out)
}
mean.out

descFun = function(x){
  x = data.frame(t(x))
}
mean.out = descFun(mean.out)
mean.out

# now get sds
sd.out = NULL
for(i in 1:m) {
  sd.out[[i]] = apply(datAnalysisAllDes[[i]], 2, sd)
  sd.out = data.frame(sd.out)
}
sd.out = descFun(sd.out)
sd.out
mean.sd.out= mi.meld(mean.out, sd.out)
mean.sd.out

```
Now get descriptives for 3month
```{r}
datAnalysisAllDes = lapply(1:m, function(x){subset(datAnalysisAllComplete[[x]], Time == 2)})

mean.out = NULL
for(i in 1:m) {
  mean.out[[i]] = apply(datAnalysisAllDes[[i]], 2, mean)
  mean.out = data.frame(mean.out)
}
mean.out

descFun = function(x){
  x = data.frame(t(x))
}
mean.out = descFun(mean.out)
mean.out

# now get sds
sd.out = NULL
for(i in 1:m) {
  sd.out[[i]] = apply(datAnalysisAllDes[[i]], 2, sd)
  sd.out = data.frame(sd.out)
}
sd.out = descFun(sd.out)
sd.out
mean.sd.out= mi.meld(mean.out, sd.out)
mean.sd.out

```
##########
Get wide data
##########

#datAdultAnalysisWide = reshape(data = datAdultAnalysis, v.names = c("RASTotalScore", "INQTotalScore", "SSMITotalScore", "SISTotalScore"), timevar = "Time", direction = "wide", idvar = "ID")

Need to create difference scores

```{r}
datPrePost3monthAnalysis = subset(datPrePost3monthAnalysis, Treatment == 1 | Treatment == 2 | Treatment == 3)
m = 10
datPrePost3monthAnalysisImpute = amelia(m = m, datPrePost3monthAnalysis, noms = c("Gender", "Race", "Edu"), idvars = c("ID", "Treatment"), ts = "Time")

#compare.density(datPrePost3monthAnalysisImpute, var = "Sec1Total")
#compare.density(datPrePost3monthAnalysisImpute, var = "Sec2Total")
#compare.density(datPrePost3monthAnalysisImpute, var = "Sec3Total")
#compare.density(datPrePost3monthAnalysisImpute, var = "Sec4Total")
summary(datPrePost3monthAnalysisImpute)
datAnalysisAll = lapply(1:m, function(x){datPrePost3monthAnalysisImpute$imputations[[x]]})

datAnalysisAllComplete = NULL
for(i in 1:m){
 datAnalysisAllComplete[[i]] = na.omit(datAnalysisAll[[i]])
}
datAnalysisAllComplete[[1]]$ID
# Now get data into wide format
datWideAnalysis = NULL
for(i in 1:m){
  datWideAnalysis[[i]] = reshape(datAnalysisAllComplete[[i]], v.names = c("Sec1Total", "Sec2Total", "Sec3TotalF1", "Sec3TotalF2", "Sec4Total", "Age"), timevar = "Time", direction = "wide", idvar = "ID")
}

# Now create difference scores
for(i in 1:m){
  datWideAnalysis[[i]]$Sec2PostPre = datWideAnalysis[[i]]$Sec2Total.1-datWideAnalysis[[i]]$Sec2Total.0
  datWideAnalysis[[i]]$Sec23monthPost = datWideAnalysis[[i]]$Sec2Total.2-datWideAnalysis[[i]]$Sec2Total.1
  
  datWideAnalysis[[i]]$Sec3F1PostPre = datWideAnalysis[[i]]$Sec3TotalF1.1-datWideAnalysis[[i]]$Sec3TotalF1.0
  datWideAnalysis[[i]]$Sec3F2PostPre = datWideAnalysis[[i]]$Sec3TotalF2.1-datWideAnalysis[[i]]$Sec3TotalF2.0
  
  datWideAnalysis[[i]]$Sec3F13monthPost = datWideAnalysis[[i]]$Sec3TotalF1.2-datWideAnalysis[[i]]$Sec3TotalF1.1
  datWideAnalysis[[i]]$Sec3F23monthPost = datWideAnalysis[[i]]$Sec3TotalF2.2-datWideAnalysis[[i]]$Sec3TotalF2.1
}
```
#########################
T1 Only Sec2PrePost
#########################
```{r}
datTime = NULL
for(i in 1:m){
  datTime[[i]] = subset(datAnalysisAllComplete[[i]], Time == 0 | Time == 1)
}

datTimeTreat = NULL
for(i in 1:m){
  datTimeTreat[[i]] = subset(datTime[[i]], Treatment == 1) 
}

output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL
df = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec2Total ~ Time  + Gender + Age + Race + (1 | ID), data  = datTimeTreat[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
  df[[i]] = output[[i]]$coefficients[,3]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output)
round(results,3)

```
#########################
T1 Only Section 2 3month Post
#########################
```{r}
datTime = NULL
for(i in 1:m){
  datTime[[i]] = subset(datAnalysisAllComplete[[i]], Time == 1 | Time == 2)
}

datTimeTreat = NULL
for(i in 1:m){
  datTimeTreat[[i]] = subset(datTime[[i]], Treatment == 1) 
}

output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL
df = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec2Total ~ Time  + Gender + Age + Race + (1 | ID), data  = datTimeTreat[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
  df[[i]] = output[[i]]$coefficients[,3]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)


meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output)
round(results,3)

```
####################################
T1 Section 2 Table for change over time 
####################################

```{r}
library(descr)

datAllTimeT1 = NULL
for(i in 1:m){
  datAllTimeT1[[i]] = subset(datAnalysisAllComplete[[i]], Treatment == 1)
}

test= compmeans(datAllTimeT1[[1]]$Sec2Total, datAllTimeT1[[1]]$Time)
test


```
#################################################
T1 Section2 Hedge's G for pre-post 
Use this formula because sample sizes the same: https://www.socscistatistics.com/effectsize/Default3.aspx
#################################################
```{r}
datTime = NULL
for(i in 1:m){
  datTime[[i]] = subset(datAnalysisAllComplete[[i]], Time == 0 | Time == 1)
}

datTimeTreat = NULL
for(i in 1:m){
  datTimeTreat[[i]] = subset(datTime[[i]], Treatment == 1) 
}

library(psych)
cohenDat = NULL
Sec2TotalT0 = NULL
Sec2TotalT1 = NULL
for(i in 1:m){
  Sec2TotalT0[[i]] = subset(datTimeTreat[[i]], Time == 0)
  Sec2TotalT1[[i]] = subset(datTimeTreat[[i]], Time == 1)
  cohenDat[[i]] = mean(Sec2TotalT1[[i]]$Sec2Total-Sec2TotalT0[[i]]$Sec2Total)/sqrt((sd(Sec2TotalT1[[i]]$Sec2Total)^2+sd(Sec2TotalT0[[i]]$Sec2Total)^2)/2)
}

cohenD = data.frame(cohenDat)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  return(data.frame(coefs1, ses1))
}
y = data.frame(rnorm(10))

meldAllT_stat(cohenD, y)
```
#################################################
T1 Section2 Hedge's G for T1 and T2 
Use this formula because sample sizes the same: https://www.socscistatistics.com/effectsize/Default3.aspx
#################################################
```{r}
datTime = NULL
for(i in 1:m){
  datTime[[i]] = subset(datAnalysisAllComplete[[i]], Time == 1 | Time == 2)
}

datTimeTreat = NULL
for(i in 1:m){
  datTimeTreat[[i]] = subset(datTime[[i]], Treatment == 1) 
}

library(psych)
cohenDat = NULL
Sec2TotalT0 = NULL
Sec2TotalT1 = NULL
for(i in 1:m){
  Sec2TotalT0[[i]] = subset(datTimeTreat[[i]], Time == 1)
  Sec2TotalT1[[i]] = subset(datTimeTreat[[i]], Time == 2)
  cohenDat[[i]] = mean(Sec2TotalT1[[i]]$Sec2Total-Sec2TotalT0[[i]]$Sec2Total)/sqrt((sd(Sec2TotalT1[[i]]$Sec2Total)^2+sd(Sec2TotalT0[[i]]$Sec2Total)^2)/2)
}

cohenD = data.frame(cohenDat)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  return(data.frame(coefs1, ses1))
}
y = data.frame(rnorm(10))

meldAllT_stat(cohenD, y)
```
########################
T2 Only Sec2PrePost
#########################
```{r}
datTime = NULL
for(i in 1:m){
  datTime[[i]] = subset(datAnalysisAllComplete[[i]], Time == 0 | Time == 1)
}

datTimeTreat = NULL
for(i in 1:m){
  datTimeTreat[[i]] = subset(datTime[[i]], Treatment == 2) 
}

output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL
df = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec2Total ~ Time  + Gender + Age + Race + (1 | ID), data  = datTimeTreat[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
  df[[i]] = output[[i]]$coefficients[,3]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output)
round(results,3)

```
#########################
T2 Only Section 2 3month Post
#########################
```{r}
datTime = NULL
for(i in 1:m){
  datTime[[i]] = subset(datAnalysisAllComplete[[i]], Time == 1 | Time == 2)
}

datTimeTreat = NULL
for(i in 1:m){
  datTimeTreat[[i]] = subset(datTime[[i]], Treatment == 2) 
}

output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL
df = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec2Total ~ Time  + Gender + Age + Race + (1 | ID), data  = datTimeTreat[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
  df[[i]] = output[[i]]$coefficients[,3]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)


meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output)
round(results,3)

```
####################################
T2 Sectoin 2 Table for change over time 
####################################

```{r}
library(descr)

datAllTimeT2 = NULL
for(i in 1:m){
  datAllTimeT2[[i]] = subset(datAnalysisAllComplete[[i]], Treatment == 2)
}

test= compmeans(datAllTimeT2[[1]]$Sec2Total, datAllTimeT2[[1]]$Time, ylab = "Section Two Total Score", xlab = "Time")
test
```
#################################################
T2 Section2 Hedge's G for pre-post 
Use this formula because sample sizes the same: https://www.socscistatistics.com/effectsize/Default3.aspx
#################################################
```{r}
datTime = NULL
for(i in 1:m){
datTime[[i]] = subset(datAnalysisAllComplete[[i]], Time == 0 | Time == 1)
}

datTimeTreat = NULL
for(i in 1:m){
datTimeTreat[[i]] = subset(datTime[[i]], Treatment == 2) 
}

library(psych)
cohenDat = NULL
Sec2TotalT0 = NULL
Sec2TotalT1 = NULL
for(i in 1:m){
Sec2TotalT0[[i]] = subset(datTimeTreat[[i]], Time == 0)
Sec2TotalT1[[i]] = subset(datTimeTreat[[i]], Time == 1)
cohenDat[[i]] = mean(Sec2TotalT1[[i]]$Sec2Total-Sec2TotalT0[[i]]$Sec2Total)/sqrt((sd(Sec2TotalT1[[i]]$Sec2Total)^2+sd(Sec2TotalT0[[i]]$Sec2Total)^2)/2)
}

cohenD = data.frame(cohenDat)

meldAllT_stat = function(x,y){
coefsAll = mi.meld(q = x, se = y)
coefs1 = t(data.frame(coefsAll$q.mi))
ses1 = t(data.frame(coefsAll$se.mi))
return(data.frame(coefs1, ses1))
}
y = data.frame(rnorm(10))

meldAllT_stat(cohenD, y)
```
#################################################
T1 Section2 Hedge's G for T1 and T2 
Use this formula because sample sizes the same: https://www.socscistatistics.com/effectsize/Default3.aspx
#################################################
```{r}
datTime = NULL
for(i in 1:m){
  datTime[[i]] = subset(datAnalysisAllComplete[[i]], Time == 1 | Time == 2)
}

datTimeTreat = NULL
for(i in 1:m){
  datTimeTreat[[i]] = subset(datTime[[i]], Treatment == 2) 
}

library(psych)
cohenDat = NULL
Sec2TotalT0 = NULL
Sec2TotalT1 = NULL
for(i in 1:m){
  Sec2TotalT0[[i]] = subset(datTimeTreat[[i]], Time == 1)
  Sec2TotalT1[[i]] = subset(datTimeTreat[[i]], Time == 2)
  cohenDat[[i]] = mean(Sec2TotalT1[[i]]$Sec2Total-Sec2TotalT0[[i]]$Sec2Total)/sqrt((sd(Sec2TotalT1[[i]]$Sec2Total)^2+sd(Sec2TotalT0[[i]]$Sec2Total)^2)/2)
}

cohenD = data.frame(cohenDat)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  return(data.frame(coefs1, ses1))
}
y = data.frame(rnorm(10))

meldAllT_stat(cohenD, y)
```
#########################
T3 Only Sec2PrePost
#########################
```{r}
datTime = NULL
for(i in 1:m){
  datTime[[i]] = subset(datAnalysisAllComplete[[i]], Time == 0 | Time == 1)
}

datTimeTreat = NULL
for(i in 1:m){
  datTimeTreat[[i]] = subset(datTime[[i]], Treatment == 3) 
}

output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL
df = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec2Total ~ Time  + Gender + Age + Race + (1 | ID), data  = datTimeTreat[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
  df[[i]] = output[[i]]$coefficients[,3]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output)
round(results,3)

```
#########################
T3 Only Section 2 3month Post
#########################
```{r}
datTime = NULL
for(i in 1:m){
  datTime[[i]] = subset(datAnalysisAllComplete[[i]], Time == 1 | Time == 2)
}

datTimeTreat = NULL
for(i in 1:m){
  datTimeTreat[[i]] = subset(datTime[[i]], Treatment == 3) 
}

output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL
df = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec2Total ~ Time  + Gender + Age + Race + (1 | ID), data  = datTimeTreat[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
  df[[i]] = output[[i]]$coefficients[,3]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)


meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output)
round(results,3)

```
####################################
T3 Sectoin 2 Table for change over time 
####################################

```{r}
library(descr)

datAllTimeT3 = NULL
for(i in 1:m){
  datAllTimeT3[[i]] = subset(datAnalysisAllComplete[[i]], Treatment == 3)
}

test= compmeans(datAllTimeT3[[1]]$Sec2Total, datAllTimeT3[[1]]$Time)
test
```
#################################################
T2 Section2 Hedge's G for pre-post 
Use this formula because sample sizes the same: https://www.socscistatistics.com/effectsize/Default3.aspx
#################################################
```{r}
datTime = NULL
for(i in 1:m){
datTime[[i]] = subset(datAnalysisAllComplete[[i]], Time == 0 | Time == 1)
}

datTimeTreat = NULL
for(i in 1:m){
datTimeTreat[[i]] = subset(datTime[[i]], Treatment == 3) 
}

library(psych)
cohenDat = NULL
Sec2TotalT0 = NULL
Sec2TotalT1 = NULL
for(i in 1:m){
Sec2TotalT0[[i]] = subset(datTimeTreat[[i]], Time == 0)
Sec2TotalT1[[i]] = subset(datTimeTreat[[i]], Time == 1)
cohenDat[[i]] = mean(Sec2TotalT1[[i]]$Sec2Total-Sec2TotalT0[[i]]$Sec2Total)/sqrt((sd(Sec2TotalT1[[i]]$Sec2Total)^2+sd(Sec2TotalT0[[i]]$Sec2Total)^2)/2)
}

cohenD = data.frame(cohenDat)

meldAllT_stat = function(x,y){
coefsAll = mi.meld(q = x, se = y)
coefs1 = t(data.frame(coefsAll$q.mi))
ses1 = t(data.frame(coefsAll$se.mi))
return(data.frame(coefs1, ses1))
}
y = data.frame(rnorm(10))

meldAllT_stat(cohenD, y)
```
#################################################
T1 Section2 Hedge's G for T1 and T2 
Use this formula because sample sizes the same: https://www.socscistatistics.com/effectsize/Default3.aspx
#################################################
```{r}
datTime = NULL
for(i in 1:m){
  datTime[[i]] = subset(datAnalysisAllComplete[[i]], Time == 1 | Time == 2)
}

datTimeTreat = NULL
for(i in 1:m){
  datTimeTreat[[i]] = subset(datTime[[i]], Treatment == 3) 
}

library(psych)
cohenDat = NULL
Sec2TotalT0 = NULL
Sec2TotalT1 = NULL
for(i in 1:m){
  Sec2TotalT0[[i]] = subset(datTimeTreat[[i]], Time == 1)
  Sec2TotalT1[[i]] = subset(datTimeTreat[[i]], Time == 2)
  cohenDat[[i]] = mean(Sec2TotalT1[[i]]$Sec2Total-Sec2TotalT0[[i]]$Sec2Total)/sqrt((sd(Sec2TotalT1[[i]]$Sec2Total)^2+sd(Sec2TotalT0[[i]]$Sec2Total)^2)/2)
}

cohenD = data.frame(cohenDat)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  return(data.frame(coefs1, ses1))
}
y = data.frame(rnorm(10))

meldAllT_stat(cohenD, y)
```
##################################
Section 2 display predicted values
##################################
```{r}

output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL
df = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec2Total ~ Treatment*poly(Time, degree = 2)  + Gender + Age + Race + (1 | ID), data  = datAnalysisAllComplete[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
  df[[i]] = output[[i]]$coefficients[,3]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)


meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output)
round(results,3)

```
Try mod graph
```{r}
modGraph = NULL
for(i in 1:m){
  modGraph[[i]] = cat_plot(model = outputReg[[i]], pred = "Time", modx = "Treatment", cluster = "ID", data = datAnalysisAllComplete[[i]], y.label = "Section two total score")
}
modGraph[[1]]
```



#########################
T1 Only Sec3F1PrePost
#########################
```{r}
datTime = NULL
for(i in 1:m){
  datTime[[i]] = subset(datAnalysisAllComplete[[i]], Time == 0 | Time == 1)
}

datTimeTreat = NULL
for(i in 1:m){
  datTimeTreat[[i]] = subset(datTime[[i]], Treatment == 1) 
}

output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL
df = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec3TotalF1 ~ Time  + Gender + Age + Race + (1 | ID), data  = datTimeTreat[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
  df[[i]] = output[[i]]$coefficients[,3]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)


meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output)
round(results,3)

```
#########################
T1 Only Section 3F1 3month Post
#########################
```{r}
datTime = NULL
for(i in 1:m){
  datTime[[i]] = subset(datAnalysisAllComplete[[i]], Time == 1 | Time == 2)
}

datTimeTreat = NULL
for(i in 1:m){
  datTimeTreat[[i]] = subset(datTime[[i]], Treatment == 1) 
}

output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL
df = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec3TotalF1 ~ Time  + Gender + Age + Race + (1 | ID), data  = datTimeTreat[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
  df[[i]] = output[[i]]$coefficients[,3]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)


meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output)
round(results,3)

```
####################################
T1 Section 3F1 Table for change over time 
####################################

```{r}
library(descr)

datAllTimeT1 = NULL
for(i in 1:m){
  datAllTimeT1[[i]] = subset(datAnalysisAllComplete[[i]], Treatment == 1)
}

test= compmeans(datAllTimeT1[[1]]$Sec3TotalF1, datAllTimeT1[[1]]$Time)
test
```
#################################################
T2 Section3F1 Hedge's G for pre-post 
Use this formula because sample sizes the same: https://www.socscistatistics.com/effectsize/Default3.aspx
#################################################
```{r}
datTime = NULL
for(i in 1:m){
datTime[[i]] = subset(datAnalysisAllComplete[[i]], Time == 0 | Time == 1)
}

datTimeTreat = NULL
for(i in 1:m){
datTimeTreat[[i]] = subset(datTime[[i]], Treatment == 1) 
}

library(psych)
cohenDat = NULL
Sec3TotalF1T0 = NULL
Sec3TotalF1T1 = NULL
for(i in 1:m){
Sec3TotalF1T0[[i]] = subset(datTimeTreat[[i]], Time == 0)
Sec3TotalF1T1[[i]] = subset(datTimeTreat[[i]], Time == 1)
cohenDat[[i]] = mean(Sec3TotalF1T1[[i]]$Sec3TotalF1-Sec3TotalF1T0[[i]]$Sec3TotalF1)/sqrt((sd(Sec3TotalF1T1[[i]]$Sec3TotalF1)^2+sd(Sec3TotalF1T0[[i]]$Sec3TotalF1)^2)/2)
}

cohenD = data.frame(cohenDat)

meldAllT_stat = function(x,y){
coefsAll = mi.meld(q = x, se = y)
coefs1 = t(data.frame(coefsAll$q.mi))
ses1 = t(data.frame(coefsAll$se.mi))
return(data.frame(coefs1, ses1))
}
y = data.frame(rnorm(10))

meldAllT_stat(cohenD, y)
```
#################################################
T1 Section3F1 Hedge's G for T1 and T2 
Use this formula because sample sizes the same: https://www.socscistatistics.com/effectsize/Default3.aspx
#################################################
```{r}
datTime = NULL
for(i in 1:m){
  datTime[[i]] = subset(datAnalysisAllComplete[[i]], Time == 1 | Time == 2)
}

datTimeTreat = NULL
for(i in 1:m){
  datTimeTreat[[i]] = subset(datTime[[i]], Treatment == 1) 
}

library(psych)
cohenDat = NULL
Sec3TotalF1T0 = NULL
Sec3TotalF1T1 = NULL
for(i in 1:m){
  Sec3TotalF1T0[[i]] = subset(datTimeTreat[[i]], Time == 1)
  Sec3TotalF1T1[[i]] = subset(datTimeTreat[[i]], Time == 2)
  cohenDat[[i]] = mean(Sec3TotalF1T1[[i]]$Sec3TotalF1-Sec3TotalF1T0[[i]]$Sec3TotalF1)/sqrt((sd(Sec3TotalF1T1[[i]]$Sec3TotalF1)^2+sd(Sec3TotalF1T0[[i]]$Sec3TotalF1)^2)/2)
}

cohenD = data.frame(cohenDat)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  return(data.frame(coefs1, ses1))
}
y = data.frame(rnorm(10))

meldAllT_stat(cohenD, y)
```
#########################
T2 Only Sec3F1PrePost
#########################
```{r}
datTime = NULL
for(i in 1:m){
  datTime[[i]] = subset(datAnalysisAllComplete[[i]], Time == 0 | Time == 1)
}

datTimeTreat = NULL
for(i in 1:m){
  datTimeTreat[[i]] = subset(datTime[[i]], Treatment == 2) 
}

output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL
df = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec3TotalF1 ~ Time  + Gender + Age + Race + (1 | ID), data  = datTimeTreat[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
  df[[i]] = output[[i]]$coefficients[,3]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output)
round(results,3)

```
#########################
T2 Only Section 3F1 3month Post
#########################
```{r}
datTime = NULL
for(i in 1:m){
  datTime[[i]] = subset(datAnalysisAllComplete[[i]], Time == 1 | Time == 2)
}

datTimeTreat = NULL
for(i in 1:m){
  datTimeTreat[[i]] = subset(datTime[[i]], Treatment == 2) 
}

output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL
df = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec3TotalF1 ~ Time  + Gender + Age + Race + (1 | ID), data  = datTimeTreat[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
  df[[i]] = output[[i]]$coefficients[,3]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)


meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output)
round(results,3)

```
####################################
T2 Section 3F1 Table for change over time 
####################################

```{r}
library(descr)

datAllTimeT2 = NULL
for(i in 1:m){
  datAllTimeT2[[i]] = subset(datAnalysisAllComplete[[i]], Treatment == 2)
}

test= compmeans(datAllTimeT2[[1]]$Sec3TotalF1, datAllTimeT2[[1]]$Time)
test
```
#################################################
T2 Section3F1 Hedge's G for T0 and T1 
Use this formula because sample sizes the same: https://www.socscistatistics.com/effectsize/Default3.aspx
#################################################
```{r}
datTime = NULL
for(i in 1:m){
  datTime[[i]] = subset(datAnalysisAllComplete[[i]], Time == 0 | Time == 1)
}

datTimeTreat = NULL
for(i in 1:m){
  datTimeTreat[[i]] = subset(datTime[[i]], Treatment == 2) 
}

library(psych)
cohenDat = NULL
Sec3TotalF1T0 = NULL
Sec3TotalF1T1 = NULL
for(i in 1:m){
  Sec3TotalF1T0[[i]] = subset(datTimeTreat[[i]], Time == 0)
  Sec3TotalF1T1[[i]] = subset(datTimeTreat[[i]], Time == 1)
  cohenDat[[i]] = mean(Sec3TotalF1T1[[i]]$Sec3TotalF1-Sec3TotalF1T0[[i]]$Sec3TotalF1)/sqrt((sd(Sec3TotalF1T1[[i]]$Sec3TotalF1)^2+sd(Sec3TotalF1T0[[i]]$Sec3TotalF1)^2)/2)
}

cohenD = data.frame(cohenDat)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  return(data.frame(coefs1, ses1))
}
y = data.frame(rnorm(10))

meldAllT_stat(cohenD, y)
```
#########################
T3 Only Sec3F1PrePost
#########################
```{r}
datTime = NULL
for(i in 1:m){
  datTime[[i]] = subset(datAnalysisAllComplete[[i]], Time == 0 | Time == 1)
}

datTimeTreat = NULL
for(i in 1:m){
  datTimeTreat[[i]] = subset(datTime[[i]], Treatment == 3) 
}

output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL
df = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec3TotalF1 ~ Time  + Gender + Age + Race + (1 | ID), data  = datTimeTreat[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
  df[[i]] = output[[i]]$coefficients[,3]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output)
round(results,3)

```
#########################
T3 Only Section 3F1 3month Post
#########################
```{r}
datTime = NULL
for(i in 1:m){
  datTime[[i]] = subset(datAnalysisAllComplete[[i]], Time == 1 | Time == 2)
}

datTimeTreat = NULL
for(i in 1:m){
  datTimeTreat[[i]] = subset(datTime[[i]], Treatment == 3) 
}

output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL
df = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec3TotalF1 ~ Time  + Gender + Age + Race + (1 | ID), data  = datTimeTreat[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
  df[[i]] = output[[i]]$coefficients[,3]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)


meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output)
round(results,3)

```
####################################
T3  Section 3F1 Table for change over time 
####################################

```{r}
library(descr)

datAllTimeT3 = NULL
for(i in 1:m){
  datAllTimeT3[[i]] = subset(datAnalysisAllComplete[[i]], Treatment == 3)
}

test= compmeans(datAllTimeT3[[1]]$Sec3TotalF1, datAllTimeT3[[1]]$Time)
test
```
#################################################
T3 Section3F1 Hedge's G for pre-post 
Use this formula because sample sizes the same: https://www.socscistatistics.com/effectsize/Default3.aspx
#################################################
```{r}
datTime = NULL
for(i in 1:m){
datTime[[i]] = subset(datAnalysisAllComplete[[i]], Time == 0 | Time == 1)
}

datTimeTreat = NULL
for(i in 1:m){
datTimeTreat[[i]] = subset(datTime[[i]], Treatment == 3) 
}

library(psych)
cohenDat = NULL
Sec3TotalF1T0 = NULL
Sec3TotalF1T1 = NULL
for(i in 1:m){
Sec3TotalF1T0[[i]] = subset(datTimeTreat[[i]], Time == 0)
Sec3TotalF1T1[[i]] = subset(datTimeTreat[[i]], Time == 1)
cohenDat[[i]] = mean(Sec3TotalF1T1[[i]]$Sec3TotalF1-Sec3TotalF1T0[[i]]$Sec3TotalF1)/sqrt((sd(Sec3TotalF1T1[[i]]$Sec3TotalF1)^2+sd(Sec3TotalF1T0[[i]]$Sec3TotalF1)^2)/2)
}

cohenD = data.frame(cohenDat)

meldAllT_stat = function(x,y){
coefsAll = mi.meld(q = x, se = y)
coefs1 = t(data.frame(coefsAll$q.mi))
ses1 = t(data.frame(coefsAll$se.mi))
return(data.frame(coefs1, ses1))
}
y = data.frame(rnorm(10))

meldAllT_stat(cohenD, y)
```
#################################################
T3 Section3F1 Hedge's G for T1 and T2 
Use this formula because sample sizes the same: https://www.socscistatistics.com/effectsize/Default3.aspx
#################################################
```{r}
datTime = NULL
for(i in 1:m){
  datTime[[i]] = subset(datAnalysisAllComplete[[i]], Time == 1 | Time == 2)
}

datTimeTreat = NULL
for(i in 1:m){
  datTimeTreat[[i]] = subset(datTime[[i]], Treatment == 3) 
}

library(psych)
cohenDat = NULL
Sec3TotalF1T0 = NULL
Sec3TotalF1T1 = NULL
for(i in 1:m){
  Sec3TotalF1T0[[i]] = subset(datTimeTreat[[i]], Time == 1)
  Sec3TotalF1T1[[i]] = subset(datTimeTreat[[i]], Time == 2)
  cohenDat[[i]] = mean(Sec3TotalF1T1[[i]]$Sec3TotalF1-Sec3TotalF1T0[[i]]$Sec3TotalF1)/sqrt((sd(Sec3TotalF1T1[[i]]$Sec3TotalF1)^2+sd(Sec3TotalF1T0[[i]]$Sec3TotalF1)^2)/2)
}

cohenD = data.frame(cohenDat)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  return(data.frame(coefs1, ses1))
}
y = data.frame(rnorm(10))

meldAllT_stat(cohenD, y)
```
#########################
T1 Only Sec3F2PrePost
#########################
```{r}
datTime = NULL
for(i in 1:m){
  datTime[[i]] = subset(datAnalysisAllComplete[[i]], Time == 0 | Time == 1)
}

datTimeTreat = NULL
for(i in 1:m){
  datTimeTreat[[i]] = subset(datTime[[i]], Treatment == 1) 
}

output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL
df = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec3TotalF2 ~ Time  + Gender + Age + Race + (1 | ID), data  = datTimeTreat[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
  df[[i]] = output[[i]]$coefficients[,3]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output)
round(results,3)

```
#########################
T1 Only Section 3F2 3month Post
#########################
```{r}
datTime = NULL
for(i in 1:m){
  datTime[[i]] = subset(datAnalysisAllComplete[[i]], Time == 1 | Time == 2)
}

datTimeTreat = NULL
for(i in 1:m){
  datTimeTreat[[i]] = subset(datTime[[i]], Treatment == 1) 
}

output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL
df = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec3TotalF2 ~ Time  + Gender + Age + Race + (1 | ID), data  = datTimeTreat[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
  df[[i]] = output[[i]]$coefficients[,3]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)


meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output)
round(results,3)

```
####################################
T1 Section 3F2 Table for change over time 
####################################

```{r}
library(descr)

datAllTimeT1 = NULL
for(i in 1:m){
  datAllTimeT1[[i]] = subset(datAnalysisAllComplete[[i]], Treatment == 1)
}

test= compmeans(datAllTimeT1[[1]]$Sec3TotalF2, datAllTimeT1[[1]]$Time)
test
```
#################################################
T1 Section3F1 Hedge's G for pre-post 
Use this formula because sample sizes the same: https://www.socscistatistics.com/effectsize/Default3.aspx
#################################################
```{r}
datTime = NULL
for(i in 1:m){
datTime[[i]] = subset(datAnalysisAllComplete[[i]], Time == 0 | Time == 1)
}

datTimeTreat = NULL
for(i in 1:m){
datTimeTreat[[i]] = subset(datTime[[i]], Treatment == 1) 
}

library(psych)
cohenDat = NULL
Sec3TotalF2T0 = NULL
Sec3TotalF2T1 = NULL
for(i in 1:m){
Sec3TotalF2T0[[i]] = subset(datTimeTreat[[i]], Time == 0)
Sec3TotalF2T1[[i]] = subset(datTimeTreat[[i]], Time == 1)
cohenDat[[i]] = mean(Sec3TotalF2T1[[i]]$Sec3TotalF2-Sec3TotalF2T0[[i]]$Sec3TotalF2)/sqrt((sd(Sec3TotalF2T1[[i]]$Sec3TotalF2)^2+sd(Sec3TotalF2T0[[i]]$Sec3TotalF2)^2)/2)
}

cohenD = data.frame(cohenDat)

meldAllT_stat = function(x,y){
coefsAll = mi.meld(q = x, se = y)
coefs1 = t(data.frame(coefsAll$q.mi))
ses1 = t(data.frame(coefsAll$se.mi))
return(data.frame(coefs1, ses1))
}
y = data.frame(rnorm(10))

meldAllT_stat(cohenD, y)
```
#################################################
T1 Section3F1 Hedge's G for T1 and T2 
Use this formula because sample sizes the same: https://www.socscistatistics.com/effectsize/Default3.aspx
#################################################
```{r}
datTime = NULL
for(i in 1:m){
  datTime[[i]] = subset(datAnalysisAllComplete[[i]], Time == 1 | Time == 2)
}

datTimeTreat = NULL
for(i in 1:m){
  datTimeTreat[[i]] = subset(datTime[[i]], Treatment == 1) 
}

library(psych)
cohenDat = NULL
Sec3TotalF2T0 = NULL
Sec3TotalF2T1 = NULL
for(i in 1:m){
  Sec3TotalF2T0[[i]] = subset(datTimeTreat[[i]], Time == 1)
  Sec3TotalF2T1[[i]] = subset(datTimeTreat[[i]], Time == 2)
  cohenDat[[i]] = mean(Sec3TotalF2T1[[i]]$Sec3TotalF2-Sec3TotalF2T0[[i]]$Sec3TotalF2)/sqrt((sd(Sec3TotalF2T1[[i]]$Sec3TotalF2)^2+sd(Sec3TotalF2T0[[i]]$Sec3TotalF2)^2)/2)
}

cohenD = data.frame(cohenDat)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  return(data.frame(coefs1, ses1))
}
y = data.frame(rnorm(10))

meldAllT_stat(cohenD, y)
```
##################################
Section 3F1 display predicted values
##################################
```{r}

output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL
df = NULL


for(i in 1:m){
  output[[i]] = lmer(Sec3TotalF1 ~ Treatment*poly(Time, degree = 2)  + Gender + Age + Race + (1 | ID), data  = datAnalysisAllComplete[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
  df[[i]] = output[[i]]$coefficients[,3]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)


meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output)
round(results,3)

```
Try mod graph
```{r}
modGraph = NULL
for(i in 1:m){
  modGraph[[i]] = cat_plot(model = outputReg[[i]], pred = "Time", modx = "Treatment", cluster = "ID", data = datAnalysisAllComplete[[i]], y.label = "Section Three Factor 1 total score")
}
modGraph[[1]]
```


#########################
T2 Only Sec3F2PrePost
#########################
```{r}
datTime = NULL
for(i in 1:m){
  datTime[[i]] = subset(datAnalysisAllComplete[[i]], Time == 0 | Time == 1)
}

datTimeTreat = NULL
for(i in 1:m){
  datTimeTreat[[i]] = subset(datTime[[i]], Treatment == 2) 
}

output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL
df = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec3TotalF2 ~ Time  + Gender + Age + Race + (1 | ID), data  = datTimeTreat[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
  df[[i]] = output[[i]]$coefficients[,3]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output)
round(results,3)

```
#########################
T2 Only Section 3F2 3month Post
#########################
```{r}
datTime = NULL
for(i in 1:m){
  datTime[[i]] = subset(datAnalysisAllComplete[[i]], Time == 1 | Time == 2)
}

datTimeTreat = NULL
for(i in 1:m){
  datTimeTreat[[i]] = subset(datTime[[i]], Treatment == 2) 
}

output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL
df = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec3TotalF2 ~ Time  + Gender + Age + Race + (1 | ID), data  = datTimeTreat[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
  df[[i]] = output[[i]]$coefficients[,3]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)


meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output)
round(results,3)

```
####################################
T2 Section 3F2 Table for change over time 
####################################

```{r}
library(descr)

datAllTimeT2 = NULL
for(i in 1:m){
  datAllTimeT2[[i]] = subset(datAnalysisAllComplete[[i]], Treatment == 2)
}

test= compmeans(datAllTimeT2[[1]]$Sec3TotalF2, datAllTimeT2[[1]]$Time)
test
```
#################################################
T2 Section3F1 Hedge's G for pre-post 
Use this formula because sample sizes the same: https://www.socscistatistics.com/effectsize/Default3.aspx
#################################################
```{r}
datTime = NULL
for(i in 1:m){
datTime[[i]] = subset(datAnalysisAllComplete[[i]], Time == 0 | Time == 1)
}

datTimeTreat = NULL
for(i in 1:m){
datTimeTreat[[i]] = subset(datTime[[i]], Treatment == 2) 
}

library(psych)
cohenDat = NULL
Sec3TotalF2T0 = NULL
Sec3TotalF2T1 = NULL
for(i in 1:m){
Sec3TotalF2T0[[i]] = subset(datTimeTreat[[i]], Time == 0)
Sec3TotalF2T1[[i]] = subset(datTimeTreat[[i]], Time == 1)
cohenDat[[i]] = mean(Sec3TotalF2T1[[i]]$Sec3TotalF2-Sec3TotalF2T0[[i]]$Sec3TotalF2)/sqrt((sd(Sec3TotalF2T1[[i]]$Sec3TotalF2)^2+sd(Sec3TotalF2T0[[i]]$Sec3TotalF2)^2)/2)
}

cohenD = data.frame(cohenDat)

meldAllT_stat = function(x,y){
coefsAll = mi.meld(q = x, se = y)
coefs1 = t(data.frame(coefsAll$q.mi))
ses1 = t(data.frame(coefsAll$se.mi))
return(data.frame(coefs1, ses1))
}
y = data.frame(rnorm(10))

meldAllT_stat(cohenD, y)
```
#################################################
T2 Section3F1 Hedge's G for T1 and T2 
Use this formula because sample sizes the same: https://www.socscistatistics.com/effectsize/Default3.aspx
#################################################
```{r}
datTime = NULL
for(i in 1:m){
  datTime[[i]] = subset(datAnalysisAllComplete[[i]], Time == 1 | Time == 2)
}

datTimeTreat = NULL
for(i in 1:m){
  datTimeTreat[[i]] = subset(datTime[[i]], Treatment == 2) 
}

library(psych)
cohenDat = NULL
Sec3TotalF2T0 = NULL
Sec3TotalF2T1 = NULL
for(i in 1:m){
  Sec3TotalF2T0[[i]] = subset(datTimeTreat[[i]], Time == 1)
  Sec3TotalF2T1[[i]] = subset(datTimeTreat[[i]], Time == 2)
  cohenDat[[i]] = mean(Sec3TotalF2T1[[i]]$Sec3TotalF2-Sec3TotalF2T0[[i]]$Sec3TotalF2)/sqrt((sd(Sec3TotalF2T1[[i]]$Sec3TotalF2)^2+sd(Sec3TotalF2T0[[i]]$Sec3TotalF2)^2)/2)
}

cohenD = data.frame(cohenDat)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  return(data.frame(coefs1, ses1))
}
y = data.frame(rnorm(10))

meldAllT_stat(cohenD, y)
```
#########################
T3 Only Sec3F2PrePost
#########################
```{r}
datTime = NULL
for(i in 1:m){
  datTime[[i]] = subset(datAnalysisAllComplete[[i]], Time == 0 | Time == 1)
}

datTimeTreat = NULL
for(i in 1:m){
  datTimeTreat[[i]] = subset(datTime[[i]], Treatment == 3) 
}

output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL
df = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec3TotalF2 ~ Time  + Gender + Age + Race + (1 | ID), data  = datTimeTreat[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
  df[[i]] = output[[i]]$coefficients[,3]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output)
round(results,3)

```
#########################
T3 Only Section 3F2 3month Post
#########################
```{r}
datTime = NULL
for(i in 1:m){
  datTime[[i]] = subset(datAnalysisAllComplete[[i]], Time == 1 | Time == 2)
}

datTimeTreat = NULL
for(i in 1:m){
  datTimeTreat[[i]] = subset(datTime[[i]], Treatment == 3) 
}

output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL
df = NULL

for(i in 1:m){
  output[[i]] = lmer(Sec3TotalF2 ~ Time  + Gender + Age + Race + (1 | ID), data  = datTimeTreat[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
  df[[i]] = output[[i]]$coefficients[,3]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)


meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output)
round(results,3)

```
####################################
T3  Section 3F2 Table for change over time 
####################################

```{r}
library(descr)

datAllTimeT3 = NULL
for(i in 1:m){
  datAllTimeT3[[i]] = subset(datAnalysisAllComplete[[i]], Treatment == 3)
}

test= compmeans(datAllTimeT3[[1]]$Sec3TotalF2, datAllTimeT3[[1]]$Time)
test
```
#################################################
T3 Section3F1 Hedge's G for pre-post 
Use this formula because sample sizes the same: https://www.socscistatistics.com/effectsize/Default3.aspx
#################################################
```{r}
datTime = NULL
for(i in 1:m){
datTime[[i]] = subset(datAnalysisAllComplete[[i]], Time == 0 | Time == 1)
}

datTimeTreat = NULL
for(i in 1:m){
datTimeTreat[[i]] = subset(datTime[[i]], Treatment == 3) 
}

library(psych)
cohenDat = NULL
Sec3TotalF2T0 = NULL
Sec3TotalF2T1 = NULL
for(i in 1:m){
Sec3TotalF2T0[[i]] = subset(datTimeTreat[[i]], Time == 0)
Sec3TotalF2T1[[i]] = subset(datTimeTreat[[i]], Time == 1)
cohenDat[[i]] = mean(Sec3TotalF2T1[[i]]$Sec3TotalF2-Sec3TotalF2T0[[i]]$Sec3TotalF2)/sqrt((sd(Sec3TotalF2T1[[i]]$Sec3TotalF2)^2+sd(Sec3TotalF2T0[[i]]$Sec3TotalF2)^2)/2)
}

cohenD = data.frame(cohenDat)

meldAllT_stat = function(x,y){
coefsAll = mi.meld(q = x, se = y)
coefs1 = t(data.frame(coefsAll$q.mi))
ses1 = t(data.frame(coefsAll$se.mi))
return(data.frame(coefs1, ses1))
}
y = data.frame(rnorm(10))

meldAllT_stat(cohenD, y)
```
#################################################
T3 Section3F2 Hedge's G for T1 and T2 
Use this formula because sample sizes the same: https://www.socscistatistics.com/effectsize/Default3.aspx
#################################################
```{r}
datTime = NULL
for(i in 1:m){
  datTime[[i]] = subset(datAnalysisAllComplete[[i]], Time == 1 | Time == 2)
}

datTimeTreat = NULL
for(i in 1:m){
  datTimeTreat[[i]] = subset(datTime[[i]], Treatment == 3) 
}

library(psych)
cohenDat = NULL
Sec3TotalF2T0 = NULL
Sec3TotalF2T1 = NULL
for(i in 1:m){
  Sec3TotalF2T0[[i]] = subset(datTimeTreat[[i]], Time == 1)
  Sec3TotalF2T1[[i]] = subset(datTimeTreat[[i]], Time == 2)
  cohenDat[[i]] = mean(Sec3TotalF2T1[[i]]$Sec3TotalF2-Sec3TotalF2T0[[i]]$Sec3TotalF2)/sqrt((sd(Sec3TotalF2T1[[i]]$Sec3TotalF2)^2+sd(Sec3TotalF2T0[[i]]$Sec3TotalF2)^2)/2)
}

cohenD = data.frame(cohenDat)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  return(data.frame(coefs1, ses1))
}
y = data.frame(rnorm(10))

meldAllT_stat(cohenD, y)
```
##################################
Section 3F1 display predicted values
##################################
```{r}

output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL
df = NULL


for(i in 1:m){
  output[[i]] = lmer(Sec3TotalF2 ~ Treatment*poly(Time, degree = 2)  + Gender + Age + Race + (1 | ID), data  = datAnalysisAllComplete[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
  df[[i]] = output[[i]]$coefficients[,3]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)


meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output)
round(results,3)

```
Try mod graph
```{r}
modGraph = NULL
for(i in 1:m){
  modGraph[[i]] = cat_plot(model = outputReg[[i]], pred = "Time", modx = "Treatment", cluster = "ID", data = datAnalysisAllComplete[[i]], y.label = "Section Three Factor 2 total score", interval = FALSE, geom = "line")
}
modGraph[[1]]
```


##########################################################
Linear Model: Section  Sec2PostPre, 1v2+3, 2v1+3 and 2v3 
##########################################################
```{r}
output = NULL
outputSummary = NULL
coef_output = NULL
se_output = NULL

for(i in 1:m){
  output[[i]] = lm(Sec2PostPre ~ factor(Treatment) + Gender + Race, data = datWideAnalysis[[i]])
  outputSummary[[i]] = summary(output[[i]])
  coef_output[[i]] = outputSummary[[i]]$coefficients[,1]
  se_output[[i]] = outputSummary[[i]]$coefficients[,2]
}
coef_output = data.frame(t(data.frame(coef_output))) 
se_output = data.frame(t(data.frame(se_output)))

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  t_stat = coefs1/ses1
  p = 2*(pt(-abs(t_stat), df = outputSummary[[2]]$df[2]))
  return(data.frame(coefs1, ses1, t_stat, p))
}


results = meldAllT_stat(coef_output, se_output)
round(results, 3)

# Make sure things make sense i.e. difference in means
compmeans(datWideAnalysis[[1]]$Sec2PostPre, datWideAnalysis[[1]]$Treatment)
```
Contrasts 
```{r}
K = matrix(c(0, 1, -1, 0,0), 1)
library(multcomp)
t = NULL
for(i in 1:m){
  t[[i]] = glht(output[[i]], linfct = K)
  t[[i]] = summary(t[[i]])
}
t
```
########################################
Hedge's G Outcome 2, Post-Pre,T1 and T3
########################################
```{r}
datWideAnalysisT12 = NULL

for(i in 1:m){
  datWideAnalysisT12[[i]] = subset(datWideAnalysis[[i]], Treatment == 1 | Treatment == 3)
}

#Need the mean and sd for each treatment
# We need to get the means and sd for Section 2 Pre-Post
# So I need to get the difference scores, then I need the mean, then substract each mean of the difference score for each treatment
# how do I get the standard deviation

cohenDat = NULL
for(i in 1:m){
  cohenDat[[i]] = cohen.d(datWideAnalysisT12[[i]]$Sec2PostPre, datWideAnalysisT12[[i]]$Treatment, hedges.correction = TRUE)
  cohenDat[[i]] = cohenDat[[i]]$estimate
}

cohenD = data.frame(t(data.frame(cohenDat)))

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  return(data.frame(coefs1, ses1))
}
y = data.frame(rnorm(10))

meldAllT_stat(cohenD, y)

```
########################################
Hedge's G Outcome 2, Post-Pre,T2 and T3
########################################
```{r}
datWideAnalysisT12 = NULL

for(i in 1:m){
  datWideAnalysisT12[[i]] = subset(datWideAnalysis[[i]], Treatment == 2 | Treatment == 3)
}


cohenDat = NULL
for(i in 1:m){
  cohenDat[[i]] = cohen.d(datWideAnalysisT12[[i]]$Sec2PostPre, datWideAnalysisT12[[i]]$Treatment, hedges.correction = TRUE)
  cohenDat[[i]] = cohenDat[[i]]$estimate
}

cohenD = data.frame(t(data.frame(cohenDat)))

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  return(data.frame(coefs1, ses1))
}
y = data.frame(rnorm(10))

meldAllT_stat(cohenD, y)

```
##########################################################
Linear Model: Section  Sec23monthPost, 1v2+3, 2v1+3 and 2v3 
##########################################################
```{r}
output = NULL
outputSummary = NULL
coef_output = NULL
se_output = NULL

for(i in 1:m){
  output[[i]] = lm(Sec23monthPost ~ factor(Treatment) + Gender + Race, data = datWideAnalysis[[i]])
  outputSummary[[i]] = summary(output[[i]])
  coef_output[[i]] = outputSummary[[i]]$coefficients[,1]
  se_output[[i]] = outputSummary[[i]]$coefficients[,2]
}
coef_output = data.frame(t(data.frame(coef_output))) 
se_output = data.frame(t(data.frame(se_output)))

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  t_stat = coefs1/ses1
  p = 2*(pt(-abs(t_stat), df = outputSummary[[2]]$df[2]))
  return(data.frame(coefs1, ses1, t_stat, p))
}


results = meldAllT_stat(coef_output, se_output)
round(results, 3)

# Make sure things make sense i.e. difference in means
compmeans(datWideAnalysis[[1]]$Sec23monthPost, datWideAnalysis[[1]]$Treatment)
```
####################################
Contrasts Sec23monthPost T2 versus T3
####################################
```{r}
K = matrix(c(0, 1, -1, 0,0), 1)
library(multcomp)
t = NULL
for(i in 1:m){
  t[[i]] = glht(output[[i]], linfct = K)
  t[[i]] = summary(t[[i]])
}
t
```
No differences no need for cohen's D

##########################################################
Linear Model: Section  Sec3F1PostPre, 1v2+3, 2v1+3 and 2v3 
##########################################################
```{r}
output = NULL
outputSummary = NULL
coef_output = NULL
se_output = NULL
for(i in 1:m){
  output[[i]] = lm(Sec3F1PostPre ~ factor(Treatment) + Gender + Race, data = datWideAnalysis[[i]])
  outputSummary[[i]] = summary(output[[i]])
  coef_output[[i]] = outputSummary[[i]]$coefficients[,1]
  se_output[[i]] = outputSummary[[i]]$coefficients[,2]
}
coef_output = data.frame(t(data.frame(coef_output))) 
se_output = data.frame(t(data.frame(se_output)))

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  t_stat = coefs1/ses1
  p = 2*(pt(-abs(t_stat), df = outputSummary[[2]]$df[2]))
  return(data.frame(coefs1, ses1, t_stat, p))
}


results = meldAllT_stat(coef_output, se_output)
round(results, 3)

# Make sure things make sense i.e. difference in means
compmeans(datWideAnalysis[[1]]$Sec3F1PostPre, datWideAnalysis[[1]]$Treatment)
```
####################################
Contrasts Sec3F1PostPre T2 versus T3
####################################
```{r}
K = matrix(c(0, 1, -1, 0,0), 1)
library(multcomp)
t = NULL
for(i in 1:m){
  t[[i]] = glht(output[[i]], linfct = K)
  t[[i]] = summary(t[[i]])
}
t
```
##########################################################
Linear Model: Section  Sec3F13monthPost, 1v2+3, 2v1+3 and 2v3 
##########################################################
```{r}
output = NULL
outputSummary = NULL
coef_output = NULL
se_output = NULL

for(i in 1:m){
  output[[i]] = lm(Sec23monthPost ~ factor(Treatment) + Gender + Race, data = datWideAnalysis[[i]])
  outputSummary[[i]] = summary(output[[i]])
  coef_output[[i]] = outputSummary[[i]]$coefficients[,1]
  se_output[[i]] = outputSummary[[i]]$coefficients[,2]
}
coef_output = data.frame(t(data.frame(coef_output))) 
se_output = data.frame(t(data.frame(se_output)))

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  t_stat = coefs1/ses1
  p = 2*(pt(-abs(t_stat), df = outputSummary[[2]]$df[2]))
  return(data.frame(coefs1, ses1, t_stat, p))
}


results = meldAllT_stat(coef_output, se_output)
round(results, 3)

# Make sure things make sense i.e. difference in means
compmeans(datWideAnalysis[[1]]$Sec23monthPost, datWideAnalysis[[1]]$Treatment)
```
####################################
Contrasts Sec3F1PostPre T2 versus T3
####################################
```{r}
K = matrix(c(0, 1, -1, 0,0), 1)
library(multcomp)
t = NULL
for(i in 1:m){
  t[[i]] = glht(output[[i]], linfct = K)
  t[[i]] = summary(t[[i]])
}
t
```
##########################################################
Linear Model: Section  Sec3F2PostPre, 1v2+3, 2v1+3 and 2v3 
##########################################################
```{r}
output = NULL
outputSummary = NULL
coef_output = NULL
se_output = NULL


for(i in 1:m){
  output[[i]] = lm(Sec3F2PostPre ~ factor(Treatment) + Gender + Race, data = datWideAnalysis[[i]])
  outputSummary[[i]] = summary(output[[i]])
  coef_output[[i]] = outputSummary[[i]]$coefficients[,1]
  se_output[[i]] = outputSummary[[i]]$coefficients[,2]
}
coef_output = data.frame(t(data.frame(coef_output))) 
se_output = data.frame(t(data.frame(se_output)))

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  t_stat = coefs1/ses1
  p = 2*(pt(-abs(t_stat), df = outputSummary[[2]]$df[2]))
  return(data.frame(coefs1, ses1, t_stat, p))
}


results = meldAllT_stat(coef_output, se_output)
round(results, 3)

# Make sure things make sense i.e. difference in means
compmeans(datWideAnalysis[[1]]$Sec3F2PostPre, datWideAnalysis[[1]]$Treatment)
```
####################################
Contrasts Sec3F2PostPre T2 versus T3
####################################
```{r}
K = matrix(c(0, 1, -1, 0,0), 1)
library(multcomp)
t = NULL
for(i in 1:m){
  t[[i]] = glht(output[[i]], linfct = K)
  t[[i]] = summary(t[[i]])
}
t
```
##########################################################
Linear Model: Section  Sec3F2PostPre, 1v2+3, 2v1+3 and 2v3 
##########################################################
```{r}
output = NULL
outputSummary = NULL
coef_output = NULL
se_output = NULL

for(i in 1:m){
  output[[i]] = lm(Sec3F23monthPost~ factor(Treatment) + Gender + Race, data = datWideAnalysis[[i]])
  outputSummary[[i]] = summary(output[[i]])
  coef_output[[i]] = outputSummary[[i]]$coefficients[,1]
  se_output[[i]] = outputSummary[[i]]$coefficients[,2]
}
coef_output = data.frame(t(data.frame(coef_output))) 
se_output = data.frame(t(data.frame(se_output)))

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  t_stat = coefs1/ses1
  p = 2*(pt(-abs(t_stat), df = outputSummary[[2]]$df[2]))
  return(data.frame(coefs1, ses1, t_stat, p))
}


results = meldAllT_stat(coef_output, se_output)
round(results, 3)

# Make sure things make sense i.e. difference in means
compmeans(datWideAnalysis[[1]]$Sec3F2PostPre, datWideAnalysis[[1]]$Treatment)
```
####################################
Contrasts Sec3F1PostPre T2 versus T3
####################################
```{r}
K = matrix(c(0, 1, -1, 0,0), 1)
library(multcomp)
t = NULL
for(i in 1:m){
  t[[i]] = glht(output[[i]], linfct = K)
  t[[i]] = summary(t[[i]])
}
t
```
