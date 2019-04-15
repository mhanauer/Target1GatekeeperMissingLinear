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
library(installr)
library(konfound)
library(nnet)
library(ggplot2)
library(prettyR)
library(nlme)
library(prettyR)
library(descr)
library(Amelia)
library(mitools)
library(BaylorEdPsych)
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
library(jtools)
library(MuMIn)
library(HLMdiag)
library(Hmisc)
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
write.csv(datPre, "datPre.csv", row.names = FALSE)
dim(datPre)

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
datPrePost = merge(datPre, datPost, by = "ID", sort = TRUE)
dim(datPrePost)
dim(datPrePost)

#dat3month = read.csv("3month.csv", header  = TRUE)
head(dat3month)
dat3month = dat3month[c(7, 11:22, 23:69)]
dim(datPost)
head(datPost)

# Now rename everything 
colnames(dat3month) = c("ID", "Sec1Qa", "Sec1Qb", "Sec1Qc", "Sec1Qd", "Sec1Qe", "Sec1Qf", "Sec1Qg", "Sec1Qh", "Sec1Qi", "Sec1Qj", "Sec1Qk", "Sec1Ql", "Sec2Qa", "Sec2Qb", "Sec2Qc", "Sec2Qd", "Sec2Qe", "Sec2Qf", "Sec2Qg",  "Sec2Qh", "Sec2Qi", "Sec2Qj", "Sec2Qk", "Sec2Ql", "Sec2Qm", "Sec2Qn", "Sec2Qo", "Sec3Qa", "Sec3Qb", "Sec3Qc", "Sec3Qd", "Sec3Qe", "Sec3Qf", "Sec3Qg", "Sec3Qh", "Sec4QaA", "Sec4QaB", "Sec4QbA", "Sec4QbB", "Sec4QcA", "Sec4QcB", "Sec4QdA", "Sec4QdB", "Sec4QeA", "Sec4QeB", "Sec4QfA","Sec4QfB", "Sec4QgA", "Sec4QgB", "Sec4QhA", "Sec4QhB", "Sec4QiA", "Sec4QiB", "Sec4QjA", "Sec4QjB", "Sec4QkA", "Sec4QkB", "Sec4QlA", "Sec4QlB")


datPrePost3month = merge(datPrePost, dat3month, by = "ID", all.x = TRUE, sort = TRUE)

head(datPrePost3month)
dim(datPrePost3month)

#### Number of matched breakdown
describe.factor(datPrePost3month$Treatment)

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
describe.factor(datPrePost3monthComplete$NAs)

# I need to grab the NAs variable put it into the full data set, then create two data sets and dim them one with all the data and one without all the data, then use na.omit later
datPrePost3month$NAs = datPrePost3monthComplete$NAs 
# Get rid of 3-month follow-up, because we are not including their data
dim(subset(datPrePost3month, time  == 0))

datPrePost3monthComplete = subset(datPrePost3month, time == 0 | time == 1)
dim(subset(datPrePost3monthComplete, time ==0))
sum(is.na(datPrePost3monthComplete))
# Now get rid of those with 70% missing data 
datPrePost3monthComplete = subset(datPrePost3monthComplete, NAs == 1)
dim(subset(datPrePost3monthComplete, time == 0))
dim(subset(datPrePost3monthComplete, time == 1))

#### Post or Matching broken down
base = subset(datPrePost3monthComplete, time == 0)
describe.factor(base$Treatment)
#### Now getting rid of the actual missing data
datPrePost3monthCompleteTest = na.omit(datPrePost3monthComplete)
describe.factor(datPrePost3monthCompleteTest$time)
## 
dim(datPrePost3monthCompleteTest)
baseTest = subset(datPrePost3monthCompleteTest, time == 0)
describe.factor(baseTest$Treatment)

######### Making the change to make it easier 
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
## Female == 2
datPrePost3monthAnalysis$Gender = ifelse(datPrePost3monthAnalysis$Gender == 2,1,0)
### Not 5 equals all but white
datPrePost3monthAnalysis$Race = ifelse(datPrePost3monthAnalysis$Race != 5, 1, 0)

describe.factor(datPrePost3monthAnalysis$Edu)
### Bachelors or lower
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
base_treat = subset(datPrePost3monthAnalysis, Time == 0)

```
Assess missing values for prePost3month and prePost
```{r}


sum(is.na(datPrePost3monthAnalysis))
datPrePost3monthAnalysisComplete = na.omit(datPrePost3monthAnalysis)
dim(datPrePost3monthAnalysis)[1]
dim(datPrePost3monthAnalysisComplete)[1]

1-(dim(datPrePost3monthAnalysisComplete)[1] / dim(datPrePost3monthAnalysis)[1])

test_norm = datPrePost3monthAnalysis[,-c(1)]
test_norm = as.data.frame(test_norm)

write.csv(test_norm, "test_norm.csv", row.names = FALSE)
test_norm = read.csv("test_norm.csv", header = TRUE)
TestMCARNormality(test_norm)


describe.factor(datPrePost3monthAnalysisComplete$Time)
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

datPrePost3monthAnalysisPostTest = subset(datPrePost3monthAnalysis, Time == 1)
describe(datPrePost3monthAnalysisPostTest)

round(sd(datPrePost3monthAnalysisPost$Age, na.rm  =TRUE),2)
round(sd(datPrePost3monthAnalysisPost$Sec2Total, na.rm  =TRUE),2)
round(sd(datPrePost3monthAnalysisPost$Sec3TotalF1, na.rm  =TRUE),2)
round(sd(datPrePost3monthAnalysisPost$Sec3TotalF2, na.rm  =TRUE),2)
round(sd(datPrePost3monthAnalysisPost$Sec4Total, na.rm  =TRUE),2)



```

datAnalysisAllComplete

#########################
Within program tests
#########################
```{r}

datAnalysisAllComplete = datPrePost3monthAnalysis

datAnalysisAllComplete$Sec2Total_scaled = scale(datAnalysisAllComplete$Sec2Total)
datAnalysisAllComplete$Sec3TotalF1_scaled = scale(datAnalysisAllComplete$Sec3TotalF1)
datAnalysisAllComplete$Sec3TotalF2_scaled = scale(datAnalysisAllComplete$Sec3TotalF2)
datAnalysisAllComplete$Sec4Total_scaled = scale(datAnalysisAllComplete$Sec4Total)

datAnalysisAllComplete$Sec2Total_log = log(datAnalysisAllComplete$Sec2Total)
datAnalysisAllComplete$Sec3TotalF1_log = log(datAnalysisAllComplete$Sec3TotalF1)
datAnalysisAllComplete$Sec3TotalF2_log = log(datAnalysisAllComplete$Sec3TotalF2)
#datAnalysisAllComplete$Sec4Total_log = log(datAnalysisAllComplete$Sec4Total)

datAnalysisT1 = subset(datAnalysisAllComplete, Treatment == 1)
datAnalysisT2 = subset(datAnalysisAllComplete, Treatment == 2)
datAnalysisT3 = subset(datAnalysisAllComplete, Treatment == 3)

```


#########################
T1 Only Sec2PrePost
#########################
```{r}
library(lme4)
library(lmerTest)
output_reg = lmer(Sec2Total ~ Time + (1 | ID), data  = datAnalysisT1)
summary(output_reg)
confint(output_reg)
output_stand = lmer(Sec2Total_scaled ~ Time + (1 | ID), data  = datAnalysisT1)
summary(output_stand)
output_log = lmer(Sec2Total_log ~ Time + (1 | ID), data  = datAnalysisT1)
summary(output_log)


uninstall.packages("lmerTest")
output_reg = lmer(Sec2Total ~ Time + (1 | ID), data  = datAnalysisT1)
konfound(output_reg, Time)


```
#########################
T2 Only Sec2PrePost
#########################
```{r}
library(lme4)
library(lmerTest)
output_reg = lmer(Sec2Total ~ Time + (1 | ID), data  = datAnalysisT2)
summary(output_reg)
confint(output_reg)
output_stand = lmer(Sec2Total_scaled ~ Time + (1 | ID), data  = datAnalysisT2)
summary(output_stand)
output_log = lmer(Sec2Total_log ~ Time + (1 | ID), data  = datAnalysisT2)
summary(output_log)


uninstall.packages("lmerTest")
output_reg = lmer(Sec2Total ~ Time + (1 | ID), data  = datAnalysisT2)
konfound(output_reg, Time)


```

########################
T3 Only Sec2PrePost
#########################
```{r}
library(lme4)
library(lmerTest)
output_reg = lmer(Sec2Total ~ Time + (1 | ID), data  = datAnalysisT3)
summary(output_reg)
confint(output_reg)
output_stand = lmer(Sec2Total_scaled ~ Time + (1 | ID), data  = datAnalysisT3)
summary(output_stand)
output_log = lmer(Sec2Total_log ~ Time + (1 | ID), data  = datAnalysisT3)
summary(output_log)


uninstall.packages("lmerTest")
output_reg = lmer(Sec2Total ~ Time + (1 | ID), data  = datAnalysisT3)
konfound(output_reg, Time)

```
#########################
T1 Only Sec3TotalF1
#########################
```{r}
library(lme4)
library(lmerTest)
output_reg = lmer(Sec3TotalF1 ~ Time + (1 | ID), data  = datAnalysisT1)
summary(output_reg)
confint(output_reg)
output_stand = lmer(Sec3TotalF1_scaled ~ Time + (1 | ID), data  = datAnalysisT1)
summary(output_stand)
output_log = lmer(Sec3TotalF1_log ~ Time + (1 | ID), data  = datAnalysisT1)
summary(output_log)


uninstall.packages("lmerTest")
output_reg = lmer(Sec3TotalF1 ~ Time + (1 | ID), data  = datAnalysisT1)
konfound(output_reg, Time)


```
########################
T2 Only Sec3TotalF1
#########################
```{r}
library(lme4)
library(lmerTest)
output_reg = lmer(Sec3TotalF1 ~ Time + (1 | ID), data  = datAnalysisT2)
summary(output_reg)
confint(output_reg)
output_stand = lmer(Sec3TotalF1_scaled ~ Time + (1 | ID), data  = datAnalysisT2)
summary(output_stand)
output_log = lmer(Sec3TotalF1_log ~ Time + (1 | ID), data  = datAnalysisT2)
summary(output_log)


uninstall.packages("lmerTest")
output_reg = lmer(Sec3TotalF1 ~ Time + (1 | ID), data  = datAnalysisT2)
konfound(output_reg, Time)

```
########################
T3 Only Sec3TotalF1
#########################
```{r}
library(lme4)
library(lmerTest)
output_reg = lmer(Sec3TotalF1 ~ Time + (1 | ID), data  = datAnalysisT3)
summary(output_reg)
confint(output_reg)
output_stand = lmer(Sec3TotalF1_scaled ~ Time + (1 | ID), data  = datAnalysisT3)
summary(output_stand)
output_log = lmer(Sec3TotalF1_log ~ Time + (1 | ID), data  = datAnalysisT3)
summary(output_log)


uninstall.packages("lmerTest")
output_reg = lmer(Sec3TotalF1 ~ Time + (1 | ID), data  = datAnalysisT3)
konfound(output_reg, Time)

```
#########################
T1 Only Sec3TotalF2
#########################
```{r}
library(lme4)
library(lmerTest)
output_reg = lmer(Sec3TotalF2 ~ Time + (1 | ID), data  = datAnalysisT1)
summary(output_reg)
confint(output_reg)
output_stand = lmer(Sec3TotalF2_scaled ~ Time + (1 | ID), data  = datAnalysisT1)
summary(output_stand)
output_log = lmer(Sec3TotalF2_log ~ Time + (1 | ID), data  = datAnalysisT1)
summary(output_log)


uninstall.packages("lmerTest")
output_reg = lmer(Sec3TotalF2 ~ Time + (1 | ID), data  = datAnalysisT1)
konfound(output_reg, Time)


```
########################
T2 Only Sec3TotalF2
#########################
```{r}
library(lme4)
library(lmerTest)
output_reg = lmer(Sec3TotalF2 ~ Time + (1 | ID), data  = datAnalysisT2)
summary(output_reg)
confint(output_reg)
output_stand = lmer(Sec3TotalF2_scaled ~ Time + (1 | ID), data  = datAnalysisT2)
summary(output_stand)
output_log = lmer(Sec3TotalF2_log ~ Time + (1 | ID), data  = datAnalysisT2)
summary(output_log)


uninstall.packages("lmerTest")
output_reg = lmer(Sec3TotalF2 ~ Time + (1 | ID), data  = datAnalysisT2)
konfound(output_reg, Time)

```
########################
T3 Only Sec3TotalF2
#########################
```{r}
library(lme4)
library(lmerTest)
output_reg = lmer(Sec3TotalF2 ~ Time + (1 | ID), data  = datAnalysisT3)
summary(output_reg)
confint(output_reg)
output_stand = lmer(Sec3TotalF2_scaled ~ Time + (1 | ID), data  = datAnalysisT3)
summary(output_stand)
output_log = lmer(Sec3TotalF2_log ~ Time + (1 | ID), data  = datAnalysisT3)
summary(output_log)


uninstall.packages("lmerTest")
output_reg = lmer(Sec3TotalF2 ~ Time + (1 | ID), data  = datAnalysisT3)
konfound(output_reg, Time)

```
#########################
T1 Only Sec4Total
#########################
```{r}
library(lme4)
library(lmerTest)
output_reg = lmer(Sec4Total ~ Time + (1 | ID), data  = datAnalysisT1)
summary(output_reg)
confint(output_reg)
output_stand = lmer(Sec4Total_scaled ~ Time + (1 | ID), data  = datAnalysisT1)
summary(output_stand)


uninstall.packages("lmerTest")
output_reg = lmer(Sec4Total ~ Time + (1 | ID), data  = datAnalysisT1)
konfound(output_reg, Time)


```
########################
T2 Only Sec4Total
#########################
```{r}
library(lme4)
library(lmerTest)
output_reg = lmer(Sec4Total ~ Time + (1 | ID), data  = datAnalysisT2)
summary(output_reg)
confint(output_reg)
output_stand = lmer(Sec4Total_scaled ~ Time + (1 | ID), data  = datAnalysisT2)
summary(output_stand)


uninstall.packages("lmerTest")
output_reg = lmer(Sec4Total ~ Time + (1 | ID), data  = datAnalysisT2)
konfound(output_reg, Time)

```
########################
T3 Only Sec4Total
#########################
```{r}
library(lme4)
library(lmerTest)
output_reg = lmer(Sec4Total ~ Time + (1 | ID), data  = datAnalysisT3)
summary(output_reg)
confint(output_reg)
output_stand = lmer(Sec4Total_scaled ~ Time + (1 | ID), data  = datAnalysisT3)
summary(output_stand)


uninstall.packages("lmerTest")
output_reg = lmer(Sec4Total ~ Time + (1 | ID), data  = datAnalysisT3)
konfound(output_reg, Time)

```

##########################################################
Between Program Model: Section  Sec2PostPre 
##########################################################
```{r}
install.packages("lmerTest")
library(lmerTest)

output_reg = lmer(Sec2Total ~ Time*factor(Treatment) + Age + Gender + Race + Edu + (1 | ID), data  = datAnalysisAllComplete)
summary(output_reg)
confint(output_reg)

output_reg_stand = lmer(Sec2Total_scaled ~ Time*factor(Treatment) + Age + Gender + Race + Edu  + (1 | ID), data  = datAnalysisAllComplete)
summary(output_reg_stand)

output_reg_log = lmer( Sec2Total_log~  Time*factor(Treatment) + Age + Gender + Race + Edu  + (1 | ID), data  = datAnalysisAllComplete)
summary(output_reg_log)


uninstall.packages("lmerTest")
output_reg = lmer(Sec2Total ~ Time*factor(Treatment) + Age + Gender + Race + Edu  + (1 | ID), data  = datAnalysisAllComplete)


konfound(output_reg, "Time:factor(Treatment)3")


```
Contrasts 
```{r}
K = matrix(c(0,0,0,0,0,0,0,0,1,-1), nrow = 1, byrow = TRUE)
library(multcomp)
t = glht(output_reg, linfct = K)
t = summary(t)
t


K = matrix(c(0,0,0,0,0,0,0,0,1,-1), nrow = 1, byrow = TRUE)
t_stand = glht(output_reg_stand, linfct = K)
t_stand = summary(t_stand)
t_stand 

K = matrix(c(0,0,0,0,0,0,0,0,1,-1), nrow = 1, byrow = TRUE)
t_log = glht(output_reg_log, linfct = K)
t_log = summary(t_log)
t_log

```
#######################################
Compare graphically the differences
#######################################
```{r}
modGraph = NULL
for(i in 1:m){
  modGraph[[i]] = cat_plot(model = outputReg[[i]], pred = "Time", modx = "Treatment", cluster = "ID", data = datAnalysisAllComplete[[i]], y.label = "Section Three Factor 2 total score", interval = FALSE, geom = "line")
}
modGraph[[1]]

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
