# StackExchange


# Load Libraries ####
library(tidyverse)
library(here)
setwd(here("data"))
library(GGally)

# Load Data ####
stack <- read.csv("nondeletedposts.csv", stringsAsFactors = FALSE,
                  na.strings= c("NA", "", "<NA>"))

# Data Cleaning ####

# Repeating Jason's cleaning for only questions (I kept ID as a unique identifier)
stack_q <- stack %>% filter(PostTypeId==1)
stack_q <- stack_q %>% select(Id, CreationDate, Score, contains("Count"), 
                              Tags, Title, Body)

# Check expectations
length(unique(stack_q$Id))==nrow(stack_q) # True if all Ids are unique

# Dates
dateFormat <- "%Y-%m-%d"
stack_q$CreationDate <- as.Date(stack_q$CreationDate, format=dateFormat)
stack_q$CreationYear <- as.integer(strtrim(stack_q$CreationDate, width=4))

# Add new variables
# number of tags
stack_q$TagCount <- lapply(strsplit(stack_q$Tags, "><"), 
                         FUN=function(x) length(x)) %>% unlist()
stack_q$TagCount[is.na(stack_q$Tags)] <- 0

# length of title (in characters)
stack_q$TitleLength <- nchar(stack_q$Title)
stack_q$TitleLength[is.na(stack_q$Title)] <- 0

# length of body (in characters)
stack_q$BodyLength  <- nchar(stack_q$Body)

# dummy for observations
stack_q$Count <- 1

# Reorder columns for easy viewing
stack_q <- stack_q %>% select(Tags, Title, Body, everything())
stack_q <- stack_q %>% select("Id", contains("Creation"), "Score",
                          contains("Count"), contains("Length"), everything())
head(stack_q)
str(stack_q)
summary(stack_q)

# Create Tag List ####
tagList <- stack_q$Tags %>% unique()
tagList <- strsplit(tagList, "><") %>% unlist()
tagList <- gsub("^<|>$", "", tagList)
tagList <- tagList %>% trimws() %>% unique() %>% sort()

# Exploratory Analysis ####
stack_q$CreationDate %>% range(na.rm=TRUE)
# "2010-07-19 19:12:12 EDT" "2018-08-24 12:56:03 EDT"

hist(stack_q$CreationDate, breaks=15)
table(stack_q$CreationYear) # looks like we're missing 2012 & 2014

plot(table(stack_q$CreationDate))

stack_q %>% select(-Id, -Tags, -Title, -Body, -CreationDate, -CreationYear. -Count) %>% ggpairs()
                           
###Melissa's Additions
                           
##Adding columns to record Title and Body word counts                           
stack_q$TitleWordCount <-str_count(stack_q$Title,'\\w+')
stack_q$BodyWordCount <- str_count(stack_q$Body,'\\w+')

##Adding columns to record amount of "question" words in Title and Body                          
stack_q$Tquestion <- str_count(stack_q$Title, "who") + str_count(stack_q$Title, "what")+
  str_count(stack_q$Title, "when") + str_count(stack_q$Title, "where") + str_count(stack_q$Title, "why") + str_count(stack_q$Title, "how")

stack_q$Bquestion <- str_count(stack_q$Body, "who") + str_count(stack_q$Body, "what") +
  str_count(stack_q$Body, "when") + str_count(stack_q$Body, "where") + str_count(stack_q$Body, "why") + str_count(stack_q$Body, "how")

####################################################################################################################################
  
                           
# Load Libraries ####
library(tidyverse)
library(here)
setwd('C:/Users/Jason/Documents/Stat 6010/Datasets')
library(GGally)

# Load Data ####
stack_q <- read.csv("stackdata_mod.csv", stringsAsFactors = FALSE,
                  na.strings= c("NA", "", "<NA>"))

# Check expectations
length(unique(stack_q$Id))==nrow(stack_q) # True if all Ids are unique

# Dates
dateFormat <- "%Y-%m-%d"
stack_q$CreationDate <- as.Date(stack_q$CreationDate, format=dateFormat)
stack_q$CreationYear <- as.integer(strtrim(stack_q$CreationDate, width=4))

# Add new variables
# number of tags
stack_q$TagCount <- lapply(strsplit(stack_q$Tags, "><"), 
                           FUN=function(x) length(x)) %>% unlist()
stack_q$TagCount[is.na(stack_q$Tags)] <- 0

# length of title (in characters)
stack_q$TitleLength <- nchar(stack_q$Title)
stack_q$TitleLength[is.na(stack_q$Title)] <- 0

# length of body (in characters)
stack_q$BodyLength  <- nchar(stack_q$Body)

# dummy for observations
stack_q$Count <- 1

# Reorder columns for easy viewing
stack_q <- stack_q %>% select(Tags, Title, Body, everything())
stack_q <- stack_q %>% select("Id", contains("Creation"), "Score",
                              contains("Count"), contains("Length"), everything())
head(stack_q)
str(stack_q)
summary(stack_q)

# Create Tag List ####
tagList <- stack_q$Tags %>% unique()
tagList <- strsplit(tagList, "><") %>% unlist()
tagList <- gsub("^<|>$", "", tagList)
tagList <- tagList %>% trimws() %>% unique() %>% sort()

setwd('C:/Users/Jason/Documents/Stat 6010/Datasets')
library(MASS)
library(car)
library(olsrr)
library(leaps)
library(bestglm)

stack_q <- read.csv('stackdata_mod.csv')
# Exploratory Analysis ####
stack_q$CreationDate %>% range(na.rm=TRUE)
# "2010-07-19 19:12:12 EDT" "2018-08-24 12:56:03 EDT"

##Adding columns to record Title and Body word counts                           
stack_q$TitleWordCount <-str_count(stack_q$Title,'\\w+')
stack_q$BodyWordCount <- str_count(stack_q$Body,'\\w+')

##Adding columns to record amount of "question" words in Title and Body                          
stack_q$Tquestion <- str_count(stack_q$Title, "who") + str_count(stack_q$Title, "what")+
  str_count(stack_q$Title, "when") + str_count(stack_q$Title, "where") + str_count(stack_q$Title, "why") + str_count(stack_q$Title, "how")

stack_q$Bquestion <- str_count(stack_q$Body, "who") + str_count(stack_q$Body, "what") +
  str_count(stack_q$Body, "when") + str_count(stack_q$Body, "where") + str_count(stack_q$Body, "why") + str_count(stack_q$Body, "how")

# length of title (in characters)
stack_q$TitleLength <- nchar(stack_q$Title)
stack_q$TitleLength[is.na(stack_q$Title)] <- 0

# length of body (in characters)
stack_q$BodyLength  <- nchar(stack_q$Body)

#trying to modify the x variable into a square root
stack_q$srbody <- sqrt(stack_q$BodyLength)

stack_q$logbody <- log10(stack_q$BodyLength)

stack_q$bodysquared <- stack_q$BodyLength * stack_q$BodyLength

#running the regression with all variables
test_reg <- glm(Count.dummy ~ TitleLength + bodysquared + Tquestion + Bquestion, data = stack_q, family = binomial(link = "logit"))
summary(test_reg)
anova(test_reg)

#model selection
plotcp1 <- regsubsets(Count.dummy ~ TitleLength + BodyLength + BodyWordCount + TitleWordCount + Tquestion + Bquestion, data = stack_q, method = "exhaustive")
summary(plotcp1)
plot(plotcp1, scale = "Cp")

#testing linear conditions
ext_resids <- studres(test_reg)
plot(fitted.values(test_reg), ext_resids)
qqnorm(ext_resids)
plot(stack_q$BodyLength, stack_q$Count.dummy)


#testing for collinearity
vif(test_reg)


stack_q$apology<- as.integer(grepl(pattern = "apolog", stack_q$Body, ignore.case = T))
stack_q$sorry<- as.integer(grepl(pattern = "frustrated", stack_q$Body, ignore.case = T))

#
