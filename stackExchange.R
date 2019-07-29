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
