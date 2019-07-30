# Load Libraries ####
library(tidyverse)
library(here)
setwd('C:/Users/Jason/Documents/Stat 6010/Datasets')
library(GGally)

stack_q <- read.csv("stackdata_mod.csv", stringsAsFactors = FALSE,
                  na.strings= c("NA", "", "<NA>"))

# Check expectations
length(unique(stack_q$Id))==nrow(stack_q) # True if all Ids are unique

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

#creating variables to look for a couple of key words
stack_q$homework<- as.integer(grepl(pattern = "homework", stack_q$Body, ignore.case = T))
stack_q$assignment <- as.integer(grepl(pattern = "assignment", stack_q$Body, ignore.case = T))
stack_q$help <-  as.integer(grepl(pattern = "help", stack_q$Body, ignore.case = T))

#creating a variable to count the number of tags
stack_q$TagCount <-str_count(stack_q$Tag,"<")

#running the regression with all variables
test_reg <- glm(Count.dummy ~ TitleLength + Tquestion + Bquestion + BodyLength + Timeafter2010 + help + TagCount + homework + assignment + TitleWordCount + BodyWordCount,  data = stack_q, family = binomial(link = "logit"))
summary(test_reg)
anova(test_reg)

##################################################################################################
#model selection

#AIC running the regression with all variables
test_reg <- glm(Count.dummy ~ TitleLength + Tquestion + Bquestion + BodyLength + Timeafter2010 + help + TagCount + homework + assignment + BodyWordCount + TitleWordCount,  data = stack_q, family = binomial(link = "logit"))
summary(test_reg) #AIC = 12369
anova(test_reg)

#AIC paring it down to only include character lengths, and not wordcounts
test_reg1 <- glm(Count.dummy ~ TitleLength + Tquestion + Bquestion + BodyLength + Timeafter2010 + help + TagCount + homework + assignment,  data = stack_q, family = binomial(link = "logit"))
summary(test_reg1) #AIC 12404

#AIC trying with only the wordcounts, and not the character lengths
test_reg2 <- glm(Count.dummy ~ BodyWordCount + Tquestion + Bquestion + TitleWordCount + Timeafter2010 + help + TagCount + homework + assignment,  data = stack_q, family = binomial(link = "logit"))
summary(test_reg2) #AIC 12421

#AIC trying with character lengths and no wordsearches
test_reg3 <- glm(Count.dummy ~ BodyLength + Tquestion + Bquestion + TitleLength + Timeafter2010 + TagCount,  data = stack_q, family = binomial(link = "logit"))
summary(test_reg3) #AIC 12402

#AIC trying with character lengths and only help wordsearch
test_reg4 <- glm(Count.dummy ~ BodyLength + Tquestion + Bquestion + TitleLength + Timeafter2010 + TagCount + homework,  data = stack_q, family = binomial(link = "logit"))
summary(test_reg4) #AIC 12403

#AIC trying without key variables
#with no BodyLength
test_reg5 <- glm(Count.dummy ~  Tquestion + Bquestion + TitleLength + Timeafter2010 + TagCount,  data = stack_q, family = binomial(link = "logit"))
summary(test_reg5) #12420
#with no title length
test_reg6 <- glm(Count.dummy ~ BodyLength + Tquestion + Bquestion + Timeafter2010 + TagCount,  data = stack_q, family = binomial(link = "logit"))
summary(test_reg6) #12415
#with no Tquestion
test_reg7 <- glm(Count.dummy ~ BodyLength + Bquestion + TitleLength + Timeafter2010 + TagCount,  data = stack_q, family = binomial(link = "logit"))
summary(test_reg7) #12408
#with no Bquestion
test_reg8 <- glm(Count.dummy ~ BodyLength + Tquestion + TitleLength + Timeafter2010 + TagCount,  data = stack_q, family = binomial(link = "logit"))
summary(test_reg8) #12418
#with no Tquestion
test_reg9 <- glm(Count.dummy ~ BodyLength + Bquestion + TitleLength + Timeafter2010 + TagCount + homework,  data = stack_q, family = binomial(link = "logit"))
summary(test_reg9) #AIC 12408
#with no time
test_reg10 <- glm(Count.dummy ~ BodyLength + Tquestion + Bquestion + TitleLength + TagCount,  data = stack_q, family = binomial(link = "logit"))
summary(test_reg10) #AIC 13153
#with no tags
test_reg11 <- glm(Count.dummy ~ BodyLength + Tquestion + Bquestion + TitleLength + Timeafter2010,  data = stack_q, family = binomial(link = "logit"))
summary(test_reg11) #AIC 12406

#AIC trying one more time in our likely final model with wordcount instead of character
test_reg12 <-  glm(Count.dummy ~ BodyWordCount + Tquestion + Bquestion + TitleWordCount + Timeafter2010 + TagCount,  data = stack_q, family = binomial(link = "logit"))
summary(test_reg12) #AIC 12419

#Final, best mod based on AIC

test_reg <- glm(Count.dummy ~ BodyLength + Tquestion + Bquestion + TitleLength + Timeafter2010 + TagCount,  data = stack_q, family = binomial(link = "logit"))
summary(test_reg)
##########################################################################################################################################

#testing model against null and saturday
#null
null_model <- glm(Count.dummy ~ 1,  data = stack_q, family = binomial(link = "logit"))
summary(null_model)
anova(null_model, test_reg)
1-pchisq(903.23, 7) 

#saturated
1-pchisq(12393, 23405)

#testing for collinearity
vif(test_reg)


#
