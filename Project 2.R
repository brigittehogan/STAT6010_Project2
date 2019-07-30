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
test_reg <- glm(Count.dummy ~ TitleLength + Tquestion + Bquestion + BodyLength + Timeafter2010 + help + TagCount,  data = stack_q, family = binomial(link = "logit"))
summary(test_reg)
anova(test_reg)

#model selection
ols_step_all_possible(test_reg)
plotcp1 <- regsubsets(Count.dummy ~ TitleLength + BodyLength + Tquestion + Bquestion + Timeafter2010 + homework + assignment, data = stack_q, method = "exhaustive")
summary(plotcp1)
plot(plotcp1, scale = "Cp")

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