# STAT 6021 Project 2: StackExchange
# 31 July 2019
# Team:
# Brigitte Hogan bwh5v
# Sherry Kausch slk7s
# Melissa Phillips mcp3vj
# Jason Tiezzi jbt5am


# Load Libraries ####
library(tidyverse)
library(here)
library(GGally)
library(olsrr)
library(MASS)
library(car)
library(bestglm)

#setwd(here("data"))
#___________________________###################################################
# Read in data ####
stack_q <- read.csv("stackdata_mod.csv", stringsAsFactors = FALSE,
                    na.strings= c("NA", "", "<NA>"))
names(stack_q)[which(names(stack_q)=="Time.after.1.1.2010")] <- "Timeafter2010"

#___________________________###################################################
# Check expectations ####
length(unique(stack_q$Id))==nrow(stack_q) # True if all Ids are unique

##Adding columns to record Title and Body word counts                           
stack_q$TitleWordCount <-str_count(stack_q$Title,'\\w+')
stack_q$BodyWordCount <- str_count(stack_q$Body,'\\w+')

##Adding columns to record amount of "question" words in Title and Body                          
stack_q$Tquestion <- str_count(stack_q$Title, "who") + 
  str_count(stack_q$Title, "what")+
  str_count(stack_q$Title, "when") + 
  str_count(stack_q$Title, "where") + 
  str_count(stack_q$Title, "why") + 
  str_count(stack_q$Title, "how")

stack_q$Bquestion <- str_count(stack_q$Body, "who") + 
  str_count(stack_q$Body, "what") +
  str_count(stack_q$Body, "when") + 
  str_count(stack_q$Body, "where") + 
  str_count(stack_q$Body, "why") + 
  str_count(stack_q$Body, "how")

# length of title (in characters)
stack_q$TitleLength <- nchar(stack_q$Title)
stack_q$TitleLength[is.na(stack_q$Title)] <- 0

# length of body (in characters)
stack_q$BodyLength  <- nchar(stack_q$Body)

#creating variables to look for a couple of key words
stack_q$homework   <- as.integer(grepl(pattern = "homework", stack_q$Body,
                                    ignore.case = T))
stack_q$assignment <- as.integer(grepl(pattern = "assignment", stack_q$Body, 
                                       ignore.case = T))
stack_q$help       <- as.integer(grepl(pattern = "help", stack_q$Body, 
                                  ignore.case = T))

#creating a variable to count the number of tags
stack_q$TagCount <-str_count(stack_q$Tag,"<")

#running the regression with all variables
test_reg <- glm(Count.dummy ~ TitleLength + Tquestion + Bquestion + 
                  BodyLength + Timeafter2010 + help + TagCount + 
                  homework + assignment + TitleWordCount + BodyWordCount,  
                data = stack_q, family = binomial(link = "logit"))
summary(test_reg)
anova(test_reg)

#___________________________###################################################
#Model selection ####
#AIC running the regression with all variables
test_reg <- glm(Count.dummy ~ TitleLength + Tquestion + Bquestion + BodyLength +
                  Timeafter2010 + help + TagCount + homework + assignment + 
                  BodyWordCount + TitleWordCount,  data = stack_q, 
                family = binomial(link = "logit"))
summary(test_reg) #AIC = 12369
anova(test_reg)

#AIC paring it down to only include character lengths, and not wordcounts
test_reg1 <- glm(Count.dummy ~ TitleLength + Tquestion + Bquestion + 
                   BodyLength + Timeafter2010 + help + TagCount + homework + 
                   assignment,  data = stack_q, 
                 family = binomial(link = "logit"))
summary(test_reg1) #AIC 12404

#AIC trying with only the wordcounts, and not the character lengths
test_reg2 <- glm(Count.dummy ~ BodyWordCount + Tquestion + Bquestion + 
                   TitleWordCount + Timeafter2010 + help + TagCount + 
                   homework + assignment,  data = stack_q, 
                 family = binomial(link = "logit"))
summary(test_reg2) #AIC 12421

#AIC trying with character lengths and no wordsearches
test_reg3 <- glm(Count.dummy ~ BodyLength + Tquestion + Bquestion + 
                   TitleLength + Timeafter2010 + TagCount,  data = stack_q, 
                 family = binomial(link = "logit"))
summary(test_reg3) #AIC 12402

#AIC trying with character lengths and only help wordsearch
test_reg4 <- glm(Count.dummy ~ BodyLength + Tquestion + Bquestion + 
                   TitleLength + Timeafter2010 + TagCount + help,  
                 data = stack_q, family = binomial(link = "logit"))
summary(test_reg4) #AIC 12401

#AIC trying without key variables
#with no BodyLength
test_reg5 <- glm(Count.dummy ~  Tquestion + Bquestion + TitleLength + 
                   Timeafter2010 + TagCount,  data = stack_q, 
                 family = binomial(link = "logit"))
summary(test_reg5) #12420
#with no title length
test_reg6 <- glm(Count.dummy ~ BodyLength + Tquestion + Bquestion + 
                   Timeafter2010 + TagCount,  data = stack_q, 
                 family = binomial(link = "logit"))
summary(test_reg6) #12415
#with no Tquestion
test_reg7 <- glm(Count.dummy ~ BodyLength + Bquestion + TitleLength + 
                   Timeafter2010 + TagCount,  data = stack_q, 
                 family = binomial(link = "logit"))
summary(test_reg7) #12408
#with no Bquestion
test_reg8 <- glm(Count.dummy ~ BodyLength + Tquestion + TitleLength + 
                   Timeafter2010 + TagCount,  data = stack_q, 
                 family = binomial(link = "logit"))
summary(test_reg8) #12418
#with no Tquestion
test_reg9 <- glm(Count.dummy ~ BodyLength + Bquestion + TitleLength + 
                   Timeafter2010 + TagCount + homework,  data = stack_q, 
                 family = binomial(link = "logit"))
summary(test_reg9) #AIC 12408
#with no time
test_reg10 <- glm(Count.dummy ~ BodyLength + Tquestion + Bquestion + 
                    TitleLength + TagCount,  data = stack_q, 
                  family = binomial(link = "logit"))
summary(test_reg10) #AIC 13153
#with no tags
test_reg11 <- glm(Count.dummy ~ BodyLength + Tquestion + Bquestion + 
                    TitleLength + Timeafter2010,  data = stack_q, 
                  family = binomial(link = "logit"))
summary(test_reg11) #AIC 12406

#AIC trying one more time in our likely final model with wordcount instead of character
test_reg12 <-  glm(Count.dummy ~ BodyWordCount + Tquestion + Bquestion + 
                     TitleWordCount + Timeafter2010 + TagCount + help,  
                   data = stack_q, family = binomial(link = "logit"))
summary(test_reg12) #AIC 12419

#Final, best mod based on AIC ####
test_reg <- glm(Count.dummy ~ BodyLength + Tquestion + Bquestion + 
                  TitleLength + Timeafter2010 + TagCount + help,  
                data = stack_q, family = binomial(link = "logit"))
summary(test_reg)

#___________________________###################################################
# Testing model against null and saturated ####
#null
null_model <- glm(Count.dummy ~ 1,  data = stack_q, 
                  family = binomial(link = "logit"))
summary(null_model)
anova(null_model, test_reg)
1-pchisq(903.23, 7) 

#saturated
1-pchisq(12393, 23405)

#testing for collinearity
vif(test_reg)


#Turning logits to probabilities
logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}

logit2prob(coef(test_reg))

#___________________________###################################################
# Visualizations ####

# restructure data for visuals
stack_best <- stack_q %>% dplyr::select(Count.dummy, BodyLength, Tquestion, 
                                        Bquestion, TitleLength, Timeafter2010, 
                                        TagCount, help)
head(stack_best)
stack_plot <- stack_best
stack_plot$Answered <- stack_plot$Count.dummy
stack_plot$Answered <- as.factor(stack_plot$Answered)

# view relationship between regressors
ggpairs(stack_best[,-1])

### Scatterplot
pt1 <- stack_plot %>% 
  ggplot(aes(Timeafter2010, BodyLength, color=Answered)) +
  geom_point(alpha=0.5) + 
  ylab("Number of Characters in Body\n(BodyLength)") +
  xlab("Days after 2010 (Timeafter2010)")
pt1 + geom_jitter(width = 100, height = 1) + theme_classic()


### Histograms

# regressors
hist <- stack_plot %>% ggplot(aes(BodyLength)) + geom_histogram(bins=50)
hist + theme_classic() + 
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=14,face="bold"))

hist <- stack_plot %>% ggplot(aes(TitleLength)) + geom_histogram(bins=50)
hist + theme_classic() + 
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=14,face="bold"))

hist <- stack_plot %>% ggplot(aes(Timeafter2010)) + geom_histogram(bins=50)
hist + theme_classic() + 
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=14,face="bold"))

hist <- stack_plot %>% ggplot(aes(Bquestion)) + geom_histogram(bins=50)
hist + theme_classic() + 
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=14,face="bold"))

stack_plot$help <- as.factor(stack_plot$help)
hist <- stack_plot %>% ggplot(aes(help)) + geom_bar()
hist + theme_classic() + 
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=14,face="bold"))

stack_plot$Tquestion <- as.factor(stack_plot$Tquestion)
hist <- stack_plot %>% ggplot(aes(Tquestion)) + geom_bar()
hist + theme_classic() + 
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=14,face="bold"))

stack_plot$TagCount <- as.factor(stack_plot$TagCount)
hist <- stack_plot %>% ggplot(aes(TagCount)) + geom_bar()
hist + theme_classic() + 
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=14,face="bold"))

# dependent
stack_plot$Count.dummy <- as.factor(stack_plot$Count.dummy)
hist <- stack_plot %>% ggplot(aes(Count.dummy)) + geom_bar(fill = "#FF6666")
hist + theme_classic() + 
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=14,face="bold"))
