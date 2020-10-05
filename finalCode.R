# Aidan Draper, Robert Elmore, Hailey Tucker
# STS 325 - Final Project R Code
# Includes ANOVA assumption tests, SP/RM ANOVA, descriptive stats and figures

library(googlesheets)
library(ggplot2)
library(tidyr)
library(car)

# read in data from googlesheets
experiment.df <- gs_read(gs_key("1XnFlpsh4mgKXTsFzui1NL5I8gvnmfjpjg_iSvaPV47c"))

# convert subject id from integer to factor
experiment.df$subject_id <- as.factor(experiment.df$subject_id)

# run the SP/RM ANOVA test
sprm <- aov(time ~ test*music_level + Error(subject_id:test), data = experiment.df)
summary(sprm)

# did not use
with(experiment.df, boxplot(time ~ factor(music_level) | test, groups = subject_id, aspect = "xy"))

# CHECK ASSUMPTIONS
res.good <- aov(sqrt(time) ~ test*music_level + subject_id:test, data = experiment.df)

plot(res.good,1)
plot(res.good,2)

# check whether we need a transformation
boxCox(res.good)

# DESCRIPTIVE STATISTICS 
hist(experiment.df$time, main="Original", xlab="Response Time")
hist(sqrt(experiment.df$time), main="Transformed", xlab="Response Time")

# mean and variance calculations:
mean(experiment.df$time)

aggregate(time ~ test,data=experiment.df,FUN=mean)
aggregate(time ~ test,data=experiment.df,FUN=var)

aggregate(time ~ music_level,data=experiment.df,FUN=mean)
aggregate(time ~ music_level,data=experiment.df,FUN=var)

aggregate(time ~ music_level*test,data=experiment.df,FUN=mean)
aggregate(time ~ music_level*test,data=experiment.df,FUN=var)

# interaction plot
interaction.plot(x.factor = experiment.df$music_level,
                 trace.factor = experiment.df$test,
                 response = experiment.df$time, xlab = "Music Level", ylab="Mean Test Times")


# boxplot for SP/RM
ggplot(data=experiment.df, aes(x=test, y =time, fill=music_level)) + geom_boxplot(aes(x=test, y =time, fill=music_level)) +
  labs(x="Test Type", y = "Completion Times") +
  scale_fill_discrete("Music Level") +
  theme_classic() + theme(legend.position="bottom")
