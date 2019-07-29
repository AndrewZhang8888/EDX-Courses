#Titanic DataSet Graph Creation
options(digits = 3)    # report 3 significant digits
library(dplyr)
library(tidyverse)
library(titanic)
titanic <- titanic_train %>%
  select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare) %>%
  mutate(Survived = factor(Survived),
         Pclass = factor(Pclass),
         Sex = factor(Sex))

ggplot(titanic, aes(Age))+
  geom_density(aes(color = Sex))

params <- titanic %>%
  filter(!is.na(Age)) %>%
  summarize(mean = mean(Age), sd = sd(Age))
ggplot(titanic, aes(sample = Age))+
  stat_qq(dparams = params)+
  geom_abline()
 
ggplot(titanic, aes(Survived, fill = Sex))+
  geom_bar(position = position_dodge(1))

ggplot(titanic, aes(x = Age, stat(count), fill = Survived))+
  geom_density(alpha = 0.2)

filter(titanic, Fare > 0) %>%
  ggplot(aes(Survived, Fare))+
  geom_boxplot()+
  geom_jitter(size = 0.2)+
  scale_y_continuous(trans = "log2")

ggplot(titanic, aes(Pclass, fill = Survived))+
  geom_bar()

ggplot(titanic, aes(Pclass, fill = Survived))+
  geom_bar(position = position_fill())

ggplot(titanic, aes(Survived, fill = Pclass))+
  geom_bar(position = position_fill())

ggplot(titanic, aes(Age, stat(count), fill = Survived))

ggplot(titanic, aes(Age, stat(count), fill = Survived))+
  geom_density()+
  facet_grid(Sex ~ Pclass)

  
  
  