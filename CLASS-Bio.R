library(ggplot2)
library(ggthemes)
library(gridExtra)
library(Rmisc)
setwd('D:/Dropbox/LA Research')

#Load data
example <- read.csv('CLASSBioExample.csv') #Load example dataset
overall.ques <- c(5:31,33:36) #select columns that contain questions (excluding Q28)
example[, overall.ques][example[, overall.ques] == 0] <- NA #replace all 0's with NA
example$PrePost <- factor(example$PrePost, levels = c('Pre', 'Post'))
#load questions
questions <- read.csv('CLASSBioQuestions.csv') #Load question details
expert.consensus <- questions[c(4,1)]
categories <- questions[c(3,1)]
agree <- 4 + unique(as.numeric(expert.consensus$Number[which(expert.consensus$Consensus == 'Agree')])) #Identifies all questions where the expert response is agree/strongly agree
disagree <- 4 + unique(as.numeric(expert.consensus$Number[which(expert.consensus$Consensus == 'Disagree')])) #Identifies all questions where the expert response is disagree/strongly disagree

######################################
#Transform data from 5-item likert to 3-item likert
###################################### 
agreement <- example
agreement[overall.ques][agreement[overall.ques] == 1] <- -1 #strongly disagree to disagree
agreement[overall.ques][agreement[overall.ques] == 2] <- -1 #disagree to disagree
agreement[overall.ques][agreement[overall.ques] == 3] <- 0  #neutral to neutral
agreement[overall.ques][agreement[overall.ques] == 4] <- 1  #agree to agree
agreement[overall.ques][agreement[overall.ques] == 5] <- 1  #strongly agree to agree

######################################
#Transform data based on alignment with expert response  
######################################  
favorable <-example
favorable[agree][favorable[agree] == 1] <- "unfavorable"         #strongly disagree to unfavorable
favorable[agree][favorable[agree] == 2] <- "unfavorable"         #disagree to unfaorable
favorable[agree][favorable[agree] == 3] <- "netural"             #neutral to neutral
favorable[agree][favorable[agree] == 4] <- "favorable"           #agree to favorable
favorable[agree][favorable[agree] == 5] <- "favorable"           #strongly agree to favorable
favorable[disagree][favorable[disagree] == 5] <- "unfavorable"   #strongly agree to unfavorable
favorable[disagree][favorable[disagree] == 4] <- "unfavorable"   #agree to unfavorable
favorable[disagree][favorable[disagree] == 3] <- "netural"       #neutral to neutral
favorable[disagree][favorable[disagree] == 2] <- "favorable"     #disagree to favorable
favorable[disagree][favorable[disagree] == 1] <- "favorable"     #strongly disagree to favorable

######################################
#Add new columns to favorable dataframe
######################################
#add columns for overall favorable/unfavorable percentages
favorable$Total.fav <- rowSums(favorable[overall.ques]=="favorable", na.rm = T)/rowSums(!is.na(favorable[overall.ques]))*100
favorable$Total.unfav <- rowSums(favorable[overall.ques]=="unfavorable", na.rm = T)/rowSums(!is.na(favorable[overall.ques]))*100
Total.fav_within <- summarySEwithin(data=favorable, measurevar="Total.fav", withinvars="PrePost",idvar="ID", na.rm=FALSE, conf.interval=.95)
Total.fav_between <- summarySE(data=favorable, measurevar="Total.fav", groupvars="PrePost", na.rm=FALSE, conf.interval=.95)
Total.unfav_within <- summarySEwithin(data=favorable, measurevar="Total.unfav", withinvars="PrePost",idvar="ID", na.rm=FALSE, conf.interval=.95)
Total.unfav_between <- summarySE(data=favorable, measurevar="Total.unfav", groupvars="PrePost", na.rm=FALSE, conf.interval=.95)

#add columns for all categories favorable/unfavorable percentages
all.categories <- 4 + unique(as.numeric(categories$Number[which(categories$Category != "Questions that don't fit but we'd like to keep.")])) #don't forget to take into account the leading columns
favorable$Categ.fav <- rowSums(favorable[all.categories]=="favorable", na.rm = T)/rowSums(!is.na(favorable[all.categories]))*100
favorable$Categ.unfav <- rowSums(favorable[all.categories]=="unfavorable", na.rm = T)/rowSums(!is.na(favorable[all.categories]))*100
Categ.fav_within <- summarySEwithin(data=favorable, measurevar="Categ.fav", withinvars="PrePost",idvar="ID", na.rm=FALSE, conf.interval=.95)
Categ.fav_between <- summarySE(data=favorable, measurevar="Categ.fav", groupvars="PrePost", na.rm=FALSE, conf.interval=.95)
Categ.unfav_within <- summarySEwithin(data=favorable, measurevar="Categ.unfav", withinvars="PrePost",idvar="ID", na.rm=FALSE, conf.interval=.95)
Categ.unfav_between <- summarySE(data=favorable, measurevar="Categ.unfav", groupvars="PrePost", na.rm=FALSE, conf.interval=.95)

#add columns for Conceptual Connections/ Memorization favorable/unfavorable percentages
CCM <- 4 + as.numeric(categories$Number[which(categories$Category == 'Conceptual Connections/ Memorization')])
favorable$CCM.fav <- rowSums(favorable[CCM]=="favorable", na.rm = T)/rowSums(!is.na(favorable[CCM]))*100
favorable$CCM.unfav <- rowSums(favorable[CCM]=="unfavorable", na.rm = T)/rowSums(!is.na(favorable[CCM]))*100
CCM.fav_within <- summarySEwithin(data=favorable, measurevar="CCM.fav", withinvars="PrePost",idvar="ID", na.rm=FALSE, conf.interval=.95)
CCM.fav_between <- summarySE(data=favorable, measurevar="CCM.fav", groupvars="PrePost", na.rm=FALSE, conf.interval=.95)
CCM.unfav_within <- summarySEwithin(data=favorable, measurevar="CCM.unfav", withinvars="PrePost",idvar="ID", na.rm=FALSE, conf.interval=.95)
CCM.unfav_between <- summarySE(data=favorable, measurevar="CCM.unfav", groupvars="PrePost", na.rm=FALSE, conf.interval=.95)

#add columns for Enjoyment favorable/unfavorable percentages
Enjoy <- 4 + as.numeric(categories$Number[which(categories$Category == 'Enjoyment')])
favorable$Enjoy.fav <- rowSums(favorable[Enjoy]=="favorable", na.rm = T)/rowSums(!is.na(favorable[Enjoy]))*100
favorable$Enjoy.unfav <- rowSums(favorable[Enjoy]=="unfavorable", na.rm = T)/rowSums(!is.na(favorable[Enjoy]))*100
Enjoy.fav_within <- summarySEwithin(data=favorable, measurevar="Enjoy.fav", withinvars="PrePost",idvar="ID", na.rm=FALSE, conf.interval=.95)
Enjoy.fav_between <- summarySE(data=favorable, measurevar="Enjoy.fav", groupvars="PrePost", na.rm=FALSE, conf.interval=.95)
Enjoy.unfav_within <- summarySEwithin(data=favorable, measurevar="Enjoy.unfav", withinvars="PrePost",idvar="ID", na.rm=FALSE, conf.interval=.95)
Enjoy.unfav_between <- summarySE(data=favorable, measurevar="Enjoy.unfav", groupvars="PrePost", na.rm=FALSE, conf.interval=.95)

#add columns for Problem-solving Difficulty favorable/unfavorable percentages
PS.D <- 4 + as.numeric(categories$Number[which(categories$Category == 'Problem-solving Difficulty')])
favorable$PS.D.fav <- rowSums(favorable[PS.D]=="favorable", na.rm = T)/rowSums(!is.na(favorable[PS.D]))*100
favorable$PS.D.unfav <- rowSums(favorable[PS.D]=="unfavorable", na.rm = T)/rowSums(!is.na(favorable[PS.D]))*100
PS.D.fav_within <- summarySEwithin(data=favorable, measurevar="PS.D.fav", withinvars="PrePost",idvar="ID", na.rm=FALSE, conf.interval=.95)
PS.D.fav_between <- summarySE(data=favorable, measurevar="PS.D.fav", groupvars="PrePost", na.rm=FALSE, conf.interval=.95)
PS.D.unfav_within <- summarySEwithin(data=favorable, measurevar="PS.D.unfav", withinvars="PrePost",idvar="ID", na.rm=FALSE, conf.interval=.95)
PS.D.unfav_between <- summarySE(data=favorable, measurevar="PS.D.unfav", groupvars="PrePost", na.rm=FALSE, conf.interval=.95)

#add columns for Problem-solving Effort favorable/unfavorable percentages
PS.E <- 4 + as.numeric(categories$Number[which(categories$Category == 'Problem-solving Effort')])
favorable$PS.E.fav <- rowSums(favorable[PS.E]=="favorable", na.rm = T)/rowSums(!is.na(favorable[PS.E]))*100
favorable$PS.E.unfav <- rowSums(favorable[PS.E]=="unfavorable", na.rm = T)/rowSums(!is.na(favorable[PS.E]))*100
PS.E.fav_within <- summarySEwithin(data=favorable, measurevar="PS.E.fav", withinvars="PrePost",idvar="ID", na.rm=FALSE, conf.interval=.95)
PS.E.fav_between <- summarySE(data=favorable, measurevar="PS.E.fav", groupvars="PrePost", na.rm=FALSE, conf.interval=.95)
PS.E.unfav_within <- summarySEwithin(data=favorable, measurevar="PS.E.unfav", withinvars="PrePost",idvar="ID", na.rm=FALSE, conf.interval=.95)
PS.E.unfav_between <- summarySE(data=favorable, measurevar="PS.E.unfav", groupvars="PrePost", na.rm=FALSE, conf.interval=.95)

#add columns for Problem-solving Strategies favorable/unfavorable percentages
PS.S <- 4 + as.numeric(categories$Number[which(categories$Category == 'Problem-solving Strategies')])
favorable$PS.S.fav <- rowSums(favorable[PS.S]=="favorable", na.rm = T)/rowSums(!is.na(favorable[PS.S]))*100
favorable$PS.S.unfav <- rowSums(favorable[PS.S]=="unfavorable", na.rm = T)/rowSums(!is.na(favorable[PS.S]))*100
PS.S.fav_within <- summarySEwithin(data=favorable, measurevar="PS.S.fav", withinvars="PrePost",idvar="ID", na.rm=FALSE, conf.interval=.95)
PS.S.fav_between <- summarySE(data=favorable, measurevar="PS.S.fav", groupvars="PrePost", na.rm=FALSE, conf.interval=.95)
PS.S.unfav_within <- summarySEwithin(data=favorable, measurevar="PS.S.unfav", withinvars="PrePost",idvar="ID", na.rm=FALSE, conf.interval=.95)
PS.S.unfav_between <- summarySE(data=favorable, measurevar="PS.S.unfav", groupvars="PrePost", na.rm=FALSE, conf.interval=.95)

#add columns for Real World Connection favorable/unfavorable percentages
RWC <- 4 + as.numeric(categories$Number[which(categories$Category == 'Real World Connection')])
favorable$RWC.fav <- rowSums(favorable[RWC]=="favorable", na.rm = T)/rowSums(!is.na(favorable[RWC]))*100
favorable$RWC.unfav <- rowSums(favorable[RWC]=="unfavorable", na.rm = T)/rowSums(!is.na(favorable[RWC]))*100
RWC.fav_within <- summarySEwithin(data=favorable, measurevar="RWC.fav", withinvars="PrePost",idvar="ID", na.rm=FALSE, conf.interval=.95)
RWC.fav_between <- summarySE(data=favorable, measurevar="RWC.fav", groupvars="PrePost", na.rm=FALSE, conf.interval=.95)
RWC.unfav_within <- summarySEwithin(data=favorable, measurevar="RWC.unfav", withinvars="PrePost",idvar="ID", na.rm=FALSE, conf.interval=.95)
RWC.unfav_between <- summarySE(data=favorable, measurevar="RWC.unfav", groupvars="PrePost", na.rm=FALSE, conf.interval=.95)

#add columns for Reasoning favorable/unfavorable percentages
Reason <- 4 + as.numeric(categories$Number[which(categories$Category == 'Reasoning')])
favorable$Reason.fav <- rowSums(favorable[Reason]=="favorable", na.rm = T)/rowSums(!is.na(favorable[Reason]))*100
favorable$Reason.unfav <- rowSums(favorable[Reason]=="unfavorable", na.rm = T)/rowSums(!is.na(favorable[Reason]))*100
Reason.fav_within <- summarySEwithin(data=favorable, measurevar="Reason.fav", withinvars="PrePost",idvar="ID", na.rm=FALSE, conf.interval=.95)
Reason.fav_between <- summarySE(data=favorable, measurevar="Reason.fav", groupvars="PrePost", na.rm=FALSE, conf.interval=.95)
Reason.unfav_within <- summarySEwithin(data=favorable, measurevar="Reason.unfav", withinvars="PrePost",idvar="ID", na.rm=FALSE, conf.interval=.95)
Reason.unfav_between <- summarySE(data=favorable, measurevar="Reason.unfav", groupvars="PrePost", na.rm=FALSE, conf.interval=.95)


######################################
#Create graphs
######################################

# Show the between-S CI's in red, and the within-S CI's in black
Tot.F <- ggplot(NULL, aes(x=PrePost, y=Total.fav, group= ID))+
  geom_line(data = favorable, color='gray') +  geom_point(data = favorable, color='gray')+
  geom_line(data = Total.fav_between, group=1) +
  geom_errorbar(data = Total.fav_between,  group=1, color='red', width=.1, aes(ymin=Total.fav-ci, ymax=Total.fav+ci)) +
  geom_errorbar( group=1, color=1, width=.1, aes(ymin=Total.fav-ci, ymax=Total.fav+ci), data=Total.fav_within) +
  labs(x="CLASS-Bio", y="Percent Favorable Scores") +
  scale_y_continuous(limits = c(0,100))+
  theme_par() +  theme(legend.position = 'none', plot.margin=unit(c(0.5,0.5,0.5,0.5), "cm"))
Tot.U <- ggplot(NULL, aes(x=PrePost, y=Total.unfav, group= ID))+
  geom_line(data = favorable, color='gray') +  geom_point(data = favorable, color='gray')+
  geom_line(data = Total.unfav_between, group=1) +
  geom_errorbar(data = Total.unfav_between,  group=1, color='red', width=.1, aes(ymin=Total.unfav-ci, ymax=Total.unfav+ci)) +
  geom_errorbar( group=1, color=1, width=.1, aes(ymin=Total.unfav-ci, ymax=Total.unfav+ci), data=Total.unfav_within) +
  labs(x="CLASS-Bio", y="Percent Unfavorable Scores") +
  scale_y_continuous(limits = c(0,100))+
  theme_par() +  theme(legend.position = 'none', plot.margin=unit(c(0.5,0.5,0.5,0.5), "cm"))

grid.arrange(Tot.F, Tot.U,
             ncol = 2, nrow = 1
             )

Cat.F <- ggplot(NULL, aes(x=PrePost, y=Categ.fav, group= ID))+
  geom_line(data = favorable, color='gray') +  geom_point(data = favorable, color='gray')+
  geom_line(data = Categ.fav_between, group=1) +
  geom_errorbar(data = Categ.fav_between,  group=1, color='red', width=.1, aes(ymin=Categ.fav-ci, ymax=Categ.fav+ci)) +
  geom_errorbar( group=1, color=1, width=.1, aes(ymin=Categ.fav-ci, ymax=Categ.fav+ci), data=Categ.fav_within) +
  labs(x="CLASS-Bio", y="Percent Favorable Scores") +
  scale_y_continuous(limits = c(0,100))+
  theme_par() +  theme(legend.position = 'none', plot.margin=unit(c(0.5,0.5,0.5,0.5), "cm"))
Cat.U <- ggplot(NULL, aes(x=PrePost, y=Categ.unfav, group= ID))+
  geom_line(data = favorable, color='gray') +  geom_point(data = favorable, color='gray')+
  geom_line(data = Categ.unfav_between, group=1) +
  geom_errorbar(data = Categ.unfav_between,  group=1, color='red', width=.1, aes(ymin=Categ.unfav-ci, ymax=Categ.unfav+ci)) +
  geom_errorbar( group=1, color=1, width=.1, aes(ymin=Categ.unfav-ci, ymax=Categ.unfav+ci), data=Categ.unfav_within) +
  labs(x="CLASS-Bio", y="Percent Unfavorable Scores") +
  scale_y_continuous(limits = c(0,100))+
  theme_par() +  theme(legend.position = 'none', plot.margin=unit(c(0.5,0.5,0.5,0.5), "cm"))

grid.arrange(Cat.F, Cat.U,
             ncol = 2, nrow = 1
)


Enjoy.F <- ggplot(NULL, aes(x=PrePost, y=Enjoy.fav, group= ID))+
  geom_line(data = favorable, color='gray') +  geom_point(data = favorable, color='gray')+
  geom_line(data = Enjoy.fav_between, group=1) +
  geom_errorbar(data = Enjoy.fav_between,  group=1, color='red', width=.1, aes(ymin=Enjoy.fav-ci, ymax=Enjoy.fav+ci)) +
  geom_errorbar( group=1, color=1, width=.1, aes(ymin=Enjoy.fav-ci, ymax=Enjoy.fav+ci), data=Enjoy.fav_within) +
  labs(x="CLASS-Bio", y="Percent Unfavorable Scores") +
  scale_y_continuous(limits = c(0,100))+
  theme_par() +  theme(legend.position = 'none', plot.margin=unit(c(0.5,0.5,0.5,0.5), "cm"))
Enjoy.U <- ggplot(NULL, aes(x=PrePost, y=Enjoy.unfav, group= ID))+
  geom_line(data = favorable, color='gray') +  geom_point(data = favorable, color='gray')+
  geom_line(data = Enjoy.unfav_between, group=1) +
  geom_errorbar(data = Enjoy.unfav_between,  group=1, color='red', width=.1, aes(ymin=Enjoy.unfav-ci, ymax=Enjoy.unfav+ci)) +
  geom_errorbar( group=1, color=1, width=.1, aes(ymin=Enjoy.unfav-ci, ymax=Enjoy.unfav+ci), data=Enjoy.unfav_within) +
  labs(x="CLASS-Bio", y="Percent Unfavorable Scores") +
  scale_y_continuous(limits = c(0,100))+
  theme_par() +  theme(legend.position = 'none', plot.margin=unit(c(0.5,0.5,0.5,0.5), "cm"))

grid.arrange(Enjoy.F, Enjoy.U,
             ncol = 2, nrow = 1
)


PS.D.F <- ggplot(NULL, aes(x=PrePost, y=PS.D.fav, group= ID))+
  geom_line(data = favorable, color='gray') +  geom_point(data = favorable, color='gray')+
  geom_line(data = PS.D.fav_between, group=1) +
  geom_errorbar(data = PS.D.fav_between,  group=1, color='red', width=.1, aes(ymin=PS.D.fav-ci, ymax=PS.D.fav+ci)) +
  geom_errorbar( group=1, color=1, width=.1, aes(ymin=PS.D.fav-ci, ymax=PS.D.fav+ci), data=PS.D.fav_within) +
  labs(x="CLASS-Bio", y="Percent Favorable Scores") +
  scale_y_continuous(limits = c(0,100))+
  theme_par() +  theme(legend.position = 'none', plot.margin=unit(c(0.5,0.5,0.5,0.5), "cm"))
PS.D.U <- ggplot(NULL, aes(x=PrePost, y=PS.D.unfav, group= ID))+
  geom_line(data = favorable, color='gray') +  geom_point(data = favorable, color='gray')+
  geom_line(data = PS.D.unfav_between, group=1) +
  geom_errorbar(data = PS.D.unfav_between,  group=1, color='red', width=.1, aes(ymin=PS.D.unfav-ci, ymax=PS.D.unfav+ci)) +
  geom_errorbar( group=1, color=1, width=.1, aes(ymin=PS.D.unfav-ci, ymax=PS.D.unfav+ci), data=PS.D.unfav_within) +
  labs(x="CLASS-Bio", y="Percent Unfavorable Scores") +
  scale_y_continuous(limits = c(0,100))+
  theme_par() +  theme(legend.position = 'none', plot.margin=unit(c(0.5,0.5,0.5,0.5), "cm"))

grid.arrange(PS.D.F, PS.D.U,
             ncol = 2, nrow = 1
)

PS.E.F <- ggplot(NULL, aes(x=PrePost, y=PS.E.fav, group= ID))+
  geom_line(data = favorable, color='gray') +  geom_point(data = favorable, color='gray')+
  geom_line(data = PS.E.fav_between, group=1) +
  geom_errorbar(data = PS.E.fav_between,  group=1, color='red', width=.1, aes(ymin=PS.E.fav-ci, ymax=PS.E.fav+ci)) +
  geom_errorbar( group=1, color=1, width=.1, aes(ymin=PS.E.fav-ci, ymax=PS.E.fav+ci), data=PS.E.fav_within) +
  labs(x="CLASS-Bio", y="Percent Favorable Scores") +
  scale_y_continuous(limits = c(0,100))+
  theme_par() +  theme(legend.position = 'none', plot.margin=unit(c(0.5,0.5,0.5,0.5), "cm"))
PS.E.U <- ggplot(NULL, aes(x=PrePost, y=PS.E.unfav, group= ID))+
  geom_line(data = favorable, color='gray') +  geom_point(data = favorable, color='gray')+
  geom_line(data = PS.E.unfav_between, group=1) +
  geom_errorbar(data = PS.E.unfav_between,  group=1, color='red', width=.1, aes(ymin=PS.E.unfav-ci, ymax=PS.E.unfav+ci)) +
  geom_errorbar( group=1, color=1, width=.1, aes(ymin=PS.E.unfav-ci, ymax=PS.E.unfav+ci), data=PS.E.unfav_within) +
  labs(x="CLASS-Bio", y="Percent Unfavorable Scores") +
  scale_y_continuous(limits = c(0,100))+
  theme_par() +  theme(legend.position = 'none', plot.margin=unit(c(0.5,0.5,0.5,0.5), "cm"))

grid.arrange(PS.E.F, PS.E.U,
             ncol = 2, nrow = 1
)


PS.S.F <- ggplot(NULL, aes(x=PrePost, y=PS.S.fav, group= ID))+
  geom_line(data = favorable, color='gray') +  geom_point(data = favorable, color='gray')+
  geom_line(data = PS.S.fav_between, group=1) +
  geom_errorbar(data = PS.S.fav_between,  group=1, color='red', width=.1, aes(ymin=PS.S.fav-ci, ymax=PS.S.fav+ci)) +
  geom_errorbar( group=1, color=1, width=.1, aes(ymin=PS.S.fav-ci, ymax=PS.S.fav+ci), data=PS.S.fav_within) +
  labs(x="CLASS-Bio", y="Percent Favorable Scores") +
  scale_y_continuous(limits = c(0,100))+
  theme_par() +  theme(legend.position = 'none', plot.margin=unit(c(0.5,0.5,0.5,0.5), "cm"))
PS.S.U <- ggplot(NULL, aes(x=PrePost, y=PS.S.unfav, group= ID))+
  geom_line(data = favorable, color='gray') +  geom_point(data = favorable, color='gray')+
  geom_line(data = PS.S.unfav_between, group=1) +
  geom_errorbar(data = PS.S.unfav_between,  group=1, color='red', width=.1, aes(ymin=PS.S.unfav-ci, ymax=PS.S.unfav+ci)) +
  geom_errorbar( group=1, color=1, width=.1, aes(ymin=PS.S.unfav-ci, ymax=PS.S.unfav+ci), data=PS.S.unfav_within) +
  labs(x="CLASS-Bio", y="Percent Unfavorable Scores") +
  scale_y_continuous(limits = c(0,100))+
  theme_par() +  theme(legend.position = 'none', plot.margin=unit(c(0.5,0.5,0.5,0.5), "cm"))

grid.arrange(PS.S.F, PS.S.U,
             ncol = 2, nrow = 1
)



RWC.F <- ggplot(NULL, aes(x=PrePost, y=RWC.fav, group= ID))+
  geom_line(data = favorable, color='gray') +  geom_point(data = favorable, color='gray')+
  geom_line(data = RWC.fav_between, group=1) +
  geom_errorbar(data = RWC.fav_between,  group=1, color='red', width=.1, aes(ymin=RWC.fav-ci, ymax=RWC.fav+ci)) +
  geom_errorbar( group=1, color=1, width=.1, aes(ymin=RWC.fav-ci, ymax=RWC.fav+ci), data=RWC.fav_within) +
  labs(x="CLASS-Bio", y="Percent Favorable Scores") +
  scale_y_continuous(limits = c(0,100))+
  theme_par() +  theme(legend.position = 'none', plot.margin=unit(c(0.5,0.5,0.5,0.5), "cm"))
RWC.U <- ggplot(NULL, aes(x=PrePost, y=RWC.unfav, group= ID))+
  geom_line(data = favorable, color='gray') +  geom_point(data = favorable, color='gray')+
  geom_line(data = RWC.unfav_between, group=1) +
  geom_errorbar(data = RWC.unfav_between,  group=1, color='red', width=.1, aes(ymin=RWC.unfav-ci, ymax=RWC.unfav+ci)) +
  geom_errorbar( group=1, color=1, width=.1, aes(ymin=RWC.unfav-ci, ymax=RWC.unfav+ci), data=RWC.unfav_within) +
  labs(x="CLASS-Bio", y="Percent Unfavorable Scores") +
  scale_y_continuous(limits = c(0,100))+
  theme_par() +  theme(legend.position = 'none', plot.margin=unit(c(0.5,0.5,0.5,0.5), "cm"))

grid.arrange(RWC.F, RWC.U,
             ncol = 2, nrow = 1
)

Reason.F <- ggplot(NULL, aes(x=PrePost, y=Reason.fav, group= ID))+
  geom_line(data = favorable, color='gray') +  geom_point(data = favorable, color='gray')+
  geom_line(data = Reason.fav_between, group=1) +
  geom_errorbar(data = Reason.fav_between,  group=1, color='red', width=.1, aes(ymin=Reason.fav-ci, ymax=Reason.fav+ci)) +
  geom_errorbar( group=1, color=1, width=.1, aes(ymin=Reason.fav-ci, ymax=Reason.fav+ci), data=Reason.fav_within) +
  labs(x="CLASS-Bio", y="Percent Favorable Scores") +
  scale_y_continuous(limits = c(0,100))+
  theme_par() +  theme(legend.position = 'none', plot.margin=unit(c(0.5,0.5,0.5,0.5), "cm"))
Reason.U <- ggplot(NULL, aes(x=PrePost, y=Reason.unfav, group= ID))+
  geom_line(data = favorable, color='gray') +  geom_point(data = favorable, color='gray')+
  geom_line(data = Reason.unfav_between, group=1) +
  geom_errorbar(data = Reason.unfav_between,  group=1, color='red', width=.1, aes(ymin=Reason.unfav-ci, ymax=Reason.unfav+ci)) +
  geom_errorbar( group=1, color=1, width=.1, aes(ymin=Reason.unfav-ci, ymax=Reason.unfav+ci), data=Reason.unfav_within) +
  labs(x="CLASS-Bio", y="Percent Unfavorable Scores") +
  scale_y_continuous(limits = c(0,100))+
  theme_par() +  theme(legend.position = 'none', plot.margin=unit(c(0.5,0.5,0.5,0.5), "cm"))

grid.arrange(Reason.F, Reason.U,
             ncol = 2, nrow = 1
)