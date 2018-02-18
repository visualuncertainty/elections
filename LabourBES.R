library(reshape2)
library(tidyverse)
library(readstata13)
library(openxlsx)

#data available from BES: http://www.britishelectionstudy.com/data-object/british-election-study-combined-wave-1-13-internet-panel/
dataset = read.dta13("../data/BES2017_W13_Panel_v1.2.dta")

ge2017 = read.xlsx("../data/BES-2017-General-Election-results-file.xlsx")



useful2017 = ge2017 %>% select(pano, Winner17, Majority17, Winner15, Majority15,leaveHanretty,remainHanretty)
datasetwuse = left_join(dataset,useful2017,by=c('panoW13'='pano'))

datasetwuse$profile_eurefvote = as.character(datasetwuse$profile_eurefvote)
datasetwuse[!(datasetwuse$profile_eurefvote %in% c('Stay/remain in the EU','Leave the EU')),]$profile_eurefvote='None' 
datasetwuse$profile_eurefvote = factor(datasetwuse$profile_eurefvote,levels=c('Stay/remain in the EU','Leave the EU','None'))

datasetwuse$generalElectionVoteW13 = as.character(datasetwuse$generalElectionVoteW13)
datasetwuse[is.na(datasetwuse$generalElectionVoteW13)|(datasetwuse$generalElectionVoteW13=="Don't know"),]$generalElectionVoteW13='None' 
datasetwuse[grepl('UKIP',datasetwuse$generalElectionVoteW13),]$generalElectionVoteW13='UKIP' 
datasetwuse[!(datasetwuse$generalElectionVoteW13 %in% c('Conservative','Labour','Green Party','Liberal Democrat','UKIP','None') ),]$generalElectionVoteW13='Other' 
datasetwuse$generalElectionVoteW13 = factor(datasetwuse$generalElectionVoteW13,levels=c('Conservative','Labour','Liberal Democrat','UKIP','Green Party','Other','None'))


datasetwuse$profile_past_vote_2015 = as.character(datasetwuse$profile_past_vote_2015)
datasetwuse[is.na(datasetwuse$profile_past_vote_2015)|(datasetwuse$profile_past_vote_2015=="Don't know"),]$profile_past_vote_2015='None' 
datasetwuse[grepl('UKIP',datasetwuse$profile_past_vote_2015),]$profile_past_vote_2015='UKIP' 
datasetwuse[!(datasetwuse$profile_past_vote_2015 %in% c('Conservative','Labour','Green Party','Liberal Democrat','UKIP','None') ),]$profile_past_vote_2015='Other' 

datasetwuse$profile_past_vote_2015 = factor(datasetwuse$profile_past_vote_2015,levels=c('Conservative','Labour','Liberal Democrat','UKIP','Green Party','Other','None'))


datasetwuse$lvgrps = cut(datasetwuse$leaveHanretty,c(0,50,60,100),labels=c('Remain','Leave 50-60%','Leave 60%+'))
datasetwuse$marg15 = cut(datasetwuse$Majority15,c(0,15,100),labels=c('Marginal 15','Safe 15'))
datasetwuse$marg17 = cut(datasetwuse$Majority17,c(0,15,100),labels=c('Marginal 17','Safe 17'))
datasetwuse$socialgrade = NA
datasetwuse[datasetwuse$profile_socialgrade_cieW13 %in% c('A','B','C1'),]$socialgrade = 'ABC1' 
datasetwuse[datasetwuse$profile_socialgrade_cieW13 %in% c('C2','D','E'),]$socialgrade = 'C2DE' 

datasetwuse$agegroups = cut(datasetwuse$ageW13,c(18,30,40,55,65,100),include.lowest = TRUE,labels = c('18-29','30-39','40-54','55-64','65+'))

#region
labourleave17 = datasetwuse[!is.na(datasetwuse$gorW13)&!is.na(datasetwuse$wt_new_W13)&!is.na(datasetwuse$generalElectionVoteW13)&datasetwuse$generalElectionVoteW13=='Labour',] %>% group_by(profile_eurefvote, gorW13)  %>% summarise(votes=sum(wt_new_W13)) %>% group_by(gorW13) %>% mutate(prop=votes/sum(votes))

ggplot(labourleave17)+geom_col(aes(x=gorW13,fill=profile_eurefvote,y=prop,group=profile_eurefvote),position="dodge")+theme_bw()+theme(axis.text.x = element_text(angle = 45, hjust = 1))+labs(fill = "EU Ref Vote") +xlab("UK Region")+ylab("Proportion of Labour voters")+ggtitle("Labour 2017 voters split by EU referendum vote and region")

#age
labourage = datasetwuse[!is.na(datasetwuse$agegroups)&!is.na(datasetwuse$wt_new_W13)&!is.na(datasetwuse$generalElectionVoteW13)&datasetwuse$generalElectionVoteW13=='Labour',] %>% group_by(profile_eurefvote, agegroups)  %>% summarise(votes=sum(wt_new_W13)) %>% group_by(agegroups) %>% mutate(prop=votes/sum(votes))

ggplot(labourage)+geom_col(aes(x=agegroups,fill=profile_eurefvote,y=prop,group=profile_eurefvote),position = 'dodge')+theme_bw()+theme(axis.text.x = element_text(angle = 45, hjust = 1))+labs(fill = "EU Ref Vote") +xlab("Age Group")+ylab("Proportion of Labour voters")+ggtitle("Labour 2017 voters split by EU referendum vote and age")

#social grade
laboursocial = datasetwuse[!is.na(datasetwuse$profile_socialgrade_cieW13)&!is.na(datasetwuse$wt_new_W13)&!is.na(datasetwuse$generalElectionVoteW13)&datasetwuse$generalElectionVoteW13=='Labour',] %>% group_by(profile_eurefvote, profile_socialgrade_cieW13)  %>% summarise(votes=sum(wt_new_W13)) %>% group_by(profile_socialgrade_cieW13) %>% mutate(prop=votes/sum(votes))

ggplot(laboursocial)+geom_col(aes(x=profile_socialgrade_cieW13,fill=profile_eurefvote,y=prop,group=profile_eurefvote),position = 'dodge')+theme_bw()+theme(axis.text.x = element_text(angle = 45, hjust = 1))+labs(fill = "EU Ref Vote") +xlab("")+ylab("Proportion of Labour voters")+ggtitle("Labour 2017 voters split by EU referendum vote and social grade")

#constituency

engandw = datasetwuse[!is.na(datasetwuse$gorW13)&!(datasetwuse$gorW13 %in% c('Scotland','Northern Ireland','Non UK & Invalid'))&!is.na(datasetwuse$wt_new_W13),]

labourlv17 = engandw[!is.na(engandw$lvgrps)&!is.na(engandw$generalElectionVoteW13)&engandw$generalElectionVoteW13=='Labour',] %>% group_by(profile_eurefvote, lvgrps)  %>% summarise(votes=sum(wt_new_W13)) %>% group_by(lvgrps) %>% mutate(prop=votes/sum(votes))

ggplot(labourlv17)+geom_col(aes(x=lvgrps,fill=profile_eurefvote,y=prop,group=profile_eurefvote),position = 'dodge')+theme_bw()+theme(axis.text.x = element_text(angle = 45, hjust = 1))+labs(fill = "EU Ref Vote") +xlab("Seat Type")+ylab("Proportion of Labour voters")+ggtitle("Labour 2017 voters split by EU referendum vote and proportion of constituency vote leave")


labourlv15marg = engandw[!is.na(engandw$lvgrps)&!is.na(engandw$generalElectionVoteW13)&engandw$generalElectionVoteW13=='Labour'&engandw$Majority15<15,] %>% group_by(profile_eurefvote, lvgrps)  %>% summarise(votes=sum(wt_new_W13)) %>% group_by(lvgrps) %>% mutate(prop=votes/sum(votes)) %>% mutate(marginal='2015 marginals')

labourlv17marg = engandw[!is.na(engandw$lvgrps)&!is.na(engandw$generalElectionVoteW13)&engandw$generalElectionVoteW13=='Labour'&engandw$Majority17<15,] %>% group_by(profile_eurefvote, lvgrps)  %>% summarise(votes=sum(wt_new_W13)) %>% group_by(lvgrps) %>% mutate(prop=votes/sum(votes)) %>% mutate(marginal='2017 marginals')

laboursafe = engandw[!is.na(engandw$lvgrps)&!is.na(engandw$generalElectionVoteW13)&(engandw$generalElectionVoteW13=='Labour')&(engandw$Majority17>=15)&(engandw$Majority15>=15),] %>% group_by(profile_eurefvote, lvgrps)  %>% summarise(votes=sum(wt_new_W13)) %>% group_by(lvgrps) %>% mutate(prop=votes/sum(votes)) %>% mutate(marginal='Safe')
laburlvmarg = rbind(labourlv15marg,labourlv17marg,laboursafe)

ggplot(laburlvmarg)+geom_col(aes(x=lvgrps,fill=profile_eurefvote,y=prop,group=profile_eurefvote),position = 'dodge')+facet_wrap(~marginal)+theme_bw()+theme(axis.text.x = element_text(angle = 45, hjust = 1))+labs(fill = "EU Ref Vote") +xlab("Seat Type")+ylab("Proportion of Labour voters")+ggtitle("Labour 2017 voters split by EU referendum vote and proportion of constituency vote leave")


