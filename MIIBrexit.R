library(tidyverse)
library(readstata13)
library(openxlsx)

#data available from BES: http://www.britishelectionstudy.com/data-object/british-election-study-combined-wave-1-13-internet-panel/

dataset = read.dta13("../data/BES2017_W13_Panel_v1.2.dta")
dataset = dataset %>% filter(!is.na(gorW13)&!(gorW13 %in% c('Scotland','Northern Ireland','Non UK & Invalid')))
textan = read.spss("../data/BES2017_W13Strings_v1.0.sav", to.data.frame=TRUE)

engandw = left_join(dataset,textan)


engandw$profile_eurefvote = as.character(engandw$profile_eurefvote)
engandw[!(engandw$profile_eurefvote %in% c('Stay/remain in the EU','Leave the EU')),]$profile_eurefvote='None' 
engandw$profile_eurefvote = factor(engandw$profile_eurefvote,levels=c('Stay/remain in the EU','Leave the EU','None'))

engandw$generalElectionVoteW13 = as.character(engandw$generalElectionVoteW13)
engandw[is.na(engandw$generalElectionVoteW13)|(engandw$generalElectionVoteW13=="Don't know"),]$generalElectionVoteW13='None' 
engandw[grepl('UKIP',engandw$generalElectionVoteW13),]$generalElectionVoteW13='UKIP' 
engandw[!(engandw$generalElectionVoteW13 %in% c('Conservative','Labour','Liberal Democrat','UKIP','None') ),]$generalElectionVoteW13='Other' 
engandw$generalElectionVoteW13 = factor(engandw$generalElectionVoteW13,levels=c('Conservative','Labour','Liberal Democrat','UKIP','Other','None'))


engandw$profile_past_vote_2015 = as.character(engandw$profile_past_vote_2015)
engandw[is.na(engandw$profile_past_vote_2015)|(engandw$profile_past_vote_2015=="Don't know"),]$profile_past_vote_2015='None' 
engandw[grepl('UKIP',engandw$profile_past_vote_2015),]$profile_past_vote_2015='UKIP' 
engandw[!(engandw$profile_past_vote_2015 %in% c('Conservative','Labour','Green Party','Liberal Democrat','UKIP','None') ),]$profile_past_vote_2015='Other' 

engandw$profile_past_vote_2015 = factor(engandw$profile_past_vote_2015,levels=c('Conservative','Labour','Liberal Democrat','UKIP','Green Party','Other','None'))


engandw$combmiitext = apply(engandw[,c('MII_textW11','MII_textW12','MII_textW13')] ,1,function(x){
  paste(unique(map(x, function(y) tolower(trimws(as.character(y))))),collapse=',')
})

engandw$miibrexit = FALSE
#we searched through the answers to make sure we covered every misspelling of Brexit and that the respondent did not mean
#anything else
engandw[grepl('brexit|eu|exit|bexi|brex|blexit|britix|brxit|bretix|brixit|brett|brevet|btexit|blagtix|xit|brecht|breit|breix|bretex|brecit|breksit|bretrix|brev|briex|britax|britex|britix|brix|brrxit|brsexit|bextr',engandw$combmiitext),]$miibrexit=TRUE


engandw$euID = 'Undecided'
euID = data.frame(remain = rowSums(mutate_all(engandw[,c('euIDW11','euIDW12','euIDW13')],function(x){!is.na(x)&x=='The remain side'})),leave = rowSums(mutate_all(engandw[,c('euIDW11','euIDW12','euIDW13')],function(x){!is.na(x)&x=='The leave side'})))
engandw[euID$remain>0&euID$leave==0,]$euID = 'Remain'
engandw[euID$leave>0&euID$remain==0,]$euID = 'Leave'



#weighted responses
engandw[!is.na(engandw$wt_new_W13),] %>% group_by(miibrexit)  %>% summarise(n=sum(wt_new_W13)) 

#weighted responses split by leave and remain
engandw[!is.na(engandw$wt_new_W13),] %>% group_by(miibrexit,euID)  %>% summarise(n=sum(wt_new_W13))  %>% group_by(euID) %>% mutate(prop=n/sum(n))
engandw[!is.na(engandw$wt_new_W13),] %>% group_by(miibrexit,profile_eurefvote)  %>% summarise(n=sum(wt_new_W13))  %>% group_by(profile_eurefvote) %>% mutate(prop=n/sum(n))


reall =engandw[!is.na(engandw$wt_new_W13),] %>% group_by(euID,generalElectionVoteW13,miibrexit)  %>% summarise(n=sum(wt_new_W13)) %>% group_by(generalElectionVoteW13) %>%mutate(prop=n/(sum(n)))
reallvt =engandw[!is.na(engandw$wt_new_W13),] %>% group_by(profile_eurefvote,generalElectionVoteW13,miibrexit)  %>% summarise(n=sum(wt_new_W13)) %>% group_by(generalElectionVoteW13) %>%mutate(prop=n/(sum(n)))


#this is the graph published
ggplot(data.frame(reall))+geom_bar(aes(x=generalElectionVoteW13,weight=prop,fill=euID,alpha=miibrexit),position='dodge')+theme_bw()+scale_alpha_discrete("MII is Brexit",range=c(0.35,0.85))+scale_fill_manual("EU ID",values=c('red','blue','grey'))+ theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  xlab("2017 vote")+ylab("Weighted sample of the 2017 vote")+ggtitle("2017 Vote of each party split by MII and EU ID")


ggplot(data.frame(reallvt))+geom_bar(aes(x=generalElectionVoteW13,weight=prop,fill=profile_eurefvote,alpha=miibrexit),position='dodge')+
  theme_bw()+scale_alpha_discrete("MII is Brexit",range=c(0.35,0.85))+scale_fill_manual("Referendum Vote",values=c('red','blue','grey'))+ 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  xlab("2017 vote")+ylab("Weighted sample of the 2017 vote")+ggtitle("2017 Vote of each party split by MII and 2016 Vote")


ggplot(data.frame(reall))+geom_bar(aes(x=generalElectionVoteW13,weight=n,fill=euID,alpha=miibrexit),position='dodge')+theme_bw()+scale_alpha_discrete("MII is Brexit",range=c(0.35,0.85))+scale_fill_manual("EU ID",values=c('red','blue','grey'))+ theme(axis.text.x = element_text(angle = 90, hjust = 1))+xlab("2017 vote")+ylab("Weighted sample of the 2017 vote")

