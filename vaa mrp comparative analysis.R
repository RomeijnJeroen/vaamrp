### Script for merging and analysing MRP-ed survey estimates and VAA estimates of the policy positions of party supporters
### Last update: 27 March 2018 (Dimiter Toshkov) 
# Libraries, functions and working directory ------------------------------
library(data.table)
library(car)
library(plyr)
library(reshape2)
##prepare functions which ignore the NAs
sum.na<-function (x) {sum(x, na.rm=T)}
mean.na<-function (x) {mean(x, na.rm=T)}
median.na<-function (x) {median(x, na.rm=T)}
sd.na<-function (x) {sd(x, na.rm=T)}

setwd('/Volumes/F/VAA survey comparison')
# Load and arrange the survey data ----------------------------------------
vaa<-read.table("vaa3.txt", sep='\t', dec='.', header=T)
vaa$vaa_issue_name<-recode(vaa$policyname,"'pension age 65'='pensions';'tougher punishment'='punishments';'abolish hypotheekrenteaftrek'='mortgages';'build nuclear'='nuclear';
                           'Turkey eu'='Turkey (NL)'; 'cut unemployment benefits'='unemployment';'leave euro'='euro';'higher top income tax rate'='taxes';
                           'Turkey EU'='Turkey (DE)';'speed limits'='speed';'equal adoption same sex couples'='adoption'" )

vaa$vaa_issue<-recode(vaa$policyname,"'pension age 65'='issue17';'tougher punishment'='issue7';'abolish hypotheekrenteaftrek'='issue20';'build nuclear'='issue27'; 'Turkey eu'='issue4'; 'cut unemployment benefits'='issue19';
                      'leave euro'='issue13';'higher top income tax rate'='issue10'; 'Turkey EU'='issue18';'speed limits'='issue30';'equal adoption same sex couples'='issue1'" )

vaa$partyname<-recode(vaa$partyname, "'CDUCSU'='CDU/CSU'; 'Gruene'='Die Gruenen'; 'Linke'='Die Linke'; 'FPD'='FDP'")
vaa$index<-paste0(vaa$partyname, ".", vaa$vaa_issue)
vaa$index.name<-paste0(vaa$partyname, ".", vaa$vaa_issue_name)
vaa.nl<-subset(vaa, vaa$country=='NL')
vaa.de<-subset(vaa, vaa$country=='DE')
# Load and arrange the Dutch VAA data -------------------------------------------
tk <- fread("TK2012 anonymous 2 (2).csv")
tk<-subset(tk, tk$duration>100 &  tk$duration<(60*30) & tk$country=="NL") #subset only those who completed the survey in reasonable times and from NL
hist(tk$duration, nclass=100000)

###Recode the past vote and voting intention variables to the party names
tk$vote2012<-recode(tk$extra_question_5, "98='PvdA';
                    99='VVD';100='CDA';
                    101='CU';102='GL';103='SP';
                    104='D66';105='SGP';106='PvdD';107='PVV';
                    108='50PLUS';211='Pirates';212='Anders';
                    373='WeetNiet'")
tk$vote2010<-recode(tk$extra_question_6, "110='PvdA';
                    112='VVD';109='CDA';
                    115='CU';114='GL';111='SP';
                    116='D66';117='SGP';118='PvdD';113='PVV';
                    120='WeetNietMeer';121='nietGest';
                    122='mochtNiet';119='Anders1'")

summary(as.factor(tk$vote2012)) #summarize
summary(as.factor(tk$vote2010))

#define the party supporter
tk$supporter<-ifelse(tk$vote2010==tk$vote2012, 1, 0) #create a variable for the core supporters only
table(tk$supporter, tk$vote2012) #check results

#write.table(tk1, "E:/TK.csv")
tk1<-subset(tk, tk$supporter==1) #filter the dataset by core supporters
###alternatively, filter only on previous vote:
tk1<-subset(tk, tk$vote2010=="PvdA" | tk$vote2010=="CDA" | tk$vote2010=="CU" |
              tk$vote2010=="D66"| tk$vote2010=="SP" | tk$vote2010=="GL" |
              tk$vote2010=="SGP" | tk$vote2010=="PvdD"| tk$vote2010=="PVV" |
              tk$vote2010=="VVD") #filter only by those who voted on the previous elections
table(tk1$supporter, tk1$vote2010)

##create new variables for the issues with the proper numbers and recode -1s to NAs
tk1$issue1<-recode(tk1$answer_1, "-1=NA")
tk1$issue2<-recode(tk1$answer_2, "-1=NA")
tk1$issue3<-recode(tk1$answer_5, "-1=NA")
tk1$issue4<-recode(tk1$answer_6, "-1=NA")
tk1$issue5<-recode(tk1$answer_7, "-1=NA")
tk1$issue6<-recode(tk1$answer_8, "-1=NA")
tk1$issue7<-recode(tk1$answer_9, "-1=NA")
tk1$issue8<-recode(tk1$answer_11, "-1=NA")
tk1$issue9<-recode(tk1$answer_13, "-1=NA")
tk1$issue10<-recode(tk1$answer_14, "-1=NA")
tk1$issue11<-recode(tk1$answer_17, "-1=NA")
tk1$issue12<-recode(tk1$answer_19, "-1=NA")
tk1$issue13<-recode(tk1$answer_21, "-1=NA")
tk1$issue14<-recode(tk1$answer_22, "-1=NA")
tk1$issue15<-recode(tk1$answer_24, "-1=NA")
tk1$issue16<-recode(tk1$answer_26, "-1=NA")
tk1$issue17<-recode(tk1$answer_27, "-1=NA")
tk1$issue18<-recode(tk1$answer_28, "-1=NA")
tk1$issue19<-recode(tk1$answer_29, "-1=NA")
tk1$issue20<-recode(tk1$answer_30, "-1=NA")
tk1$issue21<-recode(tk1$answer_31, "-1=NA")
tk1$issue22<-recode(tk1$answer_33, "-1=NA")
tk1$issue23<-recode(tk1$answer_34, "-1=NA")
tk1$issue24<-recode(tk1$answer_35, "-1=NA")
tk1$issue25<-recode(tk1$answer_36, "-1=NA")
tk1$issue26<-recode(tk1$answer_38, "-1=NA")
tk1$issue27<-recode(tk1$answer_39, "-1=NA")
tk1$issue28<-recode(tk1$answer_42, "-1=NA")
tk1$issue29<-recode(tk1$answer_43, "-1=NA")
tk1$issue30<-recode(tk1$answer_46, "-1=NA")


###subset the data to the issue positions and party affiliation variables only
which(colnames(tk1)=="vote2010")
tk2<-tk1[, c(183,185:214), with = FALSE]
tk2$vote2012<-as.factor(tk2$vote2010) #make it a factor
tk2<-tk2[, c("vote2012", "issue17","issue7","issue20","issue27","issue4", "issue19")]

### this block below aggregates the data at the party level
party.means<-ddply(tk2,.(vote2012) ,numcolwise(mean.na)) #calculate party-issue level means
party.medians<-ddply(tk2,.(vote2012) ,numcolwise(median.na))#calculate party-issue level medians
party.sds<-ddply(tk2,.(vote2012) ,numcolwise(sd.na)) #calculate party-issue level standard deviations

party.nas<-    ddply(tk2,.(vote2012) , numcolwise(.fun = function(x) sum(is.na(x)))) #calculate party-issue level NAs
party.neutral<-ddply(tk2,.(vote2012) , numcolwise(.fun = function(x) sum.na(x==3))) # calculate party-issue level neutral responses
party.minus2<-ddply(tk2,.(vote2012) , numcolwise(.fun = function(x) sum.na(x==1))) # calculate party-issue level very neg responses
party.minus1<-ddply(tk2,.(vote2012) , numcolwise(.fun = function(x) sum.na(x==2))) # calculate party-issue level neg responses
party.plus1<-ddply(tk2,.(vote2012) , numcolwise(.fun = function(x) sum.na(x==4))) # calculate party-issue level pos responses
party.plus2<-ddply(tk2,.(vote2012) , numcolwise(.fun = function(x) sum.na(x==5))) # calculate party-issue level very pos responses
party.all<-    ddply(tk2,.(vote2012) , numcolwise (.fun = function(x) sum.na(is.na(x)==F))) #calculate party-issue level non NA responses

party.support1<-(party.plus1+party.plus2)/party.all #support from all an answer
party.support2<-(party.plus1+party.plus2)/(party.all-party.neutral) #support from all with opinion
party.support3<-(party.plus1+party.plus2)/(party.all+party.nas) #support from all, including NAs

party.support1$vote2012<-party.all$vote2012
party.support2$vote2012<-party.all$vote2012
party.support3$vote2012<-party.all$vote2012

party.me1<-party.means #calculate margins of error; start with a copied table just to get the labels
party.me2<-party.means #calculate margins of error; start with a copied table just to get the labels
party.me3<-party.means #calculate margins of error; start with a copied table just to get the labels
party.me1.50<-party.means #calculate margins of error; start with a copied table just to get the labels
party.me2.50<-party.means #calculate margins of error; start with a copied table just to get the labels
party.me3.50<-party.means #calculate margins of error; start with a copied table just to get the labels

party.me1[,-1]<-qnorm(.975)*(sqrt(party.support1[,-1]*(1-party.support1[,-1])/(party.all[,-1])))
party.me2[,-1]<-qnorm(.975)*(sqrt(party.support2[,-1]*(1-party.support2[,-1])/(party.all[,-1]-party.neutral[,-1])))
party.me3[,-1]<-qnorm(.975)*(sqrt(party.support3[,-1]*(1-party.support3[,-1])/(party.all[,-1]+party.nas[,-1])))
party.me1.50[,-1]<-qnorm(.75)*(sqrt(party.support1[,-1]*(1-party.support1[,-1])/(party.all[,-1])))
party.me2.50[,-1]<-qnorm(.75)*(sqrt(party.support2[,-1]*(1-party.support2[,-1])/(party.all[,-1]-party.neutral[,-1])))
party.me3.50[,-1]<-qnorm(.75)*(sqrt(party.support3[,-1]*(1-party.support3[,-1])/(party.all[,-1]+party.nas[,-1])))


m1<-melt(party.support1, "vote2012") #melt the means matrix
m1$index<-paste(m1$vote2012, m1$variable, sep=".") #create an index variable
colnames(m1)<-c("party","issue","support1","index") #rename the columns

m2<-melt(party.support2, "vote2012") #melt the means matrix
m2$index<-paste(m2$vote2012, m2$variable, sep=".") #create an index variable
colnames(m2)<-c("party","issue","support2","index") #rename the columns

m3<-melt(party.support3, "vote2012") #melt the means matrix
m3$index<-paste(m3$vote2012, m3$variable, sep=".") #create an index variable
colnames(m3)<-c("party","issue","support3","index") #rename the columns

m4<-melt(party.me1, "vote2012") #melt the means matrix
m4$index<-paste(m4$vote2012, m4$variable, sep=".") #create an index variable
colnames(m4)<-c("party","issue","me1","index") #rename the columns

m5<-melt(party.me2, "vote2012") #melt the means matrix
m5$index<-paste(m5$vote2012, m5$variable, sep=".") #create an index variable
colnames(m5)<-c("party","issue","me2","index") #rename the columns

m6<-melt(party.me3, "vote2012") #melt the means matrix
m6$index<-paste(m6$vote2012, m6$variable, sep=".") #create an index variable
colnames(m6)<-c("party","issue","me3","index") #rename the columns

m7<-melt(party.me1.50, "vote2012") #melt the means matrix
m7$index<-paste(m7$vote2012, m7$variable, sep=".") #create an index variable
colnames(m7)<-c("party","issue","me1.50","index") #rename the columns

m8<-melt(party.me2.50, "vote2012") #melt the means matrix
m8$index<-paste(m8$vote2012, m8$variable, sep=".") #create an index variable
colnames(m8)<-c("party","issue","me2.50","index") #rename the columns

m9<-melt(party.me3.50, "vote2012") #melt the means matrix
m9$index<-paste(m9$vote2012, m9$variable, sep=".") #create an index variable
colnames(m9)<-c("party","issue","me3.50","index") #rename the columns


m<-merge(m1,m2[,3:4], by="index")
m<-merge(m,m3[,3:4], by="index")
m<-merge(m,m4[,3:4], by="index")
m<-merge(m,m5[,3:4], by="index")
m<-merge(m,m6[,3:4], by="index")
m<-merge(m,m7[,3:4], by="index")
m<-merge(m,m8[,3:4], by="index")
m<-merge(m,m9[,3:4], by="index")

m<-merge(m, vaa.nl, by="index")

# NL analysis -------------------------------------------------------------
cor(m$support2, m$mrppred)
cor(m$support3, m$mrppreddk)
cor(m$mrppred, m$disaggregation)

pdf('corr_nl.pdf', width=12,height=9)
png('corr_nl.png', width=1200, height=900, res=100)
par(mfrow=(c(1,2)))
par(mar=c(3,3,3,1))
plot(m$support2, m$mrppred, col="red", pch=16, ylim=c(0,1), xlim=c(0,1), xlab="VAA estimates", ylab="MRP-ed survey estimates",
     main="Share of supporters from all with opinion")
text(m$index.name, x=jitter(m$support2), y=jitter(m$mrppred)-0.02, cex=0.7, col="grey")
text(paste0("Correlation=",round(cor(m$support2, m$mrppred),2)), x=0.8, y= 0.01)
lines(abline(0,1))
lines(abline(lm(mrppred~support2, data=m), col="red"))

plot(m$support3, m$mrppreddk, col="red", pch=16, ylim=c(0,1), xlim=c(0,1), xlab="VAA estimates", ylab="MRP-ed survey estimates",
     main="Share of supporters from all")
text(m$index.name, x=jitter(m$support3), y=jitter(m$mrppreddk)-0.02, cex=0.7, col="grey")
text(paste0("Correlation=",round(cor(m$support3, m$mrppreddk),2)), x=0.8, y= 0.01)
lines(abline(0,1))
lines(abline(lm(mrppreddk~support3, data=m), col="red"))
dev.off()

pdf('dotplot_nl.pdf', width=12,height=15)
png('dotplot_nl.png', width=1200, height=1500, res=100)
par(mfrow=(c(1,1)))
par(mar=c(3,6,1,1))
m<-m[order(m$support2),]
par(mar=c(3,10,1,1))
plot(x=rep(-1,240), y=1:240, xlim=c(0,1), xaxt="n", yaxt="n", pch=16, col="red", xlab="Estimates of policy positions of party supporters (share support)", ylab="")
points(x=m$support2, y=seq(2,240,4), col="red", pch=16, cex=0.75)
points(x=m$mrppred, y=seq(3,240,4), col="blue", pch=16, cex=0.75)
segments(x0=(m$support2-m$me1), x1=(m$support2+m$me1),y0=seq(2,240,4), col="red" )
segments(x0=(m$mrppredmin50), x1=(m$mrppredplus50),   y0=seq(3,240,4), col="blue" )
axis(1,at=seq(0, 1, 0.25), labels=seq(0, 1, 0.25))
axis(2,at=seq(2.5,240, 4), labels=m$index.name, cex.axis=.7, col.axis="darkgrey", las=1)
segments(x0=rep(0,60), x1=rep(1,60), y0=seq(0.3,241,4) , col="grey", lwd=.5)
dev.off()


pdf('dotplot2_nl.pdf', width=12,height=15)
png('dotplot2_nl.png', width=1200, height=1500, res=100)
par(mfrow=(c(1,1)))
par(mar=c(3,6,1,1))
m<-m[order(m$support3),]
par(mar=c(3,10,1,1))
plot(x=rep(-1,240), y=1:240, xlim=c(0,1), xaxt="n", yaxt="n", pch=16, col="red", xlab="Estimates of policy positions of party supporters (share support)", ylab="")
points(x=m$support3, y=seq(2,240,4), col="red", pch=16, cex=0.75)
points(x=m$mrppreddk, y=seq(3,240,4), col="blue", pch=16, cex=0.75)
segments(x0=(m$support3-m$me3), x1=(m$support3+m$me3),y0=seq(2,240,4), col="red" )
segments(x0=(m$mrppreddkmin50), x1=(m$mrppreddkplus50),   y0=seq(3,240,4), col="blue" )
axis(1,at=seq(0, 1, 0.25), labels=seq(0, 1, 0.25))
axis(2,at=seq(2.5,240, 4), labels=m$index.name, cex.axis=.7, col.axis="darkgrey", las=1)
segments(x0=rep(0,60), x1=rep(1,60), y0=seq(0.3,241,4) , col="grey", lwd=.5)
dev.off()


pdf('dotplot_party_nl.pdf', width=12,height=9)
png('dotplot_party_nl.png', width=1200, height=900, res=100)
par(mfrow=c(2,3))
u<-unique(m$issue)
for(i in 1:length(u)){
m.sub<-subset(m, m$issue==u[i])
m.sub<-m.sub[order(m.sub$support2),]
par(mar=c(3,5,3,1))
plot(x=rep(-1,40), y=1:40, xlim=c(0,1), xaxt="n", yaxt="n", pch=16, col="red", ylab="", main=m.sub$vaa_issue_name[1])
points(x=m.sub$support2, y=seq(2,40,4), col="red", pch=16, cex=1)
points(x=m.sub$mrppred, y=seq(3,40,4), col="blue", pch=16, cex=1)
segments(x0=(m.sub$support2-m.sub$me1), x1=(m.sub$support2+m.sub$me1),y0=seq(2,40,4), col="red" )
segments(x0=(m.sub$mrppredmin50), x1=(m.sub$mrppredplus50),   y0=seq(3,40,4), col="blue" )
axis(1,at=seq(0, 1, 0.25), labels=seq(0, 1, 0.25))
axis(2,at=seq(2.5,40, 4), labels=m.sub$party, cex.axis=.9, col.axis="black", las=1)
segments(x0=rep(0,40), x1=rep(1,40), y0=seq(0.3,41,4) , col="grey", lwd=.5)
}
dev.off()

pdf('dotplot2_party_nl.pdf', width=12,height=9)
png('dotplot2_party_nl.png', width=1200, height=900, res=100)
par(mfrow=c(2,3))
u<-unique(m$issue)
for(i in 1:length(u)){
  m.sub<-subset(m, m$issue==u[i])
  m.sub<-m.sub[order(m.sub$support3),]
  par(mar=c(3,5,3,1))
  plot(x=rep(-1,40), y=1:40, xlim=c(0,1), xaxt="n", yaxt="n", pch=16, col="red", ylab="", main=m.sub$vaa_issue_name[1])
  points(x=m.sub$support3, y=seq(2,40,4), col="red", pch=16, cex=1)
  points(x=m.sub$mrppreddk, y=seq(3,40,4), col="blue", pch=16, cex=1)
  segments(x0=(m.sub$support3-m.sub$me3), x1=(m.sub$support3+m.sub$me3),y0=seq(2,40,4), col="red" )
  segments(x0=(m.sub$mrppreddkmin50), x1=(m.sub$mrppreddkplus50),   y0=seq(3,40,4), col="blue" )
  axis(1,at=seq(0, 1, 0.25), labels=seq(0, 1, 0.25))
  axis(2,at=seq(2.5,40, 4), labels=m.sub$party, cex.axis=.9, col.axis="black", las=1)
  segments(x0=rep(0,40), x1=rep(1,40), y0=seq(0.3,41,4) , col="grey", lwd=.5)
}
dev.off()


pdf('dotplot_issue_nl.pdf', width=12,height=9)
png('dotplot_issue_nl.png', width=1200, height=900, res=100)
par(mfrow=c(2,5))
u<-unique(m$party)
for(i in 1:length(u)){
  m.sub<-subset(m, m$party==u[i])
  m.sub<-m.sub[order(m.sub$support2),]
  par(mar=c(3,6,3,1))
  plot(x=rep(-1,24), y=1:24, xlim=c(0,1), xaxt="n", yaxt="n", pch=16, col="red", ylab="", main=m.sub$party[1])
  points(x=m.sub$support2, y=seq(2,24,4), col="red", pch=16, cex=1)
  points(x=m.sub$mrppred, y=seq(3,24,4), col="blue", pch=16, cex=1)
  segments(x0=(m.sub$support2-m.sub$me1), x1=(m.sub$support2+m.sub$me1),y0=seq(2,24,4), col="red" )
  segments(x0=(m.sub$mrppredmin50), x1=(m.sub$mrppredplus50),   y0=seq(3,24,4), col="blue" )
  axis(1,at=seq(0, 1, 0.25), labels=seq(0, 1, 0.25))
  axis(2,at=seq(2.5,24, 4), labels=m.sub$vaa_issue_name, cex.axis=.9, col.axis="black", las=1)
  segments(x0=rep(0,24), x1=rep(1,24), y0=seq(0.3,25,4) , col="grey", lwd=.5)
}
dev.off()


pdf('dotplot2_issue_nl.pdf', width=12,height=9)
png('dotplot2_issue_nl.png', width=1200, height=900, res=100)
par(mfrow=c(2,5))
u<-unique(m$party)
for(i in 1:length(u)){
  m.sub<-subset(m, m$party==u[i])
  m.sub<-m.sub[order(m.sub$support3),]
  par(mar=c(3,6,3,1))
  plot(x=rep(-1,24), y=1:24, xlim=c(0,1), xaxt="n", yaxt="n", pch=16, col="red", ylab="", main=m.sub$party[1])
  points(x=m.sub$support3, y=seq(2,24,4), col="red", pch=16, cex=1)
  points(x=m.sub$mrppreddk, y=seq(3,24,4), col="blue", pch=16, cex=1)
  segments(x0=(m.sub$support3-m.sub$me3), x1=(m.sub$support3+m.sub$me3),y0=seq(2,24,4), col="red" )
  segments(x0=(m.sub$mrppreddkmin50), x1=(m.sub$mrppreddkplus50),   y0=seq(3,24,4), col="blue" )
  axis(1,at=seq(0, 1, 0.25), labels=seq(0, 1, 0.25))
  axis(2,at=seq(2.5,24, 4), labels=m.sub$vaa_issue_name, cex.axis=.9, col.axis="black", las=1)
  segments(x0=rep(0,24), x1=rep(1,24), y0=seq(0.3,25,4) , col="grey", lwd=.5)
}
dev.off()

m$diff<-m$mrppred-m$support2
m$diff.r<-round(m$mrppred-m$support2,2)
m<-m[order(m$diff.r),]
m[,c("index.name","diff.r")]

party.diff<-aggregate(abs(m$diff.r), by=list(m$party), FUN=sum) #total error per party
party.diff.mean<-aggregate(m$diff.r, by=list(m$party), FUN=mean) #mean of the  errors per party; bias
party.n<-aggregate(abs(m$N), by=list(m$party), FUN=mean) #part size
cor(party.diff$x, party.n$x)

cbind(party.diff, round(party.diff.mean$x,2))

issue.diff<-aggregate(abs(m$diff.r), by=list(m$vaa_issue_name), FUN=sum) #total error per issue
issue.diff.mean<-aggregate(m$diff.r, by=list(m$vaa_issue_name), FUN=mean) #mean of the  errors per issue; bias
cbind(issue.diff, round(issue.diff.mean$x,2))

# Load and arrange the German VAA data ---------------------------------------------------------
de <- fread("de2013election.csv")
de<-subset(de, de$duration>100 & de$duration<(60*30)) #subset only those who completed the survey in reasonable times 
de<-subset(de, de$country_code=="DE" & de$language_id==1)
hist(de$duration, nclass=100000)

###Recode the past vote variables to the party names
#2009 parliamentary vote
de$vote2009<-recode(de$extra_question_8, "134='CDU/CSU';
                    135='SPD';136='FDP';
                    137='Die Linke';138='Die Gruenen';139='Piraten';
                    140='NPD';
                    else=NA")
#last Lander vote
de$votefed<-recode(de$extra_question_9, "144='CDU/CSU';
                   145='SPD';146='FDP';
                   147='Die Linke';148='Die Gruene';149='Piraten';
                   150='NPD';
                   else=NA")

#based on ever voting for party (preliminary version)
de$votever0<-ifelse(de$party_form_parties_3_o8>8,'NPD',
                    ifelse(de$party_form_parties_3_o6>8,'Piraten',
                           ifelse(de$party_form_parties_3_o5>8,'Die Gruenen',
                                  ifelse(de$party_form_parties_3_o4>8,'Die Linke',
                                         ifelse(de$party_form_parties_3_o3>8,'FDP',
                                                ifelse(de$party_form_parties_3_o2>8,'SPD',
                                                       ifelse(de$party_form_parties_3_o1>8,'CDU/CSU',NA)))))))


#based on ever voting for party dummies
de$npd<-ifelse(de$party_form_parties_3_o8>8,1,0)
de$piraten<-ifelse(de$party_form_parties_3_o6>8,1,0)
de$grunen<-ifelse(de$party_form_parties_3_o5>8,1,0)
de$linke<-ifelse(de$party_form_parties_3_o4>8,1,0)
de$fdp<-ifelse(de$party_form_parties_3_o3>8,1,0)
de$spd<-ifelse(de$party_form_parties_3_o2>8,1,0)
de$cdu<-ifelse(de$party_form_parties_3_o1>8,1,0)

de$prom<-de$npd+de$piraten+de$grunen+de$linke+de$fdp+de$spd+de$cdu #total number of parties supported
#filter only those who would ever vote for one party only
de$votever<-ifelse(de$prom==1, de$votever0,NA)

#define the party supporter in various ways
de$supporter3<-ifelse(de$vote2009==de$votefed & de$vote2009==de$votever, 1, 0) #super strong supporters only
de$supporter2a<-ifelse(de$vote2009==de$votefed,1,0) #strong supporters only
de$supporter2b<-ifelse(de$vote2009==de$votever,1,0) #strong supporters only
de$supporter2c<-ifelse(de$votever==de$votefed,1,0) #strong supporters only

de$supporter1a<-ifelse(is.na(de$vote2009)==F, 1, 0) #create a variable for weaker supporters
de$supporter1b<-ifelse(is.na(de$votefed)==F, 1, 0) #create a variable for weaker supporters
de$supporter1c<-ifelse(is.na(de$votever)==F, 1, 0) #create a variable for weaker supporters

##create new variables for the issues with the proper numbers and recode -1s to NAs
de$issue1<-recode(de$answer_10, "-1=NA")
de$issue2<-recode(de$answer_6, "-1=NA")
de$issue3<-recode(de$answer_3, "-1=NA")
de$issue4<-recode(de$answer_12, "-1=NA")
de$issue5<-recode(de$answer_5, "-1=NA")
de$issue6<-recode(de$answer_13, "-1=NA")
de$issue7<-recode(de$answer_7, "-1=NA")
de$issue8<-recode(de$answer_14, "-1=NA")
de$issue9<-recode(de$answer_31, "-1=NA")
de$issue10<-recode(de$answer_1, "-1=NA")
de$issue11<-recode(de$answer_11, "-1=NA")
de$issue12<-recode(de$answer_22, "-1=NA")
de$issue13<-recode(de$answer_28, "-1=NA")
de$issue14<-recode(de$answer_8, "-1=NA")
de$issue15<-recode(de$answer_15, "-1=NA")
de$issue16<-recode(de$answer_20, "-1=NA")
de$issue17<-recode(de$answer_17, "-1=NA")
de$issue18<-recode(de$answer_32, "-1=NA")
de$issue19<-recode(de$answer_19, "-1=NA")
de$issue20<-recode(de$answer_16, "-1=NA")
de$issue21<-recode(de$answer_21, "-1=NA")
de$issue22<-recode(de$answer_4, "-1=NA")
de$issue23<-recode(de$answer_24, "-1=NA")
de$issue24<-recode(de$answer_37, "-1=NA")
de$issue25<-recode(de$answer_26, "-1=NA")
de$issue26<-recode(de$answer_27, "-1=NA")
de$issue27<-recode(de$answer_2, "-1=NA")
de$issue28<-recode(de$answer_29, "-1=NA")
de$issue29<-recode(de$answer_36, "-1=NA")
de$issue30<-recode(de$answer_18, "-1=NA")
###subset the data to the issue positions and party affiliation variables only
de1<-subset(de, de$supporter1a==1) #filter the dataset by type of supporters
which(colnames(de1)=="issue1") #check which columns to take
de2<-de1[, c(146:175), with = FALSE]
de2$vote<-as.factor(de1$vote2009) #make it a factor
de2<-de2[,c("vote","issue13", "issue10", "issue18", "issue30", "issue1")]

partyde.means<-ddply(de2,.(vote) ,numcolwise(mean.na)) #calculate party-issue level means
partyde.medians<-ddply(de2,.(vote) ,numcolwise(median.na))#calculate party-issue level medians
partyde.sds<-ddply(de2,.(vote) ,numcolwise(sd.na)) #calculate party-issue level standard deviations

partyde.nas<-    ddply(de2,.(vote) , numcolwise(.fun = function(x) sum(is.na(x)))) #calculate party-issue level NAs
partyde.neutral<-ddply(de2,.(vote) , numcolwise(.fun = function(x) sum.na(x==3))) # calculate party-issue level neutral responses
partyde.minus2<-ddply(de2,.(vote) , numcolwise(.fun = function(x) sum.na(x==1))) # calculate party-issue level very neg responses
partyde.minus1<-ddply(de2,.(vote) , numcolwise(.fun = function(x) sum.na(x==2))) # calculate party-issue level neg responses
partyde.plus1<-ddply(de2,.(vote) , numcolwise(.fun = function(x) sum.na(x==4))) # calculate party-issue level pos responses
partyde.plus2<-ddply(de2,.(vote) , numcolwise(.fun = function(x) sum.na(x==5))) # calculate party-issue level very pos responses
partyde.all<-    ddply(de2,.(vote) , numcolwise (.fun = function(x) sum.na(is.na(x)==F))) #calculate party-issue level non NA responses

partyde.support1<-(partyde.plus1+partyde.plus2)/partyde.all #support from all an answer
partyde.support2<-(partyde.plus1+partyde.plus2)/(partyde.all-partyde.neutral) #support from all with opinion
partyde.support3<-(partyde.plus1+partyde.plus2)/(partyde.all+partyde.nas) #support from all, including NAs
partyde.support1$vote<-partyde.all$vote
partyde.support2$vote<-partyde.all$vote
partyde.support3$vote<-partyde.all$vote

partyde.me1<-partyde.means #calculate margins of error; start with a copied table just to get the labels
partyde.me2<-partyde.means #calculate margins of error; start with a copied table just to get the labels
partyde.me3<-partyde.means #calculate margins of error; start with a copied table just to get the labels
partyde.me1.50<-partyde.means #calculate margins of error; start with a copied table just to get the labels
partyde.me2.50<-partyde.means #calculate margins of error; start with a copied table just to get the labels
partyde.me3.50<-partyde.means #calculate margins of error; start with a copied table just to get the labels

partyde.me1[,-1]<-qnorm(.975)*(sqrt(partyde.support1[,-1]*(1-partyde.support1[,-1])/(partyde.all[,-1])))
partyde.me2[,-1]<-qnorm(.975)*(sqrt(partyde.support2[,-1]*(1-partyde.support2[,-1])/(partyde.all[,-1]-partyde.neutral[,-1])))
partyde.me3[,-1]<-qnorm(.975)*(sqrt(partyde.support3[,-1]*(1-partyde.support3[,-1])/(partyde.all[,-1]+partyde.nas[,-1])))
partyde.me1.50[,-1]<-qnorm(.75)*(sqrt(partyde.support1[,-1]*(1-partyde.support1[,-1])/(partyde.all[,-1])))
partyde.me2.50[,-1]<-qnorm(.75)*(sqrt(partyde.support2[,-1]*(1-partyde.support2[,-1])/(partyde.all[,-1]-partyde.neutral[,-1])))
partyde.me3.50[,-1]<-qnorm(.75)*(sqrt(partyde.support3[,-1]*(1-partyde.support3[,-1])/(partyde.all[,-1]+partyde.nas[,-1])))

dm1<-melt(partyde.support1, "vote") #melt the means matrix
dm1$index<-paste(dm1$vote, dm1$variable, sep=".") #create an index variable
colnames(dm1)<-c("party","issue","support1","index") #rename the columns

dm2<-melt(partyde.support2, "vote") #melt the means matrix
dm2$index<-paste(dm2$vote, dm2$variable, sep=".") #create an index variable
colnames(dm2)<-c("party","issue","support2","index") #rename the columns

dm3<-melt(partyde.support3, "vote") #melt the means matrix
dm3$index<-paste(dm3$vote, dm3$variable, sep=".") #create an index variable
colnames(dm3)<-c("party","issue","support3","index") #rename the columns

dm4<-melt(partyde.me1, "vote") #melt the means matrix
dm4$index<-paste(dm4$vote, dm4$variable, sep=".") #create an index variable
colnames(dm4)<-c("party","issue","me1","index") #rename the columns

dm5<-melt(partyde.me2, "vote") #melt the means matrix
dm5$index<-paste(dm5$vote, dm5$variable, sep=".") #create an index variable
colnames(dm5)<-c("party","issue","me2","index") #rename the columns

dm6<-melt(partyde.me3, "vote") #melt the means matrix
dm6$index<-paste(dm6$vote, dm6$variable, sep=".") #create an index variable
colnames(dm6)<-c("party","issue","me3","index") #rename the columns

dm7<-melt(partyde.me1.50, "vote") #melt the means matrix
dm7$index<-paste(dm7$vote, dm7$variable, sep=".") #create an index variable
colnames(dm7)<-c("party","issue","me1.50","index") #rename the columns

dm8<-melt(partyde.me2.50, "vote") #melt the means matrix
dm8$index<-paste(dm8$vote, dm8$variable, sep=".") #create an index variable
colnames(dm8)<-c("party","issue","me2.50","index") #rename the columns

dm9<-melt(partyde.me3.50, "vote") #melt the means matrix
dm9$index<-paste(dm9$vote, dm9$variable, sep=".") #create an index variable
colnames(dm9)<-c("party","issue","me3.50","index") #rename the columns

dm<-merge(dm1,dm2[,3:4], by="index")
dm<-merge(dm,dm3[,3:4], by="index")
dm<-merge(dm,dm4[,3:4], by="index")
dm<-merge(dm,dm5[,3:4], by="index")
dm<-merge(dm,dm6[,3:4], by="index")
dm<-merge(dm,dm7[,3:4], by="index")
dm<-merge(dm,dm8[,3:4], by="index")
dm<-merge(dm,dm9[,3:4], by="index")

vaa.de$mrppred[vaa.de$policyname=='equal adoption same sex couples']<-1-vaa.de$mrppred[vaa.de$policyname=='equal adoption same sex couples']
vaa.de$mrppreddk[vaa.de$policyname=='equal adoption same sex couples']<-1-vaa.de$mrppreddk[vaa.de$policyname=='equal adoption same sex couples']
vaa.de$mrppredplus50[vaa.de$policyname=='equal adoption same sex couples']<-1-vaa.de$mrppredplus50[vaa.de$policyname=='equal adoption same sex couples']
vaa.de$mrppredmin50[vaa.de$policyname=='equal adoption same sex couples']<-1-vaa.de$mrppredmin50[vaa.de$policyname=='equal adoption same sex couples']
vaa.de$mrppreddkplus50[vaa.de$policyname=='equal adoption same sex couples']<-1-vaa.de$mrppreddkplus50[vaa.de$policyname=='equal adoption same sex couples']
vaa.de$mrppreddkmin50[vaa.de$policyname=='equal adoption same sex couples']<-1-vaa.de$mrppreddkmin50[vaa.de$policyname=='equal adoption same sex couples']

dm<-merge(dm, vaa.de, by="index")

# DE analysis -------------------------------------------------------------
cor(dm$support2, dm$mrppred)
cor(dm$support3, dm$mrppreddk)
cor(dm$mrppred, dm$disaggregation)

pdf('corr_de.pdf', width=12,height=9)
png('corr_de.png', width=1200, height=900, res=100)
par(mfrow=(c(1,2)))
par(mar=c(3,3,3,1))
plot(dm$support2, dm$mrppred, col="red", pch=16, ylim=c(0,1), xlim=c(0,1), xlab="VAA estimates", ylab="MRP-ed survey estimates",
     main="Share of supporters from all with opinion")
text(dm$index.name, x=jitter(dm$support2), y=jitter(dm$mrppred)-0.02, cex=0.7, col="grey")
text(paste0("Correlation=",round(cor(dm$support2, dm$mrppred),2)), x=0.8, y= 0.01)
lines(abline(0,1))
lines(abline(lm(mrppred~support2, data=dm), col="red"))

plot(dm$support3, dm$mrppreddk, col="red", pch=16, ylim=c(0,1), xlim=c(0,1), xlab="VAA estimates", ylab="MRP-ed survey estimates",
     main="Share of supporters from all")
text(dm$index.name, x=jitter(dm$support3), y=jitter(dm$mrppreddk)-0.02, cex=0.7, col="grey")
text(paste0("Correlation=",round(cor(dm$support3, dm$mrppreddk),2)), x=0.8, y= 0.01)
lines(abline(0,1))
lines(abline(lm(mrppreddk~support3, data=dm), col="red"))
dev.off()

pdf('dotplot_de.pdf', width=12,height=15)
png('dotplot_de.png', width=1200, height=1500, res=100)
par(mfrow=(c(1,1)))
par(mar=c(3,6,1,1))
dm<-dm[order(dm$support2),]
par(mar=c(3,10,1,1))
plot(x=rep(-1,100), y=1:100, xlim=c(0,1), xaxt="n", yaxt="n", pch=16, col="red", xlab="Estimates of policy positions of party supporters (share support)", ylab="")
points(x=dm$support2, y=seq(2,100,4), col="red", pch=16, cex=0.75)
points(x=dm$mrppred, y=seq(3,100,4), col="blue", pch=16, cex=0.75)
segments(x0=(dm$support2-dm$me2), x1=(dm$support2+dm$me2),y0=seq(2,100,4), col="red" )
segments(x0=(dm$mrppredmin50), x1=(dm$mrppredplus50),   y0=seq(3,100,4), col="blue" )
axis(1,at=seq(0, 1, 0.25), labels=seq(0, 1, 0.25))
axis(2,at=seq(2.5,100, 4), labels=dm$index.name, cex.axis=.7, col.axis="darkgrey", las=1)
segments(x0=rep(0,60), x1=rep(1,60), y0=seq(0.3,101,4) , col="grey", lwd=.5)
dev.off()

pdf('dotplot2_de.pdf', width=12,height=15)
png('dotplot2_de.png', width=1200, height=1500, res=100)
par(mfrow=(c(1,1)))
par(mar=c(3,6,1,1))
dm<-dm[order(dm$support3),]
par(mar=c(3,10,1,1))
plot(x=rep(-1,100), y=1:100, xlim=c(0,1), xaxt="n", yaxt="n", pch=16, col="red", xlab="Estimates of policy positions of party supporters (share support)", ylab="")
points(x=dm$support3, y=seq(2,100,4), col="red", pch=16, cex=0.75)
points(x=dm$mrppreddk, y=seq(3,100,4), col="blue", pch=16, cex=0.75)
segments(x0=(dm$support3-dm$me3), x1=(dm$support3+dm$me3),y0=seq(2,100,4), col="red" )
segments(x0=(dm$mrppreddkmin50), x1=(dm$mrppreddkplus50),   y0=seq(3,100,4), col="blue" )
axis(1,at=seq(0, 1, 0.25), labels=seq(0, 1, 0.25))
axis(2,at=seq(2.5,100, 4), labels=dm$index.name, cex.axis=.7, col.axis="darkgrey", las=1)
segments(x0=rep(0,60), x1=rep(1,60), y0=seq(0.3,101,4) , col="grey", lwd=.5)
dev.off()

pdf('dotplot_party_issue_de.pdf', width=12,height=9)
png('dotplot_party_issue_de.png', width=1200, height=900, res=100)
par(mfrow=c(2,5))
u<-unique(dm$issue)
for(i in 1:length(u)){
  dm.sub<-subset(dm, dm$issue==u[i])
  dm.sub<-dm.sub[order(dm.sub$support2),]
  par(mar=c(3,5,3,1))
  plot(x=rep(-1,20), y=1:20, xlim=c(0,1), xaxt="n", yaxt="n", pch=16, col="red", ylab="", main=dm.sub$vaa_issue_name[1])
  points(x=dm.sub$support2, y=seq(2,20,4), col="red", pch=16, cex=1)
  points(x=dm.sub$mrppred, y=seq(3,20,4), col="blue", pch=16, cex=1)
  segments(x0=(dm.sub$support2-dm.sub$me2), x1=(dm.sub$support2+dm.sub$me2),y0=seq(2,20,4), col="red" )
  segments(x0=(dm.sub$mrppredmin50), x1=(dm.sub$mrppredplus50),   y0=seq(3,20,4), col="blue" )
  axis(1,at=seq(0, 1, 0.25), labels=seq(0, 1, 0.25))
  axis(2,at=seq(2.5,20, 4), labels=dm.sub$party, cex.axis=.9, col.axis="black", las=1)
  segments(x0=rep(0,20), x1=rep(1,20), y0=seq(0.3,21,4) , col="grey", lwd=.5)
}

u<-unique(dm$party)
for(i in 1:length(u)){
  dm.sub<-subset(dm, dm$party==u[i])
  dm.sub<-dm.sub[order(dm.sub$support2),]
  par(mar=c(3,6,3,1))
  plot(x=rep(-1,20), y=1:20, xlim=c(0,1), xaxt="n", yaxt="n", pch=16, col="red", ylab="", main=dm.sub$party[1])
  points(x=dm.sub$support2, y=seq(2,20,4), col="red", pch=16, cex=1)
  points(x=dm.sub$mrppred, y=seq(3,20,4), col="blue", pch=16, cex=1)
  segments(x0=(dm.sub$support2-dm.sub$me2), x1=(dm.sub$support2+dm.sub$me2),y0=seq(2,20,4), col="red" )
  segments(x0=(dm.sub$mrppredmin50), x1=(dm.sub$mrppredplus50),   y0=seq(3,20,4), col="blue" )
  axis(1,at=seq(0, 1, 0.25), labels=seq(0, 1, 0.25))
  axis(2,at=seq(2.5,20, 4), labels=dm.sub$vaa_issue_name, cex.axis=.9, col.axis="black", las=1)
  segments(x0=rep(0,20), x1=rep(1,24), y0=seq(0.3,21,4) , col="grey", lwd=.5)
}
dev.off()


pdf('dotplot_party_issue_de2.pdf', width=12,height=9)
png('dotplot_party_issue_de2.png', width=1200, height=900, res=100)
par(mfrow=c(2,5))
u<-unique(dm$issue)
for(i in 1:length(u)){
  dm.sub<-subset(dm, dm$issue==u[i])
  dm.sub<-dm.sub[order(dm.sub$support3),]
  par(mar=c(3,5,3,1))
  plot(x=rep(-1,20), y=1:20, xlim=c(0,1), xaxt="n", yaxt="n", pch=16, col="red", ylab="", main=dm.sub$vaa_issue_name[1])
  points(x=dm.sub$support3, y=seq(2,20,4), col="red", pch=16, cex=1)
  points(x=dm.sub$mrppreddk, y=seq(3,20,4), col="blue", pch=16, cex=1)
  segments(x0=(dm.sub$support3-dm.sub$me3), x1=(dm.sub$support3+dm.sub$me3),y0=seq(2,20,4), col="red" )
  segments(x0=(dm.sub$mrppreddkmin50), x1=(dm.sub$mrppreddkplus50),   y0=seq(3,20,4), col="blue" )
  axis(1,at=seq(0, 1, 0.25), labels=seq(0, 1, 0.25))
  axis(2,at=seq(2.5,20, 4), labels=dm.sub$party, cex.axis=.9, col.axis="black", las=1)
  segments(x0=rep(0,20), x1=rep(1,20), y0=seq(0.3,21,4) , col="grey", lwd=.5)
}

u<-unique(dm$party)
for(i in 1:length(u)){
  dm.sub<-subset(dm, dm$party==u[i])
  dm.sub<-dm.sub[order(dm.sub$support3),]
  par(mar=c(3,6,3,1))
  plot(x=rep(-1,20), y=1:20, xlim=c(0,1), xaxt="n", yaxt="n", pch=16, col="red", ylab="", main=dm.sub$party[1])
  points(x=dm.sub$support3, y=seq(2,20,4), col="red", pch=16, cex=1)
  points(x=dm.sub$mrppreddk, y=seq(3,20,4), col="blue", pch=16, cex=1)
  segments(x0=(dm.sub$support3-dm.sub$me3), x1=(dm.sub$support3+dm.sub$me3),y0=seq(2,20,4), col="red" )
  segments(x0=(dm.sub$mrppreddkmin50), x1=(dm.sub$mrppreddkplus50),   y0=seq(3,20,4), col="blue" )
  axis(1,at=seq(0, 1, 0.25), labels=seq(0, 1, 0.25))
  axis(2,at=seq(2.5,20, 4), labels=dm.sub$vaa_issue_name, cex.axis=.9, col.axis="black", las=1)
  segments(x0=rep(0,20), x1=rep(1,24), y0=seq(0.3,21,4) , col="grey", lwd=.5)
}
dev.off()


dm$diff<-dm$mrppred-dm$support2
dm$diff.r<-round(dm$mrppred-dm$support2,2)
dm<-dm[order(dm$diff.r),]
dm[,c("index.name","diff.r")]

partyde.diff<-aggregate(abs(dm$diff.r), by=list(dm$party), FUN=sum) #total error per party
partyde.diff.mean<-aggregate(dm$diff.r, by=list(dm$party), FUN=mean) #mean of the  errors per party; bias
partyde.n<-aggregate(abs(dm$N), by=list(dm$party), FUN=mean) #part size
cor(partyde.diff$x, partyde.n$x)

cbind(partyde.diff, round(partyde.diff.mean$x,2))

issuede.diff<-aggregate(abs(dm$diff.r), by=list(dm$vaa_issue_name), FUN=sum) #total error per issue
issuede.diff.mean<-aggregate(dm$diff.r, by=list(dm$vaa_issue_name), FUN=mean) #mean of the  errors per issue; bias
cbind(issuede.diff, round(issuede.diff.mean$x,2))

