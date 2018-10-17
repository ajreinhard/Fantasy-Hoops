library(XML)

 

##############Get NF Proj

setwd('C:/Users/A097092/Desktop/Extra/Fantasy BB research/Players/')

 

dbl <- function(x) length(which(as.numeric(x) >= 10))

all_proj <- sapply(dir(), function(x) {

the_tree <- htmlTreeParse(x, useInternal=T)

pts <- as.numeric(sapply(xpathSApply(the_tree, '//table[2]/tbody/tr/td[2]/text()'),function(y) xmlValue(y, trim=T)))

reb <- as.numeric(sapply(xpathSApply(the_tree, '//table[2]/tbody/tr/td[6]/text()'),function(y) xmlValue(y, trim=T)))

ast <- as.numeric(sapply(xpathSApply(the_tree, '//table[2]/tbody/tr/td[7]/text()'),function(y) xmlValue(y, trim=T)))

stl <- as.numeric(sapply(xpathSApply(the_tree, '//table[2]/tbody/tr/td[8]/text()'),function(y) xmlValue(y, trim=T)))

blk <- as.numeric(sapply(xpathSApply(the_tree, '//table[2]/tbody/tr/td[9]/text()'),function(y) xmlValue(y, trim=T)))

tov <- as.numeric(sapply(xpathSApply(the_tree, '//table[2]/tbody/tr/td[10]/text()'),function(y) xmlValue(y, trim=T)))

dates <- sapply(xpathSApply(the_tree, '//div[1]/table[1]/tbody/tr/td[1]/text()'),function(y) xmlValue(y, trim=T))

opp <- sapply(xpathSApply(the_tree, '//div[1]/table[1]/tbody/tr/td[2]'),function(y) xmlValue(y, trim=T))

 

dbl_figs <- apply(rbind(pts,reb,ast,stl,blk),2,dbl)

 

dd <- ifelse(dbl_figs==2,5,0)

td <- ifelse(dbl_figs>=3,10,0)

 

score <- pts + reb*1.2 + ast*1.5 + stl*2 + blk*2 - tov + dd + td

 

cbind(substr(x,1,nchar(x)-4), dates, opp, score)

})

 

for (x in rev(which(sapply(all_proj,ncol)==2))) all_proj[[x]] <- NULL

#all_proj[[72]]

 

all_proj <- data.frame(do.call(rbind, all_proj),stringsAsFactors=F)

names(all_proj)[1] <- 'Player'

all_proj$score <- as.numeric(all_proj$score)

##############

 

##############Get Postions from ESPN

setwd('C:/Users/A097092/Desktop/Extra/Fantasy BB research/ESPN/')

 

full_list <- dir()[which(file.info(dir())$size>=90000)]

 

all_players <- sapply(full_list, function(x) {

the_tree <- htmlTreeParse(x, useInternal=T)

 

Team_Pos <- sapply(xpathSApply(the_tree, '//tr/td[1]/text()[1]'),function(y) xmlValue(y, trim=T))[-c(1:2)]

#owner <- sapply(xpathSApply(the_tree, '//tr/td[3]'),function(y) xmlValue(y, trim=T))[-c(1:2)]

Player <- sapply(xpathSApply(the_tree, '//tr/td[1]/a[1]'),function(y) xmlValue(y, trim=T))[-c(1)]

IR_elig <- substr(Team_Pos,1,regexpr(',',Team_Pos)-1)

mat_pos <- do.call(rbind, strsplit(substr(Team_Pos,regexpr(',',Team_Pos)+2,nchar(Team_Pos)),' '))

 

PG <- grepl('PG',strsplit(mat_pos[,2],', '))

WG <- (grepl('SG',strsplit(mat_pos[,2],', ')) | grepl('SF',strsplit(mat_pos[,2],', ')))

PF <- grepl('PF',strsplit(mat_pos[,2],', '))

C <- grepl('C',strsplit(mat_pos[,2],', '))

 

 

cbind(Player, mat_pos[,1],PG,WG,PF,C, IR_elig)

})

 

all_players <- data.frame(do.call(rbind,all_players),stringsAsFactors=F)

names(all_players)[2] <- 'Team'

 

final <- merge(all_players, all_proj, all=T)

 

final[,3:6] <- sapply(3:6,function(x) ifelse(final[,x],1,0))

final$Pos <- apply(final[,3:6],1,function(x) toString(names(which(x==1))))

final <- final[,-c(3:6)]

final$PlayerDate <- paste0(final$Player, final$dates)

#final$dates <-

#as.Date(final$dates,format='M%/d%/y%')

 

#missing

#unique(final$Player[which(is.na(final$score))])

#unique(final$Player[which(is.na(final$Team))])

 

 

str(final)

 

write.csv(final, 'C:/Users/A097092/Desktop/Extra/Fantasy BB research/NF_Proj.csv')

 

 

 

 

####################################

####################################

####################################

 

library(XML)

 

owners <- c('aj','seth','caleb','scott','cory','comp','chad','naps','devon','jake')

############## Get teams

team_tree <- htmlTreeParse('C:/Users/A097092/Desktop/Extra/Fantasy BB research/Teams.txt', useInternal=T)

 

roster_page <- data.frame(matrix(c(rep(1,3),rep(2,3),rep(3,3),4,1:3,1:3,1:3,1),10,2))

rownames(roster_page) <- owners

 

rosters <- lapply(1:10, function(x) {

players <- xpathSApply(team_tree, paste0('//tr[',roster_page$X1[x],']/td[',roster_page$X2[x],']/table/tr/td/a/text()'))

cbind(rownames(roster_page)[x],sapply(players, function(x) xmlValue(x)))

})

 

roster_list <- data.frame(do.call(rbind,rosters),stringsAsFactors=F)

names(roster_list) <- c('Owner','Player')

 

##############Get NF Proj

setwd('C:/Users/A097092/Desktop/Extra/Fantasy BB research/Players/')

 

dbl <- function(x) length(which(as.numeric(x) >= 10))

all_proj <- sapply(dir(), function(x) {

the_tree <- htmlTreeParse(x, useInternal=T)

pts <- as.numeric(sapply(xpathSApply(the_tree, '//table[2]/tbody/tr/td[2]/text()'),function(y) xmlValue(y, trim=T)))

reb <- as.numeric(sapply(xpathSApply(the_tree, '//table[2]/tbody/tr/td[6]/text()'),function(y) xmlValue(y, trim=T)))

ast <- as.numeric(sapply(xpathSApply(the_tree, '//table[2]/tbody/tr/td[7]/text()'),function(y) xmlValue(y, trim=T)))

stl <- as.numeric(sapply(xpathSApply(the_tree, '//table[2]/tbody/tr/td[8]/text()'),function(y) xmlValue(y, trim=T)))

blk <- as.numeric(sapply(xpathSApply(the_tree, '//table[2]/tbody/tr/td[9]/text()'),function(y) xmlValue(y, trim=T)))

tov <- as.numeric(sapply(xpathSApply(the_tree, '//table[2]/tbody/tr/td[10]/text()'),function(y) xmlValue(y, trim=T)))

dates <- sapply(xpathSApply(the_tree, '//div[1]/table[1]/tbody/tr/td[1]/text()'),function(y) xmlValue(y, trim=T))

opp <- sapply(xpathSApply(the_tree, '//div[1]/table[1]/tbody/tr/td[2]'),function(y) xmlValue(y, trim=T))

 

dbl_figs <- apply(rbind(pts,reb,ast,stl,blk),2,dbl)

 

dd <- ifelse(dbl_figs==2,5,0)

td <- ifelse(dbl_figs>=3,10,0)

 

score <- pts + reb*1.2 + ast*1.5 + stl*2 + blk*2 - tov + dd + td

 

cbind(substr(x,1,nchar(x)-4), dates, opp, score)

})

 

all_proj <- data.frame(do.call(rbind, all_proj),stringsAsFactors=F)

names(all_proj)[1] <- 'Player'

all_proj$score <- as.numeric(all_proj$score)

##############

 

##############Get Postions from ESPN

setwd('C:/Users/A097092/Desktop/Extra/Fantasy BB research/ESPN/')

 

full_list <- dir()[which(file.info(dir())$size>=70000)]

 

all_players <- sapply(full_list, function(x) {

the_tree <- htmlTreeParse(x, useInternal=T)

 

Team_Pos <- sapply(xpathSApply(the_tree, '//tr/td[1]/text()[1]'),function(y) xmlValue(y, trim=T))[-c(1:2)]

owner <- sapply(xpathSApply(the_tree, '//tr/td[3]'),function(y) xmlValue(y, trim=T))[-c(1:2)]

Player <- sapply(xpathSApply(the_tree, '//tr/td[1]/a[1]'),function(y) xmlValue(y, trim=T))[-c(1)]

IR_elig <- substr(Team_Pos,1,regexpr(',',Team_Pos)-1)

mat_pos <- do.call(rbind, strsplit(substr(Team_Pos,regexpr(',',Team_Pos)+2,nchar(Team_Pos)),' '))

 

PG <- grepl('PG',strsplit(mat_pos[,2],', '))

WG <- (grepl('SG',strsplit(mat_pos[,2],', ')) | grepl('SF',strsplit(mat_pos[,2],', ')))

PF <- grepl('PF',strsplit(mat_pos[,2],', '))

C <- grepl('C',strsplit(mat_pos[,2],', '))

 

 

cbind(Player, owner, mat_pos[,1],PG,WG,PF,C,1, IR_elig)

})

 

all_players <- data.frame(do.call(rbind,all_players),stringsAsFactors=F)

names(all_players)[3] <- 'Team'

names(all_players)[8] <- 'U'

all_players$U <- as.numeric(all_players$U)

final <- merge(all_players, all_proj, all.y=T)

final[,4:7] <- sapply(4:7,function(x) ifelse(final[,x],1,0))

 

write.csv(final, 'NF_Proj.csv')

 

elig <- expand.grid(0:1,0:1,0:1,0:1)[-c(1,10:12,14),]

 

 

 

 

 

 

 

team <- final[which(final$owner=='RAM'),]

dates <- sort(unique(final$dates))

slots <- c('PG','WG','PF','C','U')

team_dat <- final[which(final$owner=='RAM' & final$dates==dates[6]),]

team_dat

 

 

all_slots <- expand.grid(pos=slots,dates=dates)

 

apply(team,1,function(x) x[4:8])

 

library(lpSolve)

 

ifelse(apply(team_dat[,4:8],2,sum)>

 

mod <- lp(direction = "max",

          objective.in = team_dat$score,

          const.mat = t(team_dat[,4:8]),

          const.dir = c(rep('>=',4),'<='),

          const.rhs = c(rep(0,4),5),

          all.bin = TRUE)

mod

mod$solution

team_dat[which(mod$solution==1),]

team_dat[which(mod$solution==0),]

 

 

 

 

 

 

 

 

##########

str(final)

library(reshape2)

melt1 <- melt(final[which(final$owner=='OOJW'),c('Player','dates','score','PG','WG','PF','C','U')],id=c('Player','dates','score'))

matrix1 <- cast(melt1, Player+score~variable+dates,sum)

head(matrix1)

samp <- matrix1[which(matrix1$Player %in% sample(unique(final$Player),10)),]

samp <- matrix1

samp <- cbind(samp,1)

str(samp)

 

 

mod <- lp(direction = "max",

          objective.in = samp[,2],

          const.mat = t(samp[,-c(1:2)]),

          const.dir = rep('=',length(samp)-2),

          const.rhs = c(rep(1,length(samp)-3),24),

          all.bin = TRUE)

 

 

cbind(t(samp[,-c(1:2)]),rep('<=',length(samp)-2),c(rep(1,length(samp)-3),24))

length(samp[,2])

#apply(samp[,-c(1:2)],2,sum)

 

jj<-samp[which(mod$solution==1),]

jj

 

sum(apply(data.frame(jj[,-c(1:2)]),1,sum))

 

combos <- expand.grid(dates,slots)

 

team$Player[which(team$dates==dates[1] & team[,c('PG')]==1)]

sapply(1:nrow(combos), function(x) team$Player[which(team$dates==x[1] & team[,c(x[2])]==1)]))

 

sapply(dates, function(x) matrix(0,5,10))

 

str(final)

 

 

library(lpSolve)

 

####################################

####################################

####################################

 

 

all_pos <- apply(expand.grid(0:1,0:1,0:1,0:1),1,toString)[-c(1,10:12,14)]

all_rosterlist <- t(combn(15,4))

 

elig <- expand.grid(0:1,0:1,0:1,0:1)[-c(1,10:12,14),]

 

 

pos_combos <- expand.grid(1:11,1:11,1:11,1:11)

combo_counts <- t(apply(pos_combos,1,function(x) table(factor(x,1:11))))

 

pos_combos <- pos_combos[which(!(combo_counts[,1]>=2 | combo_counts[,2]>=2 | combo_counts[,4]>=2 | combo_counts[,8]>=2 | combo_counts[,3]>=3 | combo_counts[,5]>=3 | combo_counts[,6]>=3 | combo_counts[,9]>=3)),]

 

empty_test <- elig[pos_combos[,1],]+elig[pos_combos[,2],]+elig[pos_combos[,3],]+elig[pos_combos[,4],]

 

pos_combos <- pos_combos[which(!(empty_test[,1]==0 | empty_test[,2]==0 | empty_test[,3]==0 | empty_test[,4]==0)),]

combo_counts <- t(apply(pos_combos,1,function(x) table(factor(x,1:11))))

 

length(pos_combos[,1])

 

pos_combos[1:4,]

combo_counts[1:4,]

 

 

pg_x <- rep(0,length(pos_combos[,1]))

wg_x <- rep(0,length(pos_combos[,1]))

pf_x <- rep(0,length(pos_combos[,1]))

c_x <- rep(0,length(pos_combos[,1]))

 

pg_x[which(combo_counts[,1]==1)] <- 1

wg_x[which(combo_counts[,2]==1)] <- 1

pf_x[which(combo_counts[,4]==1)] <- 1

c_x[which(combo_counts[,8]==1)] <- 1

 

pg_x[which(combo_counts[,3]>=2)] <- 1

wg_x[which(combo_counts[,3]>=2)] <- 1

wg_x[which(combo_counts[,3]>=1 & pg_x==1)] <- 1

pg_x[which(combo_counts[,3]>=1 & wg_x==1)] <- 1

 

pf_x[which(combo_counts[,6]>=2)] <- 1

wg_x[which(combo_counts[,6]>=2)] <- 1

wg_x[which(combo_counts[,6]>=1 & pf_x==1)] <- 1

pf_x[which(combo_counts[,6]>=1 & wg_x==1)] <- 1

 

pf_x[which(combo_counts[,5]>=2)] <- 1

pg_x[which(combo_counts[,5]>=2)] <- 1

pg_x[which(combo_counts[,5]>=1 & pf_x==1)] <- 1

pf_x[which(combo_counts[,5]>=1 & pg_x==1)] <- 1

 

pf_x[which(combo_counts[,9]>=2)] <- 1

c_x[which(combo_counts[,9]>=2)] <- 1

c_x[which(combo_counts[,9]>=1 & pf_x==1)] <- 1

pf_x[which(combo_counts[,9]>=1 & c_x==1)] <- 1

 

 

pg_x[which(combo_counts[,7]>=2 & wg_x==1)] <- 1

pf_x[which(combo_counts[,7]>=2 & wg_x==1)] <- 1

wg_x[which(combo_counts[,7]>=2 & pg_x==1)] <- 1

pf_x[which(combo_counts[,7]>=2 & pg_x==1)] <- 1

pg_x[which(combo_counts[,7]>=2 & pf_x==1)] <- 1

wg_x[which(combo_counts[,7]>=2 & pf_x==1)] <- 1

 

pf_x[which(combo_counts[,7]>=3)] <- 1

pg_x[which(combo_counts[,7]>=3)] <- 1

wg_x[which(combo_counts[,7]>=3)] <- 1

 

pf_x[which(combo_counts[,7]>=1 & pg_x==1 & wg_x==1)] <- 1

wg_x[which(combo_counts[,7]>=1 & pg_x==1 & pf_x==1)] <- 1

pg_x[which(combo_counts[,7]>=1 & pf_x==1 & wg_x==1)] <- 1

 

c_x[which(combo_counts[,10]>=2 & wg_x==1)] <- 1

pf_x[which(combo_counts[,10]>=2 & wg_x==1)] <- 1

wg_x[which(combo_counts[,10]>=2 & c_x==1)] <- 1

pf_x[which(combo_counts[,10]>=2 & c_x==1)] <- 1

c_x[which(combo_counts[,10]>=2 & pf_x==1)] <- 1

wg_x[which(combo_counts[,10]>=2 & pf_x==1)] <- 1

 

pf_x[which(combo_counts[,10]>=3)] <- 1

c_x[which(combo_counts[,10]>=3)] <- 1

wg_x[which(combo_counts[,10]>=3)] <- 1

 

pf_x[which(combo_counts[,10]>=1 & c_x==1 & wg_x==1)] <- 1

wg_x[which(combo_counts[,10]>=1 & c_x==1 & pf_x==1)] <- 1

c_x[which(combo_counts[,10]>=1 & pf_x==1 & wg_x==1)] <- 1

 

 

pg_x[which(combo_counts[,11]>=1 & pf_x==1 & wg_x==1 & c_x==1)] <- 1

wg_x[which(combo_counts[,11]>=1 & pf_x==1 & pg_x==1 & c_x==1)] <- 1

pf_x[which(combo_counts[,11]>=1 & wg_x==1 & pg_x==1 & c_x==1)] <- 1

c_x[which(combo_counts[,11]>=1 & wg_x==1 & pg_x==1 & pf_x==1)] <- 1

 

pos_combos[which(empty_test[,1]==1 & pg_x==0),]

 

apply(empty_test[which(apply(cbind(pg_x,wg_x,pf_x,c_x),1,sum)==0),],1,min)

 

 

table(apply(cbind(pg_x,wg_x,pf_x,c_x),1,sum))

pos_combos[which(apply(cbind(pg_x,wg_x,pf_x,c_x),1,sum)==4),]

 

length(pos_combos[which(pg_x==0),][,1])

 

 

 

 

sum_pos <- apply(pos_combos[c(1:2),],1,function(z) apply(matrix(unlist(apply(z,1,function(x) elig[x,])),4,4,byrow=F),2,sum))

z<-pos_combos[500,]

apply(matrix(unlist(apply(z,1,function(x) elig[x,])),4,4,byrow=F),2,sum)

 

 

####################################

####################################

####################################

 

library(RJSONIO)

 

setwd('C:/Users/A097092/Desktop/Extra/Fantasy BB research')

 

json_data <- fromJSON('2016-gamelog.json',FLATTEN=T)

 

final <- data.frame(t(sapply(json_data$resultSets[[1]]$rowSet,as.character)),stringsAsFactors=F)

final[,c(11:31)] <- sapply(final[,c(11:31)],as.numeric)

 

names(final) <- c('YearID','PlayerID','Player','TeamID','Team','TeamLong','GameID','Game Date','Match Up','W/L','MIN','FGM','FGA','FG%','3PM','3PA','3P%','FTM','FTA','FT%','OREB','DREB','REB','AST','STL','BLK','TOV','PF','PTS','PM','Video')

write.table(final,'2016-gamelog.txt',row.names=F)

 

 

 

