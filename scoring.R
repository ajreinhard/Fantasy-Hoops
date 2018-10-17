library(RJSONIO)
library(XML)

setwd('C:/Users/Owner/Documents/GitHub/Fantasy-Hoops')

json_data <- fromJSON('ESPN.json',FLATTEN=T)

length(json_data[[1]][[1]])
json_data[[1]][[116]]

players <- t(sapply(json_data[[1]], function(x) c(x$player$fullName,x$onTeamId,x$status,x$player$id,x$player$injured,x$player$proTeamId,x$player$ownership$percentOwned)))
all_slots <- t(ifelse(sapply(json_data[[1]], function(x) is.na(match(c(0,7,3,4),x$player$eligibleSlots))),0,1))
ESPN_df <- data.frame(players,all_slots,stringsAsFactors=F)
names(ESPN_df) <- c('player','OwnerID','Pool','ESPN_ID','Hurt','NBA_TmID','OwnerPct','PG','WG','PF','C')



nf_proj <- lapply(dir('NF Pages',full.name=T), function(x) {
the_tree <- htmlTreeParse(x, useInternal=T)
proj_dates <- sapply(xpathSApply(the_tree, '//div[1]/table[1]/tbody/tr/td[1]'),function(y) xmlValue(y, trim=T))
opp <- sapply(xpathSApply(the_tree, '//div[1]/table[1]/tbody/tr/td[2]'),function(y) xmlValue(y, trim=T))
headers <- sapply(xpathSApply(the_tree, '//table[2]/thead/tr/th'),function(y) xmlValue(y, trim=T))
proj_stats <- matrix(sapply(xpathSApply(the_tree, '//table[2]/tbody/tr/td'),function(y) xmlValue(y, trim=T)),ncol=14,byrow=T)
colnames(proj_stats) <- headers
player <- xmlValue(xpathSApply(the_tree, '//h2/text()')[[3]],trim=T)
data.frame('NF-URL'=substr(x,10,nchar(x)-4),player,'date'=proj_dates,opp,proj_stats,stringsAsFactors=F)
})

nf_proj_df <- do.call(rbind,nf_proj)

cbind(unique(nf_proj_df$player),dir('NF Pages'))

str(players)



str(nf_proj_df)

