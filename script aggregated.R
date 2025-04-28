# this script is based on numbers, means and standard deviatiopns in table 4 
# for eight conditions and three time points computed from individual data

# for checking with table 4 in the article
n.outcome
means.outcome
round(means.outcome,2)
sd.outcome
round(sd.outcome,2)

# accumulated means
acc.masc <- (means.outcome$means.t1masc+means.outcome$means.t2masc)*6/52 + 
  (means.outcome$means.t2masc+means.outcome$means.t3masc)*25/52
acc.smfq <- (means.outcome$means.t1smfq+means.outcome$means.t2smfq)*6/52 + 
  (means.outcome$means.t2smfq+means.outcome$means.t3smfq)*25/52
acc.qaly <- (means.outcome$means.t1qaly+means.outcome$means.t2qaly)*6/52 + 
  (means.outcome$means.t2qaly+means.outcome$means.t3qaly)*25/52
acc.outcome <- data.frame(cbind(acc.masc,acc.smfq,acc.qaly))
row.names(acc.outcome) <- row.names(means.outcome)
acc.outcome
round(acc.outcome,2)

# computing costs
costs <- 
  data.frame(nchgroup=c(rep(16,4),rep(8,4)), parsess=rep(c(5,5,0,0),2),
             tothgroup=NA, MFSdrev=rep(c(8,0),4), GLsess=rep(4,8),
             chrecrs=rep(2,8), grlcost=rep(529,8), 
             workb=5*rep(c(rep(269+239,2),rep(269,2)),2),
             MFSappg=5*rep(c(45,0),4), CBTsupg=rep(1000,8),
             costg=NA, costch=NA,
             row.names=row.names(means.outcome))
costs$tothgroup <- 2*(costs$nchgroup + costs$parsess) + 
  2.5* (costs$parsess>0)
costs$costg <- 
  2*(costs$tothgroup+costs$MFSdrev+costs$GLsess+costs$chrecrs)*
  costs$grlcost + costs$workb + costs$MFSappg + costs$CBTsupg
costs$costch <- costs$costg/5
costs <- costs[order(costs$costch),]
costs

# sort accumulated measures by costs
acc.outcome
costs
acc.outcome[c(8,7,6,4,5,3,2,1),] # the same combinations, sorted as in costs
acc.outcome.sortc <- acc.outcome[c(8,7,6,4,5,3,2,1),]
cost.acc <- data.frame(cbind(costs=costs[,"costch"],acc.outcome.sortc))
cost.acc

# table 4
n.outcome.sort <- n.outcome[c(8,7,6,4,5,3,2,1),]
means.outcome.sort <- means.outcome[c(8,7,6,4,5,3,2,1),]
sd.outcome.sort <- sd.outcome[c(8,7,6,4,5,3,2,1),]
n.outcome.sort
means.outcome.sort
sd.outcome.sort
resort.tab4.1 <- c(1,9,17)
resort.tab4 <- c(resort.tab4.1,resort.tab4.1+1,resort.tab4.1+2,resort.tab4.1+3,
                  resort.tab4.1+4,resort.tab4.1+5,resort.tab4.1+6,resort.tab4.1+7)
table4 <- data.frame(
  t1masc=c(n.outcome.sort[,"n.t1masc"],means.outcome.sort[,"means.t1masc"],
           sd.outcome.sort[,"sd.t1masc"])[resort.tab4],
  t2masc=c(n.outcome.sort[,"n.t2masc"],means.outcome.sort[,"means.t2masc"],
           sd.outcome.sort[,"sd.t2masc"])[resort.tab4],
  t3masc=c(n.outcome.sort[,"n.t3masc"],means.outcome.sort[,"means.t3masc"],
           sd.outcome.sort[,"sd.t3masc"])[resort.tab4],
  accmasc=NA,
  t1smfq=c(n.outcome.sort[,"n.t1smfq"],means.outcome.sort[,"means.t1smfq"],
           sd.outcome.sort[,"sd.t1smfq"])[resort.tab4],
  t2smfq=c(n.outcome.sort[,"n.t2smfq"],means.outcome.sort[,"means.t2smfq"],
           sd.outcome.sort[,"sd.t2smfq"])[resort.tab4],
  t3smfq=c(n.outcome.sort[,"n.t3smfq"],means.outcome.sort[,"means.t3smfq"],
           sd.outcome.sort[,"sd.t3smfq"])[resort.tab4],
  accsmfq=NA,
  t1qaly=c(n.outcome.sort[,"n.t1qaly"],means.outcome.sort[,"means.t1qaly"],
           sd.outcome.sort[,"sd.t1qaly"])[resort.tab4],
  t2qaly=c(n.outcome.sort[,"n.t2qaly"],means.outcome.sort[,"means.t2qaly"],
           sd.outcome.sort[,"sd.t2qaly"])[resort.tab4],
  t3qaly=c(n.outcome.sort[,"n.t3qaly"],means.outcome.sort[,"means.t3qaly"],
           sd.outcome.sort[,"sd.t3qaly"])[resort.tab4],
  accqaly=NA,
  row.names=paste0(rep(row.names(costs),rep(3,8)),
                   rep(c("n","m","s"),8)))
table4$accmasc[c(3,6,9,12,15,18,21,24)] <- acc.outcome.sortc$acc.masc
table4$accsmfq[c(3,6,9,12,15,18,21,24)] <- acc.outcome.sortc$acc.smfq
table4$accqaly[c(3,6,9,12,15,18,21,24)] <- acc.outcome.sortc$acc.qaly
# computations for total sample
table4.totn <- apply(n.outcome,2,sum)
table4.totmean <- apply(n.outcome*means.outcome,2,sum)/table4.totn
table4.ssw <- (n.outcome-1)*sd.outcome^2
table4.ssw.sum <- apply(table4.ssw,2,sum)
table4.totmean.row <- 
  rbind(table4.totmean,table4.totmean,table4.totmean,table4.totmean,
        table4.totmean,table4.totmean,table4.totmean,table4.totmean)
table4.ssb.sum <- apply(n.outcome*(means.outcome-table4.totmean.row)^2,2,sum)
table4.totsd <- sqrt((table4.ssw.sum + table4.ssb.sum)/(table4.totn-1))
# insert missings for accumlatred numbers
table4.totn <- c(table4.totn[1:3],NA,table4.totn[4:6],NA,table4.totn[7:9],NA)
table4.totmean <- c(table4.totmean[1:3],NA,table4.totmean[4:6],NA,table4.totmean[7:9],NA)
table4.totsd <- c(table4.totsd[1:3],NA,table4.totsd[4:6],NA,table4.totsd[7:9],NA)
table4 <- rbind(table4, table4.totn, table4.totmean,table4.totsd)
row.names(table4)[25:27] <- c("totn","totm","tots")
table4
table4round <- table4
table4round[c(1,4,7,10,13,16,19,22,25),] <- round(table4[c(1,4,7,10,13,16,19,22,25),])
table4round[   c(2,3,5,6,8,9,11,12,14,15,17,18,20,21,23,24,26,27),] <- 
  round(table4[c(2,3,5,6,8,9,11,12,14,15,17,18,20,21,23,24,26,27),],2)
table4round
write.table(x=table4round, file="table4round.txt", quote=FALSE, sep=";",
            na=" ")

# table 5
cost.acc.masc <- cost.acc[,c(1,2)]
cost.acc.masc$incrk <- c(NA,diff(cost.acc.masc$costs))
cost.acc.masc$incrm <--c(NA,diff(cost.acc.masc$acc.masc))
cost.acc.masc$icer <- cost.acc.masc$incrk/cost.acc.masc$incrm
cost.acc.masc

# frontier only for costs and masc
cost.acc.masc.front <- cost.acc.masc[c(1,2,6),]
cost.acc.masc.front$incrkf <- c(NA,diff(cost.acc.masc.front$costs))
cost.acc.masc.front$incrmf <- -c(NA,diff(cost.acc.masc.front$acc.masc))
cost.acc.masc.front$icerf <- cost.acc.masc.front$incrkf/
  cost.acc.masc.front$incrmf
cost.acc.masc.front
# include frontier information into cost.acc.masc
cost.acc.masc$incrkf <- NA
cost.acc.masc$incrmf <- NA
cost.acc.masc$icerf <- NA
cost.acc.masc[c(1,2,6),c("incrkf","incrmf","icerf")] <- 
  cost.acc.masc.front[,c("incrkf","incrmf","icerf")]
cost.acc.masc

# table 6
cost.acc.smfq <- cost.acc[,c(1,3)]
cost.acc.smfq$incrk <- c(NA,diff(cost.acc.smfq$costs))
cost.acc.smfq$incrs <--c(NA,diff(cost.acc.smfq$acc.smfq))
cost.acc.smfq$icer <- cost.acc.smfq$incrk/cost.acc.smfq$incrs
cost.acc.smfq

# frontier only for costs and smfq
# the frontier is only the line between SLN to LLN as shown in
# figure 2 below
cost.acc.smfq.front <- cost.acc.smfq[c(1,4),]
cost.acc.smfq.front$incrkf <- c(NA,diff(cost.acc.smfq.front$costs))
cost.acc.smfq.front$incrsf <- -c(NA,diff(cost.acc.smfq.front$acc.smfq))
cost.acc.smfq.front$icerf <- cost.acc.smfq.front$incrkf/
  cost.acc.smfq.front$incrsf
cost.acc.smfq.front
# include frontier information into cost.acc.smfq
cost.acc.smfq$incrkf <- NA
cost.acc.smfq$incrsf <- NA
cost.acc.smfq$icerf <- NA
cost.acc.smfq[c(1,4),c("incrkf","incrsf","icerf")] <- 
  cost.acc.smfq.front[,c("incrkf","incrsf","icerf")]
cost.acc.smfq

# table 7
cost.acc.qaly <- cost.acc[,c(1,4)]
cost.acc.qaly$incrk <- c(NA,diff(cost.acc.qaly$costs))
cost.acc.qaly$incrq <- c(NA,diff(cost.acc.qaly$acc.qaly))
cost.acc.qaly$icer <- cost.acc.qaly$incrk/cost.acc.qaly$incrq
cost.acc.qaly

# frontier only for costs and qaly
cost.acc.qaly.front <- cost.acc.qaly[c(1,4),]
cost.acc.qaly.front$incrkf <- c(NA,diff(cost.acc.qaly.front$costs))
cost.acc.qaly.front$incrqf <- c(NA,diff(cost.acc.qaly.front$acc.qaly))
cost.acc.qaly.front$icerf <- cost.acc.qaly.front$incrkf/
  cost.acc.qaly.front$incrqf
cost.acc.qaly.front
# include frontier information into cost.acc.qaly
cost.acc.qaly$incrkf <- NA
cost.acc.qaly$incrqf <- NA
cost.acc.qaly$icerf <- NA
cost.acc.qaly[c(1,4),c("incrkf","incrqf","icerf")] <- 
  cost.acc.qaly.front[,c("incrkf","incrqf","icerf")]
cost.acc.qaly

# with rounding
# masc
cost.acc.masc
cost.acc.masc.round <- cost.acc.masc
cost.acc.masc.round[,c(1,3,5,6,8)] <- round(cost.acc.masc.round[,c(1,3,5,6,8)])
cost.acc.masc.round[,c(2,4,7)] <- round(cost.acc.masc.round[,c(2,4,7)],2)
cost.acc.masc
cost.acc.masc.round
# smfq
cost.acc.smfq
cost.acc.smfq.round <- cost.acc.smfq
cost.acc.smfq.round[,c(1,3,5,6,8)] <- round(cost.acc.smfq.round[,c(1,3,5,6,8)])
cost.acc.smfq.round[,c(2,4,7)] <- round(cost.acc.smfq.round[,c(2,4,7)],2)
cost.acc.smfq
cost.acc.smfq.round
# qaly
cost.acc.qaly
cost.acc.qaly.round <- cost.acc.qaly
cost.acc.qaly.round[,c(1,3,5,6,8)] <- round(cost.acc.qaly.round[,c(1,3,5,6,8)])
cost.acc.qaly.round[,c(2,4,7)] <- round(cost.acc.qaly.round[,c(2,4,7)],3)
cost.acc.qaly
cost.acc.qaly.round
# txt files for table 5-7 as csv files without and withrounding
write.table(x=cost.acc.masc, file="table 5.txt", sep=";",
            na="",quote=FALSE)
write.table(x=cost.acc.masc.round, file="table 5 round.txt", sep=";",
            na="",quote=FALSE)
write.table(x=cost.acc.smfq, file="table 6.txt", sep=";",
            na="",quote=FALSE)
write.table(x=cost.acc.smfq.round, file="table 6 round.txt", sep=";",
            na="",quote=FALSE)
write.table(x=cost.acc.qaly, file="table 7.txt", sep=";",
            na="",quote=FALSE)
write.table(x=cost.acc.qaly.round, file="table 7 round.txt", sep=";",
            na="",quote=FALSE)
# tar ut kostnader også
costs
write.table(x=costs, file="costs.txt", sep=";",
            na="",quote=FALSE)

# graphs
# figure 1, including frontier for MASC
lwd.front.m <- 2
lty.dom.m <- "dashed"
col.dom.m <- "orange"
par(mar=c(bottom=4, left=4, top=0, right=0)+.1)
plot(cost.acc.masc$acc.masc, cost.acc.masc$costs,
     xlim=c(66,75), ylim=c(0,14000), las=1, pch=19,
     xlab="MASC", ylab="Costs")
text(x=cost.acc.masc$acc.masc, y=cost.acc.masc$costs,
     labels=row.names(cost.acc.masc), pos=4)
# frontier
lines(x=cost.acc.masc$acc.masc[c(6,2)], y=cost.acc.masc$costs[c(6,2)],
      lwd=lwd.front.m)
lines(x=cost.acc.masc$acc.masc[c(2,1)], y=cost.acc.masc$costs[c(2,1)],
      lwd=lwd.front.m)
# showing dominance
lines(x=cost.acc.masc$acc.masc[c(8,6)], y=cost.acc.masc$costs[c(8,6)],
      lty=lty.dom.m,col=col.dom.m)
lines(x=cost.acc.masc$acc.masc[c(4,2)], y=cost.acc.masc$costs[c(4,2)],
      lty=lty.dom.m,col=col.dom.m)
lines(x=cost.acc.masc$acc.masc[c(5,2)], y=cost.acc.masc$costs[c(5,2)],
      lty=lty.dom.m,col=col.dom.m)
lines(x=cost.acc.masc$acc.masc[c(7,2)], y=cost.acc.masc$costs[c(7,2)],
      lty=lty.dom.m,col=col.dom.m)
lines(x=cost.acc.masc$acc.masc[c(3,2)], y=cost.acc.masc$costs[c(3,2)],
      lty=lty.dom.m,col=col.dom.m)
par(mar=c(bottom=5, left=4, top=4, right=2)+.1)

# figure 2, including frontier for SMFQ
lwd.front.s <- 2
lty.dom.s <- "dashed"
col.dom.s <- "orange"
lty.idom.s <- "dotdash"
col.idom.s <- "orange3"
place.idom.s <- .5
# after a look at the diagram below it turns out that SHN has moved
# up, and seems to be above the line LLN-SLN. Checking that by 
# comparing the slopes of LLN to SHN, and SHN to SLN
cost.acc.smfq[,1:2] # to see costs and acc.smfq
(cost.acc.smfq["LLN","costs"]-cost.acc.smfq["SHN","costs"])/
  (cost.acc.smfq["LLN","acc.smfq"]-cost.acc.smfq["SHN","acc.smfq"])
# slope -1324
(cost.acc.smfq["SHN","costs"]-cost.acc.smfq["SLN","costs"])/
  (cost.acc.smfq["SHN","acc.smfq"]-cost.acc.smfq["SLN","acc.smfq"])
# slope -1687, steeper, SHN is above the the line from LLN to SLN,
# then the frontier is LLN to SLN only
par(mar=c(bottom=4, left=4, top=0, right=0)+.1)
plot(cost.acc.smfq$acc.smfq, cost.acc.smfq$costs,
     xlim=c(9.5,12.5), ylim=c(0,14000), las=1, pch=19,
     xlab="SMFQ", ylab="Costs")
text(x=cost.acc.smfq$acc.smfq, y=cost.acc.smfq$costs,
     labels=row.names(cost.acc.smfq), pos=4)
# frontier
lines(x=cost.acc.smfq$acc.smfq[c(1,4)], y=cost.acc.smfq$costs[c(1,4)],
      lwd=lwd.front.s)
# showing diminance for the others, except SLF,SHN
lines(x=cost.acc.smfq$acc.smfq[c(7,4)], y=cost.acc.smfq$costs[c(7,4)],
      lty=lty.dom.s,col=col.dom.s)
lines(x=cost.acc.smfq$acc.smfq[c(6,4)], y=cost.acc.smfq$costs[c(6,4)],
      lty=lty.dom.s,col=col.dom.s)
lines(x=cost.acc.smfq$acc.smfq[c(8,4)], y=cost.acc.smfq$costs[c(8,4)],
      lty=lty.dom.s,col=col.dom.s)
lines(x=cost.acc.smfq$acc.smfq[c(5,4)], y=cost.acc.smfq$costs[c(5,4)],
      lty=lty.dom.s,col=col.dom.s)
# from SLF and SHN to the line SLN-SHN, extended dominance
place.slf.s <- (cost.acc.smfq$acc.smfq[2]-cost.acc.smfq$acc.smfq[1])/
  (cost.acc.smfq$acc.smfq[4]-cost.acc.smfq$acc.smfq[1])
lines(x=rep(cost.acc.smfq$acc.smfq[2], 2), 
      y=c(cost.acc.smfq$costs[2], place.slf.s*cost.acc.smfq$costs[4]+
            (1-place.slf.s)*cost.acc.smfq$costs[1]),
      lty=lty.dom.s,col=col.dom.s, type="o", pch=19)
place.shn.s <- (cost.acc.smfq$acc.smfq[3]-cost.acc.smfq$acc.smfq[1])/
  (cost.acc.smfq$acc.smfq[4]-cost.acc.smfq$acc.smfq[1])
lines(x=rep(cost.acc.smfq$acc.smfq[3], 2), 
      y=c(cost.acc.smfq$costs[3], place.shn.s*cost.acc.smfq$costs[4]+
            (1-place.shn.s)*cost.acc.smfq$costs[1]),
      lty=lty.dom.s,col=col.dom.s, type="o", pch=19)
par(mar=c(bottom=5, left=4, top=4, right=2)+.1)

# figur 3, including frontier for qaly
lwd.front.q <- 2
lty.dom.q <- "dashed"
col.dom.q <- "orange"
lty.idom.q <- "dotdash"
col.idom.q <- "orange3"
place.idom.q <- .5
par(mar=c(bottom=4, left=4, top=0, right=0)+.1)
plot(cost.acc.qaly$acc.qaly, cost.acc.qaly$costs,
     xlim=c(0.935,0.977), ylim=c(0,14000), las=1, pch=19,
     xlab="QALY", ylab="Costs")
text(x=cost.acc.qaly$acc.qaly, y=cost.acc.qaly$costs,
     labels=row.names(cost.acc.qaly), pos=4)
# frontier
lines(x=cost.acc.qaly$acc.qaly[c(1,4)], y=cost.acc.qaly$costs[c(1,4)],
      lwd=lwd.front.q)
# showing dominance, excluding SLF,SHN
lines(x=cost.acc.qaly$acc.qaly[c(5,1)], y=cost.acc.qaly$costs[c(5,1)],
      lty=lty.dom.q,col=col.dom.q)
lines(x=cost.acc.qaly$acc.qaly[c(8,4)], y=cost.acc.qaly$costs[c(8,4)],
      lty=lty.dom.q,col=col.dom.q)
lines(x=cost.acc.qaly$acc.qaly[c(7,4)], y=cost.acc.qaly$costs[c(7,4)],
      lty=lty.dom.q,col=col.dom.q)
lines(x=cost.acc.qaly$acc.qaly[c(6,4)], y=cost.acc.qaly$costs[c(6,4)],
      lty=lty.dom.q,col=col.dom.q)
# and from SLF,SHN (extended dominance) to the line between SLN and LLN
place.slf.q <- (cost.acc.qaly$acc.qaly[2]-cost.acc.qaly$acc.qaly[1])/
  (cost.acc.qaly$acc.qaly[4]-cost.acc.qaly$acc.qaly[1])
lines(x=rep(cost.acc.qaly$acc.qaly[2], 2), 
      y=c(cost.acc.qaly$costs[2], (1-place.slf.q)*cost.acc.qaly$costs[1]+
            place.slf.q*cost.acc.qaly$costs[4]),
      lty=lty.dom.q,col=col.dom.q, type="o", pch=19)
place.shn.q <- (cost.acc.qaly$acc.qaly[3]-cost.acc.qaly$acc.qaly[1])/
  (cost.acc.qaly$acc.qaly[4]-cost.acc.qaly$acc.qaly[1])
lines(x=rep(cost.acc.qaly$acc.qaly[3], 2), 
      y=c(cost.acc.qaly$costs[3], (1-place.shn.q)*cost.acc.qaly$costs[1]+
            place.shn.q*cost.acc.qaly$costs[4]),
      lty=lty.dom.q,col=col.dom.q, type="o", pch=19)
par(mar=c(bottom=5, left=4, top=4, right=2)+.1)
