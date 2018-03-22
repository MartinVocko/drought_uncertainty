#setwd("~/Plocha/egu_poster")
#setwd("~/Plocha/egu2")

# Rankhistograms

verifrank<-function (forecasts, observations, plotname) 
{
  rank <- apply(cbind(observations, forecasts), 1, function(x) rank(x, 
                                                                    ties = "random")[1])
  k <- ncol(forecasts)
 hist(rank, breaks = 0:(k + 1), prob = TRUE, xaxt = "n", xlab = "", 
       ylab = "", col="cadetblue4", ylim=c(0,0.2), main = plotname, border= "white", bg='gray95')
  axis(1, at = seq(0.5, to = k + 0.5, by = 1), labels = 1:(k + 
                                                            1))
  abline(h = 1/(k + 1), lty = 2)
  box(lty = 1, col = 'black')

#table(rank)    
#rank2=data.frame(t(table(rank)/sum(count(rank)$freq)))

 
#p= ggplot() +
#  geom_histogram(aes(x=rank), fill="steelblue" )+
#   theme_minimal()+
#   geom_hline(aes(yintercept=0.1))
#return(p)
   
  invisible(rank)
}




cqa=readRDS('ceu_q_a.rds')
cqs=readRDS('ceu_q_s.rds')
csa=readRDS('ceu_s_a.rds')
css=readRDS('ceu_s_s.rds')
mqa=readRDS('med_q_a.rds')
mqs=readRDS('med_q_s.rds')
msa=readRDS('med_s_a.rds')
mss=readRDS('med_s_s.rds')

par(mfrow=c(2,4),bg='gray90')
verifrank(cqa[,3:11],cqa$`1_1`, "ceu_q_a")
verifrank(cqs[,3:11],cqs$`1_1`, "ceu_q_s")
verifrank(csa[,3:11],csa$`1_1`, "ceu_s_a")
verifrank(css[,3:11],css$`1_1`, "ceu_s_s")
verifrank(mqa[,3:11],mqa$`1_1`, "med_q_a")
verifrank(mqs[,3:11],mqs$`1_1`, "med_q_s")
verifrank(msa[,3:11],msa$`1_1`, "med_s_a")
verifrank(mss[,3:11],mss$`1_1`, "med_s_s")



# Multi-rankhistogram

observations=cqa$`1_1`
forecasts=cqa[,3:11]
rank1 <- apply(cbind(observations, forecasts), 1, function(x) rank(x, 
                                                                   ties = "random")[1])

observations=cqs$`1_1`
forecasts=cqs[,3:11]
rank2 <- apply(cbind(observations, forecasts), 1, function(x) rank(x, 
                                                                   ties = "random")[1])
observations=csa$`1_1`
forecasts=csa[,3:11]
rank3 <- apply(cbind(observations, forecasts), 1, function(x) rank(x, 
                                                                   ties = "random")[1])
observations=css$`1_1`
forecasts=css[,3:11]
rank4 <- apply(cbind(observations, forecasts), 1, function(x) rank(x, 
                                                                    ties = "random")[1])
observations=mqa$`1_1`
forecasts=mqa[,3:11]
rank5 <- apply(cbind(observations, forecasts), 1, function(x) rank(x, 
                                                                   ties = "random")[1])

observations=mqs$`1_1`
forecasts=mqs[,3:11]
rank6 <- apply(cbind(observations, forecasts), 1, function(x) rank(x, 
                                                                   ties = "random")[1])
observations=msa$`1_1`
forecasts=msa[,3:11]
rank7 <- apply(cbind(observations, forecasts), 1, function(x) rank(x, 
                                                                   ties = "random")[1])
observations=mss$`1_1`
forecasts=mss[,3:11]
rank8 <- apply(cbind(observations, forecasts), 1, function(x) rank(x, 
                                                                   ties = "random")[1])

l=list(rank1, rank2, rank3, rank4, rank5, rank6, rank7, rank8)

multhist(l,freq=F,breaks = seq(0,10,by=1),names.arg=rep("",10), prob = TRUE, xaxt = "n",main="Rank MultiHistogram", col=heat.colors(8),ylim=c(0,0.2))
axis(1, at = seq(4.5, to = 93+ 0.5, by = 9), labels = 1:(9 + 1))
abline(h = 1/(9 + 1), lty = 2)
legend("topleft", c("ceu_q_a","ceu_q_s","ceu_s_a","ceu_s_s","med_q_a", "med_q_s", "med_s_a", "med_s_s"),fill=heat.colors(8),bg="transparent" )
 
