# Visualising Pr(Detection) posteriors
# Goal is to take in an array from jags.sample
# where the dimensions are:
# [Buoy, Call, Intensity, Posterior, Chain(1 for this)]

library(ggplot2)
library(manipulate)
library(data.table)
load('logistic posterior.rdata')

mcToDt <- function(mcarray) {
      require(data.table)
      tdf <- data.frame()
      
      dims <- dim(logistic.post$b1)
      for(b in 1:dims[1]){
            for(c in 1:dims[2]){
                  for(i in 1:dims[3]){
                        tempdf <- data.frame(bid=b, cid=c, iid=i,
                                             DistCoeff=logistic.post$b1[b,c,i,1:dims[4],1],
                                             AngCoeff=logistic.post$b2[b,c,i,1:dims[4],1],
                                             InterCoeff=logistic.post$b12[b,c,i,1:dims[4],1])
                        tdf <- rbind(tdf, tempdf)
                  }
            }
      }
      
      iidmap <- data.frame(Intensity=unique(df$Intensity), iid=as.numeric(unique(df$Intensity)))
      bidmap <- data.frame(Buoy=unique(df$Buoy), bid=as.numeric(unique(df$Buoy)))
      cidmap <- data.frame(CallType=unique(df$CallType), cid=as.numeric(unique(df$CallType)))
      
      tdf <- merge(tdf, iidmap)
      tdf <- merge(tdf, bidmap)
      tdf <- merge(tdf, cidmap)
      tdf <- within(tdf, rm(iid,cid,bid))
      tdf <- data.table(tdf)
      
      tdf2 <- data.frame()
      for(b in 1:dims[1]) {
            tempdf <- data.frame(bid = b, Offset = logistic.post$offset[b, 1:dims[4],1], Tau = logistic.post$tau[b,1:dims[4],1])
            tdf2 <- rbind(tdf2, tempdf)
      }
      
      tdf2 <- merge(tdf2, bidmap)
      tdf2$Stdev <- 1/sqrt(tdf2$Tau)
      tdf2 <- data.table(tdf2)
      
      list(Detection = tdf, Angle = tdf2)
}

detectionVisualization <- function(df, factorpick) {
      require(ggplot2, manipulate, data.table)
      # Inputs: df from mcToDt
      # factorpick is the factor variable we want to slice by
      # Assumes order of factors in array is b,c,i
      
      manipulate({
            ggplot(data=df[df[[factorpick]]==FactorValue,], aes_string(x=Coefficient)) + geom_density() + geom_vline(xintercept=0, color='green', alpha=.2, size=2) +
                  facet_grid(facets=(function(x) {
                        tmp <- names(df)[!(names(df) %in% c('DistCoeff', 'AngCoeff', 'InterCoeff', factorpick))]
                        paste(tmp[1], '~', tmp[2])} )()) +
                  labs(title=paste('Density plots for', Coefficient, 'with', factorpick, FactorValue)) + xlim(-10,10)},
            FactorValue=do.call(picker, list(as.list(levels(df[[factorpick]])), initial=levels(df[[factorpick]])[1])),
            Coefficient=picker(list('DistCoeff', 'AngCoeff', 'InterCoeff'), initial='DistCoeff')
      )
}

mclist <- mcToDt(logistic.post)
detdf <- mclist$Detection
angledf <- mclist$Angle

byBuoy <- detectionVisualization(detdf, 'Buoy')
byIntensity <- detectionVisualization(detdf, 'Intensity')
byCallType <- detectionVisualization(detdf, 'CallType')

rm(mclist)
manipulate({
      ggplot(data=detdf, aes_string(x=Coefficient, colour=Color)) + geom_density() + geom_vline(xintercept=0, color='green', alpha=.2, size=2) + xlim(-10,10)},
      Coefficient=picker('DistCoeff', 'AngCoeff', 'InterCoeff'), Color=picker('Buoy', 'Intensity', 'CallType'))


manipulate({
      ggplot(data=angledf, aes_string(x=Variable, colour='Buoy')) + geom_density()},
      Variable=picker('Offset', 'Stdev'))

