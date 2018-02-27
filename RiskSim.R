
Sims<-10000
InFightSim<-1000

P1_FM<-9
P1_AR<-0
P1_CV<-0
P1_SW<-0
  
P2_FM<-4
P2_AR<-0
P2_CV<-0
P2_SW<-0


#______________________________________________________________________________
VecOne=(1:Sims/1:Sims)


resultVec=0*(1:Sims)
set.seed(185)

P1_FMv2<-P1_FM*VecOne
P1_ARv2<-P1_AR*VecOne
P1_CVv2<-P1_CV*VecOne
P1_SWv2<-P1_SW*VecOne

P2_FMv2<-P2_FM*VecOne
P2_ARv2<-P2_AR*VecOne
P2_CVv2<-P2_CV*VecOne
P2_SWv2<-P2_SW*VecOne

for (i in (1:Sims)){

  
  for (j in 1:InFightSim){
    
  #Rank1

    Siege1<-(0)
    
    if (P1_SWv2[i]>=1){
      Siege1<-sample(1:6,P1_SWv2[i]*2,replace=T)
      Siege1<-sum(1*(Siege1>=3))
    }else{
      Siege1<-0
    }
    
    Siege2<-(0)
    
    if (P2_SWv2[i]>=1){
      Siege2<-sample(1:6,P2_SWv2[i]*2,replace=T)
      Siege2<-sum(1*(Siege2>=3))
    } else{
      Siege2<-(0)
    }
    
    #P2Attk
    
    P1_FMv2[i]<- (P1_FMv2[i]-Siege2)
    if(P1_FMv2[i]<0){
      Siege2<-(-1*P1_FMv2[i])
      P1_FMv2[i]<-(0)
    P1_ARv2[i]<-(P1_ARv2[i]-Siege2)
    if(P1_ARv2[i]<0){
      Siege2<-(-1*P1_ARv2[i])
      P1_ARv2[i]<-(0)
    P1_CVv2[i]<-(P1_CVv2[i]-Siege2)
    if(P1_CVv2[i]<0){
      Siege2<-(-1*P1_CVv2[i])
      P1_CVv2[i]<-(0)
    P1_SWv2[i]<-(P1_SWv2[i]-Siege2)
    if(P1_SWv2[i]<0){
      P1_SWv2[i]<-(0)
    }
    }
    }
    }
    #EndP2Attk

    #P1Attk
    
    
    P2_FMv2[i]<- (P2_FMv2[i]-Siege1)
    if(P2_FMv2[i]<0){
      Siege1<-(-1*P2_FMv2[i])
      P2_FMv2[i]<-(0)
      P2_ARv2[i]<-(P2_ARv2[i]-Siege1)
      if(P2_ARv2[i]<0){
        Siege1<-(-1*P2_ARv2[i])
        P2_ARv2[i]<-(0)
        P2_CVv2[i]<-(P2_CVv2[i]-Siege1)
        if(P2_CVv2[i]<0){
          Siege1<-(-1*P2_CVv2[i])
          P2_CVv2[i]<-(0)
          P2_SWv2[i]<-(P2_SWv2[i]-Siege1)
          if(P2_SWv2[i]<0){
            P2_SWv2[i]<-(0)
          }
        }
      }
    }
    
    #EndP1Attk
  
  if((P1_FMv2[i]+P1_ARv2[i]+P1_CVv2[i]+P1_SWv2[i]+P2_FMv2[i]+P2_ARv2[i]+P2_CVv2[i]+P2_SWv2[i])==0){
    resultVec[i]<-(0)
    break
  }
  if((P1_FMv2[i]+P1_ARv2[i]+P1_CVv2[i]+P1_SWv2[i])==0){
    resultVec[i]<-(2)
    break
  }
  if((P2_FMv2[i]+P2_ARv2[i]+P2_CVv2[i]+P2_SWv2[i])==0){
    resultVec[i]<-(1)
    break
  }
    
  #EndRank1
    
  #Rank2
    
    AR1<-(0)
    if (P1_ARv2[i]>=1){
      AR1<-sample(1:6,P1_ARv2[i],replace=T)
      AR1<-sum(1*(AR1>=5))
    }
    else{
      AR1<-(0)
    }
    
    AR2<-(0)
    if (P2_ARv2[i]>=1){
      AR2<-sample(1:6,P2_ARv2[i],replace=T)
      AR2<-sum(1*(AR2>=5))
    }
    else{
      AR2<-(0)
    }
    
    #P2Attk
    
    P1_FMv2[i]<- (P1_FMv2[i]-AR2)
    if(P1_FMv2[i]<0){
      AR2<-(-1*P1_FMv2[i])
      P1_FMv2[i]<-(0)
      P1_ARv2[i]<-(P1_ARv2[i]-AR2)
      if(P1_ARv2[i]<0){
        AR2<-(-1*P1_ARv2[i])
        P1_ARv2[i]<-(0)
        P1_CVv2[i]<-(P1_CVv2[i]-AR2)
        if(P1_CVv2[i]<0){
          AR2<-(-1*P1_CVv2[i])
          P1_CVv2[i]<-(0)
          P1_SWv2[i]<-(P1_SWv2[i]-AR2)
          if(P1_SWv2[i]<0){
            P1_SWv2[i]<-(0)
          }
        }
      }
    }
    #EndP2Attk
    
    #P1Attk
    
    
    P2_FMv2[i]<- (P2_FMv2[i]-AR1)
    if(P2_FMv2[i]<0){
      AR1<-(-1*P2_FMv2[i])
      P2_FMv2[i]<-(0)
      P2_ARv2[i]<-(P2_ARv2[i]-AR1)
      if(P2_ARv2[i]<0){
        AR1<-(-1*P2_ARv2[i])
        P2_ARv2[i]<-(0)
        P2_CVv2[i]<-(P2_CVv2[i]-AR1)
        if(P2_CVv2[i]<0){
          AR1<-(-1*P2_CVv2[i])
          P2_CVv2[i]<-(0)
          P2_SWv2[i]<-(P2_SWv2[i]-AR1)
          if(P2_SWv2[i]<0){
            P2_SWv2[i]<-(0)
          }
        }
      }
    }
    
    #EndP1Attk
    
    if((P1_FMv2[i]+P1_ARv2[i]+P1_CVv2[i]+P1_SWv2[i]+P2_FMv2[i]+P2_ARv2[i]+P2_CVv2[i]+P2_SWv2[i])==0){
      resultVec[i]<-0
      break
    }
    if((P1_FMv2[i]+P1_ARv2[i]+P1_CVv2[i]+P1_SWv2[i])==0){
      resultVec[i]<-2
      break
    }
    if((P2_FMv2[i]+P2_ARv2[i]+P2_CVv2[i]+P2_SWv2[i])==0){
      resultVec[i]<-1
      break
    }
  #EndRank2
  
  #Rank3
    
    CV1<-(0)
    if (P1_CVv2[i]>=1){
      CV1<-sample(1:6,P1_CVv2[i],replace=T)
      CV1<-sum(1*(CV1>=3))
    }
    else{
      CV1<-(0)
    }
    
    CV2<-(0)
    if (P2_CVv2[i]>=1){
      CV2<-sample(1:6,P2_CVv2[i],replace=T)
      CV2<-sum(1*(CV2>=3))
    }
    else{
      CV2<-(0)
    }
    
    #P2Attk
    
    P1_FMv2[i]<- (P1_FMv2[i]-CV2)
    if(P1_FMv2[i]<0){
      CV2<-(-1*P1_FMv2[i])
      P1_FMv2[i]<-(0)
      P1_ARv2[i]<-(P1_ARv2[i]-CV2)
      if(P1_ARv2[i]<0){
        CV2<-(-1*P1_ARv2[i])
        P1_ARv2[i]<-(0)
        P1_CVv2[i]<-(P1_CVv2[i]-CV2)
        if(P1_CVv2[i]<0){
          CV2<-(-1*P1_CVv2[i])
          P1_CVv2[i]<-(0)
          P1_SWv2[i]<-(P1_SWv2[i]-CV2)
          if(P1_SWv2[i]<0){
            P1_SWv2[i]<-(0)
          }
        }
      }
    }
    #EndP2Attk
    
    #P1Attk
    
    
    P2_FMv2[i]<- (P2_FMv2[i]-CV1)
    if(P2_FMv2[i]<0){
      CV1<-(-1*P2_FMv2[i])
      P2_FMv2[i]<-(0)
      P2_ARv2[i]<-(P2_ARv2[i]-CV1)
      if(P2_ARv2[i]<0){
        CV1<-(-1*P2_ARv2[i])
        P2_ARv2[i]<-(0)
        P2_CVv2[i]<-(P2_CVv2[i]-CV1)
        if(P2_CVv2[i]<0){
          CV1<-(-1*P2_CVv2[i])
          P2_CVv2[i]<-(0)
          P2_SWv2[i]<-(P2_SWv2[i]-CV1)
          if(P2_SWv2[i]<0){
            P2_SWv2[i]<-(0)
          }
        }
      }
    }
    
    #EndP1Attk
    
    if((P1_FMv2[i]+P1_ARv2[i]+P1_CVv2[i]+P1_SWv2[i]+P2_FMv2[i]+P2_ARv2[i]+P2_CVv2[i]+P2_SWv2[i])==0){
      resultVec[i]<-0
      break
    }
    if((P1_FMv2[i]+P1_ARv2[i]+P1_CVv2[i]+P1_SWv2[i])==0){
      resultVec[i]<-2
      break
    }
    if((P2_FMv2[i]+P2_ARv2[i]+P2_CVv2[i]+P2_SWv2[i])==0){
      resultVec[i]<-1
      break
    }
    
    
  #EndRank3
    
  #Rank4
    
    
    
    AT1<-(0)
    if ((P1_FMv2[i]+P1_ARv2[i]+P1_CVv2[i]+P1_SWv2[i])>=1){
      AT1<-sample(1:6,min(P1_FMv2[i]+P1_ARv2[i]+P1_CVv2[i]+P1_SWv2[i],2),replace=T)
      AT1<-sort(AT1,decreasing=TRUE)
    }
    else{
      AT1<-(0)
    }
    
    AT2<-(0)
    if ((P2_FMv2[i]+P2_ARv2[i]+P2_CVv2[i]+P2_SWv2[i])>=1){
      AT2<-sample(1:6,min(P2_FMv2[i]+P2_ARv2[i]+P2_CVv2[i]+P2_SWv2[i],3),replace=T)
      AT2<-sort(AT2,decreasing=TRUE)
    }
    else{
      AT2<-(0)
    }
    
    ATT1<-(0)
    ATT2<-(0)
    
 
    
    if(!(is.na(AT2[2])) && !(is.na(AT1[2]))){
      if(AT2[2]>AT1[2]){
        ATT2<-ATT2+1
      }else{
        ATT1<-ATT1+1
      }
    }
    
    if(AT2[1]>AT1[1]){
      ATT2<-ATT2+1
    }else{
      ATT1<-ATT1+1
    }
    
    #P2Attk
    
    P1_FMv2[i]<- (P1_FMv2[i]-ATT2)
    if(P1_FMv2[i]<0){
      ATT2<-(-1*P1_FMv2[i])
      P1_FMv2[i]<-(0)
      P1_ARv2[i]<-(P1_ARv2[i]-ATT2)
      if(P1_ARv2[i]<0){
        ATT2<-(-1*P1_ARv2[i])
        P1_ARv2[i]<-(0)
        P1_CVv2[i]<-(P1_CVv2[i]-ATT2)
        if(P1_CVv2[i]<0){
          ATT2<-(-1*P1_CVv2[i])
          P1_CVv2[i]<-(0)
          P1_SWv2[i]<-(P1_SWv2[i]-ATT2)
          if(P1_SWv2[i]<0){
            P1_SWv2[i]<-(0)
          }
        }
      }
    }
    #EndP2Attk
    
    #P1Attk
    
    
    P2_FMv2[i]<- (P2_FMv2[i]-ATT1)
    if(P2_FMv2[i]<0){
      ATT1<-(-1*P2_FMv2[i])
      P2_FMv2[i]<-(0)
      P2_ARv2[i]<-(P2_ARv2[i]-ATT1)
      if(P2_ARv2[i]<0){
        ATT1<-(-1*P2_ARv2[i])
        P2_ARv2[i]<-(0)
        P2_CVv2[i]<-(P2_CVv2[i]-ATT1)
        if(P2_CVv2[i]<0){
          ATT1<-(-1*P2_CVv2[i])
          P2_CVv2[i]<-(0)
          P2_SWv2[i]<-(P2_SWv2[i]-ATT1)
          if(P2_SWv2[i]<0){
            P2_SWv2[i]<-(0)
          }
        }
      }
    }
    
    #EndP1Attk
    
    if((P1_FMv2[i]+P1_ARv2[i]+P1_CVv2[i]+P1_SWv2[i]+P2_FMv2[i]+P2_ARv2[i]+P2_CVv2[i]+P2_SWv2[i])==0){
      resultVec[i]<-0
      break
    }
    if((P1_FMv2[i]+P1_ARv2[i]+P1_CVv2[i]+P1_SWv2[i])==0){
      resultVec[i]<-2
      break
    }
    if((P2_FMv2[i]+P2_ARv2[i]+P2_CVv2[i]+P2_SWv2[i])==0){
      resultVec[i]<-1
      break
    }
    
    
  #EndRank4
    
    
  }
  
}

Prob1Wins=sum(1*(resultVec==1))/Sims
Prob2Wins=sum(1*(resultVec==2))/Sims
ProbTie=sum(1*(resultVec==0))/Sims

Prob1Wins
Prob2Wins
ProbTie

hist(resultVec,freq=TRUE)
