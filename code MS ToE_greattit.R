# Projections with all sources of uncertainties ---------------------------


# Project laying dates under the RCP 8.5 scenario -------------------------


# lay date = 110.9803 (se 0.5821) - 4.9471 (se 0.5904) * Temperature (sigma=3.492873)
LDtemp <- read.table("TempWindows.RCP8.5.GreatTit.txt")

int <- rnorm(1000, mean=110.9803, sd=0.5821)
coeff_temp <- rnorm(1000, mean=-4.9471, sd=0.59041)
sigma <- rnorm(1000, mean=0, sd=3.492873)
laydate_proj <- array(NA, c(181,1000,40))

for (r in 1:40)
{
  daa <- subset(LDtemp, LDtemp$Run==r)
  
  for (s in 1:1000)
  {
    laydate_proj[,s,r] <- int[s] + daa$zScore * coeff_temp[s] + sigma[s] 
    
  }
}
  
projRCP8.5LD<-array(NA, dim=c(180,1000,40))
projRCP8.5LD<-laydate_proj[-1,,] ## from 1921 to 2100 



LDest<-matrix(NA,nrow=180,ncol=8)
for (t in 1:180)
{
  LDest[t,1:7]<-quantile(projRCP8.5LD[t,,],c(0.025, 0.25, 0.17, 0.5, 0.75, 0.83, 0.975), na.rm=T) 
}
LDest[,8]<-1921:2100
colnames(LDest)<-c("lCI", "lCI05", "lCI66", "Median", "uCI05", "uCI66", "uCI","Year")
LDest<-as.data.frame(LDest)
rownames(LDest) <- NULL



plot(LDest$Median~LDest$Year, type="l",ylab="Laying dates", xlab="Years",ylim=c(70,140),col=1,frame=F, yaxt='n', xlim=c(1905,2100)) # est
axis(2,las=2)
polygon(x = c(1921:2100,2100:1921), y = c(LDest$lCI, LDest$uCI[180:1]), col = grey.colors(3, start = 0.8), border = grey.colors(3, start = 0.8))
polygon(x = c(1921:2100,2100:1921), y = c(LDest$lCI66, LDest$uCI66[180:1]), col = grey.colors(3, start = 0.5), border = grey.colors(3, start = 0.5))
lines(LDest$Median~LDest$Year, col=1, type="l")

# Identify the ToE

### 66% CI

yh=LDest[1:30,1:7]# let's consider the time period 1921-1950 as the "historical period"
yf=LDest[31:180,1:7]

yhL = yh$lCI66
yfH= (yf$uCI66) 

ToE<- function (low_threshold) 
  
{
  
  for (t in 1:length(yfH))
  {
    if (yfH[t] < low_threshold) print (t) 
    else 
      print(NA) 
  }
} 
ToE(mean(yhL)) 


# Project food peak under the RCP 8.5 scenario -------------------------

# food peak = 138.3787 (se 0.6287) - 7.1615 (se 0.6289) * Temperature (sigma=3.719198)
FPtemp <- read.table("TempWindows.RCP8.5.WinterMoth.txt")

int <- rnorm(1000, mean=138.3787, sd=0.6287)
coeff_temp <- rnorm(1000, mean=-7.1615, sd=0.6289)
sigma <- rnorm(1000, mean=0, sd=3.719198)
FP_proj <- array(NA, c(181,1000,40))

for (r in 1:40)
{
  daa <- subset(FPtemp, FPtemp$Run==r)
  
  for (s in 1:1000)
  {
    FP_proj[,s,r] <- int[s] + daa$zScore * coeff_temp[s] + sigma[s] 
    
  }
}

projRCP8.5FP<-array(NA, dim=c(180,1000,40))
projRCP8.5FP<-FP_proj[-1,,] ## from 1921 to 2100



FPest<-matrix(NA,nrow=180,ncol=8)
for (t in 1:180)
{
  FPest[t,1:7]<-quantile(projRCP8.5FP[t,,],c(0.025, 0.25, 0.17, 0.5, 0.75, 0.83, 0.975), na.rm=T) 
}
FPest[,8]<-1921:2100
colnames(FPest)<-c("lCI", "lCI05", "lCI66", "Median", "uCI05", "uCI66", "uCI","Year")
FPest<-as.data.frame(FPest)
rownames(FPest) <- NULL



plot(FPest$Median~FPest$Year, type="l",ylab="Food peak", xlab="Years",ylim=c(70,180),col=1,frame=F, yaxt='n', xlim=c(1905,2100)) # est
axis(2,las=2)
polygon(x = c(1921:2100,2100:1921), y = c(FPest$lCI, FPest$uCI[180:1]), col = grey.colors(3, start = 0.8), border = grey.colors(3, start = 0.8))
polygon(x = c(1921:2100,2100:1921), y = c(FPest$lCI66, FPest$uCI66[180:1]), col = grey.colors(3, start = 0.5), border = grey.colors(3, start = 0.5))
lines(FPest$Median~FPest$Year, col=1, type="l")

# Identify the ToE

### 66% CI

yh=FPest[1:30,1:7]# let's consider the time period 1921-1950 as the "historical period"
yf=FPest[31:180,1:7]

yhL = yh$lCI66
yfH= (yf$uCI66) 

ToE<- function (low_threshoFP) 
  
{
  
  for (t in 1:length(yfH))
  {
    if (yfH[t] < low_threshoFP) print (t) 
    else 
      print(NA) 
  }
} 
ToE(mean(yhL)) 



# Project mismatch under the RCP 8.5 scenario -------------------------

# mismatch = laying date - food peak + 33
projRCP8.5mism <- projRCP8.5LD - projRCP8.5FP + 33

mismest<-matrix(NA,nrow=180,ncol=8)
for (t in 1:180)
{
  mismest[t,1:7]<-quantile(projRCP8.5mism[t,,],c(0.025, 0.25, 0.17, 0.5, 0.75, 0.83, 0.975), na.rm=T) 
}
mismest[,8]<-1921:2100
colnames(mismest)<-c("lCI", "lCI05", "lCI66", "Median", "uCI05", "uCI66", "uCI","Year")
mismest<-as.data.frame(mismest)
rownames(mismest) <- NULL



plot(mismest$Median~mismest$Year, type="l",ylab="Mismatch", xlab="Years",ylim=c(-20,30),col=1,frame=F, yaxt='n', xlim=c(1905,2100)) # est
axis(2,las=2)
polygon(x = c(1921:2100,2100:1921), y = c(mismest$lCI, mismest$uCI[180:1]), col = grey.colors(3, start = 0.8), border = grey.colors(3, start = 0.8))
polygon(x = c(1921:2100,2100:1921), y = c(mismest$lCI66, mismest$uCI66[180:1]), col = grey.colors(3, start = 0.5), border = grey.colors(3, start = 0.5))
lines(mismest$Median~mismest$Year, col=1, type="l")

# Identify the ToE

### 66% CI

yh=mismest[1:30,1:7] # let's consider the time period 1921-1950 as the "historical period"
yf=mismest[31:180,1:7]

yhL = yh$uCI66
yfH= (yf$lCI66) 

ToE<- function (low_threshomism) 
  
{
  
  for (t in 1:length(yfH))
  {
    if (yfH[t] > low_threshomism) print (t) 
    else 
      print(NA) 
  }
} 
ToE(mean(yhL)) 


# Simulate projections of BCI in the future -------------------------------

library(poLCA)

## 1000 draws from a three-category multinomial distribution. Simulate an increasing frequency of BIC of level 1 (bad years)

BCI_proj_1<-matrix(NA, nrow=180, ncol=1000)
n <- 180

  for (i in 1:180)
  {
    freq1<-i/200
    freq2<-0.1
    freq3<-1-freq1-freq2
    pp<-c(freq1,freq2,freq3) ## increasing frequency of bad years
    p2 <- matrix(pp,nrow=1000,ncol=length(pp),byrow=TRUE)
    rmdraws <- rmulti(p2)
    BCI_proj_1[i,] <- rmdraws 

  }

saveRDS(BCI_proj_1,"BCI_proj_stoch_badyears_class.rds")



## 1000 draws from a three-category multinomial distribution. Simulate an increasing frequency of level 3 (good years)

BCI_proj_3<-matrix(NA, nrow=180, ncol=1000)
n <- 180

for (i in 1:180)
{
  freq3<-i/200
  freq2<-0.1
  freq1<-1-freq2-freq3
  pp<-c(freq1,freq2,freq3) ## increasing frequency of good years
  p2 <- matrix(pp,nrow=1000,ncol=length(pp),byrow=TRUE)
  rmdraws <- rmulti(p2)
  BCI_proj_3[i,] <- rmdraws 
  
}
saveRDS(BCI_proj_3,"BCI_proj_stoch_goodyears_class.rds")
## Store this - this will be BCI projections (categorical) WITH all sources of uncertainties 


# Project pop size and vital rates with simulated BCI ---------------------

################################################ Scenario 1 with increasing frequency of bad years

projBCIpast<-readRDS("BCI_proj_stoch_badyears_class.rds") # BCI projections (categorical effects) WITH all sources of uncertainties 


Nz<- array(NA,c(5,181,40,100)) 
Nvec<- array(NA,c(5,181,40,100))
Ntot_pred<- array(NA,c(181,40,100))
epsilonsInt<-array(NA, dim=c(180,8,100))
surv1<-surv2<-surv3<-surv4<-recrut1<-recrut2<-recrut3<-recrut4<-immm<-array(NA,c(181,40,100))

## All initial population sizes and all vital rates (immigration, survival and recruitment) are estimated with an integrated population model, fully described in Gamelon et al. 2016 Ecology. Estimates are provided in the file "output_IPM.txt".

for (s in 1:100)
{ 
  Nz[1,1,,s]<-localN1[13,2]
  Nz[2,1,,s]<-Nt[13,2] -  localN1[13,2] -localN2[13,2] - localN3[13,2] -localN4[13,2]
  Nz[3,1,,s]<-N2[13,2]
  Nz[4,1,,s]<-N3[13,2]
  Nz[5,1,,s]<-N4[13,2]
  Ntot_pred[1,,s]<-Nt[13,2]
  
}
Nvec[1:5,1,,]<-Nz[1:5,1,,]


minY <- 1985
maxY <- 2019
yearsn <- minY:maxY


survie.all.ages <- cbind(rep(yearsn,4), c(rep(1,length(yearsn)), rep(2,length(yearsn)),rep(3,length(yearsn)),rep(4,length(yearsn))),c(logit(survi1[13:47,2]),logit(survi2[13:47,2]),logit(survi3[13:47,3]),logit(survi4[13:47,2])),c(log(fec1[13:47,2]),log(fec2[13:47,2]),log(fec3[13:47,2]),log(fec4[13:47,2])),c(Nt[13:47,2] -  localN1[13:47,2] - localN2[13:47,2] - localN3[13:47,2] - localN4[13:47,2]),c(localN1[13:47,2] + localN2[13:47,2] + localN3[13:47,2] + localN4[13:47,2]), rep(dd_red_red[,1],4),rep(Nt[13:47,2]),rep(mismatchok,4),rep(dd_red_red_before[,1],4))  
colnames(survie.all.ages)<-c("year","age","survival","recrut","immi","Nlocal","BCI","Ntotal","Mismatch", "BCIbefore") 
row.names(survie.all.ages)<-c()
survie.all.ages<-as.data.frame(survie.all.ages)
A<-as.factor(survie.all.ages$age)
N<-survie.all.ages$Ntotal[1:length(yearsn)]
N<-rep(N,4)
BEECH<-as.factor(survie.all.ages$BCI[1:length(yearsn)])
BEECH<-rep(BEECH,4)
BEECHbefore<-as.factor(survie.all.ages$BCIbefore[1:length(yearsn)])
BEECHbefore<-rep(BEECHbefore,4)
MISM<-survie.all.ages$Mismatch[1:length(yearsn)]
MISM<-rep(MISM,4)
year<-as.factor(survie.all.ages$year)
Nl<-survie.all.ages$Nlocal[1:length(yearsn)]
Nl<-Nl



data=survie.all.ages
data$N <- scale(N)
data$BEECH <- BEECH
data$BEECHbefore <- BEECHbefore
data$MISM <- MISM
data$A <- A



## Survival  
reg.S<- lme(survival ~ A + BEECH  + BEECH:A, data=data, random= ~ 1|year, method="REML", na.action=na.exclude)



## Recruitment  
reg.R<-lme(recrut ~ A + N + N:A + BEECH  + BEECH:A, data=data, random= ~ 1|year, method="REML", na.action=na.exclude)




## Immigration  

survie.all.agesI <- cbind(yearsn[-1], Nt[14:47,2] -  localN1[14:47,2] - localN2[14:47,2] - localN3[14:47,2] - localN4[14:47,2], localN1[13:46,2] + localN2[13:46,2] + localN3[13:46,2] + localN4[13:46,2], dd_red_red[,1][-35], mismatchok[-35])
colnames(survie.all.agesI)<-c("year","immi","Nlocalbefore","BCIbefore","Mismatchbefore" ) 
row.names(survie.all.agesI)<-c()
survie.all.agesI<-as.data.frame(survie.all.agesI)
NlBEFORE<-survie.all.agesI$Nlocalbefore[1:length(yearsn[-1])]
NlBEFORE<-NlBEFORE
BEECHbefore<-as.factor(survie.all.agesI$BCIbefore[1:length(yearsn[-1])])
BEECHbefore<-BEECHbefore
MISMBEFORE<-survie.all.agesI$Mismatchbefore[1:length(yearsn[-1])]
MISMBEFORE<-MISMBEFORE
year<-as.factor(survie.all.agesI$year)

data=survie.all.agesI
data$NllBEFORE <- scale(NlBEFORE)
data$NllBEFORE <- as.numeric(data$NllBEFORE)
data$BEECHbefore <- BEECHbefore
data$MISMBEFORE <- MISMBEFORE

reg.immi<- glm(round(immi) ~ NllBEFORE + BEECHbefore + MISMBEFORE , family="poisson", data=data, na.action=na.exclude)

coco<-c((ranef(reg.S))[,1][1:6],NA,(ranef(reg.S))[,1][7:34])
kiki<-c((ranef(reg.R))[,1][1:6],NA,(ranef(reg.R))[,1][7:34])

U <- cbind(
  coco + matrix(resid(reg.S), ncol=4),
  kiki + matrix(resid(reg.R), ncol=4))
SigmaInt <- cov(U,use = "complete.obs")


years<-1921:2100


for (k in 1:40) # loop on 40 climate members for the RCP 8.5 scenario 
{
  
  for (s in 1:100) # loop to perform 100 simulations per climate member
  {
    
    epsilonsInt[,,s] <- rmvnorm(length(years), sigma=SigmaInt)
    
    
    for (t in 1:length(years)) # loop over time (180 years, from 1921 to 2100)
    {
      
      tryCatch({
        
        # survival
        A<-data.frame(A=1, BEECH=as.factor(projBCIpast[t,s]))
        B<-data.frame(A=2, BEECH=as.factor(projBCIpast[t,s]))
        C<-data.frame(A=3, BEECH=as.factor(projBCIpast[t,s]))
        D<-data.frame(A=4, BEECH=as.factor(projBCIpast[t,s]))
        ndat.S<-rbind(A,B,C,D)
        ndat.S$A<-as.factor(ndat.S$A)
        ndat.S1<-subset(ndat.S,ndat.S[,1]==1)
        surv1[t,k,s]<- invlogit(predict(reg.S, ndat.S1, level=0)+ epsilonsInt[t,1,s])
        ndat.S2<-subset(ndat.S,ndat.S[,1]==2)
        surv2[t,k,s] <- invlogit(predict(reg.S, ndat.S2, level=0)+ epsilonsInt[t,2,s])
        ndat.S3<-subset(ndat.S,ndat.S[,1]==3)
        surv3[t,k,s] <- invlogit(predict(reg.S, ndat.S3, level=0)+ epsilonsInt[t,3,s])
        ndat.S4<-subset(ndat.S,ndat.S[,1]==4)
        surv4[t,k,s]<- invlogit(predict(reg.S, ndat.S4, level=0)+ epsilonsInt[t,4,s])
        
        
        # repro
        A<-data.frame(A=1, BEECH=as.factor(projBCIpast[t,s]), Ns=Ntot_pred[t,k,s])
        B<-data.frame(A=2, BEECH=as.factor(projBCIpast[t,s]), Ns=Ntot_pred[t,k,s])
        C<-data.frame(A=3, BEECH=as.factor(projBCIpast[t,s]), Ns=Ntot_pred[t,k,s])
        D<-data.frame(A=4, BEECH=as.factor(projBCIpast[t,s]), Ns=Ntot_pred[t,k,s])
        ndat.R<-rbind(A,B,C,D)
        ndat.R$A<-as.factor(ndat.R$A)
        meanN.R <- mean(survie.all.ages$Ntotal[1:length(yearsn)])
        sdN.R <- sd(survie.all.ages$Ntotal[1:length(yearsn)])
        ndat.R$N <-(ndat.R$Ns-meanN.R)/sdN.R
        ndat.R1<-subset(ndat.R,ndat.R[,1]==1)
        recrut1[t,k,s] <- exp(predict(reg.R, ndat.R1, level=0)+ epsilonsInt[t,5,s])
        ndat.R2<-subset(ndat.R,ndat.R[,1]==2)
        recrut2[t,k,s]<- exp(predict(reg.R, ndat.R2, level=0)+ epsilonsInt[t,6,s])
        ndat.R3<-subset(ndat.R,ndat.R[,1]==3)
        recrut3[t,k,s]<- exp(predict(reg.R, ndat.R3, level=0)+ epsilonsInt[t,7,s])
        ndat.R4<-subset(ndat.R,ndat.R[,1]==4)
        recrut4[t,k,s] <- exp(predict(reg.R, ndat.R4, level=0)+ epsilonsInt[t,8,s])
        
        
        Nvec[1,t+1,k,s] <- rpois(1, recrut1[t,k,s] * sum(Nz[1:2,t,k,s]))  + ## recruits age 1
          rpois(1, recrut2[t,k,s] *  Nz[3,t,k,s]) +
          rpois(1, recrut3[t,k,s] *  Nz[4,t,k,s]) +
          rpois(1, recrut4[t,k,s] *  Nz[5,t,k,s]) 
        
        Nvec[3,t+1,k,s] <- rbinom(1, sum(Nz[1:2,t,k,s]), surv1[t,k,s]) ## age 1 surviving into age class 2
        
        
        Nvec[4,t+1,k,s] <-  rbinom(1, Nz[3,t,k,s], surv2[t,k,s]) ## age 2 surviving into age class 3
        
        Nvec[5,t+1,k,s] <-  rbinom(1, Nz[4,t,k,s], surv3[t,k,s]) +  ## age 3 surviving into age class 4
          rbinom(1, Nz[5,t,k,s], surv4[t,k,s])
        
        # Immigration
        ndat.I<-data.frame(BEECHbefore=as.factor(projBCIpast[t,s]), MISMBEFORE=projRCP8.5mism[t,s,k], Ns=Nvec[1,t,k,s]+Nvec[3,t,k,s]+Nvec[4,t,k,s]+Nvec[5,t,k,s]) 
        meanN.I <- mean(survie.all.agesI$Nlocalbefore[1:length(yearsn[-1])])
        sdN.I <- sd(survie.all.agesI$Nlocalbefore[1:length(yearsn[-1])])
        ndat.I$NllBEFORE <-(ndat.I$Ns-meanN.I)/sdN.I
        immm[t,k,s] <- exp(predict(reg.immi, ndat.I))
        
        
        
        Nvec[2,t+1,k,s] <- rpois(1, immm[t,k,s]) ## immigrants
        
        
        Nz[1:5,t+1,k,s] <- Nvec[1:5,t+1,k,s] 
        Ntot_pred[t+1,k,s]<-Nvec[1,t+1,k,s]+Nvec[2,t+1,k,s]+Nvec[3,t+1,k,s]+Nvec[4,t+1,k,s]+Nvec[5,t+1,k,s]
        
      }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
      
    }
    
    cat(s, "\n")
    
    
  }
  
  cat(k, "\n")
}


############################# Estimating ToE for pop size


# Projections

Nest<-matrix(NA,nrow=180,ncol=8)
for (t in 1:180)
{
  Nest[t,1:7]<-quantile(Ntot_pred[t,,], c(0.025, 0.25, 0.17, 0.5, 0.75, 0.83, 0.975), na.rm=T) 
}

Nest[,8]<-1921:2100
colnames(Nest)<-c("lCI", "lCI05", "lCI66", "Mean", "uCI05", "uCI66", "uCI","Year")
Nest<-as.data.frame(Nest)
rownames(Nest) <- NULL


plot(Nest$Mean[-1]~Nest$Year[-1], type="l",ylab="Population size", xlab="Years",ylim=c(0,300),col=1,frame=F, yaxt='n') # est
axis(2,las=2)
polygon(x = c(1922:2100,2100:1922), y = c(Nest$lCI[-1], Nest$uCI[180:2]), col = "lightgrey", border = "lightgrey")
polygon(x = c(1922:2100,2100:1922), y = c(Nest$lCI66[-1], Nest$uCI66[180:2]), col = "darkgrey", border = "darkgrey")
lines(Nest$Mean[-1]~Nest$Year[-1], col=1, type="l")

# Identify the ToE : 66 %

yh=Nest[2:30,1:8] # let's consider the time period 1924-1950 as the "historical period"
yf=Nest[31:180,1:8]

yhL = yh$lCI66
yfH= (yf$uCI66) 

ToE<- function (low_threshold) 
  
{
  
  for (t in 1:length(yfH))
  {
    if (yfH[t] < low_threshold) print (t) 
    else 
      print(NA) 
  }
} 
ToE(mean(yhL)) 


################################# scenario 2: increasing frequency of good years


projBCIpast<-readRDS("BCI_proj_stoch_goodyears_class.rds")



Nz<- array(NA,c(5,181,40,100)) 
Nvec<- array(NA,c(5,181,40,100))
Ntot_pred<- array(NA,c(181,40,100))
epsilonsInt<-array(NA, dim=c(180,8,100))
surv1<-surv2<-surv3<-surv4<-recrut1<-recrut2<-recrut3<-recrut4<-immm<-array(NA,c(181,40,100))

for (s in 1:100)
{ 
  Nz[1,1,,s]<-localN1[13,2]
  Nz[2,1,,s]<-Nt[13,2] -  localN1[13,2] -localN2[13,2] - localN3[13,2] -localN4[13,2]
  Nz[3,1,,s]<-N2[13,2]
  Nz[4,1,,s]<-N3[13,2]
  Nz[5,1,,s]<-N4[13,2]
  Ntot_pred[1,,s]<-Nt[13,2]
  
}
Nvec[1:5,1,,]<-Nz[1:5,1,,]


minY <- 1985
maxY <- 2019
yearsn <- minY:maxY


survie.all.ages <- cbind(rep(yearsn,4), c(rep(1,length(yearsn)), rep(2,length(yearsn)),rep(3,length(yearsn)),rep(4,length(yearsn))),c(logit(survi1[13:47,2]),logit(survi2[13:47,2]),logit(survi3[13:47,3]),logit(survi4[13:47,2])),c(log(fec1[13:47,2]),log(fec2[13:47,2]),log(fec3[13:47,2]),log(fec4[13:47,2])),c(Nt[13:47,2] -  localN1[13:47,2] - localN2[13:47,2] - localN3[13:47,2] - localN4[13:47,2]),c(localN1[13:47,2] + localN2[13:47,2] + localN3[13:47,2] + localN4[13:47,2]), rep(dd_red_red[,1],4),rep(Nt[13:47,2]),rep(mismatchok,4),rep(dd_red_red_before[,1],4)) 
colnames(survie.all.ages)<-c("year","age","survival","recrut","immi","Nlocal","BCI","Ntotal","Mismatch", "BCIbefore") 
row.names(survie.all.ages)<-c()
survie.all.ages<-as.data.frame(survie.all.ages)
A<-as.factor(survie.all.ages$age)
N<-survie.all.ages$Ntotal[1:length(yearsn)]
N<-rep(N,4)
BEECH<-as.factor(survie.all.ages$BCI[1:length(yearsn)])
BEECH<-rep(BEECH,4)
BEECHbefore<-as.factor(survie.all.ages$BCIbefore[1:length(yearsn)])
BEECHbefore<-rep(BEECHbefore,4)
MISM<-survie.all.ages$Mismatch[1:length(yearsn)]
MISM<-rep(MISM,4)
year<-as.factor(survie.all.ages$year)
Nl<-survie.all.ages$Nlocal[1:length(yearsn)]
Nl<-Nl



data=survie.all.ages
data$N <- scale(N)
data$BEECH <- BEECH
data$BEECHbefore <- BEECHbefore
data$MISM <- MISM
data$A <- A



## Survival  
reg.S<- lme(survival ~ A + BEECH  + BEECH:A, data=data, random= ~ 1|year, method="REML", na.action=na.exclude)



## Recruitment  
reg.R<-lme(recrut ~ A + N + N:A + BEECH  + BEECH:A, data=data, random= ~ 1|year, method="REML", na.action=na.exclude)




## Immigration  

survie.all.agesI <- cbind(yearsn[-1], Nt[14:47,2] -  localN1[14:47,2] - localN2[14:47,2] - localN3[14:47,2] - localN4[14:47,2], localN1[13:46,2] + localN2[13:46,2] + localN3[13:46,2] + localN4[13:46,2], dd_red_red[,1][-35], mismatchok[-35])
colnames(survie.all.agesI)<-c("year","immi","Nlocalbefore","BCIbefore","Mismatchbefore" ) 
row.names(survie.all.agesI)<-c()
survie.all.agesI<-as.data.frame(survie.all.agesI)
NlBEFORE<-survie.all.agesI$Nlocalbefore[1:length(yearsn[-1])]
NlBEFORE<-NlBEFORE
BEECHbefore<-as.factor(survie.all.agesI$BCIbefore[1:length(yearsn[-1])])
BEECHbefore<-BEECHbefore
MISMBEFORE<-survie.all.agesI$Mismatchbefore[1:length(yearsn[-1])]
MISMBEFORE<-MISMBEFORE
year<-as.factor(survie.all.agesI$year)

data=survie.all.agesI
data$NllBEFORE <- scale(NlBEFORE)
data$NllBEFORE <- as.numeric(data$NllBEFORE)
data$BEECHbefore <- BEECHbefore
data$MISMBEFORE <- MISMBEFORE

reg.immi<- glm(round(immi) ~ NllBEFORE + BEECHbefore + MISMBEFORE , family="poisson", data=data, na.action=na.exclude)

coco<-c((ranef(reg.S))[,1][1:6],NA,(ranef(reg.S))[,1][7:34])
kiki<-c((ranef(reg.R))[,1][1:6],NA,(ranef(reg.R))[,1][7:34])

U <- cbind(
  coco + matrix(resid(reg.S), ncol=4),
  kiki + matrix(resid(reg.R), ncol=4))
SigmaInt <- cov(U,use = "complete.obs")


years<-1921:2100


for (k in 1:40) # loop on 40 climate members for the RCP 8.5 scenario 
{
  
  for (s in 1:100) # loop for 100 simulations per climate member
  {
    
    epsilonsInt[,,s] <- rmvnorm(length(years), sigma=SigmaInt)
    
    
    for (t in 1:length(years)) # loop over time (180 years, from 1921 to 2100)
    {
      
      tryCatch({
        
        # survival
        A<-data.frame(A=1, BEECH=as.factor(projBCIpast[t,s]))
        B<-data.frame(A=2, BEECH=as.factor(projBCIpast[t,s]))
        C<-data.frame(A=3, BEECH=as.factor(projBCIpast[t,s]))
        D<-data.frame(A=4, BEECH=as.factor(projBCIpast[t,s]))
        ndat.S<-rbind(A,B,C,D)
        ndat.S$A<-as.factor(ndat.S$A)
        ndat.S1<-subset(ndat.S,ndat.S[,1]==1)
        surv1[t,k,s]<- invlogit(predict(reg.S, ndat.S1, level=0)+ epsilonsInt[t,1,s])
        ndat.S2<-subset(ndat.S,ndat.S[,1]==2)
        surv2[t,k,s] <- invlogit(predict(reg.S, ndat.S2, level=0)+ epsilonsInt[t,2,s])
        ndat.S3<-subset(ndat.S,ndat.S[,1]==3)
        surv3[t,k,s] <- invlogit(predict(reg.S, ndat.S3, level=0)+ epsilonsInt[t,3,s])
        ndat.S4<-subset(ndat.S,ndat.S[,1]==4)
        surv4[t,k,s]<- invlogit(predict(reg.S, ndat.S4, level=0)+ epsilonsInt[t,4,s])
        
        
        # repro
        A<-data.frame(A=1, BEECH=as.factor(projBCIpast[t,s]), Ns=Ntot_pred[t,k,s])
        B<-data.frame(A=2, BEECH=as.factor(projBCIpast[t,s]), Ns=Ntot_pred[t,k,s])
        C<-data.frame(A=3, BEECH=as.factor(projBCIpast[t,s]), Ns=Ntot_pred[t,k,s])
        D<-data.frame(A=4, BEECH=as.factor(projBCIpast[t,s]), Ns=Ntot_pred[t,k,s])
        ndat.R<-rbind(A,B,C,D)
        ndat.R$A<-as.factor(ndat.R$A)
        meanN.R <- mean(survie.all.ages$Ntotal[1:length(yearsn)])
        sdN.R <- sd(survie.all.ages$Ntotal[1:length(yearsn)])
        ndat.R$N <-(ndat.R$Ns-meanN.R)/sdN.R
        ndat.R1<-subset(ndat.R,ndat.R[,1]==1)
        recrut1[t,k,s] <- exp(predict(reg.R, ndat.R1, level=0)+ epsilonsInt[t,5,s])
        ndat.R2<-subset(ndat.R,ndat.R[,1]==2)
        recrut2[t,k,s]<- exp(predict(reg.R, ndat.R2, level=0)+ epsilonsInt[t,6,s])
        ndat.R3<-subset(ndat.R,ndat.R[,1]==3)
        recrut3[t,k,s]<- exp(predict(reg.R, ndat.R3, level=0)+ epsilonsInt[t,7,s])
        ndat.R4<-subset(ndat.R,ndat.R[,1]==4)
        recrut4[t,k,s] <- exp(predict(reg.R, ndat.R4, level=0)+ epsilonsInt[t,8,s])
        
        
        Nvec[1,t+1,k,s] <- rpois(1, recrut1[t,k,s] * sum(Nz[1:2,t,k,s]))  + ## recruits age 1
          rpois(1, recrut2[t,k,s] *  Nz[3,t,k,s]) +
          rpois(1, recrut3[t,k,s] *  Nz[4,t,k,s]) +
          rpois(1, recrut4[t,k,s] *  Nz[5,t,k,s]) 
        
        Nvec[3,t+1,k,s] <- rbinom(1, sum(Nz[1:2,t,k,s]), surv1[t,k,s]) ## age 1 surviving into age class 2
        
        
        Nvec[4,t+1,k,s] <-  rbinom(1, Nz[3,t,k,s], surv2[t,k,s]) ## age 2 surviving into age class 3
        
        Nvec[5,t+1,k,s] <-  rbinom(1, Nz[4,t,k,s], surv3[t,k,s]) +  ## age 3 surviving into age class 4
          rbinom(1, Nz[5,t,k,s], surv4[t,k,s])
        
        # Immigration
        ndat.I<-data.frame(BEECHbefore=as.factor(projBCIpast[t,s]), MISMBEFORE=projRCP8.5mism[t,s,k], Ns=Nvec[1,t,k,s]+Nvec[3,t,k,s]+Nvec[4,t,k,s]+Nvec[5,t,k,s]) 
        meanN.I <- mean(survie.all.agesI$Nlocalbefore[1:length(yearsn[-1])])
        sdN.I <- sd(survie.all.agesI$Nlocalbefore[1:length(yearsn[-1])])
        ndat.I$NllBEFORE <-(ndat.I$Ns-meanN.I)/sdN.I
        immm[t,k,s] <- exp(predict(reg.immi, ndat.I))
        
        
        
        Nvec[2,t+1,k,s] <- rpois(1, immm[t,k,s]) ## immigrants
        
        
        Nz[1:5,t+1,k,s] <- Nvec[1:5,t+1,k,s] 
        Ntot_pred[t+1,k,s]<-Nvec[1,t+1,k,s]+Nvec[2,t+1,k,s]+Nvec[3,t+1,k,s]+Nvec[4,t+1,k,s]+Nvec[5,t+1,k,s]
        
      }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
      
    }
    
    cat(s, "\n")
    
    
  }
  
  cat(k, "\n")
}


############################# Estimating ToE for pop size

# Projections

Nest<-matrix(NA,nrow=180,ncol=8)
for (t in 1:180)
{
  Nest[t,1:7]<-quantile(Ntot_pred[t,,], c(0.025, 0.25, 0.17, 0.5, 0.75, 0.83, 0.975), na.rm=T) 
}

Nest[,8]<-1921:2100
colnames(Nest)<-c("lCI", "lCI05", "lCI66", "Mean", "uCI05", "uCI66", "uCI","Year")
Nest<-as.data.frame(Nest)
rownames(Nest) <- NULL


# Identify the ToE : 66 %

yh=Nest[2:30,1:6]# as an example, let's consider the time period 1922-1950 as the "historical period"
yf=Nest[31:180,1:6]

yhL = yh$uCI66
yfH= (yf$lCI66) 

ToE<- function (low_threshold) 
  
{
  
  for (t in 1:length(yfH))
  {
    if (yfH[t] > low_threshold) print (t) 
    else 
      print(NA) 
  }
} 
ToE(mean(yhL)) 









# Projections with only climate uncertainty -------------------------------


# Project laying dates under the RCP 8.5 scenario -------------------------


# lay date = 110.9803 (se 0.5821) - 4.9471 (se 0.5904) * Temperature (sigma=3.492873)
LDtemp <- read.table("TempWindows.RCP8.5.GreatTit.txt")

int <- 110.9803
coeff_temp <- -4.9471
sigma <- 0
laydate_proj <- matrix(NA, nrow=181, ncol=40)

for (r in 1:40)
{
  daa <- subset(LDtemp, LDtemp$Run==r)
  
    laydate_proj[,r] <- int + daa$zScore * coeff_temp + sigma 
    
}

projRCP8.5LD<-matrix(NA, nrow=180, ncol=40)
projRCP8.5LD<-laydate_proj[-1,] ## from 1921 to 2100 


LDest<-matrix(NA,nrow=180,ncol=8)
for (t in 1:180)
{
  LDest[t,1:7]<-quantile(projRCP8.5LD[t,],c(0.025, 0.25, 0.17, 0.5, 0.75, 0.83, 0.975), na.rm=T) 
}
LDest[,8]<-1921:2100
colnames(LDest)<-c("lCI", "lCI05", "lCI66", "Median", "uCI05", "uCI66", "uCI","Year")
LDest<-as.data.frame(LDest)
rownames(LDest) <- NULL

## Plot all trajectories

for (t in 1:180)
{
  LDest[t,4]<-mean(projRCP8.5LD[t,], na.rm=T) 
}
colnames(LDest)<-c("lCI", "lCI05", "lCI66", "Mean", "uCI05", "uCI66", "uCI","Year")
LDest<-as.data.frame(LDest)
rownames(LDest) <- NULL

plot(LDest$Mean~LDest$Year, type="l",ylab="Laying dates", xlab="Years",ylim=c(70,140),col=1,frame=F, yaxt='n', xlim=c(1905,2100)) # est
axis(2,las=2)
for (i in 1:40)
{
  lines(projRCP8.5LD[,i] ~ LDest$Year, col=viridis(40)[i], type="l")
}
lines(LDest$Mean~LDest$Year, col=1, type="l", lwd=2)


# Identify the ToE

### 66% CI

yh=LDest[2:30,1:7] # let's consider the time period 1921-1950 as the "historical period"
yf=LDest[31:180,1:7]

yhL = yh$lCI66
yfH= (yf$uCI66) 

ToE<- function (low_threshold) 
  
{
  
  for (t in 1:length(yfH))
  {
    if (yfH[t] < low_threshold) print (t) 
    else 
      print(NA) 
  }
} 
ToE(mean(yhL)) 


# Project food peak under the RCP 8.5 scenario -------------------------

FPtemp <- read.table("TempWindows.RCP8.5.WinterMoth.txt")

int <- 138.3787
coeff_temp <- -7.1615
sigma <- 0
FP_proj <- matrix(NA, nrow=181, ncol=40)

for (r in 1:40)
{
  daa <- subset(FPtemp, FPtemp$Run==r)
    FP_proj[,r] <- int + daa$zScore * coeff_temp + sigma 
    
}

projRCP8.5FP<-matrix(NA, nrow=180, ncol=40)
projRCP8.5FP<-FP_proj[-1,] ## from 1921 to 2100 



FPest<-matrix(NA,nrow=180,ncol=8)
for (t in 1:180)
{
  FPest[t,1:7]<-quantile(projRCP8.5FP[t,],c(0.025, 0.25, 0.17, 0.5, 0.75, 0.83, 0.975), na.rm=T) 
}
FPest[,8]<-1921:2100
colnames(FPest)<-c("lCI", "lCI05", "lCI66", "Median", "uCI05", "uCI66", "uCI","Year")
FPest<-as.data.frame(FPest)
rownames(FPest) <- NULL


## Plot all trajectories

for (t in 1:180)
{
  FPest[t,4]<-mean(projRCP8.5FP[t,], na.rm=T) 
}
colnames(FPest)<-c("lCI", "lCI05", "lCI66", "Mean", "uCI05", "uCI66", "uCI","Year")
FPest<-as.data.frame(FPest)
rownames(FPest) <- NULL

plot(FPest$Mean~FPest$Year, type="l",ylab="Food peak", xlab="Years",ylim=c(70,180),col=1,frame=F, yaxt='n', xlim=c(1905,2100)) # est
axis(2,las=2)
for (i in 1:40)
{
  lines(projRCP8.5FP[,i] ~ FPest$Year, col=viridis(40)[i], type="l")
}
lines(FPest$Mean~FPest$Year, col=1, type="l", lwd=2)


# Identify the ToE

### 66% CI

yh=FPest[1:30,1:7] # let's consider the time period 1921-1950 as the "historical period"
yf=FPest[31:180,1:7]

yhL = yh$lCI66
yfH= (yf$uCI66) 

ToE<- function (low_threshoFP) 
  
{
  
  for (t in 1:length(yfH))
  {
    if (yfH[t] < low_threshoFP) print (t) 
    else 
      print(NA) 
  }
} 
ToE(mean(yhL)) 



# Project mismatch under the RCP 8.5 scenario -------------------------

# mismatch = laying date - food peak + 33
projRCP8.5mism <- projRCP8.5LD - projRCP8.5FP + 33

mismest<-matrix(NA,nrow=180,ncol=8)
for (t in 1:180)
{
  mismest[t,1:7]<-quantile(projRCP8.5mism[t,],c(0.025, 0.25, 0.17, 0.5, 0.75, 0.83, 0.975), na.rm=T) 
}
mismest[,8]<-1921:2100
colnames(mismest)<-c("lCI", "lCI05", "lCI66", "Median", "uCI05", "uCI66", "uCI","Year")
mismest<-as.data.frame(mismest)
rownames(mismest) <- NULL

## Plot all trajectories

for (t in 1:180)
{
  mismest[t,4]<-mean(projRCP8.5mism[t,], na.rm=T) 
}
colnames(mismest)<-c("lCI", "lCI05", "lCI66", "Mean", "uCI05", "uCI66", "uCI","Year")
mismest<-as.data.frame(mismest)
rownames(mismest) <- NULL

plot(mismest$Mean~mismest$Year, type="l",ylab="Mismatch", xlab="Years",ylim=c(-20,30),col=1,frame=F, yaxt='n', xlim=c(1905,2100)) 
axis(2,las=2)
for (i in 1:40)
{
  lines(projRCP8.5mism[,i] ~ mismest$Year, col=viridis(40)[i], type="l")
}
lines(mismest$Mean~mismest$Year, col=1, type="l", lwd=2)


# Identify the ToE

### 66% CI

yh=mismest[1:30,1:7] # let's consider the time period 1921-1950 as the "historical period"
yf=mismest[31:180,1:7]

yhL = yh$uCI66
yfH= (yf$lCI66) 

ToE<- function (low_threshomism) 
  
{
  
  for (t in 1:length(yfH))
  {
    if (yfH[t] > low_threshomism) print (t) 
    else 
      print(NA) 
  }
} 
ToE(mean(yhL)) 



# Project pop size under the RCP 8.5 scenario and simulated scenarios of BCI  -------------------------


####################### Scenario 2: increasing frequency of BCI level 3

projBCIpast<-readRDS("BCI_proj_stoch_goodyears_class.rds")


Nz<- array(NA,c(5,181,40)) 
Nvec<- array(NA,c(5,181,40))
Ntot_pred<- matrix(NA,nrow=181, ncol=40)
surv1<- matrix(NA,nrow=181, ncol=40)
surv2<- matrix(NA,nrow=181, ncol=40)
surv3<- matrix(NA,nrow=181, ncol=40)
surv4<- matrix(NA,nrow=181, ncol=40)
recrut1<- matrix(NA,nrow=181, ncol=40)
recrut2<- matrix(NA,nrow=181, ncol=40)
recrut3<- matrix(NA,nrow=181, ncol=40)
recrut4<- matrix(NA,nrow=181, ncol=40)
immm<-matrix(NA,nrow=181, ncol=40)


Nz[1,1,]<-localN1[13,2]
Nz[2,1,]<-Nt[13,2] -  localN1[13,2] -localN2[13,2] - localN3[13,2] -localN4[13,2]
Nz[3,1,]<-N2[13,2]
Nz[4,1,]<-N3[13,2]
Nz[5,1,]<-N4[13,2]
Ntot_pred[1,]<-Nt[13,2]
Nvec[1:5,1,]<-Nz[1:5,1,]


minY <- 1985
maxY <- 2019
yearsn <- minY:maxY


survie.all.ages <- cbind(rep(yearsn,4), c(rep(1,length(yearsn)), rep(2,length(yearsn)),rep(3,length(yearsn)),rep(4,length(yearsn))),c(logit(survi1[13:47,2]),logit(survi2[13:47,2]),logit(survi3[13:47,3]),logit(survi4[13:47,2])),c(log(fec1[13:47,2]),log(fec2[13:47,2]),log(fec3[13:47,2]),log(fec4[13:47,2])),c(Nt[13:47,2] -  localN1[13:47,2] - localN2[13:47,2] - localN3[13:47,2] - localN4[13:47,2]),c(localN1[13:47,2] + localN2[13:47,2] + localN3[13:47,2] + localN4[13:47,2]), rep(dd_red_red[,1],4),rep(Nt[13:47,2]),rep(mismatchok,4),rep(dd_red_red_before[,1],4))  
colnames(survie.all.ages)<-c("year","age","survival","recrut","immi","Nlocal","BCI","Ntotal","Mismatch", "BCIbefore") 
row.names(survie.all.ages)<-c()
survie.all.ages<-as.data.frame(survie.all.ages)
A<-as.factor(survie.all.ages$age)
N<-survie.all.ages$Ntotal[1:length(yearsn)]
N<-rep(N,4)
BEECH<-as.factor(survie.all.ages$BCI[1:length(yearsn)])
BEECH<-rep(BEECH,4)
BEECHbefore<-as.factor(survie.all.ages$BCIbefore[1:length(yearsn)])
BEECHbefore<-rep(BEECHbefore,4)
MISM<-survie.all.ages$Mismatch[1:length(yearsn)]
MISM<-rep(MISM,4)
year<-as.factor(survie.all.ages$year)
Nl<-survie.all.ages$Nlocal[1:length(yearsn)]
Nl<-Nl



data=survie.all.ages
data$N <- scale(N)
data$BEECH <- BEECH
data$BEECHbefore <- BEECHbefore
data$MISM <- MISM
data$A <- A



## Survival  
reg.S<- lme(survival ~ A + BEECH  + BEECH:A, data=data, random= ~ 1|year, method="REML", na.action=na.exclude)



## Recruitment  
reg.R<-lme(recrut ~ A + N + N:A + BEECH  + BEECH:A, data=data, random= ~ 1|year, method="REML", na.action=na.exclude)




## Immigration  

survie.all.agesI <- cbind(yearsn[-1], Nt[14:47,2] -  localN1[14:47,2] - localN2[14:47,2] - localN3[14:47,2] - localN4[14:47,2], localN1[13:46,2] + localN2[13:46,2] + localN3[13:46,2] + localN4[13:46,2], dd_red_red[,1][-35], mismatchok[-35])
colnames(survie.all.agesI)<-c("year","immi","Nlocalbefore","BCIbefore","Mismatchbefore" ) 
row.names(survie.all.agesI)<-c()
survie.all.agesI<-as.data.frame(survie.all.agesI)
NlBEFORE<-survie.all.agesI$Nlocalbefore[1:length(yearsn[-1])]
NlBEFORE<-NlBEFORE
BEECHbefore<-as.factor(survie.all.agesI$BCIbefore[1:length(yearsn[-1])])
BEECHbefore<-BEECHbefore
MISMBEFORE<-survie.all.agesI$Mismatchbefore[1:length(yearsn[-1])]
MISMBEFORE<-MISMBEFORE
year<-as.factor(survie.all.agesI$year)

data=survie.all.agesI
data$NllBEFORE <- scale(NlBEFORE)
data$NllBEFORE <- as.numeric(data$NllBEFORE)
data$BEECHbefore <- BEECHbefore
data$MISMBEFORE <- MISMBEFORE

reg.immi<- glm(round(immi) ~ NllBEFORE + BEECHbefore + MISMBEFORE , family="poisson", data=data, na.action=na.exclude)


years<-1921:2100

for (k in 1:40) # loop on 40 climate members for the RCP 8.5 scenario 
{
  
  for (t in 1:length(years)) # loop over time (100 years, from 1921 to 2100)
  {
    
    tryCatch({
      
      # survival
      A<-data.frame(A=1, BEECH=as.factor(projBCIpast[t,k]))
      B<-data.frame(A=2, BEECH=as.factor(projBCIpast[t,k]))
      C<-data.frame(A=3, BEECH=as.factor(projBCIpast[t,k]))
      D<-data.frame(A=4, BEECH=as.factor(projBCIpast[t,k]))
      ndat.S<-rbind(A,B,C,D)
      ndat.S$A<-as.factor(ndat.S$A)
      ndat.S1<-subset(ndat.S,ndat.S[,1]==1)
      surv1[t,k]<- invlogit(predict(reg.S, ndat.S1, level=0))
      ndat.S2<-subset(ndat.S,ndat.S[,1]==2)
      surv2[t,k] <- invlogit(predict(reg.S, ndat.S2, level=0))
      ndat.S3<-subset(ndat.S,ndat.S[,1]==3)
      surv3[t,k] <- invlogit(predict(reg.S, ndat.S3, level=0))
      ndat.S4<-subset(ndat.S,ndat.S[,1]==4)
      surv4[t,k]<- invlogit(predict(reg.S, ndat.S4, level=0))
      
      
      # repro
      A<-data.frame(A=1, BEECH=as.factor(projBCIpast[t,k]), Ns=Ntot_pred[t,k])
      B<-data.frame(A=2, BEECH=as.factor(projBCIpast[t,k]), Ns=Ntot_pred[t,k])
      C<-data.frame(A=3, BEECH=as.factor(projBCIpast[t,k]), Ns=Ntot_pred[t,k])
      D<-data.frame(A=4, BEECH=as.factor(projBCIpast[t,k]), Ns=Ntot_pred[t,k])
      ndat.R<-rbind(A,B,C,D)
      ndat.R$A<-as.factor(ndat.R$A)
      meanN.R <- mean(survie.all.ages$Ntotal[1:length(yearsn)])
      sdN.R <- sd(survie.all.ages$Ntotal[1:length(yearsn)])
      ndat.R$N <-(ndat.R$Ns-meanN.R)/sdN.R
      ndat.R1<-subset(ndat.R,ndat.R[,1]==1)
      recrut1[t,k] <- exp(predict(reg.R, ndat.R1, level=0))
      ndat.R2<-subset(ndat.R,ndat.R[,1]==2)
      recrut2[t,k]<- exp(predict(reg.R, ndat.R2, level=0))
      ndat.R3<-subset(ndat.R,ndat.R[,1]==3)
      recrut3[t,k]<- exp(predict(reg.R, ndat.R3, level=0))
      ndat.R4<-subset(ndat.R,ndat.R[,1]==4)
      recrut4[t,k] <- exp(predict(reg.R, ndat.R4, level=0))
      
      
      Nvec[1,t+1,k] <- recrut1[t,k] * sum(Nz[1:2,t,k])  + ## recruits age 1
        recrut2[t,k] *  Nz[3,t,k] +
        recrut3[t,k] *  Nz[4,t,k] +
        recrut4[t,k] *  Nz[5,t,k] 
      
      Nvec[3,t+1,k] <- sum(Nz[1:2,t,k])* surv1[t,k] ## age 1 surviving into age class 2
      
      
      Nvec[4,t+1,k] <-  Nz[3,t,k]* surv2[t,k] ## age 2 surviving into age class 3
      
      Nvec[5,t+1,k] <-  Nz[4,t,k]* surv3[t,k] +  ## age 3 surviving into age class 4
        Nz[5,t,k]* surv4[t,k]
      
      # Immigration
      ndat.I<-data.frame(BEECHbefore=as.factor(projBCIpast[t,k]), MISMBEFORE=projRCP8.5mism[t,k], Ns=Nvec[1,t,k]+Nvec[3,t,k]+Nvec[4,t,k]+Nvec[5,t,k]) 
      meanN.I <- mean(survie.all.agesI$Nlocalbefore[1:length(yearsn[-1])])
      sdN.I <- sd(survie.all.agesI$Nlocalbefore[1:length(yearsn[-1])])
      ndat.I$NllBEFORE <-(ndat.I$Ns-meanN.I)/sdN.I
      immm[t,k] <- exp(predict(reg.immi, ndat.I))
      
      
      
      Nvec[2,t+1,k] <- immm[t,k] ## immigrants
      
      
      Nz[1:5,t+1,k] <- Nvec[1:5,t+1,k] 
      Ntot_pred[t+1,k]<-Nvec[1,t+1,k]+Nvec[2,t+1,k]+Nvec[3,t+1,k]+Nvec[4,t+1,k]+Nvec[5,t+1,k]
      
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    
  }
  
  cat(k, "\n")
}


############################# Estimating ToE for pop size

# Projections

Nest<-matrix(NA,nrow=180,ncol=8)
for (t in 1:180)
{
  Nest[t,1:7]<-quantile(Ntot_pred[t,], c(0.025, 0.25, 0.17, 0.5, 0.75, 0.83, 0.975), na.rm=T) 
}

for (t in 1:180)
{
  Nest[t,4]<-mean(Ntot_pred[t,], na.rm=T) 
}

Nest[,8]<-1921:2100
colnames(Nest)<-c("lCI", "lCI05", "lCI66", "Mean", "uCI05", "uCI66", "uCI","Year")
Nest<-as.data.frame(Nest)
rownames(Nest) <- NULL

plot(Nest$Mean[-1]~Nest$Year[-1], type="l",ylab="Population size", xlab="Years",ylim=c(0,300),col=1,frame=F, yaxt='n', lwd=2) # est
axis(2,las=2)
for (i in 1:40)
{
  lines(Ntot_pred[-1,i] ~ Nest$Year, col=viridis(40)[i], type="l")
}
lines(Nest$Mean[-1]~Nest$Year[-1], col=1, type="l", lwd=2)


# Identify the ToE : 66 %

yh=Nest[2:30,1:6] # let's consider the time period 1922-1950 as the "historical period"
yf=Nest[31:180,1:6]

yhL = yh$uCI66
yfH= (yf$lCI66) 

ToE<- function (low_threshold) 
  
{
  
  for (t in 1:length(yfH))
  {
    if (yfH[t] > low_threshold) print (t) 
    else 
      print(NA) 
  }
} 
ToE(mean(yhL)) 


################################# scenario 1: decreasing frequency of good years


projBCIpast<-readRDS("BCI_proj_stoch_badyears_class.rds")


Nz<- array(NA,c(5,181,40)) 
Nvec<- array(NA,c(5,181,40))
Ntot_pred<- matrix(NA,nrow=181, ncol=40)
surv1<- matrix(NA,nrow=181, ncol=40)
surv2<- matrix(NA,nrow=181, ncol=40)
surv3<- matrix(NA,nrow=181, ncol=40)
surv4<- matrix(NA,nrow=181, ncol=40)
recrut1<- matrix(NA,nrow=181, ncol=40)
recrut2<- matrix(NA,nrow=181, ncol=40)
recrut3<- matrix(NA,nrow=181, ncol=40)
recrut4<- matrix(NA,nrow=181, ncol=40)
immm<-matrix(NA,nrow=181, ncol=40)


Nz[1,1,]<-localN1[13,2]
Nz[2,1,]<-Nt[13,2] -  localN1[13,2] -localN2[13,2] - localN3[13,2] -localN4[13,2]
Nz[3,1,]<-N2[13,2]
Nz[4,1,]<-N3[13,2]
Nz[5,1,]<-N4[13,2]
Ntot_pred[1,]<-Nt[13,2]
Nvec[1:5,1,]<-Nz[1:5,1,]


minY <- 1985
maxY <- 2019
yearsn <- minY:maxY


survie.all.ages <- cbind(rep(yearsn,4), c(rep(1,length(yearsn)), rep(2,length(yearsn)),rep(3,length(yearsn)),rep(4,length(yearsn))),c(logit(survi1[13:47,2]),logit(survi2[13:47,2]),logit(survi3[13:47,3]),logit(survi4[13:47,2])),c(log(fec1[13:47,2]),log(fec2[13:47,2]),log(fec3[13:47,2]),log(fec4[13:47,2])),c(Nt[13:47,2] -  localN1[13:47,2] - localN2[13:47,2] - localN3[13:47,2] - localN4[13:47,2]),c(localN1[13:47,2] + localN2[13:47,2] + localN3[13:47,2] + localN4[13:47,2]), rep(dd_red_red[,1],4),rep(Nt[13:47,2]),rep(mismatchok,4),rep(dd_red_red_before[,1],4)) 
colnames(survie.all.ages)<-c("year","age","survival","recrut","immi","Nlocal","BCI","Ntotal","Mismatch", "BCIbefore") 
row.names(survie.all.ages)<-c()
survie.all.ages<-as.data.frame(survie.all.ages)
A<-as.factor(survie.all.ages$age)
N<-survie.all.ages$Ntotal[1:length(yearsn)]
N<-rep(N,4)
BEECH<-as.factor(survie.all.ages$BCI[1:length(yearsn)])
BEECH<-rep(BEECH,4)
BEECHbefore<-as.factor(survie.all.ages$BCIbefore[1:length(yearsn)])
BEECHbefore<-rep(BEECHbefore,4)
MISM<-survie.all.ages$Mismatch[1:length(yearsn)]
MISM<-rep(MISM,4)
year<-as.factor(survie.all.ages$year)
Nl<-survie.all.ages$Nlocal[1:length(yearsn)]
Nl<-Nl



data=survie.all.ages
data$N <- scale(N)
data$BEECH <- BEECH
data$BEECHbefore <- BEECHbefore
data$MISM <- MISM
data$A <- A



## Survival  
reg.S<- lme(survival ~ A + BEECH  + BEECH:A, data=data, random= ~ 1|year, method="REML", na.action=na.exclude)



## Recruitment  
reg.R<-lme(recrut ~ A + N + N:A + BEECH  + BEECH:A, data=data, random= ~ 1|year, method="REML", na.action=na.exclude)




## Immigration  

survie.all.agesI <- cbind(yearsn[-1], Nt[14:47,2] -  localN1[14:47,2] - localN2[14:47,2] - localN3[14:47,2] - localN4[14:47,2], localN1[13:46,2] + localN2[13:46,2] + localN3[13:46,2] + localN4[13:46,2], dd_red_red[,1][-35], mismatchok[-35])
colnames(survie.all.agesI)<-c("year","immi","Nlocalbefore","BCIbefore","Mismatchbefore" ) 
row.names(survie.all.agesI)<-c()
survie.all.agesI<-as.data.frame(survie.all.agesI)
NlBEFORE<-survie.all.agesI$Nlocalbefore[1:length(yearsn[-1])]
NlBEFORE<-NlBEFORE
BEECHbefore<-as.factor(survie.all.agesI$BCIbefore[1:length(yearsn[-1])])
BEECHbefore<-BEECHbefore
MISMBEFORE<-survie.all.agesI$Mismatchbefore[1:length(yearsn[-1])]
MISMBEFORE<-MISMBEFORE
year<-as.factor(survie.all.agesI$year)

data=survie.all.agesI
data$NllBEFORE <- scale(NlBEFORE)
data$NllBEFORE <- as.numeric(data$NllBEFORE)
data$BEECHbefore <- BEECHbefore
data$MISMBEFORE <- MISMBEFORE

reg.immi<- glm(round(immi) ~ NllBEFORE + BEECHbefore + MISMBEFORE , family="poisson", data=data, na.action=na.exclude)


years<-1921:2100

for (k in 1:40) # loop on 40 climate members for the RCP 8.5 scenario 
{
  
  for (t in 1:length(years)) # loop over time (100 years, from 1921 to 2100)
  {
    
    tryCatch({
      
      # survival
      A<-data.frame(A=1, BEECH=as.factor(projBCIpast[t,k]))
      B<-data.frame(A=2, BEECH=as.factor(projBCIpast[t,k]))
      C<-data.frame(A=3, BEECH=as.factor(projBCIpast[t,k]))
      D<-data.frame(A=4, BEECH=as.factor(projBCIpast[t,k]))
      ndat.S<-rbind(A,B,C,D)
      ndat.S$A<-as.factor(ndat.S$A)
      ndat.S1<-subset(ndat.S,ndat.S[,1]==1)
      surv1[t,k]<- invlogit(predict(reg.S, ndat.S1, level=0))
      ndat.S2<-subset(ndat.S,ndat.S[,1]==2)
      surv2[t,k] <- invlogit(predict(reg.S, ndat.S2, level=0))
      ndat.S3<-subset(ndat.S,ndat.S[,1]==3)
      surv3[t,k] <- invlogit(predict(reg.S, ndat.S3, level=0))
      ndat.S4<-subset(ndat.S,ndat.S[,1]==4)
      surv4[t,k]<- invlogit(predict(reg.S, ndat.S4, level=0))
      
      
      # repro
      A<-data.frame(A=1, BEECH=as.factor(projBCIpast[t,k]), Ns=Ntot_pred[t,k])
      B<-data.frame(A=2, BEECH=as.factor(projBCIpast[t,k]), Ns=Ntot_pred[t,k])
      C<-data.frame(A=3, BEECH=as.factor(projBCIpast[t,k]), Ns=Ntot_pred[t,k])
      D<-data.frame(A=4, BEECH=as.factor(projBCIpast[t,k]), Ns=Ntot_pred[t,k])
      ndat.R<-rbind(A,B,C,D)
      ndat.R$A<-as.factor(ndat.R$A)
      meanN.R <- mean(survie.all.ages$Ntotal[1:length(yearsn)])
      sdN.R <- sd(survie.all.ages$Ntotal[1:length(yearsn)])
      ndat.R$N <-(ndat.R$Ns-meanN.R)/sdN.R
      ndat.R1<-subset(ndat.R,ndat.R[,1]==1)
      recrut1[t,k] <- exp(predict(reg.R, ndat.R1, level=0))
      ndat.R2<-subset(ndat.R,ndat.R[,1]==2)
      recrut2[t,k]<- exp(predict(reg.R, ndat.R2, level=0))
      ndat.R3<-subset(ndat.R,ndat.R[,1]==3)
      recrut3[t,k]<- exp(predict(reg.R, ndat.R3, level=0))
      ndat.R4<-subset(ndat.R,ndat.R[,1]==4)
      recrut4[t,k] <- exp(predict(reg.R, ndat.R4, level=0))
      
      
      Nvec[1,t+1,k] <- recrut1[t,k] * sum(Nz[1:2,t,k])  + ## recruits age 1
        recrut2[t,k] *  Nz[3,t,k] +
        recrut3[t,k] *  Nz[4,t,k] +
        recrut4[t,k] *  Nz[5,t,k] 
      
      Nvec[3,t+1,k] <- sum(Nz[1:2,t,k])* surv1[t,k] ## age 1 surviving into age class 2
      
      
      Nvec[4,t+1,k] <-  Nz[3,t,k]* surv2[t,k] ## age 2 surviving into age class 3
      
      Nvec[5,t+1,k] <-  Nz[4,t,k]* surv3[t,k] +  ## age 3 surviving into age class 4
        Nz[5,t,k]* surv4[t,k]
      
      # Immigration
      ndat.I<-data.frame(BEECHbefore=as.factor(projBCIpast[t,k]), MISMBEFORE=projRCP8.5mism[t,k], Ns=Nvec[1,t,k]+Nvec[3,t,k]+Nvec[4,t,k]+Nvec[5,t,k]) 
      meanN.I <- mean(survie.all.agesI$Nlocalbefore[1:length(yearsn[-1])])
      sdN.I <- sd(survie.all.agesI$Nlocalbefore[1:length(yearsn[-1])])
      ndat.I$NllBEFORE <-(ndat.I$Ns-meanN.I)/sdN.I
      immm[t,k] <- exp(predict(reg.immi, ndat.I))
      
      
      
      Nvec[2,t+1,k] <- immm[t,k] ## immigrants
      
      
      Nz[1:5,t+1,k] <- Nvec[1:5,t+1,k] 
      Ntot_pred[t+1,k]<-Nvec[1,t+1,k]+Nvec[2,t+1,k]+Nvec[3,t+1,k]+Nvec[4,t+1,k]+Nvec[5,t+1,k]
      
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    
  }
  
  cat(k, "\n")
}


############################# Estimating ToE for pop size

# Projections

Nest<-matrix(NA,nrow=180,ncol=8)
for (t in 1:180)
{
  Nest[t,1:7]<-quantile(Ntot_pred[t,], c(0.025, 0.25, 0.17, 0.5, 0.75, 0.83, 0.975), na.rm=T) 
}

for (t in 1:180)
{
  Nest[t,4]<-mean(Ntot_pred[t,], na.rm=T) 
}

Nest[,8]<-1921:2100
colnames(Nest)<-c("lCI", "lCI05", "lCI66", "Mean", "uCI05", "uCI66", "uCI","Year")
Nest<-as.data.frame(Nest)
rownames(Nest) <- NULL

plot(Nest$Mean[-1]~Nest$Year[-1], type="l",ylab="Population size", xlab="Years",ylim=c(0,300),col=1,frame=F, yaxt='n', lwd=2) 
axis(2,las=2)
for (i in 1:40)
{
  lines(Ntot_pred[-1,i] ~ Nest$Year, col=viridis(40)[i], type="l")
}
lines(Nest$Mean[-1]~Nest$Year[-1], col=1, type="l", lwd=2)


# Identify the ToE : 66 %

yh=Nest[2:30,1:8] # let's consider the time period 1924-1950 as the "historical period"
yf=Nest[31:180,1:8]

yhL = yh$lCI66
yfH= (yf$uCI66) 

ToE<- function (low_threshold) 
  
{
  
  for (t in 1:length(yfH))
  {
    if (yfH[t] < low_threshold) print (t) 
    else 
      print(NA) 
  }
} 
ToE(mean(yhL)) 
