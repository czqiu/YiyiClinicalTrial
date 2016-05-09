# This is a combination of the study designs we have considered before for Bayesian Up-and-Down
# Matthew study

# two-stage design

#install.packages('coda')
#install.packages('R2WinBUGS')
#install.packages('MCMCpack')
#library(lattice)
#library(coda)
#library(boot)
#library(R2WinBUGS)
#library(MASS) # need to mvrnorm
#install.packages("MCMCpack")
#library(MCMCpack) # need for rwish
# The final2 model is the model that matches the JSM 2014 proceeding
#setwd("/Users/chenyiy/Dropbox/Share\ with\ Yiyi/Matthew\ study/Rshiny")
#setwd("/Users/zunqiu/Dropbox/Share with Yiyi/Matthew study/Rshiny")
#setwd("D:\\Documents\\Dropbox\\My Dropbox\\Dropbox\\Share with Yiyi\\Matthew study\\Rshiny")

#model.file <- system.file("model", "final2.txt", package="R2WinBUGS")
# saved under C:\Users\chenyiy\Documents\R\win-library\3.2\R2WinBUGS\model
#D:\Documents\R\win-library\3.2\R2WinBUGS\model
#file.show(model.file)

#Scenario 1 (With random allocation)
#p.true <- c(0.05, 0.08, 0.1, 0.25, 0.3)
#q.true <- c(0.5, 0.2, 0.05, 0.04, 0.03)
#r.true <- c(0.05, 0.08, 0.1, 0.2, 0.3)
#loss function
#L.true <- p.true+q.true+0.4*r.true

n1=4 # size of a cohort

max = 25 # the number of cohort in both stages 
#( with a cohort size of 4, that means 100 subjects, 28 in stage 1 and up to 72 in stage 2)

#for (repe in 1:20){
#  x <- NULL #adverse events: pin infection 
#  y <- NULL #adverse events: fragment movement 
#  z <- NULL #adverse events: elbow stiffness
#  t <- NULL # t stands for time window
#  j=2 # We start from window 2
#  pickseq <- NULL # we add one more variable because the picked optimal window may not be the actual assigned window
  #the maximum allowed sample size is 90, therefore we run 33 times
#  jseq <- NULL # need to record window for each cohort in each simulation
  # The maximum potential value for max is 33 (stands for 99 subjects)
#  c = 0  
  #Stage 1 (the rule-based Up and down stage, with max sample size of 28 (7 cohorts))
#   for (k in 1:20){
#     # t is the assigned window
#     t <- c(t,rep(j,n1))
#     n <- length(t) # The sample size so far
#     c <- c + 1 # c is the ?th cohort
#     xnew = rbinom(n1,1, p.true[j])
#     ynew = rbinom(n1,1, q.true[j])
#     znew = rbinom(n1,1, r.true[j])
#     x=c(x, xnew)
#     y=c(y, ynew)
#     z=c(z, znew)
#     move = sum(xnew)+0.4*sum(znew)-sum(ynew)
#     #move<0 indicates y happens a lot
#     #move>0 indicates x, z happens a lot
#     if (move < 0) {pickj <- min(j+1, 5) } # the picked window
#     if (move > 0) {pickj <- max(j-1, 1) }
#     if (move == 0) {pickj <- j }
#     
#     pickseq <- c(pickseq, pickj)
#     pickseq
#     j = pickj # start the next loop
#     jseq <- c(jseq,j)
#   }
  
  # pickseq
  # jseq
  # try <- cbind(x,y,z,t)
   
  # Stage 2 (the Bayesian stage)
 # for (d in 1:max) {
 #   t <- c(t,rep(j,n1))
 #   n <- length(t) # The sample size so far
 #   c <- c + 1 # c is the ?th cohort

#write.csv (try,"try.csv")
#Currentdata1 <- read.csv("updated.csv", header=TRUE,  sep = ",",stringsAsFactors = FALSE) 

# Read in data
#setwd("D:\\Documents\\Dropbox\\My Dropbox\\Dropbox\\Share with Yiyi\\Matthew study\\Rshiny")
Currentdata <- read.csv("updated.csv", header=TRUE,  sep = ",",stringsAsFactors = FALSE) 
  x <- Currentdata$Infection
  y <- Currentdata$Movement
  z <- Currentdata$Stiffness
  t <- Currentdata$Window
  n <- length(x)
  Currentdata$ncount <- rep(1,n)
   j = tail(t, n=1)
  
  c <- ceiling(n/n1)
    
#     xnew = rbinom(n1,1, p.true[j])
#     ynew = rbinom(n1,1, q.true[j])
#     znew = rbinom(n1,1, r.true[j])
#     x=c(x, xnew)
#     y=c(y, ynew)
#     z=c(z, znew)

    # Set up hyperparameter values
    #mu0 = as.vector(c(-3,-3,-3))
    #S2 = matrix(c(1,0,0,0,1,0,0,0,1),nrow=3)/1000
    #S3 = matrix(c(1,0,0,0,1,0,0,0,1),nrow=3)/10000
    
#     data<-list("x","y","z","n","t")
#     #inits <- function(){
#     # list( Ob <- matrix(c(rep(0.2,15)),nrow = 3), mu=mvrnorm(1,mu0,matrix(c(10,0,0,0,10,0,0,0,10),nrow=3) ),
#     #        tau = rwish(3,matrix(c(.02,0,0,0,.04,0,0,0,.05),nrow=3)))
#     #}
#     inits <- function(){
#       list( etax=c(-1.1,-1.2,-1.3,-1.4,-1.5), mux=c(-2, -1.9, -1.8, -1.7, -1.6), sigma.etax = 1, etay=c(-1.2,-1.3,-1.4,-1.5,-1.5), muy=c(-1.9, -1.9, -1.8, -1.7, -1.6),sigma.etay = 1, etaz=c(-1.1,-1.2,-1.3,-1.4,-1.5), muz=c(-1.7,-1.6,-1.5,-1.4,-1.5),sigma.etaz = 1)
#     }
#     
#     
#     parameters <- c("p","q","r")
# #     timing.sim <- bugs(data, inits, parameters, model.file,
# #                        n.chains = 1, n.iter = 8000,n.burnin=3000,
# #                        bugs.directory = "c:/WinBUGS14",
# #                        working.directory = NULL)
#     timing.sim <- bugs(data, inits, parameters, model.file,
#                        n.chains = 1, n.iter = 8000,n.burnin=3000,
#                        bugs.directory = "c:/program Files/WinBUGS14",
#                        working.directory = NULL)
    
    
    #C:\Program Files
    # timing.sim <- bugs(data, inits, parameters, model.file,
    #                     n.chains = 1, n.iter = 8000,n.burnin=3000,
    #                    bugs.directory = ""c:/WinBUGS14"",codaPkg=FALSE,
    #                    working.directory = NULL,debug=FALSE)
    
#    attach.bugs(timing.sim)
    # p.mean<-colMeans(p)
    # q.mean<-colMeans(q)
    # r.mean<-colMeans(r)
  px <- aggregate(Currentdata$Infection, by=list(Category=Currentdata$Window), FUN=sum)
  qy <- aggregate(Currentdata$Movement, by=list(Category=Currentdata$Window), FUN=sum)
  rz <- aggregate(Currentdata$Stiffness, by=list(Category=Currentdata$Window), FUN=sum)
  nn <- aggregate(Currentdata$ncount, by=list(Category=Currentdata$Window), FUN=sum)
  
  addnoise <- runif(5,0.000001, 0.001)
  p.mean <- addnoise + px$x/nn$x
  q.mean <- addnoise + qy$x/nn$x
  r.mean <- addnoise + rz$x/nn$x
    L <- p.mean+q.mean+0.4*r.mean
    
    # L is loss for each window
    L[j]=p.mean[j]+q.mean[j]+0.4*r.mean[j]
    a <- max(1, j-1) # a and b are the two neiborghering windows
    b <- min(j+1, 5)
    L[a] = max(0,p.mean[a]+q.mean[a]+0.4*r.mean[a])
    L[b] = max(0,p.mean[b]+q.mean[b]+0.4*r.mean[b])
    #detach.bugs()
    L.sub = L[a:b]
    Lmin = min(L.sub)
    position = which(L == Lmin)
    pickj <- position # position is the picked window

    #pickseq <- c(pickseq, pickj)
    
    # start random allocation to avoid greedy startegy (trapped in suboptimal loop)
    # for the first six cohort, 50% current window and 25% neighboring window if 2 neiboring windows.
    # for the first six cohort, 70% current window and 15% neighboring window if 1 neiboring window
    
    check = 1
   #  check = 0
     pickseq <- t
     # stopping rule;
      if (c>12){
        tt<-rle(pickseq[9:c]) # We need at least 8 cohort in the trial, therefore, start looking at the 9th cohort
       tstop <- tt$values[tt$lengths>=5] # trial will stop if we have four secucessive cohort to one single window
        check <- is.na(tstop[1]) # The window where the trial stops
        #nstop is the total sample size before stopping
        if (!is.na(tstop[1])){
          whichn <- which(tt$length>=5) # findout where the length >= 4
          tt.temp <- tt$length
          tt.temp[whichn]<-5 #replace those cells with 4
          nstop <- sum(8, tt.temp[1:whichn[1]])*n1
          break
        }
      }
    
    if (pickj == 1) { Windownext = '19-21'}
    if (pickj == 2) { Windownext = '22-24'}
    if (pickj == 3) { Windownext = '25-27'}
    if (pickj == 4) { Windownext = '28-30'}
    if (pickj == 5) { Windownext = '31-35'}
    if (check == 0 ) { Windownext = 'No more assignment'}
    if (pickj == j) { Movestep = "Stay in Current"}
    if (pickj > j) { Movestep = "Escalate"}
    if (pickj < j) { Movestep = "De-escalate"}
    if (check == 0 ) { Movestep = 'Stop Trial'}
    #}
    # Need to find the position when the window t stables (allows maximum sample size 99)
    #size.t <- length(t)
    #out <- rle(pickseq==pickseq[size.t]) # rle is run length encoding, to find out the length of sequential for the condition is true.
    #aa <- out$length[out$value] 
    #size.aa <- length(aa)
    #stab.n <- size.t - aa[size.aa]
    #stab.n is sample size that start to reach stablizing (allows maximum sample size 99)
    
    # Should allow early stopping if stablize (same pickj for four secessive chort)
    #jseq
    #pickseq
    #cbind(x,y,z,t)
    
    
    #t.freq <- c(sum(t==1),sum(t==2),sum(t==3),sum(t==4),sum(t==5))
    #t.freq
    # t[size.t] is the stablized time window
    #write(t[size.t],"Sc1stablet.txt",sep="\t",append=T)
    # stab.n is the sample size starting to stablize if the trial allowed to run 99 subjects
    #write(stab.n,"Sc1stablen.txt",sep="\t",append=T)
    # jseq is the assigned window for subjects
    #write(jseq,"Sc1jseq.txt",sep="\t",append=T)
    # pickseq is the actual optimal window pick by each interim analysis
    #write(pickseq,"Sc1pickseq.txt" ,sep="\t",append=T)
    # tstop[1] is the window where the trial stops based on early stopping rule
    #write(tstop[1], "Sc1tstop.txt",sep="\t",append=T)
    # nstop is the total sample size before the trial stops
    #write(n,"Sc1nstop.txt",sep="\t",append=T)
    # t.freq is the freqency table of subjects
    #write(t.freq,"Sc1ttable.txt",sep="\t",append=T)
    # check whether this is the same as nstop
    # write(n,"sc1ncheck.txt", sep="\t",append=T)
    # p, q, r mean should be the estimated value once the trial stopped
    
    #write(p.mean,"Sc1pmean.txt", sep="\t",append=T)
    #write(q.mean,"Sc1qmean.txt", sep="\t",append=T)
    # write(r.mean,"Sc1rmean.txt", sep="\t",append=T)
    #}
    # files stored under "C:\Users\chenyiy\Documents"
    # posterior estimate of p, q and r
    # Sc1pmean<-read.table("C:/Users/chenyiy/Documents/sc1 with random allocation/Sc1pmean.txt",header=FALSE)
    # colMeans(Sc1pmean) 
    # 
    # Sc1qmean<-read.table("C:/Users/chenyiy/Documents/sc1 with random allocation/Sc1qmean.txt",header=FALSE)
    # colMeans(Sc1qmean)
    # Sc1rmean<-read.table("C:/Users/chenyiy/Documents/sc1 with random allocation/Sc1rmean.txt",header=FALSE)
    # colMeans(Sc1rmean)
    # 
    # # Frequency table in each window;
    # Sc1sstablet<-read.table("C:/Users/chenyiy/Documents/sc1 with random allocation/Sc1ttable.txt",header=FALSE)
    # dim(Sc1sstablet)
    # # as the optimal window is t = 3, sum the total row
    # sum(Sc1sstablet[,3])/sum(Sc1sstablet)
    # 
    # sum(Sc1sstablet[,1])/sum(Sc1sstablet)
    # sum(Sc1sstablet[,2])/sum(Sc1sstablet)
    # sum(Sc1sstablet[,4])/sum(Sc1sstablet)
    # sum(Sc1sstablet[,5])/sum(Sc1sstablet)
    # 
    # # How many times the trial with early stopping rule concluded in window 3?
    # Sc1tstop<-read.table("C:/Users/chenyiy/Documents/sc1 with random allocation/Sc1tstop.txt",header=FALSE)
    # table(Sc1tstop)/sum(table(Sc1tstop))
    # Sc1nstop <- read.table("C:/Users/chenyiy/Documents/sc1 with random allocation/Sc1nstop.txt",header=FALSE)
    # table(Sc1nstop)/sum(table(Sc1nstop))
    # summary(Sc1nstop)    
  


