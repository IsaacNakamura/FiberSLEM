########################################
## The algorithm to estimate the SLEM ##
########################################

library("geigen")
library("algstat")

#Function that returns the SLEM of fiber graph
estimateSLEM=function(A,u,M,f,TT,n){

  #pg15 step2

  #Collect samples f(X0),....,f(XTT)

  samples=rep(NA,(TT+1))

  samples[1]=f(u)
  X=u

  for (i in 1:TT){

    while(T){

      s=sample(1:ncol(M),1)
      t=sample(c(1,-1),1)
      move=t*M[,s]

      Xproposal=X+move

      #escape the while loop only if all entries are non-zero
      if (all(Xproposal>=0)){

        X=Xproposal
        break
      } #close if statement
    }#close while loop

    samples[(i+1)]=f(X)
}#close for loop

s=NULL
t=NULL

#Step3 Estimate the autocovariance function
#for at lags s=0,1,...,2n-1.

#Autocovariance function for f
Cf=rep(NA,2*n)
#f.hat(XTT)
f.hat=mean(samples)

for (s in 0:(2*n-1)){

  #Let f.3.0.4 denote (f_t-f.hat_T)(f_(t+1)-f.hat_T) in (3.0.4) page 10
  f.3.0.4=rep(NA,(TT-abs(s)))

  for (t in 1:(TT-abs(s))){
    f.3.0.4[t]=(samples[t]-f.hat)*(samples[t+s]-f.hat)
  } # End of for (t in (1:TT-abs(s)))

  Cf[s+1]=sum(f.3.0.4)/(TT-abs(s))

}# end of for (s in (2*n-1))

#Step4
#Form the matrices A.hat and B.hat.

A.hat=matrix(NA,nrow=n,ncol=n,byrow=T)
B.hat=matrix(NA,nrow=n,ncol=n,byrow=T)

#Fill in A.hat
for (i in 1:n){
  for (j in 1:n){

    A.hat[i,j]=Cf[i+j]

  } #close for loop j
} #close for loop i

#Fill in B.hat
for (i in 1:n){
  for (j in 1:n){

    B.hat[i,j]=Cf[i+j-1]

  } #close for loop j
} #close for loop i

#STEP5

lambda <- geigen(A.hat, B.hat, symmetric=FALSE, only.values=TRUE)
ABS=abs(lambda[[1]])
LGEM=max(ABS)

return(LGEM)

}#Close the function estimateSLEM
