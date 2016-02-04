########################################
## The algorithm to estimate the SLEM ##
########################################
#test
#Here I picked 2 by 3 table 
#with column sums c=(4,4,4) and row sums r=(6,6).

A=matrix(c(1,0,0,1,0,0,
           0,1,0,0,1,0,
           0,0,1,0,0,1,
           1,1,1,0,0,0,
           0,0,0,1,1,1),nrow=5,ncol=6,byrow=T)

#initial node
u=matrix(c(2,2,2,2,2,2),nrow=1,ncol=6)

M=matrix(c(1 , 0, 1,
           0 , 1,-1,
           -1,-1, 0,
           -1 , 0,-1,
           0 ,-1, 1,
           1, 1, 0),nrow=6,ncol=3,byrow=T)

#This time assume that f is the product of all values in the table.
f=function(f){
  return(prod(f))
}

#Let TT denote how long the "long time"=T
TT=1000

#Let n denote the "small number" in step 3. Let's just say n=10 for now.
#n should be so that 2n-1<TT, otherwise we have a division-by-zero
n=10

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

return(list("A.hat",A.hat,"B.hat",B.hat))
  
}#Close the function estimateSLEM

#This function returns the list of A.hat and B.hat on step 4.
estimateSLEM(A,u,M,f,TT,n)
