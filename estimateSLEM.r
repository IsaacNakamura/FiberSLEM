########################################
## The algorithm to estimate the SLEM ##
########################################

#Here I picked 2 by 3 table 
#with column sums c=(4,4,4) and row sums r=(6,6).

A=matrix(c(1,0,0,1,0,0,
           0,1,0,0,1,0,
           0,0,1,0,0,1,
           1,1,1,0,0,0,
           0,0,0,1,1,1),nrow=5,ncol=6,byrow=T)

b=c(4,4,4,6,6)

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

estimateSLEM=function(A,b,M,f,TT){
  
  #pg15 step2
  
  #Let the inital state column sums of A
  
  X0=rep(NA,ncol(A))
  
  for (i in 1:ncol(A)){
    X0[i]=sum(A[,i])
  }
  
  #Collect samples f(X0),....,f(XTT)
  
  samples=rep(NA,TT)
  
  samples[1]=f(X0)
  X=X0
  
  for (i in 1:(TT-1)){
    
    while(T){
      
      s=sample(1:ncol(M),1)
      t=sample(c(1,-1),1)
      move=t*M[,s]
      
      X1=X+move
      
      if (all(X1>=0)){
      
        X=X1
        break
      } #close if statement
    }#close while loop
    
    samples[(i+1)]=f(X)
}#close for loop
  
  return(samples)
  
}#Close the function estimateSLEM

estimateSLEM(A,b,M,f,TT)