#Example 4 Size of support. How many 0s are in the table?

A=matrix(c(1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,
           0,0,0,0,1,1,1,1,0,0,0,0,0,0,0,0,
           0,0,0,0,0,0,0,0,1,1,1,1,0,0,0,0,
           0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,
           1,0,0,0,1,0,0,0,1,0,0,0,1,0,0,0,
           0,1,0,0,0,1,0,0,0,1,0,0,0,1,0,0,
           0,0,1,0,0,0,1,0,0,0,1,0,0,0,1,0,
           0,0,0,1,0,0,0,1,0,0,0,1,0,0,0,1),nrow=8,ncol=16,byrow=T)

u=matrix(c(0,1,1,0,1,0,0,0,1,0,0,0,0,0,0,0),nrow=1,ncol=16)

M=markov(A)

#This time assume that f is
#the sum of the diagonal elements of the observed table.

f=function(Y){return(sum(Y[]=="0"))}


#Let TT denote how long the "long time"=T
TT=10000

#Let n denote the "small number" in step 3. Let's just say n=10 for now.
#n should be so that 2n-1<TT, otherwise we have a division-by-zero
n=10

estimateSLEM(A,u,M,f,TT,n)
