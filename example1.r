#Here I picked 2 by 3 table
#with column sums c=(4,4,4) and row sums r=(6,6).

#A=matrix(c(1,0,0,1,0,0,
#           0,1,0,0,1,0,
#           0,0,1,0,0,1,
#           1,1,1,0,0,0,
#           0,0,0,1,1,1),nrow=5,ncol=6,byrow=T)

#initial node
#u=matrix(c(2,2,2,2,2,2),nrow=1,ncol=6)

#M=matrix(c(1 , 0, 1,
#           0 , 1,-1,
#           -1,-1, 0,
#           -1 , 0,-1,
#           0 ,-1, 1,
#           1, 1, 0),nrow=6,ncol=3,byrow=T)

#f=function(X){
#  return(prod(X))
#}

A=matrix(c(1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,
           0,0,0,0,1,1,1,1,0,0,0,0,0,0,0,0,
           0,0,0,0,0,0,0,0,1,1,1,1,0,0,0,0,
           0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,
           1,0,0,0,1,0,0,0,1,0,0,0,1,0,0,0,
           0,1,0,0,0,1,0,0,0,1,0,0,0,1,0,0,
           0,0,1,0,0,0,1,0,0,0,1,0,0,0,1,0,
           0,0,0,1,0,0,0,1,0,0,0,1,0,0,0,1),nrow=8,ncol=16,byrow=T)

u=matrix(c(2,0,0,0,1,0,0,0,0,1,0,0,0,0,0,0),nrow=1,ncol=16)

M=markov(A)


#This time assume that f is the product of all values in the table.
f=function(Y){
    #make case distinction
    if (all(Y==c(0,1,1,0,1,0,0,0,1,0,0,0,0,0,0,0))) {
      value=-0.475963
    } else if (all(Y==c(1,1,0,0,0,0,1,0,1,0,0,0,0,0,0,0))){
      value=-.162235
    } else if (all(Y==c(1,1,0,0,1,0,0,0,0,0,1,0,0,0,0,0))){
      value=-.162235
    } else if (all(Y==c(1,0,1,0,0,1,0,0,1,0,0,0,0,0,0,0))){
      value=-.162235
    } else if (all(Y==c(1,0,1,0,1,0,0,0,0,1,0,0,0,0,0,0))){
      value=-.162235
    } else {
      value= .553905
    }
    return(value)
} #close function


#Let TT denote how long the "long time"=T
TT=1000

#Let n denote the "small number" in step 3. Let's just say n=10 for now.
#n should be so that 2n-1<TT, otherwise we have a division-by-zero
n=10

estimateSLEM(A,u,M,f,TT,n)
