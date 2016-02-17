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
f=function(f){
    #make case distinction
    if f=c(0,1,1,0,1,0,0,0,1,0,0,0,0,0,0,0) then return 0.475963
  return(sum(f))
}

#Let TT denote how long the "long time"=T
TT=1000

#Let n denote the "small number" in step 3. Let's just say n=10 for now.
#n should be so that 2n-1<TT, otherwise we have a division-by-zero
n=10

