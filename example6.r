#Example6

A=matrix(c(1,1,1,1,0,0,0,0,
           0,0,0,0,1,1,1,1,
           1,0,0,0,1,0,0,0,
           0,1,0,0,0,1,0,0,
           0,0,1,0,0,0,1,0,
           0,0,0,1,0,0,0,1),nrow=6,ncol=8,byrow=T)

u=matrix(c(0,3,0,1,2,0,4,4),nrow=1,ncol=8)

M=markov(A)


#This time assume that f is the product of all values in the table.
f=function(Y){
  #make case distinction
  if (all(Y==c(0,3,0,1,2,0,4,4))) {
    value=.0337243
  } else if (all(Y==c(0,2,0,2,2,1,4,3))){
    value=.123493
  } else if (all(Y==c(0,1,0,3,2,2,4,2))){
    value=.27776
  } else if (all(Y==c(0,0,0,4,2,3,4,1))){
    value=.441023
  } else if (all(Y==c(0,3,1,0,2,0,3,5))){
    value=-.0337243
  } else if (all(Y==c(0,2,1,1,2,1,3,4))){
    value=0
  } else if (all(Y==c(0,1,1,2,2,2,3,3))){
    value=.101933
  } else if (all(Y==c(0,0,1,3,2,3,3,2))){
    value=.249914
  } else if (all(Y==c(1,3,0,0,1,0,4,5))){
    value=0
  } else if (all(Y==c(1,2,0,1,1,1,4,4))){
    value=.0396829
  } else if (all(Y==c(1,1,0,2,1,2,4,3))){
    value=.144427
  } else if (all(Y==c(1,0,0,3,1,3,4,2))){
    value=.283163
  } else if (all(Y==c(0,2,2,0,2,1,2,5))){
    value=-.123493
  } else if (all(Y==c(0,1,2,1,2,2,2,4))){
    value=-.101933
  } else if (all(Y==c(0,0,2,2,2,3,2,3))){
    value=0
  } else if (all(Y==c(1,2,1,0,1,1,3,5))){
    value=-.0396829
  } else if (all(Y==c(1,1,1,1,1,2,3,4))){
    value=0
  } else if (all(Y==c(1,0,1,2,1,3,3,3))){
    value=.105341
  } else if (all(Y==c(2,2,0,0,0,1,4,5))){
    value=0
  } else if (all(Y==c(2,1,0,1,0,2,4,4))){
    value=.0490948
  } else if (all(Y==c(2,0,0,2,0,3,4,3))){
    value=.151627
  } else if (all(Y==c(0,1,3,0,2,2,1,5))){
    value=-.27776
  } else if (all(Y==c(1,1,2,0,1,2,2,5))){
    value=-.144427
  } else if (all(Y==c(2,1,1,0,0,2,3,5))){
    value=-.0490948
  } else if (all(Y==c(0,0,3,1,2,3,1,4))){
    value=-.249914
  } else if (all(Y==c(1,0,2,1,1,3,2,4))){
    value=-.105341
  } else if (all(Y==c(2,0,1,1,0,3,3,4))){
    value=0
  } else if (all(Y==c(0,0,4,0,2,3,0,5))){
    value=-.441023
  } else if (all(Y==c(1,0,3,0,1,3,1,5))){
    value=-.283163
  } else if (all(Y==c(2,0,2,0,0,3,2,5))){
    value=-.151627
  }
  return(value)
} #close function


#Let TT denote how long the "long time"=T
TT=10000

#Let n denote the "small number" in step 3. Let's just say n=10 for now.
#n should be so that 2n-1<TT, otherwise we have a division-by-zero
n=10

estimateSLEM(A,u,M,f,TT,n)

