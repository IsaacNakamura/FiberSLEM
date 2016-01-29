########################################
## The algorithm to estimate the SLEM ##
########################################

X=sample(1:6,size=50,replace=T,prob=rep(1/6,6))
hist(X)
1+1
print(X)

#[1/27/16, 1:39:08 PM] Tobias Windisch: estimate(f,A,b,M,n)
#[1/27/16, 1:41:07 PM] Tobias Windisch: M = markov basis of A
#[1/27/16, 1:41:09 PM] Tobias Windisch: A= matrix
#[1/27/16, 1:41:12 PM] Tobias Windisch: b= right-hand side
#[1/27/16, 1:41:37 PM] Tobias Windisch: n= integer number
#[1/27/16, 1:41:46 PM] Tobias Windisch: f= any function which is defined on ZZ^d
#[1/27/16, 1:42:05 PM] Tobias Windisch: \fiber{A}{b}\subset\NN^d
#[1/27/16, 1:42:18 PM] Tobias Windisch: f(x)=1