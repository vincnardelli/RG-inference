library(mice)
library(MASS)
n=100
mu=c(3,5,1)
vars=diag(c(2,1,1.5))

set.seed(123)
Z = mvrnorm(n=n,mu=mu,Sigma=vars)
X=cbind(1,Z)

#y=2+Z_1+3Z_2+.5Z_3
b=c(2,1,3,0.5)
y=X%*%b+rnorm(n,sd=.25)

#patterns specifica dei particolari pattern di risposta, per esempio se vogliamo vedere
#dati mancanti nella prima e nella seconda variabile scriveremo c(1,1,0)
patterns=matrix(c(1,1,0,0,0,1,1,0,1),ncol = 3,byrow = T)

#freq specifica la frequenza di ciascun pattern dato precedentemente
freq=c(.4,.4,.2)
mech="MCAR"

Zmiss=ampute(
  Z,
  prop = 0.3,
  patterns = patterns,
  freq = freq,
  mech = mech
)


