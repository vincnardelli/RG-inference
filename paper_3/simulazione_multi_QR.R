library(ggplot2)
library(dplyr)
library(patchwork)
library(quantreg)


# primo esempio -----------------------------------------------------------
n = 1000
sig=1
beta0 = 2.1
beta1 = 2
set.seed(12345)
X = runif(n, 0, 4)

#heteroskedastic error
eps <- X*rnorm(n,sd=sig) 
Y = beta0 + beta1*X + eps

beta0.90=beta0
# True slope of the .90 quantile function
beta1.90 = beta1 + qnorm(.90,sd=sig) 
beta1.90
library(quantreg)  
fit.90 = rq(Y ~ X, tau = .90)
summary(fit.90)

plot(X,Y)
abline(lm(Y~X), lwd=2, col="blue")
abline(rq(Y~X,tau=.9), lwd=2, col="red")

#lapply(seq(0.1, 0.9, by=0.1), function(tt) abline(rq(Y~X, tau=tt)))


# errore nella rq ---------------------------------------------------------
simulazione_rq <- function(n = 100, u_sigma = 0.75) {
  varX = 2
  varU = u_sigma
  varZ = 1
  x = as.matrix(rnorm(n, 3, varX), ncol = 1) # variabile non osservabile
  z = as.matrix(rnorm(n, 5, varZ), ncol = 1) # variabile osservabile senza errore
  
  u = as.matrix(rnorm(n, 0, varU), ncol = 1) # errore
  
  b0 = 2
  bx = 1
  bz = 3
  
  eps = x[1:n, 1] * rnorm(n, 0, 1)
  y = b0 + bx * x[1:n, 1] + bz * z[1:n, 1] + eps
  w = x + u   # modello classico
  
  dat = data.frame(y = y, w = w, z = z)
  
  tt = .9
  modxz = rq(y ~ x + z, data = dat, tau = tt)
  summary(modxz)
  
  modwz = rq(y ~ w + z, data = dat, tau = tt)
  summary(modwz)
  
}


# true parameters ---------------------------------------------------------
varX = 2
varZ = 1
x = as.matrix(rnorm(n, 3, varX), ncol = 1) # variabile non osservabile
z = as.matrix(rnorm(n, 5, varZ), ncol = 1) # variabile osservabile senza errore

b0 = 2
bx = 1
bz = 3

eps = x[1:n, 1] * rnorm(n, 0, 1)
y = b0 + bx * x[1:n, 1] + bz * z[1:n, 1] + eps
dat = data.frame(y = y, z = z)

tt = .9
modxz = rq(y ~ x + z, data = dat, tau = tt)

# cont'd ------------------------------------------------------------------
true_coef=modxz$coefficients

u_sigma <- seq(0, 5, by=0.2)
n_sim <- 1:500
simulazione <- expand.grid(n_sim = n_sim, u_sigma = u_sigma)

simulazione_lista <- lapply(1:nrow(simulazione), 
                            function(x) simulazione_rq(n=100, u_sigma=simulazione[x, ]$u_sigma))


simulazione$beta0=sapply(simulazione_lista, function(x) coef(x)[1])
simulazione$betaW=sapply(simulazione_lista, function(x) coef(x)[2])
simulazione$betaZ=sapply(simulazione_lista, function(x) coef(x)[3])

beta0 <- simulazione %>% 
  group_by(u_sigma) %>% 
  summarise(beta0_mean = mean(beta0), 
            beta0_min = min(beta0), 
            beta0_max = max(beta0)) %>% 
  ggplot() + 
  geom_ribbon(aes(x= u_sigma, 
                  ymin = beta0_min, 
                  ymax = beta0_max), fill="gray80") +
  geom_line(aes(u_sigma, beta0_mean)) +
  geom_hline(yintercept=true_coef[1], linetype="dotted") +
  theme_minimal() +
  xlab("Variance of Measurement Error") +
  ylab("Beta 0") +
  ggtitle("Beta 0") 

betaW <- simulazione %>% 
  group_by(u_sigma) %>% 
  summarise(betaW_mean = mean(betaW), 
            betaW_min = min(betaW), 
            betaW_max = max(betaW)) %>% 
  ggplot() + 
  geom_ribbon(aes(x= u_sigma, 
                  ymin = betaW_min, 
                  ymax = betaW_max), fill="gray80") +
  geom_line(aes(u_sigma, betaW_mean)) +
  geom_hline(yintercept=true_coef[2], linetype="dotted") +
  theme_minimal() +
  xlab("Variance of Measurement Error") +
  ylab("Beta W") +
  ggtitle("Beta W")

betaZ <- simulazione %>% 
  group_by(u_sigma) %>% 
  summarise(betaZ_mean = mean(betaZ), 
            betaZ_min = min(betaZ), 
            betaZ_max = max(betaZ)) %>% 
  ggplot() + 
  geom_ribbon(aes(x= u_sigma, 
                  ymin = betaZ_min, 
                  ymax = betaZ_max), fill="gray80") +
  geom_line(aes(u_sigma, betaZ_mean)) +
  geom_hline(yintercept=true_coef[3], linetype="dotted") +
  theme_minimal() +
  xlab("Variance of Measurement Error") +
  ylab("Beta Z") +
  ggtitle("Beta Z")


beta0 | betaW | betaZ
