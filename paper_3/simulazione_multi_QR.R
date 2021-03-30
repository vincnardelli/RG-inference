<<<<<<< HEAD
library(ggplot2)
library(dplyr)
library(patchwork)
library(quantreg)


# primo esempio (solo per chiarire) -----------------------------------------------------------
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
simulazione_rq <- function(n = 100, u_sigma = 0.75, tt=.9) {
  varX = 2
  varU = u_sigma
  varZ = 1
  x = as.matrix(rnorm(n, 3, varX), ncol = 1) # Unobservable variable
  z = as.matrix(rnorm(n, 5, varZ), ncol = 1) # Measurable variable (without any error)
  u = as.matrix(rnorm(n, 0, varU), ncol = 1) # Measurement error
  w = x + u                                  # Variable measuring X (with error)
  eps = x[1:n, 1] * rnorm(n, 0, 1)           # Heteroskedastic error
  
  b0 = 2
  bx = 1
  bz = 3
  y = b0 + bx * x[1:n, 1] + bz * z[1:n, 1] + eps
  dat = data.frame(y = y,
                   w = w,
                   z = z,
                   x = x)
  
 # tt = .9
  # modxz = rq(y ~ x + z, data = dat, tau = tt)
  # summary(modxz)
  
  modwz = rq(y ~ w + z, data = dat, tau = tt)
  summary(modwz)
  modwz
}

#true parameters
set.seed(123)
varX = 2
varZ = 1
n=100
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
true_coef=modxz$coefficients

#sim
u_sigma <- seq(0, 5, by=0.2)
n_sim <- 1:500
simulazione <- expand.grid(n_sim = n_sim, u_sigma = u_sigma)

simulazione_lista <- lapply(1:nrow(simulazione), 
                            function(x) simulazione_rq(n=100, u_sigma=simulazione[x, ]$u_sigma , tt=.9))


simulazione$beta0=sapply(simulazione_lista, function(x) coef(x)[1])
simulazione$betaW=sapply(simulazione_lista, function(x) coef(x)[2])
simulazione$betaZ=sapply(simulazione_lista, function(x) coef(x)[3])

se <- parallel::mclapply(simulazione_lista, function(x) summary(x,se = "boot"),
                         mc.cores = parallel::detectCores()-1)
#se <- lapply(simulazione_lista, function(x) summary(x,se = "boot"))    

simulazione$beta0_se = sapply(se, function(x) x$coefficients[,2][1])
simulazione$betaW_se = sapply(se, function(x) x$coefficients[,2][2])
simulazione$betaZ_se = sapply(se, function(x) x$coefficients[,2][3])

x_bound=3

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
  ggtitle("Beta 0") +
  xlim(c(0, x_bound))

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
  ggtitle("Beta W")+
  xlim(c(0, x_bound))

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
  ggtitle("Beta Z")+
  xlim(c(0, x_bound))


beta0 | betaW | betaZ
ggsave("figures/estim_err.pdf", beta0 | betaW | betaZ, height = 4, width = 14)


# plot SE - ---------------------------------------------------
beta0_se <- simulazione %>%
  group_by(u_sigma) %>%
  summarise(beta0_se_mean = mean(beta0_se),
            beta0_se_min = min(beta0_se),
            beta0_se_max = max(beta0_se)) %>%
  ggplot() +
  geom_ribbon(aes(x= u_sigma,
                  ymin = beta0_se_min,
                  ymax = beta0_se_max), fill="gray80") +
  geom_line(aes(u_sigma, beta0_se_mean)) +
  theme_minimal() +
  xlab("Varianza Errore di Misura") +
  ylab("Beta 0 SE") +
  ggtitle("Beta 0 SE") +
  xlim(c(0, 1))

betaW_se <- simulazione %>%
  group_by(u_sigma) %>%
  summarise(betaW_se_mean = mean(betaW_se),
            betaW_se_min = min(betaW_se),
            betaW_se_max = max(betaW_se)) %>%
  ggplot() +
  geom_ribbon(aes(x= u_sigma,
                  ymin = betaW_se_min,
                  ymax = betaW_se_max), fill="gray80") +
  geom_line(aes(u_sigma, betaW_se_mean)) +
  theme_minimal() +
  xlab("Varianza Errore di Misura") +
  ylab("Beta W SE") +
  ggtitle("Beta W SE") +
  xlim(c(0, 1))

betaZ_se <- simulazione %>%
  group_by(u_sigma) %>%
  summarise(betaZ_se_mean = mean(betaZ_se),
            betaZ_se_min = min(betaZ_se),
            betaZ_se_max = max(betaZ_se)) %>%
  ggplot() +
  geom_ribbon(aes(x= u_sigma,
                  ymin = betaZ_se_min,
                  ymax = betaZ_se_max), fill="gray80") +
  geom_line(aes(u_sigma, betaZ_se_mean)) +
  theme_minimal() +
  xlab("Varianza Errore di Misura") +
  ylab("Beta Z SE") +
  ggtitle("Beta Z SE") +
  xlim(c(0, 1))


beta0_se | betaW_se | betaZ_se
ggsave("figures/SE_err.pdf", beta0_se | betaW_se | betaZ_se, height = 4, width = 14)


# errore nelle stime per diversi quantili ---------------------------------
true_par=NULL
set.seed(123)
varX = 2
varZ = 1
n=100
x = as.matrix(rnorm(n, 3, varX), ncol = 1) # variabile non osservabile
z = as.matrix(rnorm(n, 5, varZ), ncol = 1) # variabile osservabile senza errore
b0 = 2
bx = 1
bz = 3
eps = x[1:n, 1] * rnorm(n, 0, 1)
y = b0 + bx * x[1:n, 1] + bz * z[1:n, 1] + eps
dat = data.frame(y = y, z = z)
for(i in 1:length(tt)){
  
  modxz = rq(y ~ x + z, data = dat, tau = tt[i])
  coef=rep(1,500) %*% t.default(modxz$coefficients)
  true_par=rbind(true_par,coef)
}

colnames(true_par)=c("beta0_true","betaX_true","betaZ_true")


u_sigma <- 1
n_sim <- 1:500
tt = seq(0.1,0.9,0.1)
simulazione_2 <- expand.grid(n_sim = n_sim, tt = tt)

simulazione_lista2 <- lapply(1:nrow(simulazione_2), 
                            function(x) simulazione_rq(n=100, tt=simulazione_2[x, ]$tt , u_sigma =1))


simulazione_2$beta0=sapply(simulazione_lista2, function(x) coef(x)[1])
simulazione_2$betaW=sapply(simulazione_lista2, function(x) coef(x)[2])
simulazione_2$betaZ=sapply(simulazione_lista2, function(x) coef(x)[3])

simulazione_2=cbind(simulazione_2,true_par)

sim_box = simulazione_2 %>%
  summarise(
    tt = tt,
    bias_beta0 = beta0 - beta0_true,
    bias_betaW = betaW - betaX_true,
    bias_betaZ = betaZ - betaZ_true
  )


#boxplot delle simulazioni, sarebbe carino aggiungere ai vari boxplot
#il valore "reale" del parametro come allocato in true_par

beta0q <- sim_box %>%  
  group_by(tt) %>%  
  ggplot(aes(x=tt,y=bias_beta0,color=tt,group=tt)) + 
  geom_boxplot()+
  theme_minimal() +
  xlab("Quantiles") +
  ylab(" ")+
  ggtitle("Beta 0")

betaZq <- sim_box %>%  
  group_by(tt) %>%  
  ggplot(aes(x=tt,y=bias_betaW,color=tt,group=tt)) + 
  geom_boxplot()+
  theme_minimal() +
  xlab("Quantiles") +
  ylab(" ")+
  ggtitle("Beta Z")

betaWq <- sim_box %>%  
  group_by(tt) %>%  
  ggplot(aes(x=tt,y=bias_betaW,color=tt,group=tt)) + 
  geom_boxplot()+
  theme_minimal() +
  xlab("Quantiles") +
  ylab(" ")+
  ggtitle("Beta W")

beta0q | betaWq | betaZq
ggsave("figures/boxplot_err.pdf", beta0q | betaWq | betaZq, height = 8, width = 24)
=======
library(ggplot2)
library(dplyr)
library(patchwork)
library(quantreg)



# primo esempio (solo per chiarire) -----------------------------------------------------------
n = 1000
sig=1
beta0 = 2.1
beta1 = 2
set.seed(12345)
X = runif(n, 0, 4)

# heteroskedastic error
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
abline(rq(Y~X,tau=.1), lwd=2, col="seagreen")

#lapply(seq(0.1, 0.9, by=0.1), function(tt) abline(rq(Y~X, tau=tt)))


# errore nella rq ---------------------------------------------------------
simulazione_rq <- function(n = 100, u_sigma = 0.75, tt=.9, se = "ker", bsmethod= NA, R = NA) {
  varX = 2
  varU = u_sigma
  varZ = 1
  x = as.matrix(rnorm(n, 3, varX), ncol = 1) # Unobservable variable
  z = as.matrix(rnorm(n, 5, varZ), ncol = 1) # Measurable variable (without any error)
  u = as.matrix(rnorm(n, 0, varU), ncol = 1) # Measurement error
  w = x + u                                  # Variable measuring X (with error)
  eps = x[1:n, 1] * rnorm(n, 0, 1)           # Heteroskedastic error
  
  b0 = 2
  bx = 1
  bz = 3
  y = b0 + bx * x[1:n, 1] + bz * z[1:n, 1] + eps
  dat = data.frame(y = y,
                   w = w,
                   z = z,
                   x = x)
  
 # tt = .9
  modxz = rq(y ~ x + z, data = dat, tau = tt)
  summary(modxz,se=se, bsmethod=bsmethod, R=R)
  
  modwz = rq(y ~ w + z, data = dat, tau = tt)
  summary(modwz,se=se, bsmethod=bsmethod, R=R)
  
}


u_sigma <- seq(0, 5, by=0.2)
n_sim <- 1:500

taus <- c(0.9,0.1,seq(0.8,0.2,by=-0.1))

for(i in 1:length(taus)){

simulazione <- expand.grid(n_sim = n_sim, u_sigma = u_sigma)

simulazione_lista <- lapply(1:nrow(simulazione), 
                            function(x) simulazione_rq(n=100, u_sigma=simulazione[x, ]$u_sigma , tt=taus[i]))


simulazione$beta0=sapply(simulazione_lista, function(x) coef(x)[1])
simulazione$betaW=sapply(simulazione_lista, function(x) coef(x)[2])
simulazione$betaZ=sapply(simulazione_lista, function(x) coef(x)[3])
simulazione$beta0_se = sapply(simulazione_lista, function(x) coef(x)[,2][1])
simulazione$betaW_se = sapply(simulazione_lista, function(x) coef(x)[,2][2])
simulazione$betaZ_se = sapply(simulazione_lista, function(x) coef(x)[,2][3])
simulazione$betaW_p = sapply(simulazione_lista, function(x) coef(x)[,4][2])
simulazione$betaZ_p = sapply(simulazione_lista, function(x) coef(x)[,4][3])


simulazione_lista_boot <- lapply(1:nrow(simulazione), 
                            function(x) simulazione_rq(n=100, u_sigma=simulazione[x, ]$u_sigma , tt=taus[i], 
                                                       se = "boot", bsmethod= "xy",R = 500))

simulazione$beta0_se_boot = sapply(simulazione_lista_boot, function(x) coef(x)[,2][1])
simulazione$betaW_se_boot = sapply(simulazione_lista_boot, function(x) coef(x)[,2][2])
simulazione$betaZ_se_boot = sapply(simulazione_lista_boot, function(x) coef(x)[,2][3])

# true parameters ---------------------------------------------------------

true_coef <- simulazione %>%
  filter(u_sigma == 0) %>%
  select(beta0, betaW, betaZ) %>%
  apply(2, mean)
true_coef
# 0.9
# beta0    betaW    betaZ 
# 3.276685 1.898559 2.988795 
# 0.1
# beta0     betaW     betaZ 
# 0.7911875 0.1018745 2.9957438 


# Plots -------------------------------------------------------------------

# beta0, betaW and betaZ 

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
  #geom_hline(yintercept=2, linetype="dotted") +
  theme_minimal() +
  xlab("Variance of Measurement Error") +
  ylab("Beta 0") +
  ggtitle(paste("Beta 0", taus[i])) #+
  #xlim(c(0, 1))+
  #ylim(c(-10,10))

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
  #geom_hline(yintercept=1, linetype="dotted") +
  theme_minimal() +
  xlab("Variance of Measurement Error") +
  ylab("Beta W") +
  ggtitle(paste("Beta W", taus[i]))#+
  #xlim(c(0, 1))+
  #ylim(c(-10,10))

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
  #geom_hline(yintercept=3, linetype="dotted") +
  theme_minimal() +
  xlab("Variance of Measurement Error") +
  ylab("Beta Z") +
  ggtitle(paste("Beta Z", taus[i]))#+
  #xlim(c(0, 1))+
  #ylim(c(-10,10))


beta0 | betaW | betaZ
setwd("Y:/Desktop/PhD Bicocca/ReadingGroups/Some issues in stat model/QuantileRegression/")
ggsave(paste("figures/quant_reg",taus[i],".pdf", sep=""),
       beta0 | betaW | betaZ, height = 4, width = 8)

# beta0, betaW and betaZ standard errors (kernel)

beta0_se <- simulazione %>% 
  group_by(u_sigma) %>% 
  summarise(beta0_se_mean = mean(beta0_se), 
            beta0_se_min = min(beta0_se), 
            beta0_se_max = max(beta0_se)) %>% 
  ggplot() + 
  geom_ribbon(aes(x= u_sigma, 
                  ymin = beta0_se_min, 
                  ymax = beta0_se_max), fill="gray80") +
  geom_line(aes(u_sigma, beta0_se_mean)) +
  theme_minimal() +
  xlab("Variance of Measurement Error") +
  ylab("Beta 0 SE") +
  ggtitle(paste("Beta 0 SE (ker)", taus[i])) #+
#xlim(c(0, 1))


betaW_se <- simulazione %>% 
  group_by(u_sigma) %>% 
  summarise(betaW_se_mean = mean(betaW_se), 
            betaW_se_min = min(betaW_se), 
            betaW_se_max = max(betaW_se)) %>% 
  ggplot() + 
  geom_ribbon(aes(x= u_sigma, 
                  ymin = betaW_se_min, 
                  ymax = betaW_se_max), fill="gray80") +
  geom_line(aes(u_sigma, betaW_se_mean)) +
  theme_minimal() +
  xlab("Variance of Measurement Error") +
  ylab("Beta W SE") +
  ggtitle(paste("Beta W SE (ker)", taus[i])) #+
# xlim(c(0, 1))


betaZ_se <- simulazione %>% 
  group_by(u_sigma) %>% 
  summarise(betaZ_se_mean = mean(betaZ_se), 
            betaZ_se_min = min(betaZ_se), 
            betaZ_se_max = max(betaZ_se)) %>% 
  ggplot() + 
  geom_ribbon(aes(x= u_sigma, 
                  ymin = betaZ_se_min, 
                  ymax = betaZ_se_max), fill="gray80") +
  geom_line(aes(u_sigma, betaZ_se_mean)) +
  theme_minimal() +
  xlab("Variance of Measurement Error") +
  ylab("Beta Z SE") +
  ggtitle(paste("Beta Z SE (ker)", taus[i])) #+
# xlim(c(0, 1))


beta0_se | betaW_se | betaZ_se
ggsave(paste("figures/quant_reg_se",taus[i],".pdf", sep=""), 
       beta0_se | betaW_se | betaZ_se, height = 4, width = 8)


# beta0, betaW and betaZ standard errors (bootstrap)

beta0_se_boot <- simulazione %>% 
  group_by(u_sigma) %>% 
  summarise(beta0_se_mean = mean(beta0_se_boot), 
            beta0_se_min = min(beta0_se_boot), 
            beta0_se_max = max(beta0_se_boot)) %>% 
  ggplot() + 
  geom_ribbon(aes(x= u_sigma, 
                  ymin = beta0_se_min, 
                  ymax = beta0_se_max), fill="gray80") +
  geom_line(aes(u_sigma, beta0_se_mean)) +
  theme_minimal() +
  xlab("Variance of Measurement Error") +
  ylab("Beta 0 SE") +
  ggtitle(paste("Beta 0 SE (boot)", taus[i])) #+
#xlim(c(0, 1))


betaW_se_boot <- simulazione %>% 
  group_by(u_sigma) %>% 
  summarise(betaW_se_mean = mean(betaW_se_boot), 
            betaW_se_min = min(betaW_se_boot), 
            betaW_se_max = max(betaW_se_boot)) %>% 
  ggplot() + 
  geom_ribbon(aes(x= u_sigma, 
                  ymin = betaW_se_min, 
                  ymax = betaW_se_max), fill="gray80") +
  geom_line(aes(u_sigma, betaW_se_mean)) +
  theme_minimal() +
  xlab("Variance of Measurement Error") +
  ylab("Beta W SE") +
  ggtitle(paste("Beta W SE (boot)", taus[i])) #+
# xlim(c(0, 1))


betaZ_se_boot <- simulazione %>% 
  group_by(u_sigma) %>% 
  summarise(betaZ_se_mean = mean(betaZ_se_boot), 
            betaZ_se_min = min(betaZ_se_boot), 
            betaZ_se_max = max(betaZ_se_boot)) %>% 
  ggplot() + 
  geom_ribbon(aes(x= u_sigma, 
                  ymin = betaZ_se_min, 
                  ymax = betaZ_se_max), fill="gray80") +
  geom_line(aes(u_sigma, betaZ_se_mean)) +
  theme_minimal() +
  xlab("Variance of Measurement Error") +
  ylab("Beta Z SE") +
  ggtitle(paste("Beta Z SE (boot)", taus[i])) #+
# xlim(c(0, 1))


beta0_se_boot | betaW_se_boot | betaZ_se_boot
ggsave(paste("figures/quant_reg_se_boot",taus[i],".pdf", sep=""), 
       beta0_se_boot | betaW_se_boot | betaZ_se_boot, height = 4, width = 8)


# Power

betaW_test <- simulazione %>% 
  group_by(u_sigma) %>% 
  summarise(betaW_p = mean(betaW_p< 0.05)) %>% 
  ggplot() + 
  geom_line(aes(u_sigma, betaW_p)) +
  theme_minimal() +
  xlab("Variance of Measurement Error") +
  ylab("Power") +
  ggtitle(paste("Power of the test - Beta W", taus[i]))

betaZ_test <- simulazione %>% 
  group_by(u_sigma) %>% 
  summarise(betaZ_p = mean(betaZ_p< 0.05)) %>% 
  ggplot() + 
  geom_line(aes(u_sigma, betaZ_p)) +
  theme_minimal() +
  xlab("Variance of Measurement Error") +
  ylab("Power") +
  ggtitle(paste("Power of the test - Beta Z", taus[i]))

betaW_test | betaZ_test
ggsave(paste("figures/quant_reg_power",taus[i],".pdf", sep=""), 
       betaW_test | betaZ_test, height = 4, width = 8)


}
>>>>>>> 3590e8430d53dcf9f8fd87e92e563d21e1d583b3
