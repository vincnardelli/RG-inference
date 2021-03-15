library(ggplot2)
library(dplyr)
library(patchwork)
library(quantreg)

simulazione_rq_rc <- function(n = 100, u_sigma = 0.75, tt = .9) {

  varX = 2
  varU = u_sigma
  varZ = 1
  x = as.matrix(rnorm(n, 3, varX), ncol = 1) # Unobservable variable
  z = as.matrix(rnorm(n, 5, varZ), ncol = 1) # Measurable variable (without any error)
  u = as.matrix(rnorm(n, 0, varU), ncol = 1) # Measurement error
  w = x + u                                  # Variable measuring X (with error)
  eps = x[1:n, 1] * rnorm(n, 0, 1)         # Heteroskedastic error
  
  b0 = 2
  bx = 1
  bz = 3
  y = b0 + bx * x[1:n, 1] + bz * z[1:n, 1] + eps
  dat = data.frame(y = y,
                   w = w,
                   z = z,
                   x = x)
  
  #RC-step 1 : we assume "variable" validation dataset (va bene?)
  size_val = .2
  ind = sample(nrow(dat), size = n * size_val, replace = F)
  
  #regression on validation dataset X ~ W + Z (relazione lineare)
  dat.val = dat[ind, ]
  reg1 = lm(x ~ w + z, data = dat.val)
  
  dat.ex = dat[-ind, ]
  x_hat = coef(reg1)[1] + coef(reg1)[2] * dat.ex$w + coef(reg1)[3] * dat.ex$z
  dat.ex = cbind(dat.ex, x_hat)
  
  #q-regression on non-validation dataset Y ~ Xhat + Z
  modwz = rq(y ~ x_hat + z, data = dat.ex, tau = tt)
  summary(modwz)
  
}

#"true" coefficients can be obtained from the following
modxz_true = rq(y ~ x + z, data = dat, tau = tt)
true_coef=modxz_true$coefficients

u_sigma <- seq(0, 5, by=0.2)
n_sim <- 1:500
simulazione <- expand.grid(n_sim = n_sim, u_sigma = u_sigma)

simulazione_lista <- lapply(1:nrow(simulazione), 
                            function(x) simulazione_rq_rc(n=100, u_sigma=simulazione[x, ]$u_sigma,tt=.9))


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





