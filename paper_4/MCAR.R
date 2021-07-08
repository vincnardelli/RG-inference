library(mice)
library(MASS)
library(quantreg)
library(dplyr)
library(ggplot2)
library(patchwork)

simulazione_rq_MCAR <- function(n = 500, tt = .9, prop=0) {
  #aggiunto commento inutile
  mu=c(3,5,1)
  vars=diag(c(2,1,1.5))
  Z = mvrnorm(n=n,mu=mu,Sigma=vars)
  X=cbind(1,Z)
  
  #y=2+Z_1+3Z_2+.5Z_3
  b=c(2,1,3,0.5)
  #eps = Z[1:n, 1] * rnorm(n, 0, 1)  
  eps=rnorm(n, 0, 1) 
  
  y=X%*%b+eps
  
  #patterns specifica dei particolari pattern di risposta, per esempio se vogliamo vedere
  #dati mancanti nella prima e nella seconda variabile scriveremo c(1,1,0)
  patterns=matrix(c(0,1,1,1,0,1),ncol = 3,byrow = T)
  
  #freq specifica la frequenza di ciascun pattern dato precedentemente
  freq=c(.7,.3)
  
  #Quanti NA sul totale delle n osservazioni?
  prop=prop
  
  Zmiss=ampute(
    Z,
    prop = prop,
    patterns = patterns,
    freq = freq,
    mech ="MCAR"
  )
  dat=cbind(y,Zmiss$amp)
  dat=na.omit(dat)
  
  colnames(dat)=c("y","Z1","Z2","Z3")
  
  mod = rq(y ~., data = dat, tau = tt)
  summary(mod)
  mod
}

n_sim <- 1:500
prop=seq(0,.9,.1)
simulazione <- expand.grid(n_sim = n_sim, prop=prop)

simulazione_lista <- lapply(1:nrow(simulazione), 
                            function(x) simulazione_rq_MCAR(n=500, prop=simulazione[x, ]$prop,tt=.1))


simulazione$beta0=sapply(simulazione_lista, function(x) coef(x)[1])
simulazione$betaZ1=sapply(simulazione_lista, function(x) coef(x)[2])
simulazione$betaZ2=sapply(simulazione_lista, function(x) coef(x)[3])
simulazione$betaZ3=sapply(simulazione_lista, function(x) coef(x)[4])

tt=.1
beta0 <- simulazione %>% 
  group_by(prop) %>% 
  summarise(beta0_mean = mean(beta0), 
            beta0_min = min(beta0), 
            beta0_max = max(beta0)) %>% 
  ggplot() + 
  geom_ribbon(aes(x= prop, 
                  ymin = beta0_min, 
                  ymax = beta0_max), fill="gray80") +
  geom_line(aes(prop, beta0_mean)) +
  geom_hline(yintercept=b[1]+qnorm(tt), linetype="dotted") +
  theme_minimal() +
  xlab("Proportion of NA") +
  ylab("Beta 0") +
  ggtitle("Beta 0") 

betaZ1 <- simulazione %>% 
  group_by(prop) %>% 
  summarise(betaZ1_mean = mean(betaZ1), 
            betaZ1_min = min(betaZ1), 
            betaZ1_max = max(betaZ1)) %>% 
  ggplot() + 
  geom_ribbon(aes(x= prop, 
                  ymin = betaZ1_min, 
                  ymax = betaZ1_max), fill="gray80") +
  geom_line(aes(prop, betaZ1_mean)) +
  geom_hline(yintercept=b[2], linetype="dotted") +
  theme_minimal() +
  xlab("Proportion of NA") +
  ylab("Beta Z1") +
  ggtitle("Beta Z1") 

betaZ2 <- simulazione %>% 
  group_by(prop) %>% 
  summarise(betaZ2_mean = mean(betaZ2), 
            betaZ2_min = min(betaZ2), 
            betaZ2_max = max(betaZ2)) %>% 
  ggplot() + 
  geom_ribbon(aes(x= prop, 
                  ymin = betaZ2_min, 
                  ymax = betaZ2_max), fill="gray80") +
  geom_line(aes(prop, betaZ2_mean)) +
  geom_hline(yintercept=b[3], linetype="dotted") +
  theme_minimal() +
  xlab("Proportion of NA") +
  ylab("Beta Z2") +
  ggtitle("Beta Z2") 

betaZ3 <- simulazione %>% 
  group_by(prop) %>% 
  summarise(betaZ3_mean = mean(betaZ3), 
            betaZ3_min = min(betaZ3), 
            betaZ3_max = max(betaZ3)) %>% 
  ggplot() + 
  geom_ribbon(aes(x= prop, 
                  ymin = betaZ3_min, 
                  ymax = betaZ3_max), fill="gray80") +
  geom_line(aes(prop, betaZ3_mean)) +
  geom_hline(yintercept=b[4], linetype="dotted") +
  theme_minimal() +
  xlab("Proportion of NA") +
  ylab("Beta Z3") +
  ggtitle("Beta Z3") 

beta0|betaZ1|betaZ2|betaZ3

se <- parallel::mclapply(simulazione_lista, function(x) summary(x,se = "boot"),
                         mc.cores = parallel::detectCores()-3)

simulazione$beta0_se = sapply(se, function(x) x$coefficients[,2][1])
simulazione$betaZ1_se = sapply(se, function(x) x$coefficients[,2][2])
simulazione$betaZ2_se = sapply(se, function(x) x$coefficients[,2][3])
simulazione$betaZ3_se = sapply(se, function(x) x$coefficients[,2][4])

beta0_se <- simulazione %>%
  group_by(prop) %>%
  summarise(beta0_se_mean = mean(beta0_se),
            beta0_se_min = min(beta0_se),
            beta0_se_max = max(beta0_se)) %>%
  ggplot() +
  geom_ribbon(aes(x= prop,
                  ymin = beta0_se_min,
                  ymax = beta0_se_max), fill="gray80") +
  geom_line(aes(prop, beta0_se_mean)) +
  theme_minimal() +
  xlab("Proportion of NA") +
  ylab("Beta 0 SE") +
  ggtitle("Beta 0 SE") 

betaZ1_se <- simulazione %>%
  group_by(prop) %>%
  summarise(betaZ1_se_mean = mean(betaZ1_se),
            betaZ1_se_min = min(betaZ1_se),
            betaZ1_se_max = max(betaZ1_se)) %>%
  ggplot() +
  geom_ribbon(aes(x= prop,
                  ymin = betaZ1_se_min,
                  ymax = betaZ1_se_max), fill="gray80") +
  geom_line(aes(prop, betaZ1_se_mean)) +
  theme_minimal() +
  xlab("Proportion of NA") +
  ylab("Beta Z1 SE") +
  ggtitle("Beta Z1 SE") 

betaZ2_se <- simulazione %>%
  group_by(prop) %>%
  summarise(betaZ2_se_mean = mean(betaZ2_se),
            betaZ2_se_min = min(betaZ2_se),
            betaZ2_se_max = max(betaZ2_se)) %>%
  ggplot() +
  geom_ribbon(aes(x= prop,
                  ymin = betaZ2_se_min,
                  ymax = betaZ2_se_max), fill="gray80") +
  geom_line(aes(prop, betaZ2_se_mean)) +
  theme_minimal() +
  xlab("Proportion of NA") +
  ylab("Beta Z2 SE") +
  ggtitle("Beta Z2 SE") 



betaZ3_se <- simulazione %>%
  group_by(prop) %>%
  summarise(betaZ3_se_mean = mean(betaZ3_se),
            betaZ3_se_min = min(betaZ3_se),
            betaZ3_se_max = max(betaZ3_se)) %>%
  ggplot() +
  geom_ribbon(aes(x= prop,
                  ymin = betaZ3_se_min,
                  ymax = betaZ3_se_max), fill="gray80") +
  geom_line(aes(prop, betaZ3_se_mean)) +
  theme_minimal() +
  xlab("Proportion of NA") +
  ylab("Beta Z3 SE") +
  ggtitle("Beta Z3 SE") 

beta0_se|betaZ1_se|betaZ2_se|betaZ3_se