library(ggplot2)
library(dplyr)
library(patchwork)


set.seed(123)


simulazione_normale <- function(n=100, u_sigma=0.75){
  
  varX = 2
  varU = u_sigma
  varZ = 1
  x = as.matrix(rnorm(n, 3, varX), ncol=1) # variabile non osservabile
  z = as.matrix(rnorm(n, 5, varZ), ncol=1) # variabile osservabile senza errore
  
  u = as.matrix(rnorm(n, 0, varU), ncol=1) # errore
  
  b0 = 2
  bx = 1
  bz = 3
  
  y = b0 + bx*x[1:n,1] + bz*z[1:n,1] + rnorm(n,0,1)
  
  w = x+u   # modello classico
  
  dat = data.frame(y=y, w=w, z=z)
  
  modxz = lm(y~x+z, data = dat)
  
  modwz = lm(y~w+z, data = dat)
  
}



u_sigma <- seq(0, 5, by=0.2)
n_sim <- 1:500
simulazione <- expand.grid(n_sim = n_sim, u_sigma = u_sigma)


# Versione parallela
# simulazione_lista <- parallel::mclapply(1:nrow(simulazione), 
#                                         function(x) simulazione_normale(n=100, 
#                                                                         u_sigma=simulazione[x, ]$u_sigma), 
#                                                                         mc.cores = 1)


# Versione non parallela
simulazione_lista <- lapply(1:nrow(simulazione), 
                            function(x) simulazione_normale(n=100, u_sigma=simulazione[x, ]$u_sigma))


simulazione$beta0=sapply(simulazione_lista, function(x) coef(x)[1])
simulazione$betaW=sapply(simulazione_lista, function(x) coef(x)[2])
simulazione$betaZ=sapply(simulazione_lista, function(x) coef(x)[3])
simulazione$beta0_se = sapply(simulazione_lista, function(x) summary(x)$coefficients[,2][1])
simulazione$betaW_se = sapply(simulazione_lista, function(x) summary(x)$coefficients[,2][2])
simulazione$betaZ_se = sapply(simulazione_lista, function(x) summary(x)$coefficients[,2][3])
simulazione$betaW_p = sapply(simulazione_lista, function(x) summary(x)$coefficients[, 4][2])
simulazione$betaZ_p = sapply(simulazione_lista, function(x) summary(x)$coefficients[, 4][3])



simulazione %>% 
  group_by(u_sigma) %>% 
  summarise_at(vars(beta0:betaZ_se), mean)

simulazione %>% 
  group_by(u_sigma) %>% 
  summarise(betaW_p = mean(betaW_p< 0.05))

simulazione %>% 
  group_by(u_sigma) %>% 
  summarise(betaZ_p = mean(betaZ_p< 0.05))



# beta0, betaW e betaZ 
# u_sigma tra 0 e 5 (tutto il range)

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
  geom_hline(yintercept=2, linetype="dotted") +
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
  geom_hline(yintercept=1, linetype="dotted") +
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
  geom_hline(yintercept=3, linetype="dotted") +
  theme_minimal() +
  xlab("Variance of Measurement Error") +
  ylab("Beta Z") +
  ggtitle("Beta Z")


beta0 | betaW | betaZ
setwd("Y:/Desktop/PhD Bicocca/ReadingGroups/Some issues in stat model/")
ggsave("figures/normal_multi_estimate.pdf", beta0 | betaW | betaZ, 
       height = 4, width = 8)



# beta0, betaW e betaZ standard error

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
  ggtitle("Beta 0 SE") #+
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
  ggtitle("Beta W SE") #+
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
  ggtitle("Beta Z SE") #+
# xlim(c(0, 1))


beta0_se | betaW_se | betaZ_se
ggsave("figures/normal_multi_se.pdf", beta0_se | betaW_se | betaZ_se, 
       height = 4, width = 8)



# Power

betaW_test <- simulazione %>% 
  group_by(u_sigma) %>% 
  summarise(betaW_p = mean(betaW_p< 0.05)) %>% 
  ggplot() + 
  geom_line(aes(u_sigma, betaW_p)) +
  theme_minimal() +
  xlab("Variance of Measurement Error") +
  ylab("Power") +
  ggtitle("Power of the test - Beta W")

betaZ_test <- simulazione %>% 
  group_by(u_sigma) %>% 
  summarise(betaZ_p = mean(betaZ_p< 0.05)) %>% 
  ggplot() + 
  geom_line(aes(u_sigma, betaZ_p)) +
  theme_minimal() +
  xlab("Variance of Measurement Error") +
  ylab("Power") +
  ggtitle("Power of the test - Beta Z")

betaW_test | betaZ_test
ggsave("figures/normal_multi_power.pdf", betaW_test | betaZ_test, height = 4, width = 8)


