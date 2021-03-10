library(ggplot2)
library(dplyr)
library(patchwork)
set.seed(123)


simulazione_logit<- function(n=80, u_sigma=1){
  
  b0=2
  b1=-1
  x=rnorm(n, mean=2, sd=1)

  z = b0 + b1*x 
  pr = 1/(1+exp(-z))        
  y = rbinom(n,1,pr)
  #mne=glm( y~x,family="binomial")

  u=rnorm(n,0,u_sigma)
  w=x+u
  
  glm( y~w,family="binomial")
  
}

u_sigma <- seq(0, 5, by=0.2)
n_sim <- 1:500
simulazione <- expand.grid(n_sim = n_sim, u_sigma = u_sigma)

simulazione_lista <- parallel::mclapply(1:nrow(simulazione), function(x) simulazione_logit(n=80, u_sigma=simulazione[x, ]$u_sigma), 
                                        mc.cores = 6)

# Versione non parallela
#simulazione_lista <- lapply(1:nrow(simulazione), function(x) simulazione_logit(n=80, u_sigma=simulazione[x, ]$u_sigma))

simulazione$beta0=sapply(simulazione_lista, function(x) coef(x)[1])
simulazione$beta1=sapply(simulazione_lista, function(x) coef(x)[2])
simulazione$beta0_se = sapply(simulazione_lista, function(x) summary(x)$coefficients[,2][1])
simulazione$beta1_se = sapply(simulazione_lista, function(x) summary(x)$coefficients[,2][2])
simulazione$beta1_p = sapply(simulazione_lista, function(x) summary(x)$coefficients[, 4][2])

simulazione %>% 
  group_by(u_sigma) %>% 
  summarise_at(vars(beta0:beta1_se), mean)

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
  xlab("Varianza Errore di Misura") +
  ylab("Beta 0") +
  ggtitle("Beta 0") +
  xlim(c(0, 1))


beta1 <- simulazione %>% 
  group_by(u_sigma) %>% 
  summarise(beta1_mean = mean(beta1), 
            beta1_min = min(beta1), 
            beta1_max = max(beta1)) %>% 
  ggplot() + 
  geom_ribbon(aes(x= u_sigma, 
                  ymin = beta1_min, 
                  ymax = beta1_max), fill="gray80") +
  geom_line(aes(u_sigma, beta1_mean)) +
  geom_hline(yintercept=-1, linetype="dotted") +
  theme_minimal() +
  xlab("Varianza Errore di Misura") +
  ylab("Beta 1") +
  ggtitle("Beta 1") +
  xlim(c(0, 1))


beta0 | beta1
ggsave("figures/logit_stima1.pdf", beta0 | beta1, height = 4, width = 8)

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
  xlab("Varianza Errore di Misura") +
  ylab("Beta 0") +
  ggtitle("Beta 0")


beta1 <- simulazione %>%
  group_by(u_sigma) %>%
  summarise(beta1_mean = mean(beta1),
            beta1_min = min(beta1),
            beta1_max = max(beta1)) %>%
  ggplot() +
  geom_ribbon(aes(x= u_sigma,
                  ymin = beta1_min,
                  ymax = beta1_max), fill="gray80") +
  geom_line(aes(u_sigma, beta1_mean)) +
  geom_hline(yintercept=-1, linetype="dotted") +
  theme_minimal() +
  xlab("Varianza Errore di Misura") +
  ylab("Beta 1") +
  ggtitle("Beta 1")


beta0 | beta1
ggsave("figures/logit_stima2.pdf", beta0 | beta1, height = 4, width = 8)


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


beta1_se <- simulazione %>% 
  group_by(u_sigma) %>% 
  summarise(beta1_se_mean = mean(beta1_se), 
            beta1_se_min = min(beta1_se), 
            beta1_se_max = max(beta1_se)) %>% 
  ggplot() + 
  geom_ribbon(aes(x= u_sigma, 
                  ymin = beta1_se_min, 
                  ymax = beta1_se_max), fill="gray80") +
  geom_line(aes(u_sigma, beta1_se_mean)) +
  theme_minimal() +
  xlab("Varianza Errore di Misura") +
  ylab("Beta 1 SE") +
  ggtitle("Beta 1 SE") +
  xlim(c(0, 1))


beta0_se | beta1_se
ggsave("figures/logit_stima3.pdf", beta0_se | beta1_se, height = 4, width = 8)

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
  ggtitle("Beta 0 SE") 


beta1_se <- simulazione %>% 
  group_by(u_sigma) %>% 
  summarise(beta1_se_mean = mean(beta1_se), 
            beta1_se_min = min(beta1_se), 
            beta1_se_max = max(beta1_se)) %>% 
  ggplot() + 
  geom_ribbon(aes(x= u_sigma, 
                  ymin = beta1_se_min, 
                  ymax = beta1_se_max), fill="gray80") +
  geom_line(aes(u_sigma, beta1_se_mean)) +
  theme_minimal() +
  xlab("Varianza Errore di Misura") +
  ylab("Beta 1 SE") +
  ggtitle("Beta 1 SE") 

beta0_se | beta1_se
ggsave("figures/logit_stima4.pdf", beta0_se | beta1_se, height = 4, width = 8)

# potenza test ------------------------------------------------------------

beta1_test <- simulazione %>% 
  group_by(u_sigma) %>% 
  summarise(beta1_p = mean(beta1_p< 0.05)) %>% 
  ggplot() + 
  geom_line(aes(u_sigma, beta1_p)) +
  theme_minimal() +
  xlab("Varianza Errore di Misura") +
  ylab("Potenza") +
  ggtitle("Potenza del test - Beta 1")

ggsave("figures/logit_potenza.pdf", beta1_test, height = 4, width = 8)
  