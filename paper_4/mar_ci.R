library(ggplot2)
library(dplyr)
library(patchwork)
library(quantreg)
library(tidyr)
library(MASS)
library(mice)

data_generation_step1 <- function(id){
  n=500
  mu=c(3,5,1)
  vars=diag(c(2,1,1.5))
  Z = mvrnorm(n=n,mu=mu,Sigma=vars)
  X=cbind(1,Z)
  
  #y=2+Z_1+3Z_2+.5Z_3
  b=c(2,1,3,0.5)
  #eps = Z[1:n, 1] * rnorm(n, 0, 1)  
  eps=rnorm(n, 0, 1) 
  
  y=X%*%b+eps
  df <- data.frame(cbind(y, X[,2:4]))
  names(df) <- c("y", "x1", "x2", "x3")
  df
}


data_generation_step2 <- function(data, prop=0){
  patterns=matrix(c(0,1,1,1,0,1),ncol = 3,byrow = T)
  freq=c(.7,.3)
  Z <- data[, 2:4]
  Zmiss=ampute(
    Z,
    prop = prop,
    patterns = patterns,
    freq = freq,
    mech ="MCAR"
  )
  
  data[, 2:4] <- Zmiss$amp
  data
}


#parametri simulazione
n_dataset <- 100
props <- seq(0.1, 0.9, by=0.2)
n_sim <- 1:200
tts = c(0.1, 0.9)

# genero dataset
dataset <- lapply(1:n_dataset, data_generation_step1)

# per ogni dataset genero diverse versioni con prop variabile

dataset_list <- expand.grid(dataset=1:n_dataset, 
                            prop = props)

data_list <- lapply(1:nrow(dataset_list), function(x){
  dataset_id <- dataset_list[x, 1]
  data_generation_step2(data=dataset[[dataset_id]], prop = dataset_list[x, 2])
})


sim <- function(dataset, prop, tt, seed){
  
  dataset_id <- which(dataset_list$dataset==dataset & dataset_list$prop == as.character(prop))
  data <- data_list[[dataset_id]]
  
  imp <- mice(data, m = 5, method = "norm.nob", seed=seed)
  data$x1[is.na(data$x1)] <- imp$imp$x1[,1]
  data$x2[is.na(data$x2)] <- imp$imp$x2[,1]
  data$x3[is.na(data$x3)] <- imp$imp$x3[,1]
  model <- rq(y ~ ., data = data, tau = tt)
  model$coefficients
}


simulazione <- expand.grid(n_sim = n_sim, 
                           dataset = 1:n_dataset,
                           prop = props, 
                           tt = tts)

simulazione_lista <- parallel::mclapply(1:nrow(simulazione), 
                                        function(x) sim(dataset = simulazione[x, ]$dataset,
                                                        prop=simulazione[x, ]$prop, 
                                                        tt=simulazione[x, ]$tt, 
                                                        seed=x), 
                                        mc.cores = parallel::detectCores()-1)

simulazione$beta0=unlist(sapply(simulazione_lista, function(x) x[1]))
simulazione$beta1=unlist(sapply(simulazione_lista, function(x) x[2]))
simulazione$beta2=unlist(sapply(simulazione_lista, function(x) x[3]))
simulazione$beta3=unlist(sapply(simulazione_lista, function(x) x[4]))

# real values
sim_real <- function(dataset_id, tt){
  
  data <- dataset[[dataset_id]]
  #q-regression on non-validation dataset Y ~ Xhat + Z
  model <- boot.rq(x=cbind(1,data[,2:4]), y=data[,1], tau = tt, bsmethod= "xy", R=500)
  list(t(apply(model$B, 2, quantile, c(0.025, 0.975))),
  t(apply(model$B, 2, mean)))
}

simulazione_real <- expand.grid(dataset = 1:n_dataset,
                                tt = tts)

simulazione_real_lista <- parallel::mclapply(1:nrow(simulazione_real), 
                                             function(x) sim_real(dataset_id = simulazione_real[x, ]$dataset,
                                                                  tt=simulazione_real[x, ]$tt), 
                                             mc.cores = parallel::detectCores()-1)

simulazione_real$beta0_lower=unlist(sapply(simulazione_real_lista, function(x) x[[1]][1,1]))
simulazione_real$beta0_mean=unlist(sapply(simulazione_real_lista, function(x) x[[2]][1]))
simulazione_real$beta0_upper=unlist(sapply(simulazione_real_lista, function(x) x[[1]][1,2]))

simulazione_real$beta1_lower=unlist(sapply(simulazione_real_lista, function(x) x[[1]][2,1]))
simulazione_real$beta1_mean=unlist(sapply(simulazione_real_lista, function(x) x[[2]][2]))
simulazione_real$beta1_upper=unlist(sapply(simulazione_real_lista, function(x) x[[1]][2,2]))

simulazione_real$beta2_lower=unlist(sapply(simulazione_real_lista, function(x) x[[1]][3,1]))
simulazione_real$beta2_mean=unlist(sapply(simulazione_real_lista, function(x) x[[2]][3]))
simulazione_real$beta2_upper=unlist(sapply(simulazione_real_lista, function(x) x[[1]][3,2]))

simulazione_real$beta3_lower=unlist(sapply(simulazione_real_lista, function(x) x[[1]][4,1]))
simulazione_real$beta3_mean=unlist(sapply(simulazione_real_lista, function(x) x[[2]][4]))
simulazione_real$beta3_upper=unlist(sapply(simulazione_real_lista, function(x) x[[1]][4,2]))


simulazione_real <- simulazione_real %>% 
  pivot_longer(beta0_lower:beta3_upper, names_to = "parameter", values_to = "rq") %>%
  rowwise() %>%
  mutate(bound = strsplit(parameter, "_")[[1]][2], 
        parameter =strsplit(parameter, "_")[[1]][1]) %>%
  pivot_wider(names_from = bound, values_from = rq)


save(simulazione_real, simulazione, file="paper_4/simulazione.RData")

load("paper_4/simulazione.RData")



risultati <- simulazione %>% 
  pivot_longer(beta0:beta3, names_to = "parameter") %>% 
  group_by(dataset, prop, tt, parameter) %>%
  summarise(boot_mean=mean(value), 
            boot_lower = mean(value)-1.96*sd(value), 
            boot_upper = mean(value)+1.96*sd(value)) %>% 
  left_join(simulazione_real, by=c("dataset", "tt", "parameter")) %>%
  filter(prop != 0) %>%
  mutate(inside_bootstrap = ifelse(mean > boot_lower & mean < boot_upper, T, F))

risultati <- simulazione %>% 
  pivot_longer(beta0:beta3, names_to = "parameter") %>% 
  group_by(dataset, prop, tt, parameter) %>%
  summarise(boot_mean=mean(value), 
            boot_lower = quantile(value, 0.025), 
            boot_upper = quantile(value, 0.975)) %>% 
  left_join(simulazione_real, by=c("dataset", "tt", "parameter")) %>%
  filter(prop != 0) %>%
  mutate(inside_bootstrap = ifelse(mean > boot_lower & mean < boot_upper, T, F))

fig1 <- risultati %>%
  filter(dataset == 1) %>%
ggplot() + 
  geom_ribbon(aes(x= prop, 
                  ymin = boot_lower, 
                  ymax = boot_upper), fill="gray80") +
  geom_hline(aes(yintercept = lower)) +
  geom_hline(aes(yintercept = upper)) +
  facet_grid(cols = vars(parameter), rows = vars(tt), scales = "free", space="free") +
  theme_bw()
fig1
ggsave("paper_4/ci_bootstrap_fig1.pdf", fig1, height = 5, width = 8)



fig2 <- risultati %>% 
  group_by(prop, tt, parameter) %>% 
  summarise(coverage = mean(inside_bootstrap)) %>% 
  ggplot() +
  geom_line(aes(prop, coverage)) +
  facet_grid(rows=vars(tt), cols=vars(parameter)) +
  theme_bw()  +
  ylab("Copertura") +
  xlab("Prop") +
  ggtitle("Copertura CI")
fig2
ggsave("paper_4/prop.pdf", fig2, height = 5, width = 8)

