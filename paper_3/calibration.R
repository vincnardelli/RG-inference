library(ggplot2)
library(dplyr)
library(patchwork)
library(quantreg)
library(tidyr)

data_generation_step1 <- function(id){
  n=1000
  varX = 2
  varZ = 1
  x = as.matrix(rnorm(n, 3, varX), ncol = 1) # Unobservable variable
  z = as.matrix(rnorm(n, 5, varZ), ncol = 1) # Measurable variable (without any error)
  eps = x[1:n, 1] * rnorm(n, 0, 1)         # Heteroskedastic error
  
  b0 = 2
  bx = 1
  bz = 3
  y = b0 + bx * x[1:n, 1] + bz * z[1:n, 1] + eps
  data.frame(y = y,
             z = z,
              x = x)
}
data_generation_step2 <- function(data, u_sigma=0){
  n=1000
  u = rnorm(n, 0, u_sigma) # Measurement error
  w = data$x + u                                  # Variable measuring X (with error)
  data.frame(y = data$y,
             z = data$z,
             x = data$x,
             w=w)
}


#parametri simulazione
n_dataset <- 10
n <- 1000
u_sigmas <- seq(0, 2, by=0.5)
n_sim <- 1:500
tts = c(0.05, 0.5, 0.95)

# genero dataset
dataset <- lapply(1:n_dataset, data_generation_step1)

# per ogni dataset genero diverse versioni con sigmau variabile

dataset_list <- expand.grid(dataset=1:n_dataset, 
                            u_sigma = u_sigmas)

data_list <- lapply(1:nrow(dataset_list), function(x){
  dataset_id <- dataset_list[x, 1]
  data_generation_step2(data=dataset[[dataset_id]], u_sigma = dataset_list[x, 2])
})




sim <- function(dataset, u_sigma, tt){

  dataset_id <- which(dataset_list$dataset==dataset & dataset_list$u_sigma == u_sigma)
  data <- data_list[[dataset_id]]
  
  #RC-step 1 : we assume "variable" validation dataset (va bene?)
  size_val = .2
  ind = sample(nrow(data), size = n * size_val, replace = F)
  
  #regression on validation dataset X ~ W + Z (relazione lineare)
  reg1 = lm(x ~ w + z, data = data[ind, ])
  
  data.ex = data[-ind, ]
  data.ex$x_hat = coef(reg1)[1] + coef(reg1)[2] * data.ex$w + coef(reg1)[3] * data.ex$z
  
  #q-regression on non-validation dataset Y ~ Xhat + Z
  model <- rq(y ~ x_hat + z, data = data.ex, tau = tt)
  model$coefficients
}


simulazione <- expand.grid(n_sim = n_sim, 
                           dataset = 1:n_dataset,
                           u_sigma = u_sigmas, 
                           tt = tts)

simulazione_lista <- parallel::mclapply(1:nrow(simulazione), 
                            function(x) sim(dataset = simulazione[x, ]$dataset,
                                            u_sigma=simulazione[x, ]$u_sigma, 
                                            tt=simulazione[x, ]$tt), 
                            mc.cores = parallel::detectCores()-1)



simulazione$beta0=unlist(sapply(simulazione_lista, function(x) x[1]))
simulazione$betaW=unlist(sapply(simulazione_lista, function(x) x[2]))
simulazione$betaZ=unlist(sapply(simulazione_lista, function(x) x[3]))

# real values
sim_real <- function(dataset, u_sigma, tt){
  
  dataset_id <- which(dataset_list$dataset==dataset & dataset_list$u_sigma == u_sigma)
  data <- data_list[[dataset_id]]
  
  data.ex = data[-ind, ]
  data.ex$x_hat = coef(reg1)[1] + coef(reg1)[2] * data.ex$w + coef(reg1)[3] * data.ex$z
  
  #q-regression on non-validation dataset Y ~ Xhat + Z
  model <- rq(y ~ w + z, data = data.ex, tau = tt)
  model$coefficients
}

simulazione_real <- expand.grid(dataset = 1:n_dataset,
                                u_sigma = u_sigmas, 
                                tt = tts)

simulazione_real_lista <- parallel::mclapply(1:nrow(simulazione_real), 
                                        function(x) sim_real(dataset = simulazione_real[x, ]$dataset,
                                                        u_sigma=simulazione_real[x, ]$u_sigma, 
                                                        tt=simulazione_real[x, ]$tt), 
                                        mc.cores = parallel::detectCores()-1)

simulazione_real$beta0=unlist(sapply(simulazione_real_lista, function(x) x[1]))
simulazione_real$betaW=unlist(sapply(simulazione_real_lista, function(x) x[2]))
simulazione_real$betaZ=unlist(sapply(simulazione_real_lista, function(x) x[3]))


simulazione_real <- simulazione_real %>% 
  pivot_longer(beta0:betaZ, names_to = "parameter", values_to = "real")

simulazione %>% 
  pivot_longer(beta0:betaZ, names_to = "parameter") %>% 
  group_by(dataset, u_sigma, tt, parameter) %>%
  summarise(estimate=mean(value), 
            lower = mean(value)-1.96*sd(value), 
            upper = mean(value)+1.96*sd(value)) %>% 
  left_join(simulazione_real) %>% 
  mutate(inside = ifelse(real > lower & real < upper, T, F))

#pivot_wider(names_from = parameter, values_from = estimate:upper)
