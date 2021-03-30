library(tidyverse)
library(patchwork)
library(AER)
library(mvtnorm)
library(quantreg)
library(doParallel)
library(foreach)

# Part 0: data simulation and IV example ----------------------------------

## Data simulation

set.seed(123)

n = 100
varX = 2
varU = 0.75
varZ = 1
x = rnorm(n, 3, varX) # Unobservable variable
z = rnorm(n, 5, varZ) # Measurable variable (without any error)
u = rnorm(n, 0, varU) # Measurement error
w = x+u                                  # Variable measuring X (with error)

b0 = 2
bx = 1
bz = 3
y = b0 + bx*x + bz*z + rnorm(n, 0, 1)
dat = data.frame(y=y, w=w, z=z)

modwz = lm(y~w+z, data = dat)
modxz = lm(y~x+z, data = dat)
qr.wz <- quantreg::rq(y ~ w + z, data = dat, tau = 0.5)
qr.xz <- quantreg::rq(y ~ x + z, data = dat, tau = 0.5)$coefficients


# Part 0b: some functions -------------------------------------------------


IVqreg <- function(y, x, z, w, t, tau) {
  w.on.t <- quantreg::rq(w ~ t, tau = tau)
  w.hat <- coef(w.on.t)[[2]]*t
  
  y.on.what <- quantreg::rq(y ~ w.hat + z, tau = tau)
  
  return(y.on.what)
}


IV.qfunction <- function(n, sd.x, var.u1, var.u2, sd.z, b0, b.x, b.z, bias.iv, cov.u, tau) {
  u.sigma <- diag(c(var.u1, var.u2), ncol = 2, nrow = 2)
  u.sigma[1,2] = u.sigma[2,1] = cov.u
  
  x <- rnorm(n, 3, sd.x)
  z <- rnorm(n, 5, sd.z)
  u <- mvtnorm::rmvnorm(n, c(0,0), u.sigma)
  
  w <- x + u[,1]
  t <- bias.iv + x + u[,2]
  
  eps <- x*rnorm(n)
  
  y <- b0 + b.x*x + b.z*z + eps
  
  data <- data.frame(y = y, x = x, w = w, z = z)
  
  ivqreg <- IVqreg(y = y, w = w, z = z, t = t, tau = tau)
  trueqreg <- quantreg::rq(y ~ x + z, data = data, tau = tau)
  
  return(list(ivqreg = ivqreg, trueqreg = trueqreg))
}

IV.qfunction.mat <- function(n, sd.x, var.u1, var.u2, sd.z, b0, b.x, b.z, bias.iv, cov.u, tau) {
  u.sigma <- diag(c(var.u1, var.u2), ncol = 2, nrow = 2)
  u.sigma[1,2] = u.sigma[2,1] = cov.u
  
  x <- rnorm(n, 3, sd.x)
  z <- rnorm(n, 5, sd.z)
  u <- mvtnorm::rmvnorm(n, c(0,0), u.sigma)
  
  w <- x + u[,1]
  t <- bias.iv + x + u[,2]
  
  eps <- x*rnorm(n)
  
  y <- b0 + b.x*x + b.z*z + eps
  
  data <- data.frame(y = y, x = x, w = w, z = z)
  
  ivqreg <- IVqreg(y = y, w = w, z = z, t = t, tau = tau)$coefficients
  trueqreg <- quantreg::rq(y ~ x + z, data = data, tau = tau)$coefficients
  
  return(c(ivqreg, trueqreg))
}


# Part 0c: evaluating the IV with quantile regression on homoskeda --------

IV.results.mat <- matrix(nrow = 10^3, ncol = 6)
for (i in 1:10^3) {
  IV.results.mat[i,] <- IV.qfunction.mat(n = 100, sd.x = 2, var.u1 = 1, var.u2 = 1, 
                                         sd.z = 1, b0 = 2, b.x = 1, b.z = 3, bias.iv = 1, cov.u = 0, 
                                         tau = 0.9)
}

IV.results <- as.data.frame(IV.results.mat)
names(IV.results) <- c("beta0", "betaw", "betaz", "beta0.true", "betaw.true", "betaz.true")
IV.results <- IV.results %>% 
  mutate(beta0.bias = beta0 - beta0.true,
         betaw.bias = betaw - betaw.true,
         betaz.bias = betaz - betaz.true) %>% 
  select(beta0.bias, betaw.bias, betaz.bias) %>% 
  rename(beta_0 = beta0.bias, beta_w = betaw.bias, beta_z = betaz.bias) %>% 
  reshape2::melt() %>% rename(Coefficient = variable) 

Iv.resultsplot <- ggplot(IV.results, aes(x = Coefficient, y = value)) + 
  geom_boxplot() + theme_minimal()

ggsave("IVqreg_boxplots.pdf", Iv.resultsplot, height = 4, width =  6)


# Part 1: How measurement errror affects the procedure --------------------

n_sim <- 1:500
u.var <- seq(0,5, by = 0.2)

sim <- expand.grid(n = n_sim, u.var = u.var)

set.seed(1234)
sim.list <- lapply(1:nrow(sim), function(i) IV.qfunction(n = 100, sd.x = 2, var.u1 = sim$u.var[i], var.u2 = 1, 
                                                         sd.z = 1, b0 = 2, b.x = 1, b.z = 3, bias.iv = 1, cov.u = 0, 
                                                         tau = 0.9))


sim$beta0 <- sapply(1:nrow(sim), function(i) coef(sim.list[[i]]$ivqreg)[[1]])
sim$betaw <- sapply(1:nrow(sim), function(i) coef(sim.list[[i]]$ivqreg)[[2]])
sim$betaz <- sapply(1:nrow(sim), function(i) coef(sim.list[[i]]$ivqreg)[[3]])

beta0.true <- mean(sapply(1:nrow(sim), function(i) sim.list[[i]]$trueqreg$coefficients[[1]]))
betaw.true <- mean(sapply(1:nrow(sim), function(i) sim.list[[i]]$trueqreg$coefficients[[2]]))
betaz.true <- mean(sapply(1:nrow(sim), function(i) sim.list[[i]]$trueqreg$coefficients[[3]]))

sim$beta0_se <- sapply(1:nrow(sim), function(i) summary(sim.list[[i]]$ivqreg, 
                                                        se = "boot")$coefficients[1,2])
sim$betaw_se <- sapply(1:nrow(sim), function(i) summary(sim.list[[i]]$ivqreg, 
                                                        se = "boot")$coefficients[2,2])
sim$betaz_se <- sapply(1:nrow(sim), function(i) summary(sim.list[[i]]$ivqreg, 
                                                        se = "boot")$coefficients[3,2])

  ## Plotting

beta0 <- sim %>% 
  group_by(u.var) %>% 
  summarise(b0.mean = mean(beta0),
            b0.min = min(beta0),
            b0.max = max(beta0)) %>% 
  ggplot() +
  geom_ribbon(aes(x = u.var, ymin = b0.min, ymax = b0.max), fill = "grey80") + 
  geom_line(aes(x = u.var, y = b0.mean)) +
  xlab("Var(U)") + ylab("beta_0") + ggtitle("Beta 0") +
  geom_hline(yintercept = beta0.true, linetype = "dotted") + theme_minimal()

betaw <- sim %>% 
  group_by(u.var) %>% 
  summarise(bw.mean = mean(betaw),
            bw.min = min(betaw),
            bw.max = max(betaw)) %>% 
  ggplot() +
  geom_ribbon(aes(x = u.var, ymin = bw.min, ymax = bw.max), fill = "grey80") + 
  geom_line(aes(x = u.var, y = bw.mean)) +
  xlab("Var(U)") + ylab("beta_w") + ggtitle("Beta W") +
  geom_hline(yintercept = betaw.true, linetype = "dotted") + theme_minimal()

betaz <- sim %>% 
  group_by(u.var) %>% 
  summarise(bz.mean = mean(betaz),
            bz.min = min(betaz),
            bz.max = max(betaz)) %>% 
  ggplot() +
  geom_ribbon(aes(x = u.var, ymin = bz.min, ymax = bz.max), fill = "grey80") + 
  geom_line(aes(x = u.var, y = bz.mean)) +
  xlab("Var(U)") + ylab("beta_z") + ggtitle("Beta Z") +
  geom_hline(yintercept = betaz.true, linetype = "dotted") + theme_minimal()


beta0_se <- sim %>% 
  group_by(u.var) %>% 
  summarise(b0.mean_se = mean(beta0_se),
            b0.min_se = min(beta0_se),
            b0.max_se = max(beta0_se)) %>% 
  ggplot() +
  geom_ribbon(aes(x = u.var, ymin = b0.min_se, ymax = b0.max_se), fill = "grey80") + 
  geom_line(aes(x = u.var, y = b0.mean_se)) +
  xlab("Var(U)") + ylab("beta_0 SE") + ggtitle("Beta 0 SE") +
   theme_minimal()

betaw_se <- sim %>% 
  group_by(u.var) %>% 
  summarise(bw.mean_se = mean(betaw_se),
            bw.min_se = min(betaw_se),
            bw.max_se = max(betaw_se)) %>% 
  ggplot() +
  geom_ribbon(aes(x = u.var, ymin = bw.min_se, ymax = bw.max_se), fill = "grey80") + 
  geom_line(aes(x = u.var, y = bw.mean_se)) +
  xlab("Var(U)") + ylab("beta_w SE") + ggtitle("Beta W SE") +
  theme_minimal()

betaz_se <- sim %>% 
  group_by(u.var) %>% 
  summarise(bz.mean_se = mean(betaz_se),
            bz.min_se = min(betaz_se),
            bz.max_se = max(betaz_se)) %>% 
  ggplot() +
  geom_ribbon(aes(x = u.var, ymin = bz.min_se, ymax = bz.max_se), fill = "grey80") + 
  geom_line(aes(x = u.var, y = bz.mean_se)) +
  xlab("Var(U)") + ylab("beta_z SE") + ggtitle("Beta Z SE") +
  theme_minimal()

beta0 | betaw | betaz
beta0_se | betaw_se | betaz_se
ggsave("IVcoeff_sigmau.pdf", beta0 | betaw | betaz, height = 4, width = 14)
ggsave("IVcoeffse_sigmau.pdf", beta0_se | betaw_se | betaz_se, height = 4, width = 14)


# Part 2: Evaluating bias of the procedure --------------------------------


taus <- seq(0.1, 0.9, by = 0.1)
sim2 <- expand.grid(n = n_sim, u.var = u.var, tau = taus)

set.seed(1234)

# sim2.list <- parallel::mclapply(1:nrow(sim2), function(i) IV.qfunction(n = 100, sd.x = 2, var.u1 = sim2$u.var[i], var.u2 = 1, 
#                          sd.z = 1, b0 = 2, b.x = 1, b.z = 3, bias.iv = 1, cov.u = 0, 
#                          tau = sim2$tau[i]), mc.cores = detectCores() - 1)

# sim2.list <- lapply(1:nrow(sim2), function(i) {
#   values <- IV.qfunction(n = 100, sd.x = 2, var.u1 = sim2$u.var[i], var.u2 = 1,
#                sd.z = 1, b0 = 2, b.x = 1, b.z = 3, bias.iv = 1, cov.u = 0,
#                tau = sim2$tau[i])
#   print(i)
#   return(values)})

sim2.mat <- matrix(nrow = nrow(sim2), ncol = 6)
for (i in 1:nrow(sim2)) {
  sim2.mat[i,] <- IV.qfunction.mat(n = 100, sd.x = 2, var.u1 = sim2$u.var[i], var.u2 = 1,
                                   sd.z = 1, b0 = 2, b.x = 1, b.z = 3, bias.iv = 1, cov.u = 0,
                                   tau = sim2$tau[i])
  print(i)
}

# ?foreach
# 
# library(doParallel)
# detectCores()
# registerDoParallel(cores = 4)
# 
# foreach(i = 1:nrow(sim2)) %dopar% 
#   IV.qfunction(n = 100, sd.x = 2, var.u1 = sim2$u.var[i], var.u2 = 1, 
#                sd.z = 1, b0 = 2, b.x = 1, b.z = 3, bias.iv = 1, cov.u = 0, 
#                tau = sim2$tau[i])


sim2$beta0 <- sim2.mat[,1]
sim2$betaw <- sim2.mat[,3]
sim2$betaz <- sim2.mat[,3]

sim2$beta0.true <- sim2.mat[,4]
sim2$betaw.true <- sim2.mat[,5]
sim2$betaz.true <- sim2.mat[,6]

sim2$beta0.bias <- sim2.mat[,1] - sim2.mat[,4]
sim2$betaw.bias <- sim2.mat[,2] - sim2.mat[,5]
sim2$betaz.bias <- sim2.mat[,3] - sim2.mat[,6]


beta0q <- sim2 %>% 
  mutate(tau = factor(tau, levels = seq(0.1, 0.9, 0.1))) %>% 
  group_by(tau) %>% 
  ggplot(aes(x = tau,y = beta0.bias, group = tau)) + 
  geom_boxplot() + theme_minimal() + 
  xlab("Quantiles") + ylab("") + ggtitle("Beta 0")

betaWq <- sim2 %>% 
  mutate(tau = factor(tau, levels = seq(0.1, 0.9, 0.1))) %>% 
  group_by(tau) %>% 
  ggplot(aes(x = tau,y = betaw.bias, group = tau)) + 
  geom_boxplot() + theme_minimal() + ylim(c(-2, 4)) +
  xlab("Quantiles") + ylab("") + ggtitle("Beta W")

betaZq <- sim2 %>% 
  mutate(tau = factor(tau, levels = seq(0.1, 0.9, 0.1))) %>% 
  group_by(tau) %>% 
  ggplot(aes(x = tau,y = betaz.bias, group = tau)) + 
  geom_boxplot() + theme_minimal() + 
  xlab("Quantiles") + ylab("") + ggtitle("Beta Z")

beta0q | betaWq | betaZq
ggsave("IVcoeff_bias.pdf", beta0q | betaWq | betaZq, height = 4, width = 14)
