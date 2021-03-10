library(tidyverse)
library(AER)
library(mvtnorm)


# Part 0: data simulation and IV example ----------------------------------

  ## Data simulation

set.seed(123)

n = 100
varX = 2
varU = 0.75
varZ = 1
x = as.matrix(rnorm(n, 3, varX), ncol=1) # Unobservable variable
z = as.matrix(rnorm(n, 5, varZ), ncol=1) # Measurable variable (without any error)
u = as.matrix(rnorm(n, 0, varU), ncol=1) # Measurement error
w = x+u                                  # Variable measuring X (with error)

b0 = 2
bx = 1
bz = 3
y = b0 + bx*x[1:n,1] + bz*z[1:n,1] + rnorm(n,0,1)
dat = data.frame(y=y, w=w, z=z)

modwz = lm(y~w+z, data = dat)
modxz = lm(y~x+z, data = dat)
round(summary(modwz)$coefficients,4)
round(summary(modxz)$coefficients,4)

  ## Simple example

u1 = as.matrix(rnorm(n, 0, 1), ncol=1)
iv = x + u1

cor(iv, u)
cor(iv, u1)
cor(iv, x)
lm(iv ~ x + u)

cov(iv, y)/cov(iv, x)

dat = data.frame (dat, iv=iv)
modivreg = ivreg(y~w+z|iv+z, data = dat)
round(summary(modivreg)$coefficients,4)


# Unstability of estimate if instrument is weak ---------------------------

IV.function <- function(n, sd.x, sd.u1, sd.u2, sd.z, b0, b.x, b.z, bias.iv, cov.u) {
  u.sigma <- diag(c(sd.u1^2, sd.u2^2), ncol = 2, nrow = 2)
  u.sigma[1,2] = u.sigma[2,1] = cov.u
  
  x <- rnorm(n, 3, sd.x)
  z <- rnorm(n, 5, sd.z)
  eps <- rnorm(n, 0, 1)
  u <- rmvnorm(n, c(0,0), u.sigma)
  
  y <- b0 + b.x*x + b.z*z + eps
  
  w <- x + u[,1]
  t <- bias.iv + x + u[,2]
  
  data <- data.frame(y = y, w = w, z = z, t = t)
  
  ivreg <- ivreg(y~w+z|t+z, data = data)
  
  return(ivreg)
}

n_sim <- 1:500
u.sd <- seq(0,4, by = 0.2)

sim <- expand.grid(n = n_sim, u.sd = u.sd)

set.seed(123)
sim.list <- lapply(1:nrow(sim), function(i) IV.function(n = 100, sd.x = 2, sd.u1 = 1, sd.u2 = sim$u.sd[i], sd.z = 1, 
                                                     b0 = 2, b.x = 1, b.z = 3, bias.iv = 1, cov.u = 0))

# sim$beta.0 <- sapply(sim.list, function(x) coef(x)[[1]])
# sim$beta.0.sd <- sapply(sim.list, function(x) summary(x)$coefficients[1,2])
sim$beta.w <- sapply(sim.list, function(x) coef(x)[[2]])
# sim$beta.w.sd <- sapply(sim.list, function(x) summary(x)$coefficients[2,2])
# sim$beta.z <- sapply(sim.list, function(x) coef(x)[[3]])
# sim$beta.z.sd <- sapply(sim.list, function(x) summary(x)$coefficients[3,2])

  ## Plotting

betaw <- sim %>% 
  group_by(u.sd) %>% 
  summarise(bw.mean = mean(beta.w),
            bw.min = min(beta.w),
            bw.max = max(beta.w)) %>% 
  ggplot() +
  geom_ribbon(aes(x = u.sd, ymin = bw.min, ymax = bw.max), fill = "grey80") + 
  geom_line(aes(x = u.sd, y = bw.mean)) +
  xlab("sd(U)") + ylab("beta_x") + ggtitle("Estimates of beta_x") +
  geom_hline(yintercept = 1, linetype = "dotted") + theme_minimal()

# betaw.sd <- sim %>% 
#   group_by(u.sd) %>% 
#   summarise(bw.sd.mean = mean(beta.w.sd),
#             bw.sd.min = min(beta.w.sd),
#             bw.sd.max = max(beta.w.sd)) %>% 
#   ggplot() +
#   geom_ribbon(aes(x = u.sd, ymin = bw.sd.min, ymax = bw.sd.max), fill = "grey80") + 
#   geom_line(aes(x = u.sd, y = bw.sd.mean)) + 
#   theme_minimal()


# Consistency of IV when indep is violated --------------------------------


n.vec <- seq(10, 100, by = 15)
u.cov <- seq(0, 1, by = 0.25)

sim2 <- expand.grid(n.sim = n_sim, u.cov = u.cov, n = n.vec)

set.seed(123)
sim2.list <- lapply(1:nrow(sim2), function(i) IV.function(n = sim2$n[i], sd.x = 2, sd.u1 = 1, sd.u2 = 1, sd.z = 1, 
                                                         b0 = 2, b.x = 1, b.z = 3, bias.iv = 1, cov.u = sim2$u.cov[i]))


sim2$beta.w <- sapply(sim2.list, function(x) coef(x)[[2]])
# sim2$beta.w.sd <- sapply(sim2.list, function(x) summary(x)$coefficients[2,2])

  ## Plotting

betaw2 <- sim2 %>% 
  group_by(n, u.cov) %>% 
  summarise(betaw2.mean = mean(beta.w)) %>% 
  ggplot() + 
  geom_line(aes(x = n, y = betaw2.mean, color = as.factor(u.cov))) + 
  xlab("n") + ylab("beta_x") + ggtitle("Asymtpotical behavior of beta_x") + 
  guides(colour=guide_legend(title="Cov(T,U)")) + 
  theme_minimal()

library(patchwork)
betaw|betaw2
ggsave("IV_results.pdf", betaw | betaw2, height = 4, width = 8)
