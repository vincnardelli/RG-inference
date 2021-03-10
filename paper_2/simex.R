library(ggplot2)
library(dplyr)
library(patchwork)
require(simex)
library(xtable)

set.seed(123)
n=100
varX = 2
varU = 2
varZ = 1
x = as.matrix(rnorm(n, 3, varX), ncol=1) # variabile non osservabile
z = as.matrix(rnorm(n, 5, varZ), ncol=1) # variabile osservabile senza errore
u = as.matrix(rnorm(n, 0, varU), ncol=1) # errore
w = x+u   # modello classico

b0 = 2
bx = 1
bz = 3
y = b0 + bx*x[1:n,1] + bz*z[1:n,1] + rnorm(n,0,1)
dat = data.frame(y=y, w=w, z=z)
modwz = lm(y~w+z, data = dat)
modxz = lm(y~x+z, data = dat)
  round(summary(modwz)$coefficients,4)
  round(summary(modxz)$coefficients,4)

set.seed(123)
modwz = lm(y~w+z, data = dat, x=T)
fit2df <- function(x){
  p.names <- names(coef(x))
  b <- modsimex$SIMEX.estimates
  a <- seq(-1, max(b[, 1]), by = 0.01)
  d <- matrix(data = NA, nrow = length(a), ncol = NCOL(b) - 1)
  
  switch(x$fitting.method,
         quad = d <- matrix(predict(x$extrapolation, newdata = data.frame(lambda = a)), nrow = length(a), ncol = NCOL(b) - 1),
         line = d <- matrix(predict(x$extrapolation, newdata = data.frame(lambda = a)), nrow = length(a), ncol = NCOL(b) - 1),
         nonl = for (i in 1:length(p.names)) d[, i] <- predict(x$extrapolation[[p.names[i]]], newdata = data.frame(lambda = a)))
  cbind(a+1, d) %>% 
    data.frame() %>% 
    rename(lambda = X1,
           Intercept=X2, 
           w=X3, 
           z=X4) %>% 
    mutate(fit=x$fitting.method)
}

functions <- c("linear", "quadratic")
simulations <- lapply(functions, function(x){
  set.seed(123)
  simex(modwz, SIMEXvariable = "w", B=500, measurement.error = sqrt(varU),
        fitting.method = x,
        lambda=seq(0.25,2,0.25))
})



fits <- lapply(simulations, fit2df)
fits_df <- do.call(rbind, fits)

points <- simulations[[1]][["SIMEX.estimates"]] %>% 
  as.data.frame() %>% 
  mutate(lambda = lambda+1) %>% 
  filter(lambda > 0.5) %>% 
  rename(Intercept = '(Intercept)')


interc <- ggplot() +
  geom_point(data=points, aes(lambda, Intercept), alpha=0.4) +
  geom_line(data=fits_df, aes(lambda, Intercept, color=fit)) +
  theme_bw() +
  ggtitle("Beta 0") +
  ylab("Beta 0") +
  geom_hline(yintercept = b0, linetype="dotted") +
  geom_hline(yintercept = summary(modwz)$coefficients[1,1]) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())



w <- ggplot() +
  geom_point(data=points, aes(lambda, w), alpha=0.4) +
  geom_line(data=fits_df, aes(lambda, w, color=fit)) +
  theme_bw()+
  ggtitle("Beta W") +
  ylab("Beta W") +
  geom_hline(yintercept = bx, linetype="dotted") +
  geom_hline(yintercept = summary(modwz)$coefficients[2,1]) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())



z <- ggplot() +
  geom_point(data=points, aes(lambda, z), alpha=0.4) +
  geom_line(data=fits_df, aes(lambda, z, color=fit)) +
  theme_bw() +
  ggtitle("Beta Z") +
  ylab("Beta Z") +
  geom_hline(yintercept = bz, linetype="dotted") +
  geom_hline(yintercept = summary(modwz)$coefficients[3,1]) +
  ylim(c(2.75, 3.25)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


graph <- (interc + w + z) + plot_layout(guides = "collect") & theme(legend.position = 'bottom')

graph


ggsave(paste0("figures/simex_varU",varU, ".pdf"), graph, height = 4, width = 8)

data.frame("generation" = c("(Intercept)"=b0, "w"=bx, "z"=bz),
           "without"  = summary(modwz)$coefficients[,1],
"linear" = summary(simulations[[1]])$coefficients$jackknife[,1],
"quadratic" = summary(simulations[[2]])$coefficients$jackknife[,1]) %>% 
  round(2) %>% 
  xtable()

