mu_list <- list(c(-1, -1), c(1, 1))
sd_list <- list(0.4, 0.4)

f = function(q){
  log_densities <- sapply(1:length(mu_list), function(i){
    rowSums(t(dnorm(t(q), mean = mu_list[[i]], sd = sd_list[[i]], log=TRUE)))
  })
  log(rowSums(exp(log_densities)))
}

library(tidyverse)

x1 <- seq(-4, 4, length=100)
x2 <- seq(-4, 4, length=100)

inv_temp <- 1.0

crossing(x1, x2) %>%
  mutate(log_p = inv_temp*f(cbind(x1, x2))) %>%
  ggplot(aes(x1, x2)) +
  geom_contour(aes(z = exp(log_p))) + 
  theme_void()

ggsave("2017_12_CMStatistics/fig/PT_0.png", height=4, width=4)  

p1 <- crossing(x1, x2) %>%
  mutate(log_p = f(cbind(x1, x2))) %>%
  ggplot(aes(x1, x2)) +
  geom_contour(aes(z = exp(log_p))) + 
  theme_void() + 
  ggtitle(bquote(list("Target", pi(x)))) + 
  geom_point(x = 1.2, y = 1.3, col="red", size=4)
p2 <- crossing(x1, x2) %>%
  mutate(log_p = 0.25*f(cbind(x1, x2))) %>%
  ggplot(aes(x1, x2)) +
  geom_contour(aes(z = exp(log_p))) + 
  theme_void() + 
  ggtitle(bquote(list("Tempered target", pi(x)^beta)))+ 
  geom_point(x = -0.5, y = -0.5, col="red", size=4)

g <- gridExtra::grid.arrange(p1, ncol=2)
ggsave("2017_12_CMStatistics/fig/PT_0.png", g, height=3, width=6)  

g <- gridExtra::grid.arrange(p1, p2, ncol=2)
ggsave("2017_12_CMStatistics/fig/PT_2.png", g, height=3, width=6)  
