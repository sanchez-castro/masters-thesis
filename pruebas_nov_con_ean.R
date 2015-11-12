
library(dplyr)


length(unique(dat$Hotel))

qplot(Posicion, data=dat, binwidth=1) +
  geom_vline(xintercept=1+quantile(dat$Posicion, c(0.025,0.1,0.9,0.975)),
             color='red')

# Cuantiles del número de posiciones movidas
quantile(dat$Posicion, seq(0,1,0.1))

# Casos extremos
dat













