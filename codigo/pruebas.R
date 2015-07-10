
Rprof('logs/log1.out', line.profiling=TRUE)
source('codigo/modelo.R')
Rprof(NULL)
x <- summaryRprof('logs/log1.out', lines = 'show')
slf <- x$by.self
x
slf

slf$self.time %>% sum




recomendados %>% mutate(sel = (p2 <=(1+price_range)*p1)) %>% View

