
Rprof('profiling/log1.out', line.profiling=TRUE)
source('codigo/modelo.R')
Rprof(NULL)
x <- summaryRprof('profiling/log1.out', lines = 'show')
slf <- x$by.self
x
slf

slf$self.time %>% sum

