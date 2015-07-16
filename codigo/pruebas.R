
Rprof('logs/log1.out', line.profiling=TRUE)
source('codigo/modelo.R')
Rprof(NULL)
x <- summaryRprof('logs/log1.out', lines = 'show')
slf <- x$by.self
x
slf

slf$self.time %>% sum




recomendados %>% mutate(sel = (p2 <=(1+price_range)*p1)) %>% View


# Prueba query con el texto en un archivo

library(RODBC)
library(dplyr)
con <- odbcConnect(dsn = 'sysmatrixi',
                   uid = 'bmxddt005062',
                   pwd = '')

cod <-readLines('codigo/prueba_query.sql') %>%
  paste(collapse=' ') %>%
  gsub(pattern = '\\t', replacement = ' ')
h <- sqlQuery(con, query = cod)

sqlQuery(con, query = 'SELECT top 5 * FROM Matrix_Reloaded.dbo.Hoteles')

sqlQuery(con, query = "
DECLARE @x TABLE (y INT)

INSERT INTO @x VALUES
(1)
,(3)
,(5)
,(2)

SELECT *
FROM @x
")

sqlQuery(con, query = "
EXEC Matrix_Reloaded.dbo.spfw_CheckFraud @CreditCardNumber = '4931610775238191'
")

sqlQuery(con, query = "
EXEC Matrix_Reloaded.dbo.spfw_CheckFraud @Email = 'alicce@gmail.com'
")



close(con)
