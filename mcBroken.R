#' TK
#' Oct 23
#' cron job to get raw broken ice cream machines at mcdonalds
#' 
# 

# Libs
library(jsonlite)
library(tidyr)
library(fst)

# Fetch
iceCream <- fromJSON('https://mcbroken2.nyc3.cdn.digitaloceanspaces.com/geo.json')
iceCream <- data.frame(do.call(rbind, iceCream$features$geometry$coordinates), iceCream$features$properties)
names(iceCream) <- c('lon', 'lat', 'meh', 'is_broken', 'is_active', 'state', 'city', 'street', 'last_checked')
iceCream$meh <- NULL
iceCream$last_checked <- Sys.time() - extract_numeric(iceCream$last_checked)*60

# Save
nam <- paste0('~/mcBrokenData/',make.names(paste('mcBrokenAPI',Sys.time(),'fst')))
write_fst(iceCream, nam)
#cron_add('~/mcBroken/mcBroken.R', frequency = '1 */6 * * *', id = 'iceCream_data_fetch', description = 'Every 6hrs get mcD icream machine status')

# End
