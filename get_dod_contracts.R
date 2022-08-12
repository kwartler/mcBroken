library(rvest)
library(stringr)
library(stringi)
st    <- '2015-01-01'
en    <- '2020-12-31'
maxPG <- 1000

getDailies <- function(stDate, enDate, maxPG){
  contractURLs <- paste0('https://www.defense.gov/Newsroom/Contracts/StartDate/',stDate,'/EndDate/',enDate,'/Page=',1:maxPG)
  cat('getting all urls assembled ')
  allDailies <- list()
  for(i in 1:length(contractURLs)){
    #print(i)
    #Sys.sleep(1)
    cat('.')
    dailyRelease <- read_html(contractURLs[i]) %>% 
      html_nodes('.title') %>% 
      html_attr("href")
    if (length(dailyRelease)==0) {
      warning(paste('maxPG value:',maxPG, 'did not read URLs.  Likely the last page in the scrape has occured and this is not an issue. Page:',i))
      break
    }
    allDailies[[i]] <- data.frame(sourceURL = contractURLs[i],
                                  dailyURLs = dailyRelease)
  }
  allDailies <- do.call(rbind, allDailies)
  cat('.complete\n')
  return(allDailies)
}

getRelease <- function(dailyURL){
  allContracts <- list()
  cat('getting all releases assembled ')
  for(j in 1:nrow(dailyURL)){
    #cat('.')
    #print(j)
    site <- as.character(dailyURL[j,2])
    dailyArticle <- read_html(site)
    header   <- dailyArticle %>% html_node('.maintitle') %>% html_text()
    header   <- gsub('\r|\n','',header) %>% trimws()

    txtBreak <- dailyArticle %>% html_nodes('strong') %>% html_text()
    txtBreak <- gsub('\r\n','', txtBreak)  #rando formatting on a few pgs
    txtBreak <- subset(txtBreak, nchar(txtBreak)>1) #rando formatting on a few pgs
    if(length(txtBreak)==0){ #rando page format
      txtBreak <- dailyArticle %>% html_nodes('b') %>% html_text()
    }
    rawTxt <- dailyArticle %>% html_nodes('p') %>% html_text()
    if(length(rawTxt)==3){ #rando page format & historical pages are F'ed
      content  <- dailyArticle %>% html_nodes('.content') %>% html_text()
      content  <- capture.output(cat(content))
      content  <- gsub('\r','', content) %>% trimws()
      rawTxt  <- stri_remove_empty(content)
    }
    
    rawTxt <- subset(rawTxt, nchar(rawTxt)>0)
    idx <- grep(paste0('\\b',txtBreak,'\\b', collapse = '|'),rawTxt)
    
#    for(i in 1:(length(idx)-1)){
#      st <- idx[i]
#      en <- idx[i+1]
#      getcontract <- setdiff(seq(st, en), c(st, en))
#      print(rawTxt[getcontract])
#      print('break')
#    }

    indContracts <- list()
    for(i in 1:length(idx)){
      if(i<=length(idx)-1){
        st <- idx[i]
        en <- idx[i+1]
        getcontract <- setdiff(seq(st, en), c(st, en))
        if(length(txtBreak)==0|length(getcontract)==0){txtAgency <- 'remove'} else {txtAgency <- txtBreak[i]}
        if(length(txtBreak)==0|length(getcontract)==0){txtSpecific <- 'remove'} else {txtSpecific <- rawTxt[getcontract]}
        x <- data.frame(agency = txtAgency,
                        txt    = trimws(txtSpecific))
        indContracts[[i]] <- x
        #print(txtBreak[i])
        #print(rawTxt[getcontract])
      } else {
        st <- tail(idx,1)
        en <- length(rawTxt)
        getcontract <- setdiff(seq(st, en), c(st, en))
        if(length(txtBreak)==0|length(getcontract)==0){txtAgency <- '~remove'} else {txtAgency <- txtBreak[i]} #historical pages are a mess.
        if(length(txtBreak)==0|length(getcontract)==0){txtSpecific <- '~remove'} else {txtSpecific <- rawTxt[getcontract]} #historical pages are a mess.
        x <- data.frame(agency = txtAgency,
                        txt    = trimws(txtSpecific))
        indContracts[[i]] <- x
        #print(txtBreak[i])
        #print(rawTxt[getcontract])
      }
   
    }
    indContracts <- do.call(rbind, indContracts)
    indContracts$date <- header
    indContracts$url <- site
    indContracts$pointer <- as.character(dailyURL[j,1])
    allContracts[[j]] <- indContracts
    cat('.')
  }
  cat('.complete\n')
  
  allContracts$txt <- allContracts$txt[-grep(paste0('\\b','*Small business', '\\b'),   allContracts$txt)]
  allContracts$txt <- allContracts$txt[-grep(paste0('\\b','Choose which Defense.gov products you want delivered to your inbox.', '\\b'),   
                                             allContracts$txt)]
  return(allContracts)
}

cleanTxt <- function(releases){
  allPRs <- do.call(rbind, releases) 
  allPRs <- allPRs[-grep(paste0('\\b','*Small business', '\\b'),   allPRs$txt),]
  allPRs <- allPRs[-grep(paste0('\\b','*Choose which Defense.gov products you want delivered to your inbox.', '\\b'),   allPRs$txt),]
  idx <- grep('~remove', allPRs$agency)
  allPRs <- allPRs[-idx,]
  return(allPRs)
}


allDays <- getDailies(st, en, maxPG)
rawTxt  <- getRelease(allDays)
finalDF <- cleanTxt(rawTxt)

write.csv(finalDF, paste0('~/dod_contracts/DOD_pressReleases_st_',st,'_en_',en,'.csv'), row.names = F)
