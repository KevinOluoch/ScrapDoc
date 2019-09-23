
# Load library

# if (!require("pacman")) install.packages("pacman")
# pacman::p_load_gh("trinker/textreadr")
# 
# pacman::p_load(textreadr, magrittr)
pacman::p_load_gh("trinker/pathr")
library(textreadr)
library(magrittr)
library(stringr)

# Get files
pricedocs <- list.files("./data",recursive = TRUE, full.names = TRUE)


for (pricedoc in pricedocs) {
    doc <- read_document("1.Retail Price for The Month of JULY  2013.docx") 
    i <- 1
    j <- 0
    table_num <- 0
    
    anchor <- 0
    birr <- 0
    
    while (TRUE) {
      print(i)
      if (substr(doc[i], 1, 5) == "Table"){
        table_num <- table_num + 1
        anchor <- i
        birr <- 0
        print(paste0("table_num: ", table_num))
        
        j <- j + 1
        if (j == 5)break()
      }
      
      
      
      if (i == anchor + 1 ){
        monthyear <- strsplit(doc[i],"\\s+")
      }
      if (i == anchor + 7 ){
        region <- gsub("\\s{6,}", "&&&", doc[i]) 
        region <- gsub(" ", "", region ) 
        region <- gsub("&&&", " ", region )
        region <- strsplit(region,"\\s+")
        region <- region[[1]][grepl("^[A-Za-z]+.+$", region[[1]])]
      }
      
      if (i == anchor + 7 ){
      str_locate(string, pattern)
      }
      if (i == anchor + 10 ){
        mrkts <- strsplit(doc[i],"\\s+")
        mrkts <- mrkts[[1]][grepl("^[A-Za-z]+.+$", mrkts[[1]])]
      }
      
      if (doc[i] == "P R I C E   I N    B I R R") birr <- 1
      print(doc[i])
      
      if (birr == 1){
        if (!grepl("...... Kg", doc[i])) ttle <- doc[i]
        if (grepl("...... Kg", doc[i])){
          prices_str <- strsplit(doc[i],"...... Kg")[[1]][2]
          prices <- strsplit(prices_str,"\\s+")
          prices <- prices[[1]][(prices[[1]] %in% "-") | grepl("^[[:digit:]]+", prices[[1]])]
        }
          
          
      }
      
      
      
      
      
      i <- i + 1
    }
    print(monthyear)
    print(region)
    print(mrkts)
    print(birr)
    print(prices)
    print(length(prices))
    # tmp <- prices[[1]][(prices[[1]]) %in% "-"]
    # print(tmp)
  break()
}


