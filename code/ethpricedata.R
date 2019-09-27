
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
pricedocs <- list.files("./data",recursive = TRUE, full.names = TRUE, pattern = "*[.]docx$")


for (pricedoc in pricedocs) {
    ##### Initialize Variables  #####
    doc <- read_document(pricedoc) 
    i <- 1
    j <- 0
    reg_set_num <- 0 # Table number: As labelled in the data
    table_num <- 0 # Number of consecutive tables with region(s) data
    mrkts <- list()
    
    anchor <- 0
    birr <- 0
    item_count <- 0
    mrkts_count <- 0
    prices <- list()
    region <- NA
    mrkts <- list()
    monthyear <- NA
    num_of_regions <- 0
    mylist <- list()
    
    ##### While Loop The DOc  #####    
    while (TRUE) {
      ##### Initiate and Save Table ####
      # print (paste("i", i))
      # print(doc[i])
      if ( grepl("^Table[[:space:]]+[0-9]+[[:space:]]+[:]", doc[i])){
        line_one <- doc[i]
        
        if (as.integer(gsub("[^0-9]", "", substr(doc[i], 1, 12))) != reg_set_num ){
          reg_set_num <- as.integer(gsub("[^0-9]", "", substr(doc[i], 1, 12)))
          table_num <- 0
        }
        
        if (j == 0 & paste(region, sep = '', collapse = '_') == "GAMBELLA_DIREDAWA"){
          j <- j + 1
          tmp  <- list(region, mrkts, table_num, table_id) 
          }
        # if (j == 300)break()
        if (table_num == 1){
          print('%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TABLE CHANGE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%')
          Regions <- vector()
          for (entry2 in c(1:num_of_regions)) {
            region_vec1 <- vector(mode = "character", length = length(unlist(mrkts[[entry2]])))
            region_vec1[] <- region[entry2]
            Regions <- c(Regions, region_vec1)
          }
          
          write.table(t(data.frame(Regions, Markets)), filename, sep = ",", col.names =F , append = T)
        }
        
        table_num <- table_num + 1
        if (table_num != 1){
          myDF <- data.frame()
          for (aa_region in c(1:num_of_regions)) {
            
            datalist = list()
            list_names <- names(prices)
            if(is.null(list_names)) break() 
            list_names <- list_names[endsWith(list_names, paste0("_", aa_region))]
            
            for (entry in c(1:length(mrkts[[aa_region]]))) {
              datalist[[length(datalist) + 1]] <- c(region[aa_region], mrkts[[aa_region]][entry])
              for (entry1 in  c(1:length(list_names))) {
                  datalist[[length(datalist)]] <- c(datalist[[length(datalist)]], prices[[ list_names[entry1 ] ]][entry] )
              }
            }
            mylist[[aa_region]] <- do.call(cbind, (datalist))[-c(1, 2),]
          }
          
          myDF <- do.call(cbind, (mylist))
          
          write.table(myDF, filename, sep = ",", col.names =F , append = T)
          mylist <- list()
          print(region)
        }
        
        
        
        item_count <- 0
        mrkts_count <- 0
        prices <- list()
        anchor <- i
        birr <- 0
        print(paste0("table_num: ", table_num))
        
      }
      
      
      ##### Get Month year information #####
      if (i == anchor + 1 ){
        monthyear <- strsplit(doc[i],"\\s+")
      }
      ##### Get the regions #####
      if (i == anchor + 7 ){
        region <- gsub("\\s{6,}", "&&&", doc[i])
        region <- gsub(" ", "", region )
        region <- gsub("&&&", " ", region )
        region <- strsplit(region,"\\s+")
        region_tmp <- region
        region <- region[[1]][grepl("^[A-Za-z]+.+$", region[[1]])]
        if (length(region) == 0){
          if(region_tmp[[1]][2] == "S") region <- "SNNP"
          }
        #### output filename with region as prefix ###
        filename_tmp <- gsub(
          "^.*Retail[[:space:]]+Price[[:space:]]+for[[:space:]]+The[[:space:]]+Month[[:space:]]+of[[:space:]]+",
          "", 
          pricedoc
          )
        filename_tmp <- gsub("[.]docx", "", gsub("[[:space:]]+", "", filename_tmp))
        
        table_id <- as.integer(gsub("[^0-9]", "", substr(line_one, 1, 12)))
        
        #### Make output file name
        filename <- paste0("output", "/", paste(region, sep = '', collapse = '_'), "_", table_id, "_", filename_tmp, ".csv")
      }
      
      ##### Get breaks in region (markets) #####
      if (i == anchor + 9 ){
        # Get region names (muiltiple in the table)
        ave_str.stp <- str_locate_all(doc[i], "AVERAGE")
        num_of_regions <- 0
        if (!is.na(ave_str.stp[[1]][1])){
          num_of_regions <- dim(ave_str.stp[[1]])[1]
          }
        # Get region names (one in the table)
        if (is.na(ave_str.stp[[1]][1])){
          num_of_regions <- 1
          
          ave_str.stp[[1]] <- t(as.matrix(c(nchar(doc[i]), nchar(doc[i]))))
          colnames(ave_str.stp[[1]]) <- c("start", "end")
          }
        }
      
      ##### Extract market names #####
      if (i == anchor + 10 ){
        start0_mrkt <- 1
        mrkts <- list()
        
        for (a_region in c(1:num_of_regions)){
          part_str <- substr(doc[i], start0_mrkt, as.numeric(ave_str.stp[[1]][a_region, 2]))
          mrkts1 <- strsplit(part_str,"\\s+")
          mrkts_count <- mrkts_count + 1
          mrkts[[mrkts_count]] <- mrkts1[[1]][grepl("^[A-Za-z]+.+$", mrkts1[[1]])]
          # print(part_str)
          # print(paste0("test: ", as.numeric(ave_str.stp[[1]][a_region, 2])))
          start0_mrkt <- as.numeric(ave_str.stp[[1]][a_region, 2]) + 1
          
        }
        
        Markets <- unlist(mrkts)
        mrkts_namepos <- str_locate_all(gsub("AVERAGE", "       ", doc[i+1]), "[A-Za-z]+[-]*[A-Za-z]*[[:space:]]")
        for (mrkts_pos in c(1:dim(mrkts_namepos[[1]])[1]) ) {
          for (Market in c(1:length(Markets))) {
            pos1 <- mrkts_namepos[[1]][mrkts_pos, "start"]
            pos2 <- mrkts_namepos[[1]][mrkts_pos, "end"]
            if (grepl(Markets[Market], substr(doc[i], pos1, pos1 + 10 ))) {
              Markets[Market] <- paste0(Markets[Market], " ", substr(doc[i+1], pos1, pos2 ))
              substr(doc[i],  pos1, pos1 + 3 ) <- "A2C"
              }
            
          }
           
        }
      }
      
      ##### GEt section head ####
      if (doc[i] == "P R I C E   I N    B I R R") birr <- 1
      # print(doc[i])
      
      ##### Get Price strings ####
      if (birr == 1){
        raw_price_str1 <- substr(doc[i], 1, 37)
        raw_price_str2 <- substr(doc[i], 37, nchar(doc[i]))
        if (nchar(raw_price_str2) == 0) ttle <- doc[i]
        
        
        if (!nchar(raw_price_str2) == 0){
          # Get item name
          item_name <- gsub('[.]{2,}', "", raw_price_str1)
          
          prices_str <- raw_price_str2 # strsplit(doc[i],"...... Kg")[[1]][2]
          start0_price <- 1
          
          for (a_region in c(1:num_of_regions)){
            part_prices_str <- doc[i]
            substr(part_prices_str, 1, 37) <- "abcdefghijklmnopqrstuvwxyzABCDEFGHIJK"
            part_prices_str <- substr(part_prices_str, start0_price, as.numeric(ave_str.stp[[1]][a_region, 2]))
            prices1 <- strsplit(part_prices_str,"\\s+")
            # item_count <- item_count + 1
            prices[[paste0(ttle, item_name, "_", a_region)]] <- prices1[[1]][(prices1[[1]] %in% "-") | 
                                                                               grepl("^[[:digit:]]+[.]{0,1}[[:digit:]]+$", prices1[[1]]) |
                                                                               grepl("^[.][[:digit:]][[:digit:]]$", prices1[[1]])
                                                                             ]
            # print(prices)
            if(is.na(prices[[paste0(ttle, item_name, "_", a_region)]][1])){
              prices[[paste0(ttle, item_name, "_", a_region)]] <- c()
              
              }
            start0_price <- as.numeric(ave_str.stp[[1]][a_region, 2]) + 1
          }
        }
          
          
      }
      
      
      
      
      
      i <- i + 1
      if (i > length(doc)) break()
    }
    
    
    ##### Results of "While Loop The DOc"  #####     
    # print(monthyear)
    print("$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$")
    # print(region)
    # print(mrkts)
    # print(birr)
    # print(prices)
    # print(length(prices))
    # tmp <- ave_str.stp
    # print(tmp)
   break()
}


