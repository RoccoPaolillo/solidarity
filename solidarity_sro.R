# uploads ####
options(scipen = 999)
library(gdata)
# for crawling Twitter data 
library(academictwitteR)
# library(rtweet)
# library(corpus)
library(quanteda)
library(udpipe)
library(stopwords)
library(corpustools)
library(quanteda.textstats)
library(dplyr)
library(tidyverse)
library(lubridate)
library(tokenizers)
library(tidytext)
library(stringi)
library(readtext)
library(parallel)
library(stringr)
library(widyr)
library(irlba)
library(furrr)
library(stm)
library(slider)
library(ggthemes)
library(stminsights)
library("gridExtra")
library("ggrepel")  
library("dplyr")
library(readxl)
library(ggrepel)
library(ggpubr)

"%nin%" <- Negate("%in%")

setwd("C:/Users/rocpa/OneDrive/Desktop/CNT/solidarity_server/")

# IGNORE, to be deleted, back data.frame formation ####

# path <- list.files("translation/csvv/")
# # Dataframe processing and dfm creation (lists for processing in keywords_cnt.xls)
# 
# for (i in path) {
#   load(paste0("sample/orig_df/",i))
# }
# 
# df1921 <- rbind(de2019solidaritat,de2019solidaritaet,it2019solidarietà,de2020solidaritaet,de2020solidaritat,it2020solidarietà,
#                 de2021solidaritat,de2021solidaritaet,it2021solidarietà)
# 
# save(df1921,file="df1921.Rdata")
# 
# load("sample/df1921.Rdata")
# df1921 <- df1921[!duplicated(df1921$tweet_id),]
# 
# df1921 <- df1921 %>% filter((sourcetweet_type != "retweeted") %>% replace_na(TRUE))
# 
# ast <- df1921 %>% filter(str_detect(text,regex("solidar"))) 
# ast <- df1921 %>% filter(lang == "en" | lang == "de" | lang == "it")
# 
# # save(ast, file="sample/ast1921_filt.Rdata")
# # load("sample/ast1921_filt.Rdata")
# 
##### Adding variables
# ast$country <- "xxx"
# ast[ast$tweet_id %in% de2019solidaritaet$tweet_id,]$country <- "Germany"
# ast[ast$tweet_id %in% de2019solidaritat$tweet_id,]$country <- "Germany"
# ast[ast$tweet_id %in% de2020solidaritaet$tweet_id,]$country <- "Germany"
# ast[ast$tweet_id %in% de2020solidaritat$tweet_id,]$country <- "Germany"
# ast[ast$tweet_id %in% de2021solidaritaet$tweet_id,]$country <- "Germany"
# ast[ast$tweet_id %in% de2021solidaritat$tweet_id,]$country <- "Germany"
# ast[ast$tweet_id %in% it2019solidarietà$tweet_id,]$country <- "Italy"
# ast[ast$tweet_id %in% it2020solidarietà$tweet_id,]$country <- "Italy"
# ast[ast$tweet_id %in% it2021solidarietà$tweet_id,]$country <- "Italy"
# 
# ast$date <- as_date(ast$created_at)
# ast$datenum <- as.integer(ast$date)
# ast$dateyear <- format(as.Date(ast$date, format = "%d/%m/%Y"), "%Y")
# ast$dateyear <- as.numeric(ast$dateyear)
# 
# 
# 
# spc <- pull(read.csv("special_characters.csv", encoding = "UTF-8"),1) # delete emoticon, keep ASCII
# spct <- paste(spc,collapse="")
# chrmt <- paste0("[^\x01-\x7F",spct,"]")
# # cleaning apostrophes, @, https etc.
# ast$text <- tolower(ast$text)
# ast$text <- gsub("-","_",ast$text) # for keyword search
# # ast$text <- gsub("#"," ",ast$text) # hashtags read as words, not deleted
# ast$text <- gsub(chrmt," ", ast$text)  # for not ASCII (emoticon), keeps normal characthers + spct list
# ast$text <- gsub("&amp;", " ", ast$text) # remove how ";" is translated
# ast$text <- gsub("&gt;", " ", ast$text)
# ast$text <- gsub("&lt;", " ", ast$text)
# ast$text <- gsub("&ct;", " ", ast$text)
# ast$text <- gsub("&le;", " ", ast$text)
# ast$text <- gsub("&ge;", " ", ast$text)
# ast$text <- gsub("\\s+", " ", ast$text)
# ast$text <- gsub("\n", " ",  ast$text)
# ast$text <- gsub("\t", " ",  ast$text)
# ast$text <- gsub("'", " ",  ast$text)
# ast$text <- gsub("’", " ",  ast$text)
# ast$text <- str_replace_all(ast$text,
#                                 regex("http[s]?:/[/]?(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+",
#                                       ignore_case = TRUE)," ") # regex to identify websites, we don't need
# # ast$text <- str_replace_all(ast$text, regex("(?<=^|\\s)@[^\\s]+",
# #                                                     ignore_case=TRUE)," ") # regex to delete mentions @
# # ast$text <- str_replace_all(ast$text, regex("(?<=^|\\s).@[^\\s]+",ignore_case=TRUE)," ")
# # df_act20$text <- str_replace_all(df_act20$text,   regex("[a-zA-Z]*@[a-zA-Z]*",ignore_case=TRUE)," ") # regex to delete for .@ (didn't manage to merge with above)
# ast[ast$lang =="it",]$text <- gsub("á","à",   ast[ast$lang =="it",]$text)
# ast[ast$lang =="it",]$text <- gsub("é","è",   ast[ast$lang =="it",]$text)
# ast[ast$lang =="it",]$text <- gsub( "í","ì",  ast[ast$lang =="it",]$text)
# ast[ast$lang =="it",]$text <- gsub( "ó","ò",  ast[ast$lang =="it",]$text)
# ast[ast$lang =="it",]$text <- gsub( "ú","ù",  ast[ast$lang =="it",]$text)
# ast[ast$lang =="it",]$text <- gsub( "ú","ù",  ast[ast$lang =="it",]$text)
# 
# ast$text <- str_squish(ast$text)
# 
# ast$text <- gsub("solidaritaet","solidarität",ast$text)
# ast$text <- gsub("solidarieta","solidarietà",ast$text)
# 
# corpus_ast <- corpus(ast)
# docnames(corpus_ast) <- ast$tweet_id
# dfm_ast <- tokens(corpus_ast) %>% dfm()
# 
# save(corpus_ast,file="sample/corpus_ast.Rdata")
# save(dfm_ast,file="sample/dfm_ast.Rdata")
# 
# # ast_sim <- textstat_simil(dfm_ast, method="jaccard",margin="documents",min_simil = 0.85)
# 
# # ast2 <- ast %>% filter(!str_detect(text,"#solidarity|#solidaritaet|#solidarität|#solidarietà|#solidarità")) 
# 
# load("sample/sim_dflDE.Rdata")
# 
####  testing similarity
# sim_dflDE <- as.list(sim_dflDE,diag=FALSE)
# load("sample/sim_dflDElist.Rdata")
# 
# results_sim = list()
# for (i in names(sim_dflDElist)) {
#   txt <- dfl_de[dfl_de$tweet_id == i,]$text
#   id <- dfl_de[dfl_de$tweet_id == i,]$tweet_id
#   usrn <- dfl_de[dfl_de$tweet_id == i,]$user_username
#   b <- tibble(name = names(sim_dflDElist[i]),id = id, usrn = usrn, double = length(sim_dflDElist[[i]]),
#               text = txt)
#   results_sim[[i]] = b
# }
# 
# dfres <- bind_rows(results_sim, .id = "name")
# 
# load("sample/dfresIT.Rdata")
# 
# dfresIT <- dfresIT[!duplicated(dfresIT$text),]
# write.csv(dfresIT,file="sample/dfresITnast.csv",sep=",", row.names = F)
#
# 
# dfresdelete <- dfres %>% filter(id %nin% doublecheck[doublecheck$select == 1,]$id)
# 
# ast2 <- ast %>% filter(tweet_id %nin% dfresdelete$id)
# 
# load("sample/ast_final.Rdata")
# 
##### preprocessing
#
# # ast2$country <- "xxx"
# # ast2[ast2$tweet_id %in% de2019solidaritaet$tweet_id,]$country <- "Germany"
# # ast2[ast2$tweet_id %in% de2019solidaritat$tweet_id,]$country <- "Germany"
# # ast2[ast2$tweet_id %in% de2020solidaritaet$tweet_id,]$country <- "Germany"
# # ast2[ast2$tweet_id %in% de2020solidaritat$tweet_id,]$country <- "Germany"
# # ast2[ast2$tweet_id %in% de2021solidaritaet$tweet_id,]$country <- "Germany"
# # ast2[ast2$tweet_id %in% de2021solidaritat$tweet_id,]$country <- "Germany"
# # ast2[ast2$tweet_id %in% it2019solidarietà$tweet_id,]$country <- "Italy"
# # ast2[ast2$tweet_id %in% it2020solidarietà$tweet_id,]$country <- "Italy"
# # ast2[ast2$tweet_id %in% it2021solidarietà$tweet_id,]$country <- "Italy"
# # 
# # ast2$date <- as_date(ast2$created_at)
# # ast2$datenum <- as.integer(ast2$date)
# # ast2$dateyear <- format(as.Date(ast2$date, format = "%d/%m/%Y"), "%Y")
# # ast2$dateyear <- as.numeric(ast2$dateyear)
# 
# # save(ast2,file="sample/ast_final.Rdata")
# # Germany
# location_de <- read.xls("sample/location.xls",sheet = "location_de",encoding = "latin1")[,1]
# location_de <-  gsub("\\s*\\([^\\)]+\\)", "", location_de)
# location_de <- str_trim(location_de, side = "right")
# location_de <- tolower(location_de)
# location_de <- paste(location_de,collapse = "|")
# 
# dfl$user_location <- tolower(dfl$user_location)
# nolocde <- dfl %>% filter(country == "Germany") %>% filter(! str_detect(user_location,location_de))
# locde <- dfl %>% filter(country == "Germany") %>% filter( str_detect(user_location,location_de))
# save(locde,file="sample/ast_finDE.Rdata")
# 
# # Italy
# location_it <- read.xls("sample/location.xls",sheet = "location_it",encoding = "latin1")[,1]
# location_it <-  gsub("\\s*\\([^\\)]+\\)", "", location_it)
# location_it <- str_trim(location_it, side = "right")
# location_it <- tolower(location_it)
# location_it <- paste(location_it,collapse = "|")
# 
# dfl$user_location <- tolower(dfl$user_location)
# nolocit <- dfl %>% filter(country == "Italy") %>% filter(! str_detect(user_location,location_it))
# locit <- dfl %>% filter(country == "Italy") %>% filter( str_detect(user_location,location_it))
# save(locit,file="sample/ast_finIT.Rdata")
# 
# 
# nolocitlist <- unique(nolocit$user_location)
# write.csv(nolocitlist,file="sample/nolocitlist.csv",row.names = F)
# 
# load("sample/ast_finIT.Rdata")
# load("sample/ast_finDE.Rdata")
# 
# locde$countwrd <- str_count(locde$text, "\\S+") 
# locit$countwrd <- str_count(locit$text, "\\S+") 
# 
# locit <- locit %>% filter(countwrd >= 15)
# locde <- locde %>% filter(countwrd >= 15)
# 
# dfl_de <- locde
# dfl_it <- locit
# 
# dfl_finloc <- rbind(dfl_de,dfl_it)
# 
# save(dfl_de,file="sample/dfl_nastDE.Rdata")
# save(dfl_it,file="sample/dfl_nastIT.Rdata")
# 
# save(dfl_finloc,file="sample/dfl_finloc.Rdata")
# 
# load("sample/sim_dflDElist.Rdata")
# 
# load("sample/dfresDEfull.Rdata")
# load("sample/dfresITfull.Rdata")
# 
# dfresfiltDE <- read.csv("sample/checksimDEnast.csv",sep =";")
# dfresfiltDE$name <- gsub("_","",dfresfiltDE$name)
# 
# dfresfiltIT <- read.csv("sample/dfresITnast.csv",sep =";")
# dfresfiltIT$name <- gsub("_","",dfresfiltIT$name)
# 
# dfresDEfull <- dfresDEfull %>% filter(name %nin% dfresfiltDE[dfresfiltDE$select == 1,]$name)
# dfresITfull <- dfresITfull %>% filter(name %nin% dfresfiltIT[dfresfiltIT$select == 1,]$name)
# 
# dfresout <- rbind(dfresDEfull,dfresITfull)
# 
# dfl_finloc <- dfl_finloc %>% filter(tweet_id %nin% dfresout$name)
# save(dfl_finloc,file="sample/dfl_finloc.Rdata")
# 
# load("sample/dfl_finloc.Rdata")
# 
# dfl_finloc$text <- str_replace_all(dfl_finloc$text, regex("(?<=^|\\s)@[^\\s]+",
#                                             ignore_case=TRUE)," ") # regex to delete mentions @
# dfl_finloc$text <- str_replace_all(dfl_finloc$text, regex("(?<=^|\\s).@[^\\s]+",ignore_case=TRUE)," ")
# dfl_finloc$text <- str_replace_all(dfl_finloc$text,   regex("[a-zA-Z]*@[a-zA-Z]*",ignore_case=TRUE)," ") # regex to delete for .@ (didn't manage to merge with above)
# 
# dfl_finlocast <- dfl_finloc %>% filter(str_detect(text,regex("#solidar"))) 
# # dfl_finlocast <- dfl_finlocast %>% filter(lang != "en")
# 
# dfl_finlocastIT19 <- dfl_finlocast[dfl_finlocast$country == "Italy" & dfl_finlocast$lang == "it" & dfl_finlocast$dateyear == 2019,]
# dfl_finlocastIT_de <- dfl_finlocastIT_de %>% select(tweet_id,text)
# dfl_finlocastIT_de$tweet_id <- paste0("_",dfl_finlocastIT_de$tweet_id,"_")
# save(dfl_finlocastIT_de,file="sample/dfl_finlocastIT_de.Rdata")
# 
# write.csv(dfl_finlocastDE20,file="sample/dfl_finlocastDE20.csv",sep=";",row.names = F)
# 
# dfl_finlocastIT <- dfl_finlocast[dfl_finlocast$country == "Italy",]
# 
# save(dfl_finlocastDE,file="sample/dfl_finlocastDE.Rdata")
# save(dfl_finlocastIT,file="sample/dfl_finlocastIT.Rdata")
# dfl_finlocastIT <- dfl_finlocastIT %>% select(tweet_id,text)
# dfl_finlocastIT$tweet_id <- paste0("_",dfl_finlocastIT$tweet_id,"_")
# save(dfl_finlocastIT,file="sample/dfl_finlocastIT_trsl.Rdata")
# 
# write.csv(dfl_finlocastDE,file="sample/dfl_finlocastDE_trsl.csv",sep=";",row.names = F)
# 
# dfl_trsl <- dfl_finloc %>% filter(dateyear == 2021 & country == "Italy")
# dfl_trsl <- dfl_trsl %>% select(tweet_id,text)
# dfl_trsl$tweet_id <- paste0("_",dfl_trsl$tweet_id,"_")
# 
# save(dfl_trsl,file="sample/dfl_trslIT2021.Rdata")
# 
# load("dfl_trsl2020.Rdata")
# write.csv(dfl_trsl,file="sample/dfl_trsl2020.csv",sep=",",row.names = F)
#
###### compose dataframe final
# DE19_1 <- read.csv2("translation/csvv/dfl_finlocastDE19_1.csv",sep =";", encoding = "latin1")
# DE19_1 <- DE19_1[1:3000,]
# DE19_2 <- read.csv2("translation/csvv/dfl_finlocastDE19_2.csv",sep =";", encoding = "UTF-8")
# DE19_2 <- DE19_2[1:2184,]
# DE20_1 <- read.csv2("translation/csvv/dfl_finlocastDE20_1.csv",sep =";", encoding = "UTF-8")
# DE20_2 <- read.csv2("translation/csvv/dfl_finlocastDE20_2.csv",sep =";", encoding = "UTF-8")
# DE20_3 <- read.csv2("translation/csvv/dfl_finlocastDE20_3.csv",sep =";", encoding = "UTF-8")
# DE20_4 <- read.csv2("translation/csvv/dfl_finlocastDE20_4.csv",sep =";", encoding = "UTF-8")
# DE21_1 <- read.csv2("translation/csvv/dfl_finlocastDE21_1.csv",sep =",", encoding = "UTF-8")
# DE21_2 <- read.csv2("translation/csvv/dfl_finlocastDE21_2.csv",sep =";", encoding = "UTF-8")
# DE21_3 <- read.csv2("translation/csvv/dfl_finlocastDE21_3.csv",sep =";", encoding = "UTF-8")
# DE21_3 <- DE21_3[1:1555,]
# IT_de <- read.csv2("translation/csvv/dfl_finlocastIT_de.csv",sep =";", encoding = "UTF-8")
# IT19_1 <- read.csv2("translation/csvv/dfl_finlocastIT19_1.csv",sep =";", encoding = "UTF-8")
# IT19_1 <- IT19_1[1:4366,]
# IT20_1 <- read.csv2("translation/csvv/dfl_finlocastIT20_1.csv",sep =";", encoding = "UTF-8")
# IT20_1 <- IT20_1[1:4500,]
# IT20_2 <- read.csv2("translation/csvv/dfl_finlocastIT20_2.csv",sep =";", encoding = "UTF-8")
# IT21_1 <- read.csv2("translation/csvv/dfl_finlocastIT21_1.csv",sep =";", encoding = "UTF-8")
# IT21_1 <- IT21_1[1:3151,]
# 
# df_finlocastDEtrl <- rbind(DE19_1,DE19_2,DE20_1,DE20_2,DE20_3,DE20_4,DE21_1,DE21_2,DE21_3)
# df_finlocastDEtrl <- df_finlocastDEtrl[!duplicated(df_finlocastDEtrl$tweet_id),]
# df_finlocastITtrl <- rbind(IT_de,IT19_1,IT20_1,IT20_2,IT21_1)
# 
# load("translation/df_finlocast.Rdata")
# df_finlocasttrl <- rbind(df_finlocastDEtrl,df_finlocastITtrl)
# 
# df_finlocasttrl$tweet_id <- gsub("_","",df_finlocasttrl$tweet_id)
# save(df_finlocasttrl, file="translation/csvv/df_finlocasttrl.Rdata")
# 
# df_finlocast <- df_finlocast[,-36]
# df_finlocast <- merge(df_finlocast,df_finlocasttrl,by = "tweet_id")
# load("sample/df_finlocast.Rdata")
# 
# df_finlocast$text <- str_replace_all(df_finlocast$text,"\\b's\\b","") 
# df_finlocast$text <- str_replace_all(df_finlocast$text,"\\b-\\b","_") 
# 
# save(df_finlocast,file="sample/df_finlocast.Rdata")
#
#### for n-gram detection
#
# bigrams_tx  <- df_we[,-3] %>% unnest_tokens(bigram, text, token = "ngrams", n = 2)
# bigrams_tx  %>% dplyr::count(bigram, sort = TRUE)
# bigrams_separate  <- bigrams_tx  %>% separate(bigram,c("word1","word2"),sep=" ")
# bigrams_filtered  <- bigrams_separate  %>%
#   filter(!word1 %in% stopwords_en) %>%
#   filter(!word2 %in% stopwords_en)
# bigrams_filtered <- bigrams_filtered  %>%  dplyr::count(word1, word2, sort = TRUE)
# bigrams_united <- bigrams_filtered  %>% unite(bigram, word1, word2, sep = " ")
# # bigrams_united  <- bigrams_united$bigram
# bigrams_united <- unique(bigrams_united)
# 
# write.csv(bigrams_united,"sample/bigrams_dfwe.csv",row.names= F)
# 
# trigrams_tx <- df_finlocast[,-3] %>%  unnest_tokens(trigram, text, token = "ngrams", n = 3)
# trigrams_tx %>% dplyr::count(trigram, sort = TRUE)
# trigrams_separate  <- trigrams_tx %>% separate(trigram,c("word1","word2","word3"),sep=" ")
# trigrams_filtered <- trigrams_separate  %>%
#   filter(!word1 %in% stopwords_en) %>%
#   filter(!word2 %in% stopwords_en) %>%
#   filter(!word3 %in% stopwords_en)
# trigrams_filtered  <- trigrams_filtered %>%  dplyr::count(word1, word2,word3, sort = TRUE)
# trigrams_united  <- trigrams_filtered  %>% unite(trigram, word1, word2,word3, sep = " ")
# # trigrams_united <- trigrams_united$trigram
# trigrams_united <- unique(trigrams_united)
# # 
# write.csv(trigrams_united,"sample/trigrams_hifen.csv",row.names= F)
# 
# quadrigrams_tx  <- df_finlocast[,-3] %>%  unnest_tokens(quadrigram, text, token = "ngrams", n = 4)
# quadrigrams_tx  %>% dplyr::count(quadrigram, sort = TRUE)
# quadrigrams_separate  <- quadrigrams_tx  %>% separate(quadrigram,c("word1","word2","word3","word4"),sep=" ")
# quadrigrams_filtered  <- quadrigrams_separate  %>%
#   filter(!word1 %in% stopwords_en) %>%
#   filter(word2 == "and") %>%
#   filter(!word3 %in% stopwords_en) %>%
#   filter(!word4 %in% stopwords_en)
# quadrigrams_filtered <- quadrigrams_filtered  %>%  dplyr::count(word1, word2,word3,word4, sort = TRUE)
# quadrigrams_united <- quadrigrams_filtered  %>% unite(quadrigrams, word1, word2,word3,word4, sep = " ") 
# 
# write.csv(quadrigrams_united,"sample/quadrigrams_and_hifen.csv",row.names= F)


# # Empirical datasets ####
# 
# # Eurostat inflation
# 
# umpl <- read.csv("sample/unemployment1923.csv",sep=";")
# names(umpl[,2:52]) <- as.numeric(names(umpl[,2:52]))
# names(umpl) <- gsub("X","",names(umpl))
# names(umpl) <- gsub("\\b.\\b" ,"-",names(umpl))
# 
# umpl <- gather(umpl, date, unemployment, c(2:52), factor_key=TRUE)
# umpl$unemployment <- as.numeric(umpl$unemployment)
# umpl[is.na(umpl)] <- 0
# 
# umpl$date <- paste0(umpl$date,"-01")
# umpl$date <- as_date(umpl$date)
# umpl$datenum <- as.integer(umpl$date)
# 
# infl <- read.csv("sample/inflation1923.csv",sep=";")
# names(infl[,2:48]) <- as.numeric(names(infl[,2:48]))
# names(infl) <- gsub("X","",names(infl))
# names(infl) <- gsub("\\b.\\b" ,"-",names(infl))
# 
# 
# 
# infl <- gather(infl, date, inflation, c(2:48), factor_key=TRUE)
# infl$inflation <- gsub("\\b,\\b",".",infl$inflation)
# infl$inflation <- as.numeric(infl$inflation)
# infl[is.na(infl)] <- 0
# 
# infl$date <- paste0(infl$date,"-01")
# infl$date <- as_date(infl$date)
# infl$datenum <- as.integer(infl$date)
# 
# debt <- read.csv("sample/gross_debt1923.csv",sep=";")
# names(debt[,2:24]) <- as.numeric(names(debt[,2:24]))
# names(debt) <- gsub("X","",names(debt))
# names(debt) <- gsub("\\b.\\b" ,"-",names(debt))
# 
# debt <- gather(debt, date, debtgross, c(2:24), factor_key=TRUE)
# debt$debtgross <- gsub("\\b,\\b",".",debt$debtgross)
# debt$debtgross <- as.numeric(debt$debtgross)
# debt[is.na(debt)] <- 0
# 
# debt$date <- paste0(debt$date,"-01-01")
# debt$date <- as_date(debt$date)
# debt$datenum <- as.integer(debt$date)
# 
# debt[debt$country == "Germany (until 1990 former territory of the FRG)",]$country <- "Germany"
# 
# debt <- gather(debt,variable,value,3, factor_key=TRUE)
# infl <- gather(infl,variable,value,3, factor_key=TRUE)
# umpl <- gather(umpl,variable,value,3, factor_key=TRUE)
# 
# df_eurostat <- rbind(debt,infl,umpl)
# 
# 
# # df_eurostat[df$GEO == "Germany" & df_eurostat$date == "2021-03",]$inflation <- 2.0
# # df_eurostat[df$GEO == "Germany" & df_eurostat$date == "2021-11",]$inflation <- 6.0
# # df_eurostat[df$GEO == "Italy" & df_eurostat$date == "2020-09",]$inflation <- -1.0
# # df_eurostat[df$GEO == "Italy" & df_eurostat$date == "2021-02",]$inflation <- 1.0
# # df_eurostat[df$GEO == "Italy" & df_eurostat$date == "2021-04",]$inflation <- 1.0
# # df_eurostat[df$GEO == "Italy" & df_eurostat$date == "2021-07",]$inflation <- 1.0
# # df_eurostat <- df_eurostat %>% rename(country = GEO)
# names(df_eurostat)[names(df_eurostat) == "country"] <- "GEO"
# df_eurostat$date <- paste0(df_eurostat$date,"-01")
# df_eurostat$date <- as_date(df_eurostat$date)
# df_eurostat$datenum <- as.integer(df_eurostat$date)
# 
# 
# save(df_eurostat,file="sample/df_eurostat.Rdata")
# rm(df_eurostat)
# load("sample/df_eurostat.Rdata")
# 
# who <- read.csv("WHO.csv",sep=";")
# who <- who %>% rename(
#   country = Country,
#   date = Date_reported
# )
# 
# who <- who %>% filter(country %in% c("Italy","Germany"))
# who$date <- gsub("/","-",who$date)
# who$datenum <- as_date(who$date,format = "%d-%m-%y")
# who$datenum <- as.integer(who$datenum)
# who <- who %>% filter(datenum <= 18992)
# save(who,file="sample/whodf.Rdata")
# 
# df <- who %>% filter(Country == "Italy")
# df$normcase <- norm_minmax(df$New_cases)
# dfde <- who %>% filter(Country == "Germany")
# dfde$normcase <- norm_minmax(dfde$New_cases)
# 
# whodf <- rbind(df,dfde)
# 
# 
# save(whodf,file="sample/whodf.Rdata")






# upload data ####

load("sample/df_finlocast.Rdata")
df_finlocast$text <- str_squish(df_finlocast$text)
# rem <- read.xls("sample/compound.xls",sheet = "rem", encoding = "latin1")[,1]
rem <- read_xls("sample/compound.xls",sheet = "rem")[,1]
rem <- as.character(rem$remove_list)
rem <- unique(rem)
rem <- gsub("#","",rem)
# cvd <- read.xls("sample/compound.xls",sheet = "cvd", encoding = "latin1")[,1] 
cvd <- read_xls("sample/compound.xls",sheet = "cvd")[,1] 
cvd <- as.character(cvd$cvd_list)
cvd <- gsub("#","",cvd)
rem_char <- c("http*","@*","€","+","|","s","faq","=","_","__","~","___")
# rem_de <- read.xls("sample/compound.xls",sheet = "rem_de", encoding = "latin1")[,1]
# rem_de <- unique(rem_de)
# rem_de <- gsub("#","",rem_de)

# clean duplicates
df_finlocast$selected <- 1
df_finlocast[11919,]$selected <- 0
df_finlocast <- df_finlocast[!duplicated(df_finlocast$original_language),]
df_finlocast <- df_finlocast %>% filter(! user_username %in% c("AntonioPinna11","AssGSintini","ThierryCuisine","FranceCarlucci_","StadtViersen")) # "StadtViersen"
df_finlocast <- df_finlocast %>% filter(selected == 1)

# delete tweets
# dl_tw <- read.xls("sample/compound.xls",sheet = "dl_tw", encoding = "latin1")[,1]
dl_tw <- read_xls("sample/compound.xls",sheet = "dl_tw")[,1]
dl_tw <- as.character(dl_tw$delete_tweets)
dl_tw <- gsub("_","",dl_tw)
df_finlocast <- df_finlocast %>% filter(! tweet_id %in% dl_tw)
# df_finlocast <- df_finlocast %>% filter(tweet_id != "1300053606247337984")
# df_finlocast <- df_finlocast %>% filter(tweet_id != "1253757045616107521")

df_finlocast[df_finlocast$tweet_id == "1324995953661718528",]$text <- gsub("_"," ",df_finlocast[df_finlocast$tweet_id == "1324995953661718528",]$text)
df_finlocast[df_finlocast$user_username == "Valori_it",]$text <- str_replace_all(df_finlocast[df_finlocast$user_username == "Valori_it",]$text, "\\bvalues\\b","" )
df_finlocast[df_finlocast$user_username == "CGavinana",]$text <- str_replace_all(df_finlocast[df_finlocast$user_username == "CGavinana",]$text, "\\bflorence\\b","" )
df_finlocast[df_finlocast$user_username == "fgbrdkuba",]$text <- str_replace_all(df_finlocast[df_finlocast$user_username == "fgbrdkuba",]$text, "\\bcuba\\b","" )
df_finlocast[df_finlocast$user_username == "kubakunde",]$text <- str_replace_all(df_finlocast[df_finlocast$user_username == "kubakunde",]$text, "\\bcuba\\b","" )
df_finlocast[df_finlocast$user_username == "fgbrdkuba",]$text <- str_replace_all(df_finlocast[df_finlocast$user_username == "fgbrdkuba",]$text, "\\bbrd_cuba\\b","" )
df_finlocast[df_finlocast$user_username == "kubakunde",]$text <- str_replace_all(df_finlocast[df_finlocast$user_username == "kubakunde",]$text, "\\bbrd_cuba\\b","" )
df_finlocast[df_finlocast$user_username %in% c("MiTomorrow","pcr_lombardia"),]$text <- str_replace_all(df_finlocast[df_finlocast$user_username %in% c("MiTomorrow","pcr_lombardia"),]$text, "\\bmilan\\b","" )
df_finlocast[df_finlocast$user_username == "san_moscati",]$text <- str_replace_all(df_finlocast[df_finlocast$user_username == "san_moscati",]$text, "\\bnapoli\\b","" )
df_finlocast[df_finlocast$user_username == "froehlich_mia",]$text <- str_replace_all(df_finlocast[df_finlocast$user_username == "froehlich_mia",]$text, "\\bcohesion\\b","" )
df_finlocast[df_finlocast$user_username == "CaritasPadova",]$text <- str_replace_all(df_finlocast[df_finlocast$user_username == "CaritasPadova",]$text, "\\bcaritas\\b","" )
df_finlocast[df_finlocast$user_username == "Borderline_24",]$text <- str_replace_all(df_finlocast[df_finlocast$user_username == "Borderline_24",]$text, "\\bbari\\b","" )
df_finlocast[df_finlocast$user_username == "CaritasPadova",]$text <- str_replace_all(df_finlocast[df_finlocast$user_username == "CaritasPadova",]$text, "\\bpadova\\b","" )
df_finlocast[df_finlocast$user_username %in% c("DieLinke_HH","LinksfraktionHH"),]$text <- str_replace_all(df_finlocast[df_finlocast$user_username %in% c("DieLinke_HH","LinksfraktionHH"),]$text, "\\bhamburg\\b","" )
df_finlocast[df_finlocast$user_username == "comunerimini",]$text <- str_replace_all(df_finlocast[df_finlocast$user_username == "comunerimini",]$text, "\\brimini\\b","" )
df_finlocast[df_finlocast$country == "Italy",]$text <- str_replace_all(df_finlocast[df_finlocast$country == "Italy",]$text, "\\bmes\\b","esm" )
df_finlocast[df_finlocast$user_username == "SolidarischeN",]$text <- str_replace_all(df_finlocast[df_finlocast$user_username == "SolidarischeN",]$text, "\\btübingen\\b","" )
df_finlocast[df_finlocast$user_username == "ilGiunco",]$text <- str_replace_all(df_finlocast[df_finlocast$user_username == "ilGiunco",]$text, "\\bgrosseto\\b","" )
df_finlocast[df_finlocast$user_username == "ilGiunco",]$text <- str_replace_all(df_finlocast[df_finlocast$user_username == "ilGiunco",]$text, "\\bthejun\\b","" )
df_finlocast[df_finlocast$user_username %in% c("Toscanaoggi","toscanatoday"),]$text <- str_replace_all(df_finlocast[df_finlocast$user_username %in% c("Toscanaoggi","toscanatoday"),]$text, "\\btoscana\\b","" )
df_finlocast[df_finlocast$user_username == "Brunopolik",]$text <- str_replace_all(df_finlocast[df_finlocast$user_username == "Brunopolik",]$text, "\\bdada\\b","" )
df_finlocast[df_finlocast$user_username == "SolidarietaBG",]$text <- str_replace_all(df_finlocast[df_finlocast$user_username == "SolidarietaBG",]$text, "\\bcividino\\b","" )
df_finlocast[df_finlocast$user_username == "SolidarietaBG",]$text <- str_replace_all(df_finlocast[df_finlocast$user_username == "SolidarietaBG",]$text, "\\bbortolotti\\b","" )
df_finlocast[df_finlocast$user_username == "SolidarietaBG",]$text <- str_replace_all(df_finlocast[df_finlocast$user_username == "SolidarietaBG",]$text, "\\bfacchetti\\b","" )
df_finlocast[df_finlocast$user_username == "SolidarietaBG",]$text <- str_replace_all(df_finlocast[df_finlocast$user_username == "SolidarietaBG",]$text, "\\bmorotti\\b","" )
df_finlocast[df_finlocast$user_username == "SolidarietaBG",]$text <- str_replace_all(df_finlocast[df_finlocast$user_username == "SolidarietaBG",]$text, "\\blosportislife\\b","" )
df_finlocast[df_finlocast$user_username == "WingedVictory5",]$text <- str_replace_all(df_finlocast[df_finlocast$user_username == "WingedVictory5",]$text, "\\bmongolrally\\b","" )
df_finlocast[df_finlocast$user_username == "LaMilanesiana",]$text <- str_replace_all(df_finlocast[df_finlocast$user_username == "LaMilanesiana",]$text, "\\blamilanesiana2019\\b","" )
df_finlocast[df_finlocast$user_username == "OsservaSocialis",]$text <- str_replace_all(df_finlocast[df_finlocast$user_username == "OsservaSocialis",]$text, "\\bsocialresponsibility\\b","" )
df_finlocast[df_finlocast$user_username == "OsservaSocialis",]$text <- str_replace_all(df_finlocast[df_finlocast$user_username == "OsservaSocialis",]$text, "\\bterritory\\b","" )
df_finlocast[df_finlocast$country == "Italy",]$text <- str_replace_all(df_finlocast[df_finlocast$country == "Italy",]$text, "\\broma\\b","rome" )
df_finlocast[df_finlocast$tweet_id == "1190216177974415360",]$text <- "for a solidary district! 14 o'clock vinetaplatz vonovia expropriate usury of rents end with high rents bad_apartments! end with rip-off by housing_corporations displacement!"
df_finlocast[df_finlocast$tweet_id == "1337268447424737281",]$text <- "mask on stay_at_home powerdenladenend immediately, also to prevent such individual fates finally solidarity besides 10_20 % of the officially recovered are not healthy! recovered but not healthy continue to suffer from longcovid, brainfog, mecfs, etc."

# df_finlocast[df_finlocast$user_username == "kubakunde",]$text <- str_replace_all(df_finlocast[df_finlocast$user_username == "cubasalvavidas",]$text, "\\bcuba\\b","" )
# df_finlocast[df_finlocast$user_username == "fgbrdkuba",]$text <- str_replace_all(df_finlocast[df_finlocast$user_username == "cubasalvavidas",]$text, "\\bbrd_cuba\\b","" )


# cmpd <- read.xls("sample/compound.xls",sheet = "cmpd", encoding = "latin1")[,1]
cmpd <- read_xls("sample/compound.xls",sheet = "cmpd")[1]
cmpd <- as.character(cmpd$cmpd_target)
cmpd <- paste0("\\b",cmpd,"\\b")
# cmpdsubstitute <- read.xls("sample/compound.xls",sheet = "cmpd", encoding = "latin1")[,2]
cmpdsubstitute <- read_xls("sample/compound.xls",sheet = "cmpd")[,2]
cmpdsubstitute <- as.character(cmpdsubstitute$cmpd_change)
names(cmpdsubstitute) <- cmpd
df_finlocast$text <- str_replace_all(df_finlocast$text,cmpdsubstitute)
df_finlocast$text <- gsub("#","",df_finlocast$text) ###  delete hashtag

# for covid-only tweets ####
load("sample/tweet_cvd.Rdata")
tweet_cvd <- gsub("_","",tweet_cvd)
df_finlocast <- df_finlocast %>% filter(tweet_id %in% tweet_cvd)
# !! in word_embedding script this subsample is saved as df_we for shortness

# distribution top-actors (R&R) ####
# 
#   df2 <- df_finlocast %>% group_by(user_username) %>%   mutate(count_name_occurr = n())
#   
#   dfcountry <- arrange(df2[df2$country == "Germany",],-count_name_occurr)
#  # dfcountry30 <- dfcountry[1:30,]
#   topact <- dfcountry[,c("user_username","count_name_occurr","user_description")] 
#   topact <- topact[!duplicated(dfcountry$user_username), ]
#   write.csv(topact,file = paste0(unique(dfcountry$country),"username.csv"), row.names=FALSE)
#   
#   topact[1:100,]  %>% filter(count_name_occurr > 5) %>% 
#     ggplot(aes(x = reorder(user_username,count_name_occurr), y = count_name_occurr)) + 
#     geom_col() +
#   scale_y_continuous(limits = c(0, max(topact[1:30,]$count_name_occurr)),
#                      breaks = seq(0, max(topact[1:30,]$count_name_occurr), 10)) +
#     coord_flip() +
#     ggtitle(topact[1:30,]$country) +
#     xlab("User") +
#     ylab("Count Tweets") +
#     theme_bw()
#  # ggsave(unique(paste0("topactors100",dfcountry$country,".jpg")), width = 10, height = 8)
#   
#   dfcountry %>% ggplot(aes(x=count_name_occurr)) + geom_density() +
#     scale_x_continuous(breaks = c(median(dfcountry$count_name_occurr),seq(0,max(dfcountry$count_name_occurr),10))) +
#     geom_vline(xintercept = median(dfcountry$count_name_occurr)) +
#     xlab("counts") +
#    # ggtitle(dfcountry$country) +
#     labs(title = dfcountry$country, subtitle =  "Intercept = median") + 
#     theme_bw()
#   ggsave(unique(paste0("density",dfcountry$country,".jpg")), width = 6, height = 5)

# Users classification
  
# dftop <- read.csv("Italy_topusers.csv",sep=";")
# dftop <- dftop[,c(1:6)]
# dftop[dftop$description == "",]$description <- "null"
# dftop[dftop$detailed_description == "",]$detailed_description <- "null"
# dftop[235,]$user_username <- "3e32"  # this because actor at this line was passed with username \"name\" in the csv format and would cause problems
# dftop[235,]$detailed_description <- "aquila earthquake survivors"  for detailed advocacy group
# write.csv(dftop,file="Italy_top.csv", row.names = FALSE)
  


# frequencies ####

dftop_ita <- read.csv("Italy_cat.csv",sep = ",")
dftop_ita[dftop_ita$user_description == "Comitato aquilano nato a seguito del sisma del 6 aprile del 2009. Si batte per una ricostruzione sociale della città, giusta e partecipata.",]$user_username <- "3e32"  
  
dftop_deu <- read.csv("Germany_cat.csv",sep = ",")
  
dftop <- rbind(dftop_ita, dftop_deu)

# aggregation at description_2
dftop[dftop$description_1 == "citizen",]$description_1 <- "private account"
dftop$description_2 <- dftop$description_1
dftop[dftop$description_1 == "sport association",]$description_2 <- "association"
dftop[dftop$description_1 == "scientific association",]$description_2 <- "association"
dftop[dftop$description_1 == "sport association",]$description_2 <- "association"
dftop[dftop$description_1 == "advocacy group",]$description_2 <- "association"
dftop[dftop$description_1 == "religion association",]$description_2 <- "association"
dftop[dftop$description_1 == "trade union",]$description_2 <- "association"

dftop[dftop$description_1 == "government_regional",]$description_2 <- "government"
dftop[dftop$description_1 == "government_national",]$description_2 <- "government"
dftop[dftop$description_1 == "government_local",]$description_2 <- "government"
dftop[dftop$description_1 == "government_international",]$description_2 <- "government"

dftop[dftop$description_1 == "journalist",]$description_2 <- "private account"
dftop[dftop$description_1 == "scholar",]$description_2 <- "private account"
dftop[dftop$description_1 == "entrepreneur",]$description_2 <- "private account"
dftop[dftop$description_1 == "activist",]$description_2 <- "private account"
dftop[dftop$description_1 == "artist",]$description_2 <- "private account"

dftop[dftop$description_1 == "trade association",]$description_2 <- "market sector"
dftop[dftop$description_1 == "firm",]$description_2 <- "market sector"
dftop[dftop$description_1 == "think tank",]$description_2 <- "nonprofit"

dftop[dftop$description_1 == "politician",]$description_2 <- "governance & politics"
dftop[dftop$description_1 == "political party",]$description_2 <- "governance & politics"
dftop[dftop$description_1 == "political association",]$description_2 <- "governance & politics"

dftop[dftop$description_1 == "blog",]$description_2 <- "media"
dftop[dftop$description_1 == "news",]$description_2 <- "media"
dftop[dftop$description_1 == "embassy",]$description_2 <- "governance & politics"

# levels(as.factor(df_freq[df_freq$description_1 == "politician",]$detailed_description))
# levels(as.factor(df_freq[df_freq$description_1 == "political association",]$detailed_dsescription))
# levels(as.factor(df_freq[df_freq$description_1 == "political party",]$detailed_description))

# aggregation at description_3

# aggregation at description_2
dftop$description_3 <- dftop$description_2
dftop[dftop$description_2 == "grassroots",]$description_3 <- "association"
dftop[dftop$description_2 == "nonprofit",]$description_3 <- "association"
dftop[dftop$description_2 == "cultural association",]$description_3 <- "association"
dftop[dftop$description_1 == "trade union",]$description_3 <- "market sector" # from description 1
dftop[dftop$description_2 == "government",]$description_3 <- "governance & politics"

# aggregation at description_4: trade union as association

dftop$description_4 <- dftop$description_3
dftop[dftop$description_1 == "trade union",]$description_4 <- "association"


# only for categories graph
dftop <- dftop %>% filter(note == "keep") %>% filter(! detailed_description == "embassy")
#

deusers <- dftop %>%
  filter(country == "Germany") %>%
  top_n(30,count_name_occurr) %>%
  ggplot(aes(x = fct_reorder(user_username, count_name_occurr) , y = count_name_occurr)) +
  geom_col(aes(fill = description_4)) +
  facet_wrap(~ country, scales = "free") +
  coord_flip() +
  ylab("count tweets")  + 
  scale_fill_manual(values= c(
    "private account" = "lightsalmon",
    "media" = "lightskyblue",
    "market sector" = "khaki",
    "governance & politics" = "violet",
    "association" = "palegreen"
  )) +
  theme_bw()  +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  guides(fill=guide_legend(title="Users Category")) 

itusers <- dftop %>%
  filter(country == "Italy") %>%
  top_n(30,count_name_occurr) %>%
  ggplot(aes(x = fct_reorder(user_username, count_name_occurr) , y = count_name_occurr)) +
  geom_col(aes(fill = description_4)) +
  facet_wrap(~ country, scales = "free") +
  coord_flip() +
  ylab("count tweets") +
  scale_fill_manual(values= c(
    "private account" = "lightsalmon",
    "media" = "lightskyblue",
    "market sector" = "khaki",
    "governance & politics" = "violet",
    "association" = "palegreen"
  )) + 
  theme_bw()  +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  guides(fill=guide_legend(title="Users Category")) 

ggpubr::ggarrange(deusers,itusers, nrow =2)
ggsave(file="review_1/sample_rv1/topusers30.jpg", height = 8, width = 8)
#

dftop %>% filter(description_1 == "political party") %>% 
  group_by(country, detailed_description) %>% summarise(sum_tot = sum(count_name_occurr),
                                                        .groups = "drop") %>%
ggplot(aes(x = detailed_description,y = sum_tot,fill = detailed_description)) + geom_col() +
  facet_wrap(~ country, scales = "free") +
  scale_y_continuous( labels = function(x) sprintf("%0.0f", x)) +
  ylab("n") +
  theme(legend.position = "bottom") +
    theme_bw()
  ggsave("review_1/political_party.jpg", width = 10, height = 6)

dftop %>% filter(description_1 == "political association") %>% 
    group_by(country, detailed_description) %>% summarise(sum_tot = sum(count_name_occurr),
                                                          .groups = "drop") %>%
ggplot(aes(x = detailed_description,y = sum_tot,fill = detailed_description)) + geom_col() +
    facet_wrap(~ country, scales = "free") +
    scale_y_continuous( labels = function(x) sprintf("%0.0f", x)) +
  coord_flip() +
    ylab("n") +
    theme(legend.position = "bottom") +
    theme_bw()
  ggsave("review_1/political_association.jpg", width = 10, height = 6)
  
  dftop %>% filter(description_1 == "politician") %>% 
    group_by(country, detailed_description) %>% summarise(sum_tot = sum(count_name_occurr),
                                                          .groups = "drop") %>%
    ggplot(aes(x = detailed_description,y = sum_tot,fill = detailed_description)) + geom_col() +
    facet_wrap(~ country, scales = "free") +
    scale_y_continuous( labels = function(x) sprintf("%0.0f", x)) +
    coord_flip() +
    ylab("n") +
    theme(legend.position = "bottom") +
    theme_bw()
  ggsave("review_1/politician.jpg", width = 10, height = 6)
  
# categories
dftop %>% 
    group_by(country, description_4) %>% 
    summarise(sum_tot = sum(count_name_occurr),.groups = "drop") %>% 
  ggplot(aes(description_4,
                    y = sum_tot,fill = description_4, label = sum_tot)) + 
  geom_col() +
    facet_wrap(~ country, scales = "free", dir = "v") +
    scale_y_continuous( labels = function(x) sprintf("%0.0f", x)) +
    coord_flip() +
    geom_text() +
    ylab("count tweets") +
  xlab("Users' categories") +
  labs(fill = "Users Categories") +
  theme_bw() +
  theme(legend.position = "none",
        axis.text.y = element_text(size = 12),
        strip.text.x = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12)
        )
  ggsave("macrocat_descr.jpg", width = 8, height = 8)

# merge annotation to dataframe ####

df_finlocast <- merge(df_finlocast, dftop, by = c("user_username","country"))
# to filter out embassy and outplaced
df_finlocast <- df_finlocast %>% filter(note == "keep") %>% filter(! detailed_description == "embassy")

# tokenization ####

df_finlocast <- df_finlocast %>% filter(! user_username %in% dftop[dftop$note == "delete",]$user_username)


df_finlocast <- df_finlocast %>% unnest_tokens(word, text, token = stringr::str_split, pattern = " ")
df_finlocast$word <- str_remove_all(df_finlocast$word, "[^#_[:^punct:]]")
df_finlocast <- df_finlocast %>% filter(!word %in% stop_words$word,
                                        !word %in% rem_char,
                                        !word %in% stopwords("en"), 
                                       # !word %in% stopwords_en,
                                        !word %in% rem)
                                      #  !word %in% "^[0-9]*$")

df_finlocast <- replace(df_finlocast, df_finlocast=='', NA)
df_finlocast <- df_finlocast %>% drop_na(word)

#df_finlocast$word <- gsub("#","",df_finlocast$word)
#### Top frequencies https://www.tidytextmining.com/twitter.html ####


 frequency <- df_finlocast %>%
 count(country, monthyear, word, sort = TRUE)



df_words <- df_finlocast %>% 
  filter(!word %in% c("","#","_")) %>%
  count(country,monthyear,dateyear,quadrimester,word,sort = TRUE)

total_words <- df_words %>% 
  group_by(country,monthyear) %>% 
  # group_by(country) %>%
  summarize(total = sum(n))

df_words <- left_join(df_words, total_words)

df_words$catlev <- paste0(df_words$country,"_",df_words$monthyear) 

df_tf_idf <- df_words %>%
  bind_tf_idf(word,catlev, n)

df_tf_idf %>%  arrange(desc(tf_idf))

df <- df_tf_idf %>% filter(! word %in% c("solidarity","#solidarity","#solidarityisfuture",
                                         "#pressdelivery"
                                         #,"#prnnews","#fondazionemoscati"
                                         )) %>% 
  group_by(country,monthyear) %>% top_n(1)

df[df$catlev == "Germany_10-2021",]$word <- "#freedy, #freejo"
df[df$catlev == "Germany_11-2019",]$word <- "#anti imperialism, #h2311"


dfs <- frequency %>% filter(word == "#solidarity")
dfs <- merge(dfs,df,by = c("country","monthyear"))
dfs$label <- ""
dfs[dfs$country == "Germany" & dfs$monthyear == "03-2020",]$label <- "#corona"
dfs[dfs$country == "Germany" & dfs$monthyear == "12-2020",]$label <- "#corona"
dfs[dfs$country == "Germany" & dfs$monthyear == "01-2019",]$label <- "#christchurch"
dfs[dfs$country == "Germany" & dfs$monthyear == "05-2019",]$label <- "#europawahl2019"
dfs[dfs$country == "Germany" & dfs$monthyear == "10-2019",]$label <- "#rojava"
dfs[dfs$country == "Germany" & dfs$monthyear == "04-2020",]$label <- "#corona"
dfs[dfs$country == "Germany" & dfs$monthyear == "04-2020",]$label <- "#solidaritynotalone"

dfs[dfs$country == "Germany" & dfs$monthyear == "06-2020",]$label <- "#sogehtsolidarisch"
dfs[dfs$country == "Germany" & dfs$monthyear == "05-2021",]$label <- "#1mai"
dfs[dfs$country == "Germany" & dfs$monthyear == "07-2021",]$label <- "#flood disaster"
dfs[dfs$country == "Germany" & dfs$monthyear == "09-2021",]$label <- "#btw21"
dfs[dfs$country == "Germany" & dfs$monthyear == "12-2021",]$label <- "#corona"
dfs[dfs$country == "Italy" & dfs$monthyear == "04-2020",]$label <- "#coronavirus"#christmas
dfs[dfs$country == "Italy" & dfs$monthyear == "05-2020",]$label <- "#coronavirus"#christmas
dfs[dfs$country == "Italy" & dfs$monthyear == "12-2020",]$label <- "#christmas"
dfs[dfs$country == "Italy" & dfs$monthyear == "02-2019",]$label <- "#iostoconipastorisardi"
dfs[dfs$country == "Italy" & dfs$monthyear == "04-2019",]$label <- "#easter"
dfs[dfs$country == "Italy" & dfs$monthyear == "12-2019",]$label <- "#christmas" #fondazioneantiusura
dfs[dfs$country == "Italy" & dfs$monthyear == "07-2020",]$label <- "#fondazioneantiusura"
dfs[dfs$country == "Italy" & dfs$monthyear == "05-2021",]$label <- "#fastconfsal"
dfs[dfs$country == "Italy" & dfs$monthyear == "09-2021",]$label <- "#mimmolucano"
dfs[dfs$country == "Italy" & dfs$monthyear == "12-2021",]$label <- "#christmas2021"


# ggplot(frequency[frequency$word == "#solidarity",], aes(x = monthyear,y = n, color = country)) +
  ggplot(dfs, aes(x = monthyear,y = n.x, color = country)) +
  geom_point(aes(color = country)) + 
  geom_line(aes(group = as.factor(country))) +
  ggrepel::geom_text_repel(aes(label = label)) +
  xlab("Time (month-year)") +
  ylab("Frequency") +
  labs(color = "Country") +
  ggtitle("Term #solidarity in Twitter and most associated words") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = -75, hjust = 0), legend.position = "bottom")
ggsave(file="review_1/sample_rv1/frequency.jpg",width = 12,height = 6)



# delete
# ggplot(top_word_pairsold,aes(x = monthyear, y = n)) +  geom_col() +
#   geom_text_repel(aes(x = monthyear, y = n, label = item2), vjust = -1) + 
#  # geom_text(aes(label = item2), vjust = -1) + 
#   facet_wrap(~country, dir = "v")



## tf-idf ####

 
#
# to plot each month
# df_tf_idf %>%
# filter(dateyear %in% c(2021)) %>%
#   filter(country == "Germany") %>%
#   group_by(monthyear) %>%
#   slice_max(tf_idf, n = 3) %>%
#  # group_by(catquad,tf_idf) %>%
#   ungroup() %>%
#   ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = catlev)) +
#   geom_col(show.legend = FALSE) +
#   facet_wrap(~ catlev , scales = "free", ncol = 1) 
  


# keyness ####

# df_finlocast[df_finlocast$user_username == "OsservaSocialis",]$text <- str_replace_all(df_finlocast[df_finlocast$user_username == "OsservaSocialis",]$text, "\\bsocialresponsibility\\b","" )
# df_finlocast[df_finlocast$user_username == "OsservaSocialis",]$text <- str_replace_all(df_finlocast[df_finlocast$user_username == "OsservaSocialis",]$text, "\\bterritory\\b","" )


df_finlocast %>% 
  filter(! word %in% c(cvd)) %>%
  filter(! word %in% rem_char) %>%
  filter(! word %in% rem) %>%
  count(word, country) %>%
  group_by(word) %>%
  filter(sum(n) >= 20) %>%
  ungroup() %>%
  filter(!word %in% c("","#")) %>%
  pivot_wider(names_from = country, values_from = n, values_fill = 0) %>%
  mutate_if(is.numeric, list(~(. + 1) / (sum(.) + 1))) %>%
  mutate(logratio = log(Germany / Italy)) %>%
  arrange(desc(logratio))  %>%
  group_by(logratio < 0) %>%
  slice_max(abs(logratio), n = 14) %>% 
  ungroup() %>%
  mutate(word = reorder(word, logratio)) %>%
  ggplot(aes(word, logratio, fill = logratio < 0)) +
  geom_col() +
  coord_flip() +
  ylab("log odds ratio (Germany/Italy)") +
  scale_fill_discrete(name = "", labels = c("Germany", "Italy")) +
  theme_bw() + 
  theme(axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 15),
        axis.title.x = element_text(size = 15),
        legend.text = element_text(size = 13))
 ggsave(file="review2/review_2/submissionR2/figures/3_keyness.jpg",width = 11, height = 10)


# dfm ####
 
 corpus_df <- corpus(df_finlocast[df_finlocast$country == "Italy",])
 docnames(corpus_df) <-  df_finlocast[df_finlocast$country == "Italy",]$tweet_id
 
 dfm_itsolcv <- tokens(corpus_df,
                     remove_punct = TRUE, remove_numbers = TRUE,remove_url = TRUE) %>% 
   tokens_remove(c(rem_char,
                   rem)) %>%
   dfm()
 save(dfm_itsolcv,file="review_1/sample_rv1/dfm_itsolcv.Rdata")

 textstat_frequency(dfm_itsolcv) %>% subset(feature %in% "national_borders")
 
 
# load("sample/dfm_sol.Rdata")

# dfm_solde <- dfm_subset(dfm_solde,country == "Germany")
# save(dfm_solde,file="sample/dfm_solde.Rdata")
 
# dfm_solit <- dfm_subset(dfm_sol,country == "Italy")
# save(dfm_solit,file="sample/dfm_solit.Rdata")
 
## topic modelling ####
 # cvd <- read.xls("sample/compound.xls",sheet = "cvd", encoding = "latin1")[,1] 
 # cvd <- gsub("#","",cvd)
 # rem_char <- c("http*","@*","€","+","|","s","faq","=","_","__","~","___")
# load("sample/6/df_we6.Rdata")
 load("review_1/sample_rv1/DE/stm_de20.Rdata")
 load("review_1/sample_rv1/DE/dfm_desolcv.Rdata")
 stm_m <- stm_de20
 stm_df <- quanteda::convert(dfm_desolcv,to = "stm")  
 numm <- 20
 
 dfb <- df_finlocast %>% filter(country == "Germany")
 

# covariates ####
 
 sg <- sageLabels(stm_m,10)
 sg_prob <- tibble(topic = 1:numm,sg$marginal$prob) # marginal probability
 sg_frex <- tibble(topic = 1:numm,sg$marginal$frex) # marginal frex
 sg_prob_ass <- tibble(topic = 1:numm,sg$covnames[[1]],sg$cov.betas[[1]]$problabels) # ASSOCIATION prob
 sg_prob_gov <- tibble(topic = 1:numm,sg$covnames[[2]],sg$cov.betas[[2]]$problabels) # GOVERNANCE prob
 sg_prob_mkt <- tibble(topic = 1:numm,sg$covnames[[3]],sg$cov.betas[[3]]$problabels) # MARKET prob
 sg_prob_med <- tibble(topic = 1:numm,sg$covnames[[4]],sg$cov.betas[[4]]$problabels) # MEDIA prob
 sg_prob_pac <- tibble(topic = 1:numm,sg$covnames[[5]],sg$cov.betas[[5]]$problabels) # PRIVATE ACCOUNT prob

 sg_frex_ass <- tibble(topic = 1:numm,sg$covnames[[1]],sg$cov.betas[[1]]$frexlabels) # ASSOCIATION frex
 sg_frex_gov <- tibble(topic = 1:numm,sg$covnames[[2]],sg$cov.betas[[2]]$frexlabels) # GOVERNANCE frex
 sg_frex_mkt <- tibble(topic = 1:numm,sg$covnames[[3]],sg$cov.betas[[3]]$frexlabels) # MARKET frex
 sg_frex_med <- tibble(topic = 1:numm,sg$covnames[[4]],sg$cov.betas[[4]]$frexlabels) # MEDIA frex
 sg_frex_pac <- tibble(topic = 1:numm,sg$covnames[[5]],sg$cov.betas[[5]]$frexlabels) # PRIVATE ACCOUNT frex

 thoughts <- list()
 for (i in 1:numm){ #
   # thg_det <- findThoughts(stm_m, texts = df_de$text,n = 3, topics =i)$docs[[1]]
   thought_text = list()
   dates <- findThoughts(stm_m, texts = dfb$text,n = 20, topics =i)$index[[1]] #
   for (n in dates) {
     txx <-  print(c(paste0(" ACT: ", dfb[n,]$description_3,
                            " ACTsj: ", dfb[n,]$user_username," TXT: ",dfb[n,]$text," DATE: ", dfb[n,]$created_at,
                            " TWID: ",dfb[n,]$tweet_id," CNID ",dfb[n,]$conversation_id)))
     thought_text[[n]] <- txx
     thought_textfin <- do.call(rbind.data.frame, thought_text)
   }
   thoughts[[i]] <- thought_textfin

 }
 bind_rows(thoughts)
 thoughts <- do.call(rbind.data.frame, lapply(thoughts,'[[',1))
 colnames(thoughts) = c("1", "2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20") # columns where texts go
 thoughts <- cbind(topic = 1:numm,thoughts)

 # combining final report pieces and write excel
 long_report <- cbind(sg_prob,sg_frex,sg_prob_ass,sg_prob_pac,sg_prob_gov,sg_prob_mkt,sg_prob_med,
                      sg_frex_ass,sg_frex_pac,sg_frex_gov,sg_frex_mkt,sg_frex_med,thoughts)

 label_it <- c("hospital donations", "charity", "economy", "covid outbreak", "food donations",
               "digital solidarity", "local corporates", "health emergency", "europe", "local donations",
               "bergamo", "welfare", "community donations", "services municipalities", "culture",
               "corporate responsibility", "emergency donations", "fundraising", "municipalities initiatives",
               "lockdown", "vaccination", "governance", "families", "artisans", "volunteering")

 label_de <- c("doctors international","housing","social responsibility","stayhome","refugees","masks",
               "cohesion","governance media","europe","mobilizations","international coop","vaccination",
               "freedom","flattencurve","social exclusion","charity","local initiatives","access vaccination",
               "fight together","health crisis")


 long_report$label <- label_de

 long_report <- long_report %>% relocate(label, .after=1)


 write.csv(long_report,file= "review_1/sample_rv1/DE/Germany20_cont.csv",row.names = F, col.names=T,  sep=";",  fileEncoding = "UTF-8")


 textfreq <- textstat_frequency(dfm_itsolcv)
 textstat_frequency(dfm_itsolcv) %>% subset(feature %in% "special_thanks")
 

# topic proportion to combine ####
 
 top_terms <- tidy(stm_m) %>%
   arrange(beta) %>%
#   group_by(topic)  %>%
   group_by(topic,term)  %>%
   filter(! term %in% c(cvd,"solidarity")) %>%   # to filter out either it_policies or de_policies
 #  group_by(topic,term)  %>%
    summarise(beta = mean(beta)) %>%
     top_n(7, beta) %>%
   arrange(-beta) %>%
   select(topic, term) %>%
   summarise(terms = list(unique(term))) %>%
   mutate(terms = map(terms, paste, collapse = ", ")) %>% 
   unnest()
 
 
td_gamma <- tidy(stm_m, matrix = "gamma")
ID_row <- names(stm_df$documents) # the name of documents gets lost, the row number is reported
td_gamma <- cbind(td_gamma,ID_row) # Here I map each document to its name via row, I checked with content, it works
td_gamma <- cbind(td_gamma,dfb) # merge the gamma matrix with the dataframe via ID, so the variables to sort documents can be used

# gamma it ####
gamma_terms_it <- td_gamma %>%
  group_by(topic) %>%
  summarise(gamma = mean(gamma)) %>%
  arrange(desc(gamma)) %>%
 left_join(top_terms, by = "topic") %>%
mutate(label = recode(topic,
                      "1" = "Hospital\ndonations",
                      "2" = "Charity",
                      "3" = "Economy", #
                      "4" = "Covid outbreak", # ex covid outbreak
                      "5" = "Food\ndonations",
                      "6" = "Digital\nsolidarity",
                     # "7" = "Third\nsector",
                      "7" = "Local\ncorporates",
                      "8" = "Health\nemergency", #*
                      "9" = "Europe",
                      "10" = "Local\ndonations",
                      "11" = "Bergamo",
                     # "12" = "Global\nwelfare",
                     "12" = "Welfare", # ex cooperation
                      "13" = "Community\ndonations",
                      "14" = "Services\nmunicipalities",
                      "15" = "Culture",
                     # "16" = "Companies\ndonations", #
                     "16" = "Corporate\nresponsibility", #
                      "17" = "Emergency\ndonations",
                      "18" = "Fundraising", #
                      "19" = "Municipalities\ninitiatives",
                      "20" = "Lockdown", #
                      "21" = "Vaccination",
                      "22" = "Governance", #
                      "23" = "Families",
                      "24" = "Artisans",
                      "25" = "Volunteering"
)) %>%
  mutate(type = recode(topic,
                       "1" = "donations",
                       "2" = "targets",
                       "3" = "donations",
                       "4" = "transnational",
                       "5" = "donations",
                       "6" = "society",
                       "7" = "donations",
                       "8" = "public health",
                       "9" = "transnational",
                       "10" = "donations",
                       "11" = "targets",
                       "12" = "society",
                       "13" = "donations",
                       "14" = "governance",
                       "15" = "society",
                       "16" = "donations",
                       "17" = "donations",
                       "18" = "donations",
                       "19" = "society",
                       "20" = "donations",
                       "21" = "public health",
                       "22" = "society",
                       "23" = "society",
                       "24" = "donations",
                       "25" = "public health" ))

gamma_terms_it$country <- "Italy"

save(gamma_terms_it,file="review_1/sample_rv1/IT/gamma_terms_it.Rdata")

# gamma de ####

gamma_terms_de <- td_gamma %>%
  group_by(topic) %>%
  summarise(gamma = mean(gamma)) %>%
  arrange(desc(gamma)) %>%
  left_join(top_terms, by = "topic")  %>%
  mutate(label = recode(topic,
                        "1" = "Doctors\ninternational",
                        "2" = "Housing",
                        "3" = "Social\nresponsibility", #*
                        "4" = "Stayhome",
                        "5" = "Refugees",
                        "6" = "Masks",
                        "7" = "Cohesion",
                      #  "8" = "Governance",
                        "8" = "Governance\nmedia",
                        "9" = "Europe",
                        "10" = "Mobilizations",
                        "11" = "International\ncoop",
                        "12" = "Vaccination",
                        "13" = "Freedom",
                        "14" = "Flattencurve",
                        "15" = "Social exclusion",
                        "16" = "Charity",
                        "17" = "Local\ninitiatives",
                        "18" = "Access\nvaccination", # European\ngovernance
                        "19" = "Fight\ntogether", #* lockdown
                        "20" = "Health crisis")) %>%
  mutate(type = recode(topic,
                       "1" = "transnational",
                       "2" = "society",
                       "3" = "society",
                       "4" = "public health",
                       "5" = "transnational",
                       "6" = "public health",
                       "7" = "society",
                       "8" = "governance",
                       "9" = "transnational",
                       "10" = "society",
                       "11" = "transnational",
                       "12" = "public health",
                       "13" = "public health",
                       "14" = "public health",
                       "15" = "society",
                       "16" = "donations",
                       "17" = "targets",
                       "18" = "targets" ,# "governance",
                       "19" = "society", # "public health",
                       "20" = "society"))

gamma_terms_de$country <- "Germany"

save(gamma_terms_de,file="review_1/sample_rv1/DE/gamma_terms_de.Rdata")



# combine topic proportion ####
load("review_1/sample_rv1/DE/gamma_terms_de.Rdata")
load("review_1/sample_rv1/IT/gamma_terms_it.Rdata")

gamma_tot <- rbind(gamma_terms_it,gamma_terms_de)

gamma_tot[gamma_tot$label == "Mobilitations",]$label <- "Mobilizations"
gamma_tot[gamma_tot$label == "Hospital\ndonations",]$label <- "Hospital donations"
gamma_tot[gamma_tot$label == "Health\nemergency",]$label <- "Health emergency"
gamma_tot[gamma_tot$label == "Emergency\ndonations",]$label <- "Emergency donations"
gamma_tot[gamma_tot$label == "Community\ndonations",]$label <- "Community donations"
gamma_tot[gamma_tot$label == "Digital\nsolidarity",]$label <- "Digital solidarity"
gamma_tot[gamma_tot$label == "Food\ndonations",]$label <- "Food donations"
gamma_tot[gamma_tot$label == "Local\ncorporates",]$label <- "Local corporates"
gamma_tot[gamma_tot$label == "Local\ndonations",]$label <- "Local donations"
gamma_tot[gamma_tot$label == "Fight\ntogether",]$label <- "Fight together"
gamma_tot[gamma_tot$label == "Local\ninitiatives",]$label <- "Local initiatives"
gamma_tot[gamma_tot$label == "Social\nresponsibility",]$label <- "Social responsibility"
gamma_tot[gamma_tot$label == "Services\nmunicipalities",]$label <- "Services municipalities"
gamma_tot[gamma_tot$label == "Doctors\ninternational",]$label <- "Doctors international"
gamma_tot[gamma_tot$label == "Governance\nmedia",]$label <- "Governance media"
gamma_tot[gamma_tot$label == "International\ncoop",]$label <- "International coop"
gamma_tot[gamma_tot$label == "Access\nvaccination",]$label <- "Access vaccination"
gamma_tot[gamma_tot$label == "Corporate\nresponsibility",]$label <- "Corporate responsibility"
gamma_tot[gamma_tot$label == "Municipalities\ninitiatives",]$label <- "Municipalities initiatives"

# combine plots
detop <- gamma_tot %>%
filter(country == "Germany") %>%
  ggplot(aes(reorder(label,gamma), gamma, fill = type)) +
  geom_col() +
  geom_text(aes(label = terms),hjust = 0, y = 0.001, size = 6, # nudge_y = 0.00005, size = 5, # 0.0005
            family = "IBMPlexSans") +
  coord_flip() +
  scale_fill_manual(values= c(
    "donations" = "lightsalmon",
    "governance" = "peachpuff",
    "public health" = "plum1",
    "society" = "lightskyblue",
    "targets" = "khaki",
    "transnational" = "palegreen"
  ),
  name = "Solidarity categories") +
  facet_wrap(~ country, scales = "free") + 
  scale_y_continuous(label = scales::percent) +
  theme_bw() +
  theme(axis.title.y = element_blank(),axis.title.x = element_blank(),
        axis.text.x = element_text(size = 13),
        strip.text.x = element_text(size = 20), 
        axis.text.y = element_text(size = 19),
        legend.position = "bottom",
        legend.text = element_text(size=18),
        legend.title = element_text(size=19),
        plot.margin = unit(c(0.4, 0.4, 0.4, 0.4), 
                           "inches")
        ) +
  guides(fill=guide_legend(nrow=1))

ittop <- gamma_tot %>%
  filter(country == "Italy") %>%
  ggplot(aes(reorder(label,gamma), gamma, fill = type)) +
  geom_col() +
  geom_text(aes(label = terms),hjust = 0, y = 0.001, size = 6, # nudge_y = 0.00005, size = 5, # 0.0005
            family = "IBMPlexSans") +
  coord_flip() +
  scale_fill_manual(values= c(
    "donations" = "lightsalmon",
    "governance" = "peachpuff",
    "public health" = "plum1",
    "society" = "lightskyblue",
    "targets" = "khaki",
    "transnational" = "palegreen"
  ),
  name = "Solidarity categories") +
  facet_wrap(~ country, scales = "free") + 
  scale_y_continuous(label = scales::percent) +
  theme_bw() +
  theme(axis.title.y = element_blank(),axis.title.x = element_blank(),
        axis.text.x = element_text(size = 13),
        strip.text.x = element_text(size = 20), 
        axis.text.y = element_text(size = 19),
        legend.position = "bottom",
        legend.text = element_text(size=18),
        legend.title = element_text(size=19),
        plot.margin = unit(c(0.4, 0.4, 0.4, 0.4), 
                           "inches")
  ) +
  guides(fill=guide_legend(nrow=1))

ggpubr::ggarrange(detop,ittop,  common.legend = TRUE, legend="bottom", nrow=2,heights = c(1, 1.3))
ggsave("review2/review_2/submissionR2/figures/4_topic_proportion.png",height = 19,width = 15)

 # time topic modelling ####
 
# tidystm <- tidy(stm_m)
 # tidystm <- rename(tidystm, actor = y.level)
 

load("review_1/sample_rv1/DE/stm_de20.Rdata")
load("review_1/sample_rv1/DE/dfm_desolcv.Rdata")
stm_dfDE <- quanteda::convert(dfm_desolcv,to = "stm")  
numm <- 20

dfbDE <- df_finlocast %>% filter(country == "Germany")


prepDE <- estimateEffect(1:20 ~ s(datenum), stm_de20, metadata = stm_dfDE$meta, uncertainty = "Global")
tidystmDE <- tidy(stm_de20)

effects_intDE <- get_effects(estimates = prepDE,
                             variable = 'datenum',
                             type = 'continuous'  )  

effects_intDE <- effects_intDE %>% mutate(label = recode(topic,
                                                             "1" = "Doctors\ninternational",
                                                             "2" = "Housing",
                                                             "3" = "Social\nresponsibility", #*
                                                             "4" = "Stayhome",
                                                             "5" = "Refugees",
                                                             "6" = "Masks",
                                                             "7" = "Cohesion",
                                                             #  "8" = "Governance",
                                                             "8" = "Governance\nmedia",
                                                             "9" = "Europe",
                                                             "10" = "Mobilizations",
                                                             "11" = "International\ncoop",
                                                             "12" = "Vaccination",
                                                             "13" = "Freedom",
                                                             "14" = "Flattencurve",
                                                             "15" = "Social exclusion",
                                                             "16" = "Charity",
                                                             "17" = "Local\ninitiatives",
                                                             "18" = "Access\nvaccination", # European\ngovernance
                                                             "19" = "Fight\ntogether", #* lockdown
                                                             "20" = "Health crisis")) 


prep <- estimateEffect(1:25 ~ s(datenum), stm_it25, metadata = stm_dfIT$meta, uncertainty = "Global")


effects_intIT <- get_effects(estimates = prep,
                             variable = 'datenum',
                             type = 'continuous'  )  

load("sample/6/6DE/de6_effects_int.Rdata")
load("sample/6/6IT/it6_effects_int.Rdata")

tidystm <- tidy(stm_it25)

effects_intIT <- effects_intIT %>% mutate(label = recode(topic,
                                                             "1" = "Hospital\ndonations",
                                                             "2" = "Charity",
                                                             "3" = "Economy", #
                                                             "4" = "Covid\noutbreak",
                                                             "5" = "Food\ndonations",
                                                             "6" = "Digital\nsolidarity",
                                                             # "7" = "Third\nsector",
                                                             "7" = "Local\ncorporates",
                                                             "8" = "Health\nemergency", #*
                                                             "9" = "Europe",
                                                             "10" = "Local\ndonations",
                                                             "11" = "Bergamo",
                                                             # "12" = "Global\nwelfare",
                                                             "12" = "Cooperation",
                                                             "13" = "Community\ndonations",
                                                             "14" = "Services\nmunicipalities",
                                                             "15" = "Culture",
                                                             # "16" = "Companies\ndonations", #
                                                             "16" = "Corporate\nresponsibility", #
                                                             "17" = "Emergency\ndonations",
                                                             "18" = "Fundraising", #
                                                             "19" = "Municipalities\ninitiatives",
                                                             "20" = "Lockdown", #
                                                             "21" = "Vaccination",
                                                             "22" = "Governance", #
                                                             "23" = "Families",
                                                             "24" = "Artisans",
                                                             "25" = "Volunteering"
                                                             ))


numm <- 20
effects_int <- effects_intDE
tidystm <- tidy(stm_de20)

for (i in 1:numm) {
    
tm <- effects_int %>%  filter(topic == i) %>%
   ggplot(aes(x = value, y = proportion )) + geom_line() + scale_x_continuous(breaks = c(18262,18414,18627,18809,18992),
                      labels = c("18262" = "JAN 2020","18414" = "JUN 2020","18627" = "JAN 2021","18809" = "JUN 2021",
                                 "18992" = "DEC 2021")) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2)  +
  ggtitle(paste( "Topic: ",effects_int[effects_int$topic == i,]$label)) + 
  ylab("Expected Proportion") +
  xlab("Time") +
  theme_bw() +
  theme(axis.title.y = element_text(size = 16),
        axis.title.x = element_text(size = 16),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        title = element_text(size = 17),
        plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), 
                           "inches"))
   
wd <- tidystm %>% filter(topic == i) %>%
  arrange(- beta) %>%
  #   group_by(topic)  %>%
  group_by(topic,term)  %>%
  filter(! term %in% c(cvd,"solidarity")) %>%   # to filter out either it_policies or de_policies
  #  group_by(topic,term)  %>%
  summarise(beta = mean(beta)) %>%
  top_n(10, beta) %>%
  ggplot(aes(reorder(term,beta),beta)) +
  geom_col(show.legend = FALSE) +
  scale_y_continuous(label = scales::percent ) +
  coord_flip() +
  xlab("") +
  ylab("Probability words per topic") +
  theme_bw() +
  theme(axis.title.x = element_text(size = 16),
        axis.text.y = element_text(size = 20),
        axis.text.x = element_text(size = 15),
        plot.margin = unit(c(0.2, 0.3, 0.2, 0.2), 
                           "inches")
        )
  
 
cm <- grid.arrange(tm,wd,ncol = 2)
ggsave(cm,file = paste0("review2/review_2/submissionR2/figures/DE/","DE_20","_",i,".jpg"),width = 18, height = 4)
 
 }
 

prep <- estimateEffect(1:numm ~ s(datenum), stm_m, metadata = stm_df$meta, uncertainty = "Global")

de_effects_int <- get_effects(estimates = prep,
                              variable = 'datenum',
                              type = 'continuous'  )  


load("sample/6/6DE/de6_effects_int.Rdata")
load("sample/6/6IT/it6_effects_int.Rdata")

it_tm <- effects_intIT %>% mutate(country = "Italy")  %>%  filter(topic %in% c(2,23,3,14,4,21,20)) %>%
  ggplot(aes(x = value, y = proportion, color = as.factor(topic))) + geom_line() + 
  scale_x_continuous(breaks = c(18289,18414,18627,18809,18992),
                     labels = c("18289" = "GEN 20","18414" = "JUN 20","18627" = "JAN 21","18809" = "JUN 21",
                                "18992" = "DEC 21")) +
 scale_color_manual(values = c("2" = "red","23" = "blue", "3" = "black","14" = "gray",
                               "4" = "pink","21" = "purple","20" = "brown"),
                    labels = c("2" = "Charity","23" = "Families", "3" = "Economy",
                               "14" = "Services\nmunicipalities","4" = "Covid\noutbreak","21" = "Vaccination",
                               "20" = "Lockdown" ),
                    name = "Topics" ) +
 scale_y_continuous(labels = scales::percent_format() ) +
  facet_wrap(~ country) +
 # ggtitle("Italy") + 
  #  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2)  +
  ylab("Expected Proportion") +
  xlab("Time") +
  theme_bw() +
  theme(axis.title.x = element_blank(), legend.position = "bottom",
        axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size = 13),
        axis.title.y = element_text(size = 14),
        strip.text.x = element_text(size = 16), legend.text = element_text(size=14),
        legend.title =  element_text(size=15))

de_tm <- effects_intDE %>% mutate(country = "Germany")  %>%  filter(topic %in% c(5,12,13,10,16,17,19)) %>%
  ggplot(aes(x = value, y = proportion, color = as.factor(topic))) + geom_line() + 
  scale_x_continuous(breaks = c(18304,18414,18627,18809,18992),
                     labels = c("18304" = "FEB 20","18414" = "JUN 20","18627" = "JAN 21","18809" = "JUN 21",
                                "18992" = "DEC 21")) +
  scale_y_continuous(labels = scales::percent_format() ) +
  scale_color_manual(values = c("5" = "limegreen","12" = "purple", "13" = "orange","10" = "black",
                                "16" = "red","17" = "blue", "19" = "brown"),
                       labels = c("5" = "Refugees","12" = "Vaccination", "13" = "Freedom","10" = "Mobilizations",
                                  "16" = "Charity","17" = "Local Initiatives", "19" = "Fight together"),
                     name = "Topics" ) +
  facet_wrap(~ country) + 
 # ggtitle("Germany") + 
 #  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2)  +
  ylab("Expected Proportion") +
  xlab("Time") +
  theme_bw() +
  theme(axis.title.x = element_blank(), legend.position = "bottom",
        axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size = 13),
        axis.title.y = element_text(size = 14),
        strip.text.x = element_text(size = 16), legend.text = element_text(size=14),
        legend.title =  element_text(size=15))


prev <- grid.arrange(de_tm,it_tm)
ggsave(prev,file="review_1/sample_rv1/topic_prevalence.jpg",width = 8,height = 7)




#  # topicCorr ####
#  
#  
# #  library(stm)
# #  library(ggraph)
# #  library(quanteda)
# #  
# #  stm_corrs <- get_network(model = stm_m,
# #                           method = 'simple',
# #                           labels = paste('Topic', 1:numm),
# #                           cutoff = 0.001,
# #                           cutiso = TRUE)
# #  
# #  stm_corrs <- get_network(model = stm_corrs,
# #                           method = 'simple',
# #                           labels = paste('Topic', 1:numm),
# #                           cutoff = 0.001,
# #                           cutiso = TRUE)
# #  
# #  ggraph(stm_corrs, layout = 'fr') +
# #    geom_edge_link(
# #      aes(edge_width = weight),
# #      label_colour = '#fc8d62',
# #      edge_colour = '#377eb8') +
# #    geom_node_point(size = 4, colour = 'black')  +
# #    geom_node_label(
# #      aes(label = name, size = props),
# #      colour = 'black',  repel = TRUE, alpha = 0.85) +
# #    scale_size(range = c(2, 10), labels = scales::percent) +
# #    labs(size = 'Topic Proportion',  edge_width = 'Topic Correlation') +
# #    scale_edge_width(range = c(1, 3)) +
# #    theme_graph()
# # #
# #  
# # tidy_stm <- tidy(stm_m)
# # 
# # kn_stm <- tidy_stm %>% filter(term %in% top_keynessDE) %>% group_by(term) %>% slice_max(n = 2, beta)
# #  
# #  
#  
#  
#  
# 
# # co-occurrences #### 
#  library(igraph)
#  library(ggraph)
#  
# 
# # df_finlocast %>% unnest_tokens(word, text, token = stringr::str_split, pattern = " ")
#  
#  skip_df <- df_we4 %>% filter(country == "Germany") %>%
#    unnest_tokens(ngram, text, token = stringr::str_split, pattern = " ") %>%
#   #  unnest_tokens(ngram, text, token = "ngrams", n = 5) %>%
#    mutate(ngramID = row_number()) %>% 
#   # tidyr::unite(skipgramID, tweet_id, ngramID) %>%
#    unnest_tokens(word, ngram) %>%
#    filter(!word %in% stop_words$word,
#           !word %in% rem_char,
#           !word %in% rem)
 
 
 
 
 # unigram_probs <- df_we %>% filter(country == "Germany") %>%
 #   unnest_tokens(word, text) %>%
 #   filter(!word %in% stop_words$word,
 #          !word %in% rem_char,
 #          !word %in% stopwords("en"), 
 #          !word %in% stopwords_en,
 #          !word %in% rem) %>% # no cvd
 #   # !word %in% "^[0-9]*$")  %>%
 #   count(word, sort = TRUE) %>%
 #   mutate(p = n / sum(n))
 # 
#  skipgram_probs <- skip_df %>%
#    pairwise_count(word, tweet_id, diag = FALSE, sort = TRUE) %>%
#    mutate(p = n / sum(n)) 
# 
#  corrr <- skip_df %>%   pairwise_cor(word,tweet_id,sort = TRUE)
#  
#  a <- skipgram_probs %>% filter(item1 == "leavenoonebehind")
#  a <- a %>% filter(! item1 == item2)
#  b <- skipgram_probs %>% filter(item1 %in% a$item2)
#  b <- b %>% filter(! item2 %in% a$item1)
#  
#  d <- rbind(a,b)
# 
# # plot co-occurrences
#  
# a %>% filter(n >= 8) %>% graph_from_data_frame() %>%
#    ggraph(layout = "fr") +
#    geom_edge_link(aes(edge_alpha = n)) + 
#   # geom_node_point(size = 5) +
#     geom_node_text(aes(label = name), repel = TRUE, 
#                    point.padding = unit(0.2, "lines")) +
#    theme_void()
#  
#  plot(occit)
#  ggsave(occit,file="occit.jpg",width =10,height=10)
#  
#  # 
#  
#  tidy_it <- tidy(stm_it25) %>% group_by(topic) %>% 
#    filter(! term %in% cvd) %>%
#    slice_max(beta,n = 10)
#  
#  tidy_de <- tidy(stm_de25) %>% group_by(topic) %>% 
#    filter(! term %in% cvd) %>%
#    slice_max(beta,n = 10)
#  
#  tidy_it$topic <- as.character(tidy_it$topic)
#  
#  tidy_it[tidy_it$topic == "1",]$topic <-  "CARITAS"
#  tidy_it[tidy_it$topic == "2",]$topic <-  "FIRST\nPHASE"
#  tidy_it[tidy_it$topic == "3",]$topic <-  "HEALTH\nEMERGENCY"
#  tidy_it[tidy_it$topic == "4",]$topic <-  "FOOD\nDONATIONS"
#  tidy_it[tidy_it$topic == "5",]$topic <-  "EUROPE"
#  tidy_it[tidy_it$topic == "6",]$topic <-  "CITIZENS"
#  tidy_it[tidy_it$topic == "7",]$topic <-  "UDINE"
#  tidy_it[tidy_it$topic == "8",]$topic <-  "CHILDREN"
#  tidy_it[tidy_it$topic == "9",]$topic <-  "SICILY"
#  tidy_it[tidy_it$topic == "10",]$topic <-  "BERGAMO"
#  tidy_it[tidy_it$topic == "11",]$topic <-  "WELFARE"
#  tidy_it[tidy_it$topic == "12",]$topic <-  "MONEY"
#  tidy_it[tidy_it$topic == "13",]$topic <-  "DIGITAL\nSERVICES"
#  tidy_it[tidy_it$topic == "14",]$topic <-  "CHALLENGES"
#  tidy_it[tidy_it$topic == "15",]$topic <-  "DONATIONS\nCALLS"
#  tidy_it[tidy_it$topic == "16",]$topic <-  "FUNDRAISING"
#  tidy_it[tidy_it$topic == "17",]$topic <-  "GESTURES"
#  tidy_it[tidy_it$topic == "18",]$topic <-  "LOCKDOWN"
#  tidy_it[tidy_it$topic == "19",]$topic <-  "REPRESENTATIONS"
#  tidy_it[tidy_it$topic == "20",]$topic <-  "GOVERNMENT"
#  tidy_it[tidy_it$topic == "21",]$topic <-  "POVERTY"
#  tidy_it[tidy_it$topic == "22",]$topic <-  "CHRISTMAS"
#  tidy_it[tidy_it$topic == "23",]$topic <-  "SUPPORT"
#  tidy_it[tidy_it$topic == "24",]$topic <-  "COMPANIES"
#  tidy_it[tidy_it$topic == "25",]$topic <-  "NEWS"
#  
#  
#  vert_class <- c(tidy_it$topic,tidy_it$term) %>% unique()
#  vert_class <- data.frame(name = vert_class)
#  vert_class$classified <- "x"
#  vert_class[vert_class$name %in% c("CARITAS","FOOD\nDONATIONS","SICILY",
#                               "DONATIONS\nCALLS","FUNDRAISING","GESTURES",
#                               "CHRISTMAS","SUPPORT","COMPANIES"),]$classified <- "donations"
#  vert_class[vert_class$name %in% c("NEWS","GOVERNMENT"),]$classified <- "governance media"
#  vert_class[vert_class$name %in% c("HEALTH\nEMERGENCY","DIGITAL\nSERVICES","LOCKDOWN"),]$classified <- "public\nhealth"
#  vert_class[vert_class$name %in% c("WELFARE","MONEY","CHALLENGES","REPRESENTATIONS","POVERTY"),]$classified <- "society"
#  vert_class[vert_class$name %in% c("FIRST\nPHASE","CITIZENS","UDINE","CHILDREN","BERGAMO"),]$classified <- "targets"
#  vert_class[vert_class$name %in% c("EUROPE"),]$classified <- "transnational"
#  vert_class[vert_class$name %in% top_keynessIT,]$classified <- "keyness"
#  # vert_class[vert_class$name == "solidarity",]$classified <- "solidarity"
#  
#  
#  
#  
#  
# it_g <- tidy_it %>% graph_from_data_frame(vertices = vert_class)
# de_g <- tidy_de %>% graph_from_data_frame()
# 
#    ggraph(it_g,layout = "fr") +
#    geom_edge_link(aes(edge_alpha = (beta * 1000000)),edge_width = 1, show.legend = FALSE) + 
#    # geom_node_point(size = 5) +
#    geom_node_text(aes(label = name,
#                       color = classified),
#                  repel = TRUE
#                  # point.padding = unit(0.2, "lines"),
#                  ) +
#      scale_color_manual(values=c("donations" = "red","governance media" = "cyan","public\nhealth" = "purple",
#                           "society" = "orange","targets" = "brown",
#                           "transnational" = "darkgreen","keyness" = "blue",
#                           "x" = "black"),
#                         breaks = c("donations","governance","public\nhealth","society",
#                                    "targets","transnational","governance media"),
#                         name = "Solidarity category topics:") +
#      theme_void() +
#      theme(legend.position = "bottom")
#  
#    
#    ggraph(de_g,layout = "fr") +
#      geom_edge_link(aes(edge_alpha = (beta * 1000000)),edge_width = 1, show.legend = FALSE) + 
#      # geom_node_point(size = 5) +
#      geom_node_text(aes(label = name,
#                        ),
#                     repel = TRUE
#                     # point.padding = unit(0.2, "lines"),
#      ) +
#      theme_void() +
#      theme(legend.position = "bottom") 
  
   
   
   
   
# dataframe time ####
# 
# load("sample/df_eurostat.Rdata")
# load("sample/whodf.Rdata")
# df_eurostat$date <- as.character(df_eurostat$date)
# 
# who <- who %>% select(country, datenum, date, New_cases)
# 
# df_eurostat <- df_eurostat %>% group_by(country) %>% mutate(
#   unemplnr = norm_minmax(unemployment),
#   inflationr = norm_minmax(inflation)
# )
# 
# who <- who %>% group_by(country) %>% mutate(
#   newcasesnr = norm_minmax(New_cases)
# )
# 
# df_eurostat <- df_eurostat %>% gather(variable, value, c(unemplnr,inflationr,unemployment,inflation),  factor_key=TRUE)
# who <- who %>% gather(variable, value, c(New_cases,newcasesnr), factor_key = TRUE)
# 
# df <- rbind(df_eurostat,who)
# 
# 
# 
# 
# df <- df %>% group_by(country, variable) %>% norm_minmax(value)
# 
# 
# 
# 
# 
# 
# 
# df_eurostat <- df_eurostat %>% group_by(country) %>%
#   mutate(
#     unemplnr = norm_minmax(unemployment),
#     inflatnr = norm_minmax(inflation)
#   )
# 
# df_eurostat <- df_eurostat %>% gather(variable, value, c(unemployment,inflation, unemplnr,inflatnr), factor_key=TRUE)
# df_eurostat <- df_eurostat %>% filter(variable %in% c("unemplnr","inflatnr"))
# 
# df <- rbind(df_eurostat,whodf_lg)
# df$variable <- as.factor(df$variable)
# 
# df %>% filter(variable %in% c("unemplnr","inflationr","newcasesnr")) %>% 
#   ggplot(aes(x = datenum, y = value, color = variable )) + geom_line() +
#   facet_wrap(~ country) +
#   scale_color_manual(values = c("unemplnr" = "darkgreen","inflationr" = "red","newcasesnr" = "blue"),
#                      labels = c("unemplnr" = "unemployment","inflationr" = "inflation","newcasesnr" = "new cases"),
#                      name = "") +
#   scale_x_continuous(breaks = c(18400,18600,18800,18992),
#                      labels = c("18400" = "MAY 20","18600" = "DEC 20","18800" = "JUN 21","18992" = "DEC 21")) +
#   xlab("Date") + 
#   theme_bw() +
#   theme(legend.position = "bottom", axis.title.y = element_blank())
# 
# 
# 
# df %>% filter(variable %in% c("unemployment","inflation","New_cases")) %>% 
#   ggplot(aes(x = datenum, y = value, color = country )) + geom_line() +
#   facet_wrap(~ country) +
#   scale_color_manual(values = c("Italy" = "darkgreen","Germany" = "red"),
#                      name = "Country") +
#   scale_x_continuous(breaks = c(18400,18600,18800,18992),
#                      labels = c("18400" = "MAY 20","18600" = "DEC 20","18800" = "JUN 21","18992" = "DEC 21")) +
#   facet_wrap(~ variable, scales = "free") +
#   xlab("Date") + 
#   theme_bw() +
#   theme(legend.position = "bottom", axis.title.y = element_blank())

##########################
load("sample/df_eurostat.Rdata")
load("sample/whodf.Rdata")
df_eurostat$date <- as.character(df_eurostat$date)
# 2019-01-01 17897
# 2020-01-01 18262
# 2021-01-01 18628
# 2022-01-01 18993

eurostat.labs <- c("Debt Gross", "Inflation", "% Unemployment")
names(eurostat.labs) <- c("debtgross", "inflation", "unemployment")

espic <- df_eurostat %>% filter(country %in% c("EU","Italy","Germany") & datenum >= 17897 & datenum <= 18993) %>%
  ggplot(aes(x = datenum, y = value, color = country)) +
          # geom_point() +
           geom_line() +
  scale_color_manual(values = c("EU" = "blue","Italy" = "red", "Germany" = "darkgreen"),
                     name = "Country") +
           facet_wrap(~  variable, scales = "free",
                      labeller = labeller(variable = eurostat.labs) ) +
  scale_x_continuous(breaks = c(17897,18262,18628,18993),
                     labels = c("17897" = "JAN 19","18262" = "JAN 20","28628" = "JAN 21","18993" = "DEC 21")) +
  geom_vline(xintercept = 18262, color = "gray") +
  ggtitle("Economic indicators") +
 # xlab("Time") +
  theme_bw() + 
  theme(axis.title.y = element_blank(),axis.title.x = element_blank(),
        strip.text.x = element_text(size = 16),
        axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size = 13),
        plot.title = element_text(hjust = 0.5, size = 18),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 15),
        plot.margin = unit(c(0.2, 0.3, 0.2, 0.2), 
                           "inches"))

who <- who %>% gather(variable, value, c(New_cases,New_deaths), factor_key=TRUE)

who.labs <- c("New cases", "New deaths")
names(who.labs) <- c("New_cases", "New_deaths")

whopic <- who %>% ggplot(aes(x = datenum, y = value, color = country)) +
  geom_line() +
  scale_color_manual(values = c("EU" = "blue","Italy" = "red", "Germany" = "darkgreen"),
                     name = "Country") +
  scale_x_continuous(breaks = c(18262,18628,18993),
                     labels = c("18262" = "JAN 20","28628" = "JAN 21","18993" = "DEC 21")) +
  facet_wrap(~ variable, scales = "free",
             labeller = labeller(variable = who.labs) ) +
  ggtitle("Pandemic indicators") +
  # xlab("Time") +
  theme_bw() +
  theme(axis.title.y = element_blank(),axis.title.x = element_blank(),
        strip.text.x = element_text(size = 16),
        axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size = 13),
        plot.title = element_text(hjust = 0.5, size = 18),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 15),
        plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), 
                           "inches"))
  

ggpubr::ggarrange(espic,whopic, nrow  = 2, common.legend = TRUE, legend = "bottom")
ggsave(file="review2/review_2/submissionR2/figures/covidexp.jpg",width = 15,height = 9)

norm_minmax <- function(x){
  (x- min(x)) /(max(x)-min(x))
}

# df_eurostat <- df_eurostat %>% gather(variable,value, c(unemployment, debtgross,inflation),factor_key = TRUE)

who <- who %>% gather(variable, value, c(New_cases,New_deaths), factor_key=TRUE)
who <- who %>% select(names(df_eurostat))

who <- who  %>% filter(datenum >= 18262 & datenum <= 18993) %>%
  filter(country %in% c("Germany","Italy")) %>%
  group_by(country, variable) %>% mutate(valuenorm = norm_minmax(value))


who$df <- "who"


df_eurostat <- df_eurostat %>% filter(datenum >= 18262 & datenum <= 18993) %>%
  group_by(country,variable) %>% mutate(valuenorm = norm_minmax(value))
df_eurostat$date <- as.character(df_eurostat$date)
df_eurostat$df <- "eurostat"

df <- rbind(who,df_eurostat)

pandpic_it <- df %>% filter(country %in% c("Italy")) %>% 
  filter(! variable == "debtgross") %>%
  ggplot(aes(x = datenum, y  = valuenorm, color = variable)) +
  scale_x_continuous(breaks = c(18262,18414,18628,18779,18993),
                     labels = c("18262" = "JAN 20","18414" = "JUN 20","28628" = "JAN 21",
                                "18779" = "JUN 21","18993" = "DEC 21")) +
  scale_color_manual(values = c("New_cases" = "blue","New_deaths" = "red","inflation" = "purple",
                                "unemployment" = "darkgreen"),
                     labels = c("New_cases" = "New cases","New_deaths" = "New deaths","inflation" = "Inflation",
                                "unemployment" = "Unemployment")) + 
  geom_line() +
  facet_wrap( ~ country,dir ="v" ) +
  theme_bw() +
  theme(axis.title.y = element_blank(),axis.title.x = element_blank(),
        strip.text.x = element_text(size = 16),
        axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size = 13),
        plot.title = element_text(hjust = 0.5, size = 18),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 15),
        plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), 
                           "inches"),
        legend.position = "bottom")
#  theme(legend.position = "bottom", axis.title.y = element_blank(), axis.title.x = element_blank(),
#        strip.text.x = element_text(size = 12), legend.text = element_text(size = 12))

pandpic_de <- df %>% filter(country %in% c("Germany")) %>% 
  filter(! variable == "debtgross") %>%
  ggplot(aes(x = datenum, y  = valuenorm, color = variable)) +
  scale_x_continuous(breaks = c(18262,18414,18628,18779,18993),
                     labels = c("18262" = "JAN 20","18414" = "JUN 20","28628" = "JAN 21","18779" = "JUN 21",
                                "18993" = "DEC 21")) +
  scale_color_manual(values = c("New_cases" = "blue","New_deaths" = "red","inflation" = "purple",
                                "unemployment" = "darkgreen"),
                     labels = c("New_cases" = "New cases","New_deaths" = "New deaths","inflation" = "Inflation",
                                "unemployment" = "Unemployment")) + 
  geom_line() +
  facet_wrap( ~ country,dir ="v" ) +
  theme_bw() +
  theme(axis.title.y = element_blank(),axis.title.x = element_blank(),
        strip.text.x = element_text(size = 16),
        axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size = 13),
        plot.title = element_text(hjust = 0.5, size = 18),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 15),
        plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), 
                           "inches"),
        legend.position = "bottom")
#  theme(legend.position = "bottom", axis.title.y = element_blank(), axis.title.x = element_blank(),
#        strip.text.x = element_text(size = 12), legend.text = element_text(size = 12))

# ggsave(file="figures/test.jpg",width =8, height = 3)

ggpubr::ggarrange(de_tm, it_tm, pandpic_de,pandpic_it)
ggsave(file="review2/review_2/submissionR2/figures/topicprev.jpg",width =13.5, height = 10)


############### extract sample for testing inter-rater categorization
dfita <- dftop[dftop$country == "Italy",]
df <- list()
for (i in dfita$description_3) {
  
  quest_1=subset(dfita,description_3==i) #selecting a sub group of data
  quest_1
  x=popsamp((nrow(quest_1)*0.05),quest_1)  # sampling in that group 
   # printing sample 
  df[[i]] = x
}

dff <- bind_rows(df)

topit <- dfita %>% filter(count_name_occurr >= 17) %>% filter(! user_username %in% dff$user_username)
sample_it <- rbind(dff,topit)

####################

load("review_1/sample_rv1/DE/stm_DE20.Rdata")
load("review_1/sample_rv1/DE/dfm_desolcv.Rdata")
stm_dfDE <- quanteda::convert(dfm_desolcv,to = "stm")  
numm <- 20


prepDE <- estimateEffect(1:numm ~ description_4 * s(datenum), stm_DE20, metadata = stm_dfDE$meta, uncertainty = "Global")
prepIT <- estimateEffect(1:numm ~ description_4 * s(datenum), stm_it25cont, metadata = stm_df$meta, uncertainty = "Global")

# save(prepDE,file=paste0(folder,"DE/prepDE.Rdata"))

# load(paste0(folder,"DE/prepDE.R"))

effects_intDEass <- get_effects(estimates = prepDE,
                             variable = 'datenum',
                             type = 'continuous',
                             moderator = 'description_4',
                             modval = "association")

effects_intDEpol <- get_effects(estimates = prepDE,
                             variable = 'datenum',
                             type = 'continuous',
                             moderator = 'description_4',
                             modval = "governance & politics")

effects_intDEms <- get_effects(estimates = prepDE,
                                variable = 'datenum',
                                type = 'continuous',
                                moderator = 'description_4',
                                modval = "market sector")

effects_intDEmedia <- get_effects(estimates = prepDE,
                               variable = 'datenum',
                               type = 'continuous',
                               moderator = 'description_4',
                               modval = "market sector")

%>%
  bind_rows(
    get_effects(estimates = prepDE,
                variable = 'datenum',
                type = 'continuous',
                moderator = 'description_4',
                modval = "governance & politics") 
  ) %>%
      
      bind_rows(
        get_effects(estimates = prepDE,
                    variable = 'datenum',
                    type = 'continuous',
                    moderator = 'description_4',
                    modval = "market sector")
      ) %>%
      
      bind_rows(
        get_effects(estimates = prepDE,
                    variable = 'datenum',
                    type = 'continuous',
                    moderator = 'description_4',
                    modval = "media")
      ) %>%
      
      bind_rows(
        get_effects(estimates = prepDE,
                    variable = 'datenum',
                    type = 'continuous',
                    moderator = 'description_4',
                    modval = "private account")
      )
    
  

effects_intIT <- effects_intIT %>% mutate(country = "Italy")  %>% 
 rename(category = moderator) %>%
 mutate(topic = as.numeric(topic)) %>%
  mutate(label = recode(topic,
                                                         "1" = "Hospital\ndonations",
                                                         "2" = "Charity",
                                                         "3" = "Economy", #
                                                         "4" = "Covid\noutbreak",
                                                         "5" = "Food\ndonations",
                                                         "6" = "Digital\nsolidarity",
                                                         # "7" = "Third\nsector",
                                                         "7" = "Local\ncorporates",
                                                         "8" = "Health\nemergency", #*
                                                         "9" = "Europe",
                                                         "10" = "Local\ndonations",
                                                         "11" = "Bergamo",
                                                         # "12" = "Global\nwelfare",
                                                         "12" = "Cooperation",
                                                         "13" = "Community\ndonations",
                                                         "14" = "Services\nmunicipalities",
                                                         "15" = "Culture",
                                                         # "16" = "Companies\ndonations", #
                                                         "16" = "Corporate\nresponsibility", #
                                                         "17" = "Emergency\ndonations",
                                                         "18" = "Fundraising", #
                                                         "19" = "Municipalities\ninitiatives",
                                                         "20" = "Lockdown", #
                                                         "21" = "Vaccination",
                                                         "22" = "Governance", #
                                                         "23" = "Families",
                                                         "24" = "Artisans",
                                                         "25" = "Volunteering"
  )) %>%
  mutate(type = recode(topic,
                       "1" = "donations",
                       "2" = "targets",
                       "3" = "donations",
                       "4" = "targets",
                       "5" = "donations",
                       "6" = "society",
                       "7" = "donations",
                       "8" = "public health",
                       "9" = "transnational",
                       "10" = "donations",
                       "11" = "targets",
                       "12" = "society",
                       "13" = "donations",
                       "14" = "governance",
                       "15" = "society",
                       "16" = "donations",
                       "17" = "donations",
                       "18" = "donations",
                       "19" = "society",
                       "20" = "donations",
                       "21" = "public health",
                       "22" = "society",
                       "23" = "society",
                       "24" = "donations",
                       "25" = "public health" ))


effects_intDE <- effects_intDE %>% mutate(country = "Germany")  %>% 
  rename(category = moderator) %>%
  mutate(topic = as.numeric(topic)) %>%
  mutate(label = recode(topic,
                        "1" = "Doctors\ninternational",
                        "2" = "Housing",
                        "3" = "Social\nresponsibility", #*
                        "4" = "Stayhome",
                        "5" = "Refugees",
                        "6" = "Masks",
                        "7" = "Cohesion",
                        #  "8" = "Governance",
                        "8" = "Governance\nmedia",
                        "9" = "Europe",
                        "10" = "Mobilizations",
                        "11" = "International\ncoop",
                        "12" = "Vaccination",
                        "13" = "Freedom",
                        "14" = "Flattencurve",
                        "15" = "Social exclusion",
                        "16" = "Charity",
                        "17" = "Local\ninitiatives",
                        "18" = "Access\nvaccination", # European\ngovernance
                        "19" = "Fight\ntogether", #* lockdown
                        "20" = "Health crisis")) %>%
  mutate(type = recode(topic,
                       "1" = "transnational",
                       "2" = "society",
                       "3" = "society",
                       "4" = "public health",
                       "5" = "transnational",
                       "6" = "public health",
                       "7" = "society",
                       "8" = "governance",
                       "9" = "transnational",
                       "10" = "society",
                       "11" = "transnational",
                       "12" = "public health",
                       "13" = "public health",
                       "14" = "public health",
                       "15" = "society",
                       "16" = "donations",
                       "17" = "targets",
                       "18" = "targets" ,# "governance",
                       "19" = "society", # "public health",
                       "20" = "society"))


##### Category * country #########

load("review_1/sample_rv1/IT/stm_it25.Rdata")
load("review_1/sample_rv1/IT/dfm_itsolcv.Rdata")
stm_dfIT <- quanteda::convert(dfm_itsolcv,to = "stm")  
numm <- 25

dfbIT <- df_finlocast %>% filter(country == "Italy")


sgIT <- sageLabels(stm_it25,15)
sg_probIT <- tibble(topic = 1:numm,sgIT$marginal$prob) # marginal probability
sg_frexIT <- tibble(topic = 1:numm,sgIT$marginal$frex) # marginal frex
sg_prob_assIT <- tibble(topic = 1:numm,sgIT$covnames[[1]],sgIT$cov.betas[[1]]$problabels) # ASSOCIATION prob
sg_prob_govIT <- tibble(topic = 1:numm,sgIT$covnames[[2]],sgIT$cov.betas[[2]]$problabels) # GOVERNANCE prob
sg_prob_mktIT <- tibble(topic = 1:numm,sgIT$covnames[[3]],sgIT$cov.betas[[3]]$problabels) # MARKET prob
sg_prob_medIT <- tibble(topic = 1:numm,sgIT$covnames[[4]],sgIT$cov.betas[[4]]$problabels) # MEDIA prob
sg_prob_pacIT <- tibble(topic = 1:numm,sgIT$covnames[[5]],sgIT$cov.betas[[5]]$problabels) # PRIVATE ACCOUNT prob

sg_frex_assIT <- tibble(topic = 1:numm,sgIT$covnames[[1]],sgIT$cov.betas[[1]]$frexlabels) # ASSOCIATION frex
sg_frex_govIT <- tibble(topic = 1:numm,sgIT$covnames[[2]],sgIT$cov.betas[[2]]$frexlabels) # GOVERNANCE frex
sg_frex_mktIT <- tibble(topic = 1:numm,sgIT$covnames[[3]],sgIT$cov.betas[[3]]$frexlabels) # MARKET frex
sg_frex_medIT <- tibble(topic = 1:numm,sgIT$covnames[[4]],sgIT$cov.betas[[4]]$frexlabels) # MEDIA frex
sg_frex_pacIT <- tibble(topic = 1:numm,sgIT$covnames[[5]],sgIT$cov.betas[[5]]$frexlabels) # PRIVATE ACCOUNT frex


dfITglobprob <-  as.data.frame(do.call(cbind, sg_probIT))

dfITglobprob <- gather(dfITglobprob,probterms,termsprob,V2:V16)%>%
  filter(! termsprob %in% c(cvd,"solidarity")) %>%
  spread(probterms,termsprob)

dfITglobprob$probterms <- NA

for (i in dfITglobprob$topic) {
  
  dfITglobprob[dfITglobprob$topic == i,]$probterms <- 
      paste0(dfITglobprob[dfITglobprob$topic == i,c(2:16)],collapse= ", ")
  }
  
dfITglobprob$probterms <- str_remove_all(dfITglobprob$probterms,"\\bNA, \\b|\\b, NA\\b")

dfITglobprob <- dfITglobprob %>% select(topic,probterms)

# frex

dfITglobfrex <-  as.data.frame(do.call(cbind, sg_frexIT))

dfITglobfrex <- gather(dfITglobfrex,frexterms,termsfrex,V2:V16)%>%
  filter(! termsfrex %in% c(cvd,"solidarity")) %>%
  spread(frexterms,termsfrex)

dfITglobfrex$frexterms <- NA

for (i in dfITglobfrex$topic) {
  
  dfITglobfrex[dfITglobfrex$topic == i,]$frexterms <- 
    paste0(dfITglobfrex[dfITglobfrex$topic == i,c(2:16)],collapse= ", ")
}

dfITglobfrex$frexterms <- str_remove_all(dfITglobfrex$frexterms,"\\bNA, \\b|\\b, NA\\b")

dfITglobfrex <- dfITglobfrex %>% select(topic,frexterms)

dfITglob <- merge(dfITglobprob,dfITglobfrex,by = "topic") %>%
mutate(label = recode(topic,
                      "1" = "Hospital\ndonations",
                      "2" = "Charity",
                      "3" = "Economy", #
                      "4" = "Covid outbreak", # ex covid outbreak
                      "5" = "Food\ndonations",
                      "6" = "Digital\nsolidarity",
                      # "7" = "Third\nsector",
                      "7" = "Local\ncorporates",
                      "8" = "Health\nemergency", #*
                      "9" = "Europe",
                      "10" = "Local\ndonations",
                      "11" = "Bergamo",
                      # "12" = "Global\nwelfare",
                      "12" = "Welfare", # ex cooperation
                      "13" = "Community\ndonations",
                      "14" = "Services\nmunicipalities",
                      "15" = "Culture",
                      # "16" = "Companies\ndonations", #
                      "16" = "Corporate\nresponsibility", #
                      "17" = "Emergency\ndonations",
                      "18" = "Fundraising", #
                      "19" = "Municipalities\ninitiatives",
                      "20" = "Lockdown", #
                      "21" = "Vaccination",
                      "22" = "Governance", #
                      "23" = "Families",
                      "24" = "Artisans",
                      "25" = "Volunteering"
)) %>%
  select(label,probterms,frexterms)
write.csv(dfITglob,file="review_1/sample_rv1/IT/dfITglob.csv",sep=";",row.names = F)



# gamma_terms_catIT <-  merge(gamma_terms_catIT,dfITfin,by = c("topic","category"))


#

td_gammaIT <- tidy(stm_it25, matrix = "gamma")
ID_rowIT <- names(stm_dfIT$documents) # the name of documents gets lost, the row number is reported
td_gammaIT <- cbind(td_gammaIT,ID_rowIT) # Here I map each document to its name via row, I checked with content, it works
td_gammaIT <- cbind(td_gammaIT,dfbIT) # merge the gamma matrix with the dataframe via ID, so the variables to sort documents can be used






top_terms_catIT <- tidy(stm_it25) %>%
  rename(category = y.level) %>% 
  group_by(topic,category,term) %>%
  summarise(beta = mean(beta))  %>%
  arrange(beta) %>% 
  filter(!term %in% c(cvd,"solidarity")) %>%   # to filter out either it_policies or de_policies
  top_n(5, beta) %>%
  arrange(-beta)%>%
  select(topic, term) %>%
  summarise(terms = list(unique(term))) %>%
  mutate(terms = map(terms, paste, collapse = ", ")) %>% 
  unnest(cols = c(terms))

gamma_terms_catIT <- td_gammaIT %>%
  rename(category = description_4) %>% 
  group_by(topic,category) %>%
  summarise(gamma = mean(gamma)) %>%
  arrange(desc(gamma)) %>%
  left_join(top_terms_catIT, by = c("topic","category")) %>%
mutate(label = recode(topic,
                      "1" = "Hospital\ndonations",
                      "2" = "Charity",
                      "3" = "Economy", #
                      "4" = "Covid outbreak", # ex covid outbreak
                      "5" = "Food\ndonations",
                      "6" = "Digital\nsolidarity",
                      # "7" = "Third\nsector",
                      "7" = "Local\ncorporates",
                      "8" = "Health\nemergency", #*
                      "9" = "Europe",
                      "10" = "Local\ndonations",
                      "11" = "Bergamo",
                      # "12" = "Global\nwelfare",
                      "12" = "Welfare", # ex cooperation
                      "13" = "Community\ndonations",
                      "14" = "Services\nmunicipalities",
                      "15" = "Culture",
                      # "16" = "Companies\ndonations", #
                      "16" = "Corporate\nresponsibility", #
                      "17" = "Emergency\ndonations",
                      "18" = "Fundraising", #
                      "19" = "Municipalities\ninitiatives",
                      "20" = "Lockdown", #
                      "21" = "Vaccination",
                      "22" = "Governance", #
                      "23" = "Families",
                      "24" = "Artisans",
                      "25" = "Volunteering"
)) %>%
  mutate(type = recode(topic,
                       "1" = "donations",
                       "2" = "targets",
                       "3" = "donations",
                       "4" = "transnational",
                       "5" = "donations",
                       "6" = "society",
                       "7" = "donations",
                       "8" = "public health",
                       "9" = "transnational",
                       "10" = "donations",
                       "11" = "targets",
                       "12" = "society",
                       "13" = "donations",
                       "14" = "governance",
                       "15" = "society",
                       "16" = "donations",
                       "17" = "donations",
                       "18" = "donations",
                       "19" = "society",
                       "20" = "donations",
                       "21" = "public health",
                       "22" = "society",
                       "23" = "society",
                       "24" = "donations",
                       "25" = "public health" ))

gamma_terms_catIT$country <- "Italy"

df_associationIT <- cbind( sg_frex_assIT[,1],sg_frex_assIT[,2],as.data.frame(do.call(rbind, sg_frex_assIT[,3])))
df_associationIT$category <- df_associationIT[,2]
df_associationIT <- df_associationIT[,-2]
df_governanceIT <- cbind( sg_frex_govIT[,1],sg_frex_govIT[,2],as.data.frame(do.call(rbind, sg_frex_govIT[,3])))
df_governanceIT$category <- df_governanceIT[,2]
df_governanceIT <- df_governanceIT[,-2]
df_marketIT <- cbind( sg_frex_mktIT[,1],sg_frex_mktIT[,2],as.data.frame(do.call(rbind, sg_frex_mktIT[,3])))
df_marketIT$category <- df_marketIT[,2]
df_marketIT <- df_marketIT[,-2]
df_mediaIT <- cbind( sg_frex_medIT[,1],sg_frex_medIT[,2],as.data.frame(do.call(rbind, sg_frex_medIT[,3])))
df_mediaIT$category <- df_mediaIT[,2]
df_mediaIT <- df_mediaIT[,-2]
df_pacIT <- cbind( sg_frex_pacIT[,1],sg_frex_pacIT[,2],as.data.frame(do.call(rbind, sg_frex_pacIT[,3])))
df_pacIT$category <- df_pacIT[,2]
df_pacIT <- df_pacIT[,-2]

dfIT <- rbind(df_associationIT, df_governanceIT, df_marketIT, df_mediaIT,df_pacIT)

dfITfin <- gather(dfIT,frexterms,termsfrex,V1:V6)%>%
  filter(! termsfrex %in% c(cvd,"solidarity")) %>%
  spread(frexterms,termsfrex)

dfITfin$frexterms <- NA

for (i in dfITfin$topic) {

  for (a in dfITfin$category) {
    dfITfin[dfITfin$topic == i & dfITfin$category == a,]$frexterms <- 
      paste0(dfITfin[dfITfin$topic == i & dfITfin$category == a,c(3:8)],collapse= ", ")
  }

}

dfITfin$frexterms <- str_remove_all(dfITfin$frexterms,"\\bNA, \\b|\\b, NA\\b")

gamma_terms_catIT <-  merge(gamma_terms_catIT,dfITfin,by = c("topic","category"))

# Germany category ####
load("review_1/sample_rv1/DE/stm_de20.Rdata")
load("review_1/sample_rv1/DE/dfm_desolcv.Rdata")
stm_dfDE <- quanteda::convert(dfm_desolcv,to = "stm")  
numm <- 20

dfbDE <- df_finlocast %>% filter(country == "Germany")

td_gammaDE <- tidy(stm_de20, matrix = "gamma")
ID_rowDE <- names(stm_dfDE$documents) # the name of documents gets lost, the row number is reported
td_gammaDE <- cbind(td_gammaDE,ID_rowDE) # Here I map each document to its name via row, I checked with content, it works
td_gammaDE <- cbind(td_gammaDE,dfbDE) # merge the gamma matrix with the dataframe via ID, so the variables to sort documents can be used


top_terms_catDE <- tidy(stm_de20) %>%
  rename(category = y.level) %>% 
  group_by(topic,category,term) %>%
  summarise(beta = mean(beta))  %>%
  arrange(beta) %>% 
  filter(!term %in% c(cvd,"solidarity")) %>%   # to filter out either it_policies or de_policies
  top_n(5, beta) %>%
  arrange(-beta)%>%
  select(topic, term) %>%
  summarise(terms = list(unique(term))) %>%
  mutate(terms = map(terms, paste, collapse = ", ")) %>% 
  unnest()

gamma_terms_catDE <- td_gammaDE %>%
  rename(category = description_4) %>% 
  group_by(topic,category) %>%
  summarise(gamma = mean(gamma)) %>%
  arrange(desc(gamma)) %>%
  left_join(top_terms_catDE, by = c("topic","category")) %>%
mutate(label = recode(topic,
                      "1" = "Doctors\ninternational",
                      "2" = "Housing",
                      "3" = "Social\nresponsibility", #*
                      "4" = "Stayhome",
                      "5" = "Refugees",
                      "6" = "Masks",
                      "7" = "Cohesion",
                      #  "8" = "Governance",
                      "8" = "Governance\nmedia",
                      "9" = "Europe",
                      "10" = "Mobilizations",
                      "11" = "International\ncoop",
                      "12" = "Vaccination",
                      "13" = "Freedom",
                      "14" = "Flattencurve",
                      "15" = "Social exclusion",
                      "16" = "Charity",
                      "17" = "Local\ninitiatives",
                      "18" = "Access\nvaccination", # European\ngovernance
                      "19" = "Fight\ntogether", #* lockdown
                      "20" = "Health crisis")) %>%
mutate(type = recode(topic,
                     "1" = "transnational",
                     "2" = "society",
                     "3" = "society",
                     "4" = "public health",
                     "5" = "transnational",
                     "6" = "public health",
                     "7" = "society",
                     "8" = "governance",
                     "9" = "transnational",
                     "10" = "society",
                     "11" = "transnational",
                     "12" = "public health",
                     "13" = "public health",
                     "14" = "public health",
                     "15" = "society",
                     "16" = "donations",
                     "17" = "targets",
                     "18" = "targets" ,# "governance",
                     "19" = "society", # "public health",
                     "20" = "society"))

gamma_terms_catDE$country <- "Germany"


sgDE <- sageLabels(stm_de20,15)
sg_probDE <- tibble(topic = 1:numm,sgDE$marginal$prob) # marginal probability
sg_frexDE <- tibble(topic = 1:numm,sgDE$marginal$frex) # marginal frex
sg_prob_assDE <- tibble(topic = 1:numm,sgDE$covnames[[1]],sgDE$cov.betas[[1]]$problabels) # ASSOCIATION prob
sg_prob_govDE <- tibble(topic = 1:numm,sgDE$covnames[[2]],sgDE$cov.betas[[2]]$problabels) # GOVERNANCE prob
sg_prob_mktDE <- tibble(topic = 1:numm,sgDE$covnames[[3]],sgDE$cov.betas[[3]]$problabels) # MARKET prob
sg_prob_medDE <- tibble(topic = 1:numm,sgDE$covnames[[4]],sgDE$cov.betas[[4]]$problabels) # MEDIA prob
sg_prob_pacDE <- tibble(topic = 1:numm,sgDE$covnames[[5]],sgDE$cov.betas[[5]]$problabels) # PRIVATE ACCOUNT prob

sg_frex_assDE <- tibble(topic = 1:numm,sgDE$covnames[[1]],sgDE$cov.betas[[1]]$frexlabels) # ASSOCIATION frex
sg_frex_govDE <- tibble(topic = 1:numm,sgDE$covnames[[2]],sgDE$cov.betas[[2]]$frexlabels) # GOVERNANCE frex
sg_frex_mktDE <- tibble(topic = 1:numm,sgDE$covnames[[3]],sgDE$cov.betas[[3]]$frexlabels) # MARKET frex
sg_frex_medDE <- tibble(topic = 1:numm,sgDE$covnames[[4]],sgDE$cov.betas[[4]]$frexlabels) # MEDIA frex
sg_frex_pacDE <- tibble(topic = 1:numm,sgDE$covnames[[5]],sgDE$cov.betas[[5]]$frexlabels) # PRIVATE ACCOUNT frex

# global


dfDEglobprob <-  as.data.frame(do.call(cbind, sg_probDE))

dfDEglobprob <- gather(dfDEglobprob,probterms,termsprob,V2:V16)%>%
  filter(! termsprob %in% c(cvd,"solidarity")) %>%
  spread(probterms,termsprob)

dfDEglobprob$probterms <- NA

for (i in dfDEglobprob$topic) {
  
  dfDEglobprob[dfDEglobprob$topic == i,]$probterms <- 
    paste0(dfDEglobprob[dfDEglobprob$topic == i,c(2:16)],collapse= ", ")
}

dfDEglobprob$probterms <- str_remove_all(dfDEglobprob$probterms,"\\bNA, \\b|\\b, NA\\b")

dfDEglobprob <- dfDEglobprob %>% select(topic,probterms)

# frex

dfDEglobfrex <-  as.data.frame(do.call(cbind, sg_frexDE))

dfDEglobfrex <- gather(dfDEglobfrex,frexterms,termsfrex,V2:V16)%>%
  filter(! termsfrex %in% c(cvd,"solidarity")) %>%
  spread(frexterms,termsfrex)

dfDEglobfrex$frexterms <- NA

for (i in dfDEglobfrex$topic) {
  
  dfDEglobfrex[dfDEglobfrex$topic == i,]$frexterms <- 
    paste0(dfDEglobfrex[dfDEglobfrex$topic == i,c(2:16)],collapse= ", ")
}

dfDEglobfrex$frexterms <- str_remove_all(dfDEglobfrex$frexterms,"\\bNA, \\b|\\b, NA\\b")

dfDEglobfrex <- dfDEglobfrex %>% select(topic,frexterms)

dfDEglob <- merge(dfDEglobprob,dfDEglobfrex,by = "topic") %>%
  mutate(label = recode(topic,
                        "1" = "Doctors\ninternational",
                        "2" = "Housing",
                        "3" = "Social\nresponsibility", #*
                        "4" = "Stayhome",
                        "5" = "Refugees",
                        "6" = "Masks",
                        "7" = "Cohesion",
                        #  "8" = "Governance",
                        "8" = "Governance\nmedia",
                        "9" = "Europe",
                        "10" = "Mobilizations",
                        "11" = "International\ncoop",
                        "12" = "Vaccination",
                        "13" = "Freedom",
                        "14" = "Flattencurve",
                        "15" = "Social exclusion",
                        "16" = "Charity",
                        "17" = "Local\ninitiatives",
                        "18" = "Access\nvaccination", # European\ngovernance
                        "19" = "Fight\ntogether", #* lockdown
                        "20" = "Health crisis")) %>%
  select(label,probterms,frexterms)
write.csv(dfDEglob,file="review_1/sample_rv1/DE/dfDEglob.csv",sep=";",row.names = F)




















df_associationDE <- cbind( sg_frex_assDE[,1],sg_frex_assDE[,2],as.data.frame(do.call(rbind, sg_frex_assDE[,3])))
df_associationDE$category <- df_associationDE[,2]
df_associationDE <- df_associationDE[,-2]
df_governanceDE <- cbind( sg_frex_govDE[,1],sg_frex_govDE[,2],as.data.frame(do.call(rbind, sg_frex_govDE[,3])))
df_governanceDE$category <- df_governanceDE[,2]
df_governanceDE <- df_governanceDE[,-2]
df_marketDE <- cbind( sg_frex_mktDE[,1],sg_frex_mktDE[,2],as.data.frame(do.call(rbind, sg_frex_mktDE[,3])))
df_marketDE$category <- df_marketDE[,2]
df_marketDE <- df_marketDE[,-2]
df_mediaDE <- cbind( sg_frex_medDE[,1],sg_frex_medDE[,2],as.data.frame(do.call(rbind, sg_frex_medDE[,3])))
df_mediaDE$category <- df_mediaDE[,2]
df_mediaDE <- df_mediaDE[,-2]
df_pacDE <- cbind( sg_frex_pacDE[,1],sg_frex_pacDE[,2],as.data.frame(do.call(rbind, sg_frex_pacDE[,3])))
df_pacDE$category <- df_pacDE[,2]
df_pacDE <- df_pacDE[,-2]

dfDE <- rbind(df_associationDE, df_governanceDE, df_marketDE, df_mediaDE,df_pacDE)

dfDEfin <- gather(dfDE,frexterms,termsfrex,V1:V6) %>%
  filter(! termsfrex %in% c(cvd,"solidarity")) %>%
  spread(frexterms,termsfrex)

dfDEfin$frexterms <- NA

for (i in dfDEfin$topic) {
  
  for (a in dfDEfin$category) {
    dfDEfin[dfDEfin$topic == i & dfDEfin$category == a,]$frexterms <- 
      paste0(dfDEfin[dfDEfin$topic == i & dfDEfin$category == a,c(3:8)],collapse= ", ")
  }
  
}

dfDEfin$frexterms <- str_remove_all(dfDEfin$frexterms,"\\bNA, \\b|\\b, NA\\b")

gamma_terms_catDE <-  merge(gamma_terms_catDE,dfDEfin,by = c("topic","category"))

gamma_terms_catTOT <- rbind(gamma_terms_catDE,gamma_terms_catIT) 
save(gamma_terms_catTOT,file="review_1/sample_rv1/gamma_terms_catTOT.Rdata")

# plots combined
load("review_1/sample_rv1/gamma_terms_catTOT.Rdata") 
gamma_terms_catTOT[gamma_terms_catTOT$label == "Mobilitations",]$label <- "Mobilizations"
gamma_terms_catTOT[gamma_terms_catTOT$label == "Covid outbreak",]$label <- "Covid\noutbreak"
gamma_terms_catTOT[gamma_terms_catTOT$label == "Social exclusion",]$label <- "Social\nexclusion"

gamma_terms_catTOT %>%
group_by(country,category) %>%
  top_n(4, gamma) %>%
  ungroup %>%
#  mutate(label = reorder_within(label, gamma, category)) %>%
  ggplot(aes(reorder_within(label, gamma, category), gamma, fill = type)) +
 # geom_col() +
  geom_bar(stat = 'identity') +
  geom_text(aes(label = frexterms),hjust = 0, y = 0.001, size = 7, # nudge_y = 0.00005, size = 5, # 0.0005
            family = "IBMPlexSans") +
  coord_flip() +
  scale_x_reordered() +
  scale_fill_manual(values= c(
    "donations" = "lightsalmon",
    "governance" = "peachpuff",
    "public health" = "plum1",
    "society" = "lightskyblue",
    "targets" = "khaki",
    "transnational" = "palegreen"
  ),
  name = "Solidarity categories") +
  facet_wrap(~ category + country, ncol = 2, scale="free") + 
  scale_y_continuous(label = scales::percent) +
  theme_bw() +
  theme(axis.title.y = element_blank(),axis.title.x = element_blank(),
        axis.text.x = element_text(size = 12),
        strip.text.x = element_text(size = 21), 
        axis.text.y = element_text(size = 19),
        legend.position = "bottom",
        legend.text = element_text(size=22),
        legend.title = element_text(size=23),
        plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), 
                            "inches")
  ) + guides(fill=guide_legend(nrow=1))
ggsave(file = paste0("review2/review_2/submissionR2/figures/categoriestopic.jpg"),width = 23.2,height = 20)


#### annex figure 6

for (i in unique(gamma_terms_catTOT$category)) {
  


decat  <- gamma_terms_catTOT %>%
  filter(country == "Germany") %>%
  filter(category == i) %>%
  group_by(country,category) %>%
#  top_n(4, gamma) %>%
  ungroup %>%
  #  mutate(label = reorder_within(label, gamma, category)) %>%
  ggplot(aes(reorder_within(label, gamma, category), gamma, fill = type)) +
  # geom_col() +
  geom_bar(stat = 'identity') +
  geom_text(aes(label = frexterms),hjust = 0, y = 0.001, size = 5.8, # nudge_y = 0.00005, size = 5, # 0.0005
            family = "IBMPlexSans") +
  coord_flip() +
  scale_x_reordered() +
  scale_fill_manual(values= c(
    "donations" = "lightsalmon",
    "governance" = "peachpuff",
    "public health" = "plum1",
    "society" = "lightskyblue",
    "targets" = "khaki",
    "transnational" = "palegreen"
  ),
  name = "Solidarity categories") +
  facet_wrap(~ category + country, ncol = 2, scale="free") + 
  scale_y_continuous(label = scales::percent) +
  theme_bw() +
  theme(axis.title.y = element_blank(),axis.title.x = element_blank(),
        axis.text.x = element_text(size = 10),
        strip.text.x = element_text(size = 18), 
        axis.text.y = element_text(size = 14),
        legend.position = "bottom",
        legend.text = element_text(size=18),
        legend.title = element_text(size=19),
        plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), 
                           "inches")
  ) + guides(fill=guide_legend(nrow=1))


itcat  <- gamma_terms_catTOT %>%
  filter(country == "Italy") %>%
  filter(category == i) %>%
  group_by(country,category) %>%
#  top_n(4, gamma) %>%
  ungroup %>%
  #  mutate(label = reorder_within(label, gamma, category)) %>%
  ggplot(aes(reorder_within(label, gamma, category), gamma, fill = type)) +
  # geom_col() +
  geom_bar(stat = 'identity') +
  geom_text(aes(label = frexterms),hjust = 0, y = 0.001, size = 5.8, # nudge_y = 0.00005, size = 5, # 0.0005
            family = "IBMPlexSans") +
  coord_flip() +
  scale_x_reordered() +
  scale_fill_manual(values= c(
    "donations" = "lightsalmon",
    "governance" = "peachpuff",
    "public health" = "plum1",
    "society" = "lightskyblue",
    "targets" = "khaki",
    "transnational" = "palegreen"
  ),
  name = "Solidarity categories") +
  facet_wrap(~ category + country, ncol = 2, scale="free") + 
  scale_y_continuous(label = scales::percent) +
  theme_bw() +
  theme(axis.title.y = element_blank(),axis.title.x = element_blank(),
        axis.text.x = element_text(size = 10),
        strip.text.x = element_text(size = 18), 
        axis.text.y = element_text(size = 14),
        legend.position = "bottom",
        legend.text = element_text(size=18),
        legend.title = element_text(size=19),
        plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), 
                           "inches")
  ) + guides(fill=guide_legend(nrow=1))


ggpubr::ggarrange(decat,itcat,common.legend = TRUE, legend = "bottom")
ggsave(file = paste0("review2/review_2/submissionR2/figures/categoriestopic_",i,".jpg"),width = 21.5,height = 15)

}


for (i in unique(gamma_terms_catTOT$country)) {
  
  annex <-  gamma_terms_catTOT %>%
    filter(country == i) %>%
    select(label,category,terms,frexterms) %>%
    rename("Topic" = label,
           "Category" = category,
           "Probs" = terms,
           "Frex" = frexterms
    )
  write.csv(annex, file=paste0("/review2/review_2/submissionR2/figures/categoriestopic_",i,"_anxcat.csv"),sep=";",row.names = F)
  
}



# 
#   df <- gamma_terms_cat[gamma_terms_cat$category == i,] %>% 
#     group_by(category) %>% top_n(5,gamma) 
#     
#  pltime <- effects_intDE[effects_intDE$category == i,] %>% filter(label %in% df$label)  %>%
#     ggplot(aes(x = value, y = proportion, color = label)) + geom_line() + 
#     scale_x_continuous(breaks = c(18289,18414,18627,18809,18992),
#                        labels = c("18289" = "GEN 20","18414" = "JUN 20","18627" = "JAN 21","18809" = "JUN 21",
#                                   "18992" = "DEC 21")) +
#     # scale_color_manual(values = c("2" = "red","23" = "blue", "3" = "black","14" = "gray",
#     #                               "4" = "pink","21" = "purple","20" = "brown"),
#     #                    labels = c("2" = "Charity","23" = "Families", "3" = "Economy",
#     #                               "14" = "Services\nmunicipalities","4" = "Covid\noutbreak","21" = "Vaccination",
#     #                               "20" = "Lockdown" ),
#     #                    name = "Topics" ) +
#     scale_y_continuous(labels = scales::percent_format() ) +
#    # facet_wrap(~ i) +
#     # ggtitle("Italy") + 
#     #  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2)  +
#     ylab("Expected Proportion") +
#     xlab("Time") +
#    guides(color=guide_legend(title="Top 5 topics:")) + 
#     theme(axis.title.x = element_blank(), legend.position = "bottom",
#           strip.text.x = element_text(size = 12), legend.text = element_text(size=12),
#           legend.title =  element_text(size=12)) + 
#       theme_bw()
#   
#  ppl <- grid.arrange(tprop,pltime, heights=c(0.75, 0.25))
#  ggsave(ppl, file = paste0("review_1/sample_rv1/DE_cattermstime_",i,".jpg"),width = 8,height = 8)
# }
# 
# 
# effects_intDE[effects_intDE$moderator == "association",] %>% 
#   filter(topic %in% c("10","17","19","12"))  %>%
#   ggplot(aes(x = value, y = proportion)) + geom_line(aes(color = topic)) + 
#  scale_x_continuous(breaks = c(18289,18414,18627,18809,18992),
#                     labels = c("18289" = "GEN 20","18414" = "JUN 20","18627" = "JAN 21","18809" = "JUN 21",
#                                "18992" = "DEC 21")) +
#   # scale_color_manual(values = c("2" = "red","23" = "blue", "3" = "black","14" = "gray",
#   #                               "4" = "pink","21" = "purple","20" = "brown"),
#   #                    labels = c("2" = "Charity","23" = "Families", "3" = "Economy",
#   #                               "14" = "Services\nmunicipalities","4" = "Covid\noutbreak","21" = "Vaccination",
#   #                               "20" = "Lockdown" ),
#   #                    name = "Topics" ) +
# #  scale_y_continuous(labels = scales::percent_format() ) +
#    facet_wrap(~ topic, scales = "free") +
#   # ggtitle("Italy") + 
#   # geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2)  +
#   ylab("Expected Proportion") +
#   xlab("Time") +
# #  guides(color=guide_legend(title="Top 5 topics:")) + 
#   theme(axis.title.x = element_blank(), legend.position = "bottom",
#         strip.text.x = element_text(size = 12), legend.text = element_text(size=12),
#         legend.title =  element_text(size=12)) + 
#   theme_bw()
# plotly::ggplotly()
# 
# ####################
# 
# a <- long_report$`sg$cov.betas[[1]]$frexlabels`[2,]
# 
# frea <- sg_frex_ass$`sg$cov.betas[[1]]$frexlabels`[1,]
# 
# frea_ass <- list()
# for (i in 1:25) {
#   frea_text <- list()
#   frea_textn <- sg_frex_ass$`sg$cov.betas[[1]]$frexlabels`[i,]
#   frea_text[[i]] <-frea_textn
#   frea_textfin <- do.call(rbind.data.frame, frea_text)
#   frea_ass[[i]] <- frea_textfin
#   }
# bind_rows(frea_ass)
# 
# 
# thoughts <- list()
# for (i in 1:numm){ # 
#   # thg_det <- findThoughts(stm_m, texts = df_de$text,n = 3, topics =i)$docs[[1]]
#   thought_text = list()
#   dates <- findThoughts(stm_m, texts = dfb$text,n = 20, topics =i)$index[[1]] # 
#   for (n in dates) {
#     txx <-  print(c(paste0(" ACT: ", dfb[n,]$description_3,
#                            " ACTsj: ", dfb[n,]$user_username," TXT: ",dfb[n,]$text," DATE: ", dfb[n,]$created_at,
#                            " TWID: ",dfb[n,]$tweet_id," CNID ",dfb[n,]$conversation_id)))
#     thought_text[[n]] <- txx
#     thought_textfin <- do.call(rbind.data.frame, thought_text)
#   }
#   thoughts[[i]] <- thought_textfin
#   
# }
# bind_rows(thoughts)
# 
# frep_df <- list()
# for (f in 1:25) {
#  frep <- unlist(sg_frex_ass[f,3], use.names = FALSE)
# 
#  frep_df[f] <- do.call(rbind.data.frame, frep)
# }
# bind_rows(frep_df)
# 
# frep <- unlist(sg_frex_ass[f,3], use.names = FALSE)




library(stm)
library(dplyr)
library(furrr)
library(ggplot2)
library(purrr)
library(tidyr)
library(glmnet)
library(splines)
library(quanteda)
library(quanteda.textstats)

load("review_1/sample_rv1/DE/k_modelDE_prev.Rdata")
load("review_1/sample_rv1/DE/heldoutDE.Rdata")
load("review_1/sample_rv1/DE/dfm_desolcv.Rdata")

plan(multicore)

k_resultDE <- k_modelDE %>%
  mutate(exclusivity = map(topic_model, exclusivity),
         semantic_coherence = map(topic_model, semanticCoherence, dfm_desolcv),
         eval_heldout = map(topic_model, eval.heldout, heldoutDE$missing),
         residual = map(topic_model, checkResiduals, dfm_desolcv),
         bound =  map_dbl(topic_model, function(x) max(x$convergence$bound)),
         lfact = map_dbl(topic_model, function(x) lfactorial(x$settings$dim$K)),
         lbound = bound + lfact,
         iterations = map_dbl(topic_model, function(x) length(x$convergence$bound)))


x <- data.frame(x=c(1,2,3),y=c(2,3,4))
write.csv(x,file="x.csv")

1+1














