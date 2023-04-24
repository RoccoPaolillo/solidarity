# uploads ####
options(scipen = 999)
library(gdata)
# for crawling Twitter data 
library(academictwitteR)
# library(rtweet)
library(corpus)
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
library("stminsights")
library("gridExtra")
library("ggrepel")  


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
# bigrams_tx  <- df_finlocast[,-3] %>% unnest_tokens(bigram, text, token = "ngrams", n = 2)
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
# write.csv(bigrams_united,"sample/bigrams_hifen.csv",row.names= F)
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


# upload data ####

load("sample/df_finlocast.Rdata")
df_finlocast$text <- str_squish(df_finlocast$text)
rem <- read.xls("sample/compound.xls",sheet = "rem", encoding = "latin1")[,1]
rem <- unique(rem)
rem <- gsub("#","",rem)
cvd <- read.xls("sample/compound.xls",sheet = "cvd", encoding = "latin1")[,1] 
cvd <- gsub("#","",cvd)
rem_char <- c("http*","@*","€","+","|","s","faq","=","_","__","~")

# for covid-only tweets ####
load("sample/tweet_cvd.Rdata")
tweet_cvd <- gsub("_","",tweet_cvd)
df_finlocast <- df_finlocast %>% filter(tweet_id %in% tweet_cvd)
# !! in word_embedding script this subsample is saved as df_we for shortness

# clean duplicates
df_finlocast$selected <- 1
df_finlocast[11919,]$selected <- 0
df_finlocast <- df_finlocast[!duplicated(df_finlocast$original_language),]
df_finlocast <- df_finlocast %>% filter(!user_username %in% c("AntonioPinna11","AssGSintini","ThierryCuisine"))
df_finlocast <- df_finlocast %>% filter(selected == 1)

# delete tweets
dl_tw <- read.xls("sample/compound.xls",sheet = "dl_tw", encoding = "latin1")[,1]
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
# df_finlocast[df_finlocast$tweet_id == "1190216177974415360",]$text <- " for a solidary district! 14 o'clock vinetaplatz vonovia expropriate usury of rents end with high rents bad apartments! end with rip-off by housing corporations displacement!"



# df_finlocast[df_finlocast$user_username == "kubakunde",]$text <- str_replace_all(df_finlocast[df_finlocast$user_username == "cubasalvavidas",]$text, "\\bcuba\\b","" )
# df_finlocast[df_finlocast$user_username == "fgbrdkuba",]$text <- str_replace_all(df_finlocast[df_finlocast$user_username == "cubasalvavidas",]$text, "\\bbrd_cuba\\b","" )


cmpd <- read.xls("sample/compound.xls",sheet = "cmpd", encoding = "latin1")[,1]
cmpd <- paste0("\\b",cmpd,"\\b")
cmpdsubstitute <- read.xls("sample/compound.xls",sheet = "cmpd", encoding = "latin1")[,2]
names(cmpdsubstitute) <- cmpd
df_finlocast$text <- str_replace_all(df_finlocast$text,cmpdsubstitute)
df_finlocast$text <- gsub("#","",df_finlocast$text)



# tokenization

df_finlocast <- df_finlocast %>% unnest_tokens(word, text, token = stringr::str_split, pattern = " ")
df_finlocast$word <- str_remove_all(df_finlocast$word, "[^#_[:^punct:]]")
df_finlocast <- df_finlocast %>% filter(!word %in% stop_words$word,
                                        !word %in% rem_char,
                                        !word %in% stopwords("en"), 
                                        !word %in% stopwords_en,
                                        !word %in% rem)
                                      #  !word %in% "^[0-9]*$")

df_finlocast <- replace(df_finlocast, df_finlocast=='', NA)
df_finlocast <- df_finlocast %>% drop_na(word)


#### Top frequencies https://www.tidytextmining.com/twitter.html ####


 frequency <- df_finlocast %>%
 count(country, monthyear, word, sort = TRUE)

# frequency <- df_finlocast %>%
#   count(country, monthyear, user_username, sort = TRUE)
# 
# a <- frequency %>% 
#   filter(user_username >= 2)
  

ggplot(frequency[frequency$word == "#solidarity",], aes(x = monthyear,y = n, color = country)) +
  geom_point(aes(color = country)) + 
  geom_line(aes(group = as.factor(country))) +
  xlab("Time (month-year)") +
  ylab("Frequency") +
  labs(color = "Country") +
  ggtitle("Term #solidarity in Twitter") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = -75, hjust = 0), legend.position = "bottom")
ggsave(file="figures/frequency.jpg",width = 12,height = 6)



# delete
# ggplot(top_word_pairsold,aes(x = monthyear, y = n)) +  geom_col() +
#   geom_text_repel(aes(x = monthyear, y = n, label = item2), vjust = -1) + 
#  # geom_text(aes(label = item2), vjust = -1) + 
#   facet_wrap(~country, dir = "v")



## tf-idf ####

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

# df_tf_idf[df_tf_idf$word == "corona",]

df_tf_idf %>%
  group_by(country, monthyear) %>%
slice_max(tf_idf, n = 1) %>%
  group_by(catlev,tf_idf) %>%
  mutate(labelling = paste0(word,collapse = ", ")) %>%
# df_tf_idf %>%
#   group_by(country, monthyear) %>%
#   slice_max(tf_idf, n = 1) 
  ungroup() %>%
ggplot(aes(x = monthyear,y = tf_idf)) + 
  geom_point(color = "white") +
  geom_segment( aes(x=monthyear, xend=monthyear, y=0, yend=tf_idf,
                    color = as.factor(quadrimester)), show.legend = FALSE) +
  scale_color_manual(breaks = c(1,2,3,4), values = c("red","green","blue","purple")) + 
  geom_text(aes(label = labelling)) +
  xlab("Time (month-year") + 
  ylab("tf-idf score") + 
  coord_flip() +
  scale_x_discrete(limits=rev) +
  facet_wrap(~ country, dir = "v", scales = "free") +
  theme_bw() 
ggsave("figures/tf_idf.jpg", width = 10,  height = 9)

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
  filter(! word %in% c(cvd,"alongside")) %>%
# filter(monthyear == "04-2020") %>%
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
  theme_bw() 
 ggsave(file="figures/keyness_cvd.jpg",width = 10, height = 9)


# dfm ####
 
 corpus_df <- corpus(df_finlocast)
 docnames(corpus_df) <- df_finlocast$tweet_id
 
 dfm_solbothcv <- tokens(corpus_df,
                     remove_punct = TRUE, remove_numbers = TRUE,remove_url = TRUE) %>% 
   tokens_remove(c(rem_char,
                   rem)) %>%
   dfm()
 save(dfm_solbothcv,file="sample/dfm_solbothcv.Rdata")

# load("sample/dfm_sol.Rdata")

# dfm_solde <- dfm_subset(dfm_solde,country == "Germany")
 save(dfm_solde,file="sample/dfm_solde.Rdata")
 
# dfm_solit <- dfm_subset(dfm_sol,country == "Italy")
 save(dfm_solit,file="sample/dfm_solit.Rdata")
 
## topic modelling ####
 load("sample/df_we.Rdata")
 load("sample/DE_1/cvd/DE_25/stm_de25cv.Rdata")
 load("sample/DE_1/cvd/dfm_soldecv.Rdata")
 stm_m <- stm_de25
 stm_df <- quanteda::convert(dfm_soldecv,to = "stm")  
 numm <- 25
 
 dfb <- df_we %>% filter(country == "Germany")
 
 sg <- labelTopics(stm_m,1:numm,15)
 sg_prob <- tibble(topic = 1:numm,sg$prob) # marginal probability
 sg_frex <- tibble(topic = 1:numm,sg$frex) # marginal frex
 # 
 # t2 <- findThoughts(stm_m, texts = dfb$text,n = 3, topics =2)
 # n <- 548
 # txx <-  print(c(paste0("ACTsj: ", dfb[n,]$user_username," TXT: ",dfb[n,]$text," DATE: ", dfb[n,]$created_at,
 #                        " TWID: ",dfb[n,]$tweet_id," CNID ",dfb[n,]$conversation_id)))
 # 
 
 thoughts <- list()
 for (i in 1:numm){ # 
   # thg_det <- findThoughts(stm_m, texts = df_de$text,n = 3, topics =i)$docs[[1]]
   thought_text = list()
   dates <- findThoughts(stm_m, texts = dfb$text,n = 10, topics =i)$index[[1]] # 
   for (n in dates) {
     txx <-  print(c(paste0(" ACT: ", dfb[n,]$country,
                            " ACTsj: ", dfb[n,]$user_username," TXT: ",dfb[n,]$text," DATE: ", dfb[n,]$created_at,
                            " TWID: ",dfb[n,]$tweet_id," CNID ",dfb[n,]$conversation_id)))
     thought_text[[n]] <- txx
     thought_textfin <- do.call(rbind.data.frame, thought_text)
   }
   thoughts[[i]] <- thought_textfin
   
 }
 bind_rows(thoughts)
 thoughts <- do.call(rbind.data.frame, lapply(thoughts,'[[',1))
 colnames(thoughts) = c("1", "2","3","4","5","6","7","8","9","10") # columns where texts go
 thoughts <- cbind(topic = 1:numm,thoughts)
 
 report <- cbind(sg_prob,sg_frex,thoughts)
 write.csv(report,file= "sample/DE_1/cvd/DE_25/report25.csv",row.names = F,  fileEncoding = "UTF-8")

 # topic proportion
 
 top_terms <- tidy(stm_m) %>%
   arrange(beta) %>%
   group_by(topic) %>%
   filter(! term %in% c(cvd,"solidarity")) %>%   # to filter out either it_policies or de_policies
   top_n(7, beta) %>%
   arrange(-beta)%>%
   select(topic, term) %>%
   summarise(terms = list(unique(term))) %>%
   mutate(terms = map(terms, paste, collapse = ", ")) %>% 
   unnest()
 

td_gamma <- tidy(stm_m, matrix = "gamma")
ID_row <- names(stm_df$documents) # the name of documents gets lost, the row number is reported
td_gamma <- cbind(td_gamma,ID_row) # Here I map each document to its name via row, I checked with content, it works
td_gamma <- cbind(td_gamma,dfb) # merge the gamma matrix with the dataframe via ID, so the variables to sort documents can be used


gamma_terms <- td_gamma %>%
  group_by(topic) %>%
  summarise(gamma = mean(gamma)) %>%
  arrange(desc(gamma)) %>%
 left_join(top_terms, by = "topic") %>%
  mutate(topic = paste0("Topic ", topic),
         topic = reorder(topic, gamma)) 
 
gamma_terms %>%
  # top_n(20, gamma) %>%
  ggplot(aes(topic, gamma, label = terms, fill = topic)) +
  geom_col(show.legend = FALSE) +
  geom_text(hjust = 0, y = 0.001,# nudge_y = 0.00005, size = 5, # 0.0005
            family = "IBMPlexSans") +
  coord_flip() +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, 0.10) , 
                     labels = scales::percent_format()) +
  theme_tufte(base_family = "IBMPlexSans", ticks = FALSE) +
  theme(plot.title = element_text(size = 20,
                                  family="IBMPlexSans-Bold"),
        plot.subtitle = element_text(size = 13),
        axis.text.y = element_text(size = 15)) +
  labs(x = NULL, y = expression(gamma))
ggsave(file="sample/DE_1/cvd/DE_25/topic_proportionDE25.jpg",width = 14, height = 12)
 

# combined plots 

gamma_terms_DE25$macro <- "xxx"
gamma_terms_DE25$label <- "xxx"
gamma_terms_DE25[gamma_terms_DE25$topic == "Topic 14",]$label <- "schooling"
gamma_terms_DE25[gamma_terms_DE25$topic == "Topic 22",]$label <- "social_cohesion"
gamma_terms_DE25[gamma_terms_DE25$topic == "Topic 5",]$label <- "europe"
gamma_terms_DE25[gamma_terms_DE25$topic == "Topic 16",]$label <- "vaccination"
gamma_terms_DE25[gamma_terms_DE25$topic == "Topic 18",]$label <- "children"
gamma_terms_DE25[gamma_terms_DE25$topic == "Topic 8",]$label <- "culture_events"
gamma_terms_DE25[gamma_terms_DE25$topic == "Topic 17",]$label <- "intensive_care"
gamma_terms_DE25[gamma_terms_DE25$topic == "Topic 15",]$label <- "stay_home"
gamma_terms_DE25[gamma_terms_DE25$topic == "Topic 9",]$label <- "leibniz"
gamma_terms_DE25[gamma_terms_DE25$topic == "Topic 20",]$label <- "economy"
 
 
 
 # Combine 2 plots
 
 tidystm <- tidy(stm_m)
 # tidystm <- rename(tidystm, actor = y.level)
 
 prep <- estimateEffect(1:numm ~ s(datenum), stm_m, metadata = stm_df$meta, uncertainty = "Global")
 
 effects_int <- get_effects(estimates = prep,
                            variable = 'datenum',
                            type = 'continuous'  )  
 
 for (i in 1:numm) {
   
 
tm <- effects_int %>%  filter(topic == i) %>%
   ggplot(aes(x = value, y = proportion )) + geom_line() + scale_x_continuous(breaks = c(18262,18414,18627,18809,18992),
                      labels = c("18262" = "JAN 2020","18414" = "JUN 2020","18627" = "JAN 2021","18809" = "JUN 2021",
                                 "18992" = "DEC 2021")) +
  ggtitle(paste( "Topic: ",i)) + 
  ylab("Expected Proportion") +
  xlab("Time") +
  theme_bw() 
   
wd <- tidystm %>% filter(topic == i) %>%
filter(term %nin% c("solidarity",cvd)) %>%
  arrange(-beta) %>%
  top_n(8,beta) %>%
  ggplot(aes(reorder(term,beta),beta)) +
  geom_col(show.legend = FALSE) +
  scale_y_continuous(label = scales::percent ) +
  coord_flip() +
  xlab("") +
  ylab("Probability words per topic") +
  theme(axis.title.x = element_blank()) +
  theme_bw() 
 
cm <- grid.arrange(tm,wd,ncol = 2)
ggsave(cm,file = paste0("sample/DE_1/cvd/","DE_1_30","_",i,".jpg"),width = 14, height = 3.5)
 
 }
 
 # topicCorr
 
 
 library(stm)
 library(ggraph)
 library(quanteda)
 
 stm_corrs <- get_network(model = stm_m,
                          method = 'simple',
                          labels = paste('Topic', 1:numm),
                          cutoff = 0.001,
                          cutiso = TRUE)
 
 stm_corrs <- get_network(model = stm_corrs,
                          method = 'simple',
                          labels = paste('Topic', 1:numm),
                          cutoff = 0.001,
                          cutiso = TRUE)
 
 ggraph(stm_corrs, layout = 'fr') +
   geom_edge_link(
     aes(edge_width = weight),
     label_colour = '#fc8d62',
     edge_colour = '#377eb8') +
   geom_node_point(size = 4, colour = 'black')  +
   geom_node_label(
     aes(label = name, size = props),
     colour = 'black',  repel = TRUE, alpha = 0.85) +
   scale_size(range = c(2, 10), labels = scales::percent) +
   labs(size = 'Topic Proportion',  edge_width = 'Topic Correlation') +
   scale_edge_width(range = c(1, 3)) +
   theme_graph()
#
 
tidy_stm <- tidy(stm_m)

kn_stm <- tidy_stm %>% filter(term %in% top_keynessDE) %>% group_by(term) %>% slice_max(n = 2, beta)
 
 
 
 
 

# co-occurrences #### 
 library(igraph)
 library(ggraph)
 
 load("sample/df_we.Rdata")

 top_keynessDE <- c("leavenoonebehind","flattenthecurve","demands","vaccinated","berlin","stayathome","soldary","lived","vaccination",
                  "spd","federal_government","bundestag","1mai","moria")
 
 top_keynessIT <- c("iorestoacasa","rome","digital_solidarity","groceries","naples","lazio","third_sector",
                    "papafrancesco","fundraiser","generosity","mln_euros","milan","health_emergency","donates")

# df_finlocast %>% unnest_tokens(word, text, token = stringr::str_split, pattern = " ")
 
 skip_df <- df_we %>% filter(country == "Germany") %>%
   # unnest_tokens(ngram, text, token = stringr::str_split, pattern = " ")   %>%
    unnest_tokens(ngram, text, token = "ngrams", n = 5) %>%
   mutate(ngramID = row_number()) %>% 
   tidyr::unite(skipgramID, tweet_id, ngramID) %>%
   unnest_tokens(word, ngram) %>%
   filter(!word %in% stop_words$word,
          !word %in% rem_char,
          !word %in% stopwords("en"), 
          !word %in% stopwords_en,
          !word %in% rem)
 
 
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
 skipgram_probs <- skip_df %>%
   pairwise_count(word, skipgramID, diag = FALSE, sort = TRUE) %>%
   mutate(p = n / sum(n)) 

 
 a <- skipgram_probs %>% filter(item1 %in% top_keynessDE)
 a <- a %>% filter(! item1 == item2)
 b <- skipgram_probs %>% filter(item1 %in% a$item2)
 b <- b %>% filter(! item2 %in% a$item1)
 
 d <- rbind(a,b)

# plot co-occurrences
 
a %>% filter(n >= 8) %>% graph_from_data_frame() %>%
   ggraph(layout = "fr") +
   geom_edge_link(aes(edge_alpha = n)) + 
  # geom_node_point(size = 5) +
    geom_node_text(aes(label = name), repel = TRUE, 
                   point.padding = unit(0.2, "lines")) +
   theme_void()
 
 plot(occit)
 ggsave(occit,file="occit.jpg",width =10,height=10)
 
 
 
 