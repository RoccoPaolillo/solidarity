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






# dfm ####

corpus_df <- corpus(df_finlocast[df_finlocast$country == "Italy",])
docnames(corpus_df) <-  df_finlocast[df_finlocast$country == "Italy",]$tweet_id

dfm_itsolcv <- tokens(corpus_df,
                      remove_punct = TRUE, remove_numbers = TRUE,remove_url = TRUE) %>% 
  tokens_remove(c(rem_char,
                  rem)) %>%
  dfm()
save(dfm_itsolcv,file="review_1/sample_rv1/dfm_itsolcv.Rdata")

corpus_df <- corpus(df_finlocast[df_finlocast$country == "Germany",])
docnames(corpus_df) <-  df_finlocast[df_finlocast$country == "Germany",]$tweet_id

dfm_desolcv <- tokens(corpus_df,
                      remove_punct = TRUE, remove_numbers = TRUE,remove_url = TRUE) %>% 
  tokens_remove(c(rem_char,
                  rem)) %>%
  dfm()
save(dfm_desolcv,file="review_1/sample_rv1/dfm_desolcv.Rdata")

textstat_frequency(dfm_itsolcv) %>% subset(feature %in% "national_borders")


# load("utils/dfm_sol.Rdata")

# dfm_solde <- dfm_subset(dfm_solde,country == "Germany")
# save(dfm_solde,file="utils/dfm_solde.Rdata")

# dfm_solit <- dfm_subset(dfm_sol,country == "Italy")
# save(dfm_solit,file="utils/dfm_solit.Rdata")

## topic modelling ####
# cvd <- read.xls("utils/compound.xls",sheet = "cvd", encoding = "latin1")[,1] 
# cvd <- gsub("#","",cvd)
# rem_char <- c("http*","@*","€","+","|","s","faq","=","_","__","~","___")
# load("utils/6/df_we6.Rdata")
load("review_1/sample_rv1/DE/stm_de20.Rdata")
load("review_1/sample_rv1/DE/dfm_desolcv.Rdata")
load("review_1/sample_rv1/IT/stm_it25.Rdata")
load("review_1/sample_rv1/IT/dfm_itsolcv.Rdata")

# For Germany
stm_m <- stm_de20
stm_df <- quanteda::convert(dfm_desolcv,to = "stm")  
numm <- 20

dfb <- df_finlocast %>% filter(country == "Germany")

# For Italy
stm_m <- stm_it25
stm_df <- quanteda::convert(dfm_itsolcv,to = "stm")  
numm <- 25

dfb <- df_finlocast %>% filter(country == "Italy")

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



# extract sample for testing inter-rater categorization #####
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








# Category * country #########

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

# topic modelling evaluation #####
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