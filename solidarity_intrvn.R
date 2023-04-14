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

"%nin%" <- Negate("%in%")

setwd("C:/Users/rocpa/OneDrive/Desktop/CNT/solidarity_server/")
path <- list.files("translation/csvv/")
# Dataframe processing and dfm creation (lists for processing in keywords_cnt.xls)

for (i in path) {
  load(paste0("sample/orig_df/",i))
}

df1921 <- rbind(de2019solidaritat,de2019solidaritaet,it2019solidarietà,de2020solidaritaet,de2020solidaritat,it2020solidarietà,
                de2021solidaritat,de2021solidaritaet,it2021solidarietà)

save(df1921,file="df1921.Rdata")

load("sample/df1921.Rdata")
df1921 <- df1921[!duplicated(df1921$tweet_id),]

df1921 <- df1921 %>% filter((sourcetweet_type != "retweeted") %>% replace_na(TRUE))

ast <- df1921 %>% filter(str_detect(text,regex("solidar"))) 
ast <- df1921 %>% filter(lang == "en" | lang == "de" | lang == "it")

# save(ast, file="sample/ast1921_filt.Rdata")
# load("sample/ast1921_filt.Rdata")

ast$country <- "xxx"
ast[ast$tweet_id %in% de2019solidaritaet$tweet_id,]$country <- "Germany"
ast[ast$tweet_id %in% de2019solidaritat$tweet_id,]$country <- "Germany"
ast[ast$tweet_id %in% de2020solidaritaet$tweet_id,]$country <- "Germany"
ast[ast$tweet_id %in% de2020solidaritat$tweet_id,]$country <- "Germany"
ast[ast$tweet_id %in% de2021solidaritaet$tweet_id,]$country <- "Germany"
ast[ast$tweet_id %in% de2021solidaritat$tweet_id,]$country <- "Germany"
ast[ast$tweet_id %in% it2019solidarietà$tweet_id,]$country <- "Italy"
ast[ast$tweet_id %in% it2020solidarietà$tweet_id,]$country <- "Italy"
ast[ast$tweet_id %in% it2021solidarietà$tweet_id,]$country <- "Italy"

ast$date <- as_date(ast$created_at)
ast$datenum <- as.integer(ast$date)
ast$dateyear <- format(as.Date(ast$date, format = "%d/%m/%Y"), "%Y")
ast$dateyear <- as.numeric(ast$dateyear)



spc <- pull(read.csv("special_characters.csv", encoding = "UTF-8"),1) # delete emoticon, keep ASCII
spct <- paste(spc,collapse="")
chrmt <- paste0("[^\x01-\x7F",spct,"]")
# cleaning apostrophes, @, https etc.
ast$text <- tolower(ast$text)
ast$text <- gsub("-","_",ast$text) # for keyword search
# ast$text <- gsub("#"," ",ast$text) # hashtags read as words, not deleted
ast$text <- gsub(chrmt," ", ast$text)  # for not ASCII (emoticon), keeps normal characthers + spct list
ast$text <- gsub("&amp;", " ", ast$text) # remove how ";" is translated
ast$text <- gsub("&gt;", " ", ast$text)
ast$text <- gsub("&lt;", " ", ast$text)
ast$text <- gsub("&ct;", " ", ast$text)
ast$text <- gsub("&le;", " ", ast$text)
ast$text <- gsub("&ge;", " ", ast$text)
ast$text <- gsub("\\s+", " ", ast$text)
ast$text <- gsub("\n", " ",  ast$text)
ast$text <- gsub("\t", " ",  ast$text)
ast$text <- gsub("'", " ",  ast$text)
ast$text <- gsub("’", " ",  ast$text)
ast$text <- str_replace_all(ast$text,
                                regex("http[s]?:/[/]?(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+",
                                      ignore_case = TRUE)," ") # regex to identify websites, we don't need
# ast$text <- str_replace_all(ast$text, regex("(?<=^|\\s)@[^\\s]+",
#                                                     ignore_case=TRUE)," ") # regex to delete mentions @
# ast$text <- str_replace_all(ast$text, regex("(?<=^|\\s).@[^\\s]+",ignore_case=TRUE)," ")
# df_act20$text <- str_replace_all(df_act20$text,   regex("[a-zA-Z]*@[a-zA-Z]*",ignore_case=TRUE)," ") # regex to delete for .@ (didn't manage to merge with above)
ast[ast$lang =="it",]$text <- gsub("á","à",   ast[ast$lang =="it",]$text)
ast[ast$lang =="it",]$text <- gsub("é","è",   ast[ast$lang =="it",]$text)
ast[ast$lang =="it",]$text <- gsub( "í","ì",  ast[ast$lang =="it",]$text)
ast[ast$lang =="it",]$text <- gsub( "ó","ò",  ast[ast$lang =="it",]$text)
ast[ast$lang =="it",]$text <- gsub( "ú","ù",  ast[ast$lang =="it",]$text)
ast[ast$lang =="it",]$text <- gsub( "ú","ù",  ast[ast$lang =="it",]$text)

ast$text <- str_squish(ast$text)

ast$text <- gsub("solidaritaet","solidarität",ast$text)
ast$text <- gsub("solidarieta","solidarietà",ast$text)

corpus_ast <- corpus(ast)
docnames(corpus_ast) <- ast$tweet_id
dfm_ast <- tokens(corpus_ast) %>% dfm()

save(corpus_ast,file="sample/corpus_ast.Rdata")
save(dfm_ast,file="sample/dfm_ast.Rdata")

# ast_sim <- textstat_simil(dfm_ast, method="jaccard",margin="documents",min_simil = 0.85)

# ast2 <- ast %>% filter(!str_detect(text,"#solidarity|#solidaritaet|#solidarität|#solidarietà|#solidarità")) 

load("sample/sim_dflDE.Rdata")

sim_dflDE <- as.list(sim_dflDE,diag=FALSE)
load("sample/sim_dflDElist.Rdata")

results_sim = list()
for (i in names(sim_dflDElist)) {
  txt <- dfl_de[dfl_de$tweet_id == i,]$text
  id <- dfl_de[dfl_de$tweet_id == i,]$tweet_id
  usrn <- dfl_de[dfl_de$tweet_id == i,]$user_username
  b <- tibble(name = names(sim_dflDElist[i]),id = id, usrn = usrn, double = length(sim_dflDElist[[i]]),
              text = txt)
  results_sim[[i]] = b
}

dfres <- bind_rows(results_sim, .id = "name")

load("sample/dfresIT.Rdata")

dfresIT <- dfresIT[!duplicated(dfresIT$text),]
write.csv(dfresIT,file="sample/dfresITnast.csv",sep=",", row.names = F)




dfresdelete <- dfres %>% filter(id %nin% doublecheck[doublecheck$select == 1,]$id)

ast2 <- ast %>% filter(tweet_id %nin% dfresdelete$id)

load("sample/ast_final.Rdata")

# ast2$country <- "xxx"
# ast2[ast2$tweet_id %in% de2019solidaritaet$tweet_id,]$country <- "Germany"
# ast2[ast2$tweet_id %in% de2019solidaritat$tweet_id,]$country <- "Germany"
# ast2[ast2$tweet_id %in% de2020solidaritaet$tweet_id,]$country <- "Germany"
# ast2[ast2$tweet_id %in% de2020solidaritat$tweet_id,]$country <- "Germany"
# ast2[ast2$tweet_id %in% de2021solidaritaet$tweet_id,]$country <- "Germany"
# ast2[ast2$tweet_id %in% de2021solidaritat$tweet_id,]$country <- "Germany"
# ast2[ast2$tweet_id %in% it2019solidarietà$tweet_id,]$country <- "Italy"
# ast2[ast2$tweet_id %in% it2020solidarietà$tweet_id,]$country <- "Italy"
# ast2[ast2$tweet_id %in% it2021solidarietà$tweet_id,]$country <- "Italy"
# 
# ast2$date <- as_date(ast2$created_at)
# ast2$datenum <- as.integer(ast2$date)
# ast2$dateyear <- format(as.Date(ast2$date, format = "%d/%m/%Y"), "%Y")
# ast2$dateyear <- as.numeric(ast2$dateyear)

# save(ast2,file="sample/ast_final.Rdata")
# Germany
location_de <- read.xls("sample/location.xls",sheet = "location_de",encoding = "latin1")[,1]
location_de <-  gsub("\\s*\\([^\\)]+\\)", "", location_de)
location_de <- str_trim(location_de, side = "right")
location_de <- tolower(location_de)
location_de <- paste(location_de,collapse = "|")

dfl$user_location <- tolower(dfl$user_location)
nolocde <- dfl %>% filter(country == "Germany") %>% filter(! str_detect(user_location,location_de))
locde <- dfl %>% filter(country == "Germany") %>% filter( str_detect(user_location,location_de))
save(locde,file="sample/ast_finDE.Rdata")

# Italy
location_it <- read.xls("sample/location.xls",sheet = "location_it",encoding = "latin1")[,1]
location_it <-  gsub("\\s*\\([^\\)]+\\)", "", location_it)
location_it <- str_trim(location_it, side = "right")
location_it <- tolower(location_it)
location_it <- paste(location_it,collapse = "|")

dfl$user_location <- tolower(dfl$user_location)
nolocit <- dfl %>% filter(country == "Italy") %>% filter(! str_detect(user_location,location_it))
locit <- dfl %>% filter(country == "Italy") %>% filter( str_detect(user_location,location_it))
save(locit,file="sample/ast_finIT.Rdata")


nolocitlist <- unique(nolocit$user_location)
write.csv(nolocitlist,file="sample/nolocitlist.csv",row.names = F)

load("sample/ast_finIT.Rdata")
load("sample/ast_finDE.Rdata")

locde$countwrd <- str_count(locde$text, "\\S+") 
locit$countwrd <- str_count(locit$text, "\\S+") 

locit <- locit %>% filter(countwrd >= 15)
locde <- locde %>% filter(countwrd >= 15)

dfl_de <- locde
dfl_it <- locit

dfl_finloc <- rbind(dfl_de,dfl_it)

save(dfl_de,file="sample/dfl_nastDE.Rdata")
save(dfl_it,file="sample/dfl_nastIT.Rdata")

save(dfl_finloc,file="sample/dfl_finloc.Rdata")

load("sample/sim_dflDElist.Rdata")

load("sample/dfresDEfull.Rdata")
load("sample/dfresITfull.Rdata")

dfresfiltDE <- read.csv("sample/checksimDEnast.csv",sep =";")
dfresfiltDE$name <- gsub("_","",dfresfiltDE$name)

dfresfiltIT <- read.csv("sample/dfresITnast.csv",sep =";")
dfresfiltIT$name <- gsub("_","",dfresfiltIT$name)

dfresDEfull <- dfresDEfull %>% filter(name %nin% dfresfiltDE[dfresfiltDE$select == 1,]$name)
dfresITfull <- dfresITfull %>% filter(name %nin% dfresfiltIT[dfresfiltIT$select == 1,]$name)

dfresout <- rbind(dfresDEfull,dfresITfull)

dfl_finloc <- dfl_finloc %>% filter(tweet_id %nin% dfresout$name)
save(dfl_finloc,file="sample/dfl_finloc.Rdata")

load("sample/dfl_finloc.Rdata")

dfl_finloc$text <- str_replace_all(dfl_finloc$text, regex("(?<=^|\\s)@[^\\s]+",
                                            ignore_case=TRUE)," ") # regex to delete mentions @
dfl_finloc$text <- str_replace_all(dfl_finloc$text, regex("(?<=^|\\s).@[^\\s]+",ignore_case=TRUE)," ")
dfl_finloc$text <- str_replace_all(dfl_finloc$text,   regex("[a-zA-Z]*@[a-zA-Z]*",ignore_case=TRUE)," ") # regex to delete for .@ (didn't manage to merge with above)

dfl_finlocast <- dfl_finloc %>% filter(str_detect(text,regex("#solidar"))) 
# dfl_finlocast <- dfl_finlocast %>% filter(lang != "en")

dfl_finlocastIT19 <- dfl_finlocast[dfl_finlocast$country == "Italy" & dfl_finlocast$lang == "it" & dfl_finlocast$dateyear == 2019,]
dfl_finlocastIT_de <- dfl_finlocastIT_de %>% select(tweet_id,text)
dfl_finlocastIT_de$tweet_id <- paste0("_",dfl_finlocastIT_de$tweet_id,"_")
save(dfl_finlocastIT_de,file="sample/dfl_finlocastIT_de.Rdata")

write.csv(dfl_finlocastDE20,file="sample/dfl_finlocastDE20.csv",sep=";",row.names = F)

dfl_finlocastIT <- dfl_finlocast[dfl_finlocast$country == "Italy",]

save(dfl_finlocastDE,file="sample/dfl_finlocastDE.Rdata")
save(dfl_finlocastIT,file="sample/dfl_finlocastIT.Rdata")
dfl_finlocastIT <- dfl_finlocastIT %>% select(tweet_id,text)
dfl_finlocastIT$tweet_id <- paste0("_",dfl_finlocastIT$tweet_id,"_")
save(dfl_finlocastIT,file="sample/dfl_finlocastIT_trsl.Rdata")

write.csv(dfl_finlocastDE,file="sample/dfl_finlocastDE_trsl.csv",sep=";",row.names = F)

dfl_trsl <- dfl_finloc %>% filter(dateyear == 2021 & country == "Italy")
dfl_trsl <- dfl_trsl %>% select(tweet_id,text)
dfl_trsl$tweet_id <- paste0("_",dfl_trsl$tweet_id,"_")

save(dfl_trsl,file="sample/dfl_trslIT2021.Rdata")

load("dfl_trsl2020.Rdata")
write.csv(dfl_trsl,file="sample/dfl_trsl2020.csv",sep=",",row.names = F)

library(deeplr)
library(devtools)

devtools::install_github("paulcbauer/deeplr")
key <- "23edbe5a-cbb3-ef41-acac-9ea075ee6f90"

available_languages(auth_key=key)

detect2("help",auth_key = "23edbe5a-cbb3-ef41-acac-9ea075ee6f90")

library(sjPlot)
sjPlot::tab_df(dfl_finlocastDE, file = "output.doc")

translate("Hallo Welt!", target_lang = "EN", auth_key = "23edbe5a-cbb3-ef41-acac-9ea075ee6f90")

DE19_1 <- read.csv2("translation/csvv/dfl_finlocastDE19_1.csv",sep =";", encoding = "latin1")
DE19_1 <- DE19_1[1:3000,]
DE19_2 <- read.csv2("translation/csvv/dfl_finlocastDE19_2.csv",sep =";", encoding = "UTF-8")
DE19_2 <- DE19_2[1:2184,]
DE20_1 <- read.csv2("translation/csvv/dfl_finlocastDE20_1.csv",sep =";", encoding = "UTF-8")
DE20_2 <- read.csv2("translation/csvv/dfl_finlocastDE20_2.csv",sep =";", encoding = "UTF-8")
DE20_3 <- read.csv2("translation/csvv/dfl_finlocastDE20_3.csv",sep =";", encoding = "UTF-8")
DE20_4 <- read.csv2("translation/csvv/dfl_finlocastDE20_4.csv",sep =";", encoding = "UTF-8")
DE21_1 <- read.csv2("translation/csvv/dfl_finlocastDE21_1.csv",sep =",", encoding = "UTF-8")
DE21_2 <- read.csv2("translation/csvv/dfl_finlocastDE21_2.csv",sep =";", encoding = "UTF-8")
DE21_3 <- read.csv2("translation/csvv/dfl_finlocastDE21_3.csv",sep =";", encoding = "UTF-8")
DE21_3 <- DE21_3[1:1555,]
IT_de <- read.csv2("translation/csvv/dfl_finlocastIT_de.csv",sep =";", encoding = "UTF-8")
IT19_1 <- read.csv2("translation/csvv/dfl_finlocastIT19_1.csv",sep =";", encoding = "UTF-8")
IT19_1 <- IT19_1[1:4366,]
IT20_1 <- read.csv2("translation/csvv/dfl_finlocastIT20_1.csv",sep =";", encoding = "UTF-8")
IT20_1 <- IT20_1[1:4500,]
IT20_2 <- read.csv2("translation/csvv/dfl_finlocastIT20_2.csv",sep =";", encoding = "UTF-8")
IT21_1 <- read.csv2("translation/csvv/dfl_finlocastIT21_1.csv",sep =";", encoding = "UTF-8")
IT21_1 <- IT21_1[1:3151,]

df_finlocastDEtrl <- rbind(DE19_1,DE19_2,DE20_1,DE20_2,DE20_3,DE20_4,DE21_1,DE21_2,DE21_3)
df_finlocastDEtrl <- df_finlocastDEtrl[!duplicated(df_finlocastDEtrl$tweet_id),]
df_finlocastITtrl <- rbind(IT_de,IT19_1,IT20_1,IT20_2,IT21_1)

load("translation/df_finlocast.Rdata")
df_finlocasttrl <- rbind(df_finlocastDEtrl,df_finlocastITtrl)

df_finlocasttrl$tweet_id <- gsub("_","",df_finlocasttrl$tweet_id)
save(df_finlocasttrl, file="translation/csvv/df_finlocasttrl.Rdata")

df_finlocast <- df_finlocast[,-36]
df_finlocast <- merge(df_finlocast,df_finlocasttrl,by = "tweet_id")
load("sample/df_finlocast.Rdata")

df_finlocast$text <- str_replace_all(df_finlocast$text,"\\b's\\b","") 
df_finlocast$text <- str_replace_all(df_finlocast$text,"\\b-\\b","_") 

save(df_finlocast,file="sample/df_finlocast.Rdata")

####

load("translation/dfl_finlocastDE_trl.Rdata")
load("translation/dfl_finlocastIT_trl.Rdata")

df_finlocast <- rbind(dfl_finlocastDE,dfl_finlocastIT)


bigrams_tx  <- df_finlocast[,-3] %>% unnest_tokens(bigram, text, token = "ngrams", n = 2)
bigrams_tx  %>% dplyr::count(bigram, sort = TRUE)
bigrams_separate  <- bigrams_tx  %>% separate(bigram,c("word1","word2"),sep=" ")
bigrams_filtered  <- bigrams_separate  %>%
  filter(!word1 %in% stopwords_en) %>%
  filter(!word2 %in% stopwords_en)
bigrams_filtered <- bigrams_filtered  %>%  dplyr::count(word1, word2, sort = TRUE)
bigrams_united <- bigrams_filtered  %>% unite(bigram, word1, word2, sep = " ")
# bigrams_united  <- bigrams_united$bigram
bigrams_united <- unique(bigrams_united)

write.csv(bigrams_united,"sample/bigrams_hifen.csv",row.names= F)

trigrams_tx <- df_finlocast[,-3] %>%  unnest_tokens(trigram, text, token = "ngrams", n = 3)
trigrams_tx %>% dplyr::count(trigram, sort = TRUE)
trigrams_separate  <- trigrams_tx %>% separate(trigram,c("word1","word2","word3"),sep=" ")
trigrams_filtered <- trigrams_separate  %>%
  filter(!word1 %in% stopwords_en) %>%
  filter(!word2 %in% stopwords_en) %>%
  filter(!word3 %in% stopwords_en)
trigrams_filtered  <- trigrams_filtered %>%  dplyr::count(word1, word2,word3, sort = TRUE)
trigrams_united  <- trigrams_filtered  %>% unite(trigram, word1, word2,word3, sep = " ")
# trigrams_united <- trigrams_united$trigram
trigrams_united <- unique(trigrams_united)
# 
write.csv(trigrams_united,"sample/trigrams_hifen.csv",row.names= F)

quadrigrams_tx  <- df_finlocast[,-3] %>%  unnest_tokens(quadrigram, text, token = "ngrams", n = 4)
quadrigrams_tx  %>% dplyr::count(quadrigram, sort = TRUE)
quadrigrams_separate  <- quadrigrams_tx  %>% separate(quadrigram,c("word1","word2","word3","word4"),sep=" ")
quadrigrams_filtered  <- quadrigrams_separate  %>%
  filter(!word1 %in% stopwords_en) %>%
  filter(word2 == "and") %>%
  filter(!word3 %in% stopwords_en) %>%
  filter(!word4 %in% stopwords_en)
quadrigrams_filtered <- quadrigrams_filtered  %>%  dplyr::count(word1, word2,word3,word4, sort = TRUE)
quadrigrams_united <- quadrigrams_filtered  %>% unite(quadrigrams, word1, word2,word3,word4, sep = " ") 

write.csv(quadrigrams_united,"sample/quadrigrams_and_hifen.csv",row.names= F)


bg <- read.csv("sample/bigrams.csv",sep=";")
bg <- bg %>% filter(select == 1)

bg_hifen <- read.csv("sample/bigrams_hifen.csv",sep = ",")

a <- bg %>% filter(bigram %nin% bg_hifen$bigram)

#### Preprocessing dfm
# df_finlocast$monthyear <- format(as.Date(df_finlocast$date, format = "%d/%m/%Y"), "%m-%Y")
# df_finlocast$monthyear <- as.factor(df_finlocast$monthyear)
# mth <- c("01","02","03","04","05","06","07","08","09","10","11","12")
# yr <- c(rep(2019,12),rep(2020,12),rep(2021,12))
# lvl <- paste0(mth,"-",yr)
# df_finlocast$monthyear <- factor(df_finlocast$monthyear, levels= lvl)
# save(df_finlocast,file="sample/df_finlocast.Rdata")

# quadrimester

# df_finlocast$quadrimester <- lubridate::quarter(df_finlocast$created_at)
# save(df_finlocast,file="sample/df_finlocast.Rdata")


load("sample/df_finlocast.Rdata")
df_finlocast$selected <- 1
df_finlocast[11919,]$selected <- 0
df_finlocast <- df_finlocast[!duplicated(df_finlocast$original_language),]
df_finlocast <- df_finlocast %>% filter(user_username != "AntonioPinna11")
df_finlocast <- df_finlocast %>% filter(selected == 1)
#
df_finlocast <- df_finlocast %>% filter(tweet_id != "1300053606247337984")
df_finlocast[df_finlocast$tweet_id == "1324995953661718528",]$text <- gsub("_"," ",df_finlocast[df_finlocast$tweet_id == "1324995953661718528",]$text)
# df_finlocast <- df_finlocast %>% filter(tweet_id != "1253757045616107521")

# possible delete
# 1092581145907220480
# 1092794141543673859 1095001413413941248 1181947460933042178


#

cmpd <- read.xls("sample/compound.xls",sheet = "cmpd", encoding = "latin1")[,1]
cmpd <- paste0("\\b",cmpd,"\\b")
cmpdsubstitute <- read.xls("sample/compound.xls",sheet = "cmpd", encoding = "latin1")[,2]
names(cmpdsubstitute) <- cmpd
df_finlocast$text <- str_replace_all(df_finlocast$text,cmpdsubstitute)
df_finlocast$text <- gsub("#","",df_finlocast$text)

rem <- read.xls("sample/compound.xls",sheet = "rem", encoding = "latin1")[,1]
cvd <- read.xls("sample/compound.xls",sheet = "cvd", encoding = "latin1")[,1] 
cvd <- gsub("#","",cvd)

# tokenization
df_finlocast <- df_finlocast %>% unnest_tokens(word, text, token = stringr::str_split, pattern = " ")
df_finlocast$word <- str_remove_all(df_finlocast$word, "[^#_[:^punct:]]")
df_finlocast <- df_finlocast %>% filter(!word %in% stop_words$word,
                                        !word %in% c("http*","@*","€","+","|","s","faq","="),
                                        !word %in% stopwords("en"), 
                                        !word %in% stopwords_en,
                                        !word %in% rem)

# dataframe for word embedding
dfcvd <- df_finlocast %>% filter(word %in% cvd)
tweet_cvd <- dfcvd$tweet_id
tweet_cvd <- unique(unlist(tweet_cvd))

df_we <- df_finlocast %>% filter(tweet_id %in% tweet_cvd)
save(df_we,file="sample/df_we.Rdata") 
##


# df <- df_finlocast %>% unnest_tokens(word,text,token = "tweets")
cvd <- df_finlocast %>% filter(str_detect(word,regex("corona|covid*|#corona|#covid|sars|pandemic")))
cvd <- unique(cvd$word)
cvd <- unlist(cvd)
write.csv(cvd,file="sample/cvd.csv",row.names = F)


# save(df_finlocast,file="sample/df_finlocast.Rdata")

dfm_cast3 <- tokens(corpus(df_finlocast),
            remove_punct = TRUE, remove_numbers = TRUE,remove_url = TRUE) %>% 
   tokens_remove(c("http*","@*","€","+","|","s","faq","=",stopwords("en"), 
                   stopwords_en,stop_words$word,rem)) %>%
  dfm()

save(dfm_cast3,file="sample/dfm_cast3.Rdata")
load("sample/dfm_cast.Rdata")


#### Top frequencies https://www.tidytextmining.com/twitter.html MUST BE ALL TOGETHER

df <- df_finlocast %>%
  unnest_tokens(word, text, token = "tweets") %>%
  filter(!word %in% stop_words$word,
         !word %in% str_remove_all(stop_words$word, "'")
                    )
        

frequency <- df %>% 
  count(country, monthyear, word, sort = TRUE) %>% 
  left_join(df %>% 
              count(country,monthyear, name = "total")) %>%
  mutate(freq = n/total)

library(tidyr)

frequency <- frequency %>% 
  select(country, word, freq) %>% 
  pivot_wider(names_from = country, values_from = freq) %>%
  arrange(Germany, Italy)

library(scales)

ggplot(frequency, aes(Germany, Italy)) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.25, height = 0.25) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  geom_abline(color = "red")

# Keyness odds ratio

# word_ratios <- df %>%
# #  filter(!str_detect(word, "^@")) %>%
#   count(word, country) %>%
#   group_by(word) %>%
#  # filter(sum(n) >= 10) %>%
#   ungroup() %>%
#   pivot_wider(names_from = country, values_from = n, values_fill = 0) %>%
#   mutate_if(is.numeric, list(~(. + 1) / (sum(.) + 1))) %>%
#   mutate(logratio = log(Germany / Italy)) %>%
#   arrange(desc(logratio))
# 
# word_ratios %>%
#   group_by(logratio < 0) %>%
#   slice_max(abs(logratio), n = 20) %>% 
#   ungroup() %>%
#   mutate(word = reorder(word, logratio)) %>%
#   ggplot(aes(word, logratio, fill = logratio < 0)) +
#   geom_col(show.legend = FALSE) +
#   coord_flip() +
#   ylab("log odds ratio (Germany/Italy)") +
#   scale_fill_discrete(name = "", labels = c("Germany", "Italy"))

# quanteda

# kn <- textstat_keyness(dfm_group(dfm_subset(dfm_cast, dateyear == 2021 ),groups = country),
#                           target = "Germany", measure = "lr")
# # kn_it$country <-  "Italy"
# 
# library(quanteda.textplots)
#  textplot_keyness(kn, n = 20, margin = 0.1,
#                    labelsize = 10, color = c("black","grey")) 
 #+
  # ylab(kn_it$country) +
  # xlab(expression(chi^2)) +
  # theme_bw()  +
  # theme( axis.title.y = element_text(size = 20),
  #        axis.text.y = element_blank(),axis.ticks.y = element_blank(),
  #        axis.title.x = element_text(size = 20), axis.text.x = element_text(size = 15),
  #        plot.title = element_text(hjust = 0.5),legend.position = "bottom",
  #        legend.text = element_text(size=20)) 


#  Word embedding
 
 tidy_skipgrams <- df_finlocast %>%
   unnest_tokens(ngram, text, token = "ngrams", n = 5) %>%
  # filter(!word %in% stop_words$word) %>%
   mutate(ngramID = row_number()) %>% 
   tidyr::unite(skipgramID, tweet_id, ngramID) %>%
   unnest_tokens(word, ngram)
 
 unigram_probs <- df_finlocast %>%
   unnest_tokens(word, text) %>%
   count(word, sort = TRUE) %>%
   mutate(p = n / sum(n))
 
 skipgram_probs <- tidy_skipgrams %>%
   pairwise_count(word, skipgramID, diag = TRUE, sort = TRUE) %>%
   mutate(p = n / sum(n)) 
 
 normalized_prob <- skipgram_probs %>%
   filter(n > 20) %>%
   rename(word1 = item1, word2 = item2) %>%
   left_join(unigram_probs %>%
               select(word1 = word, p1 = p),
             by = "word1") %>%
   left_join(unigram_probs %>%
               select(word2 = word, p2 = p),
             by = "word2") %>%
   mutate(p_together = p / p1 / p2)
 
 normalized_prob %>% 
   filter(word1 == "solidarity") %>%
   arrange(-p_together)
 
 
# correlation
 
# df <- df_finlocast %>% filter(dateyear == 2021) %>%
#   unnest_tokens(word, text) %>%
#   filter(!word %in% stop_words$word)

 df <- df_finlocast %>%
   unnest_tokens(word, text, token = "tweets") %>%
   filter(!word %in% c(stop_words$word,"solidarity","people"))
 
 word_pairs <- df %>% group_by(country, monthyear) %>%
   pairwise_count(word, tweet_id, sort = TRUE) %>%
   left_join(df %>% 
               count(country, monthyear, name = "total"))

  word_pairsold <- word_pairs %>%
 filter(item1 == "#solidarity") %>%
   mutate(sumcor = sum(n)) %>%
  mutate(freq = n/sumcor)



# word_pairspearson <- df %>% group_by(country, monthyear,word) %>%
#  filter(n() >= 20) %>%
#   pairwise_cor(word, tweet_id, sort = TRUE)

top_word_pairsold <- word_pairsold %>% 
 # filter(!item2 %in% c("solidarity","people")) %>% 
  group_by(country, monthyear) %>% top_n(1)


##
# #solidarity terms

# df <- df_finlocast %>%
#  # unnest_tokens(word, text, token = "tweets") %>%
#   filter(!word %in% stop_words$word,
#          !word %in% str_remove_all(stop_words$word, "'")
#   )


frequency <- df_finlocast %>% 
  count(country, monthyear, word, sort = TRUE)
# %>% 
#   left_join(df %>% 
#               count(country,monthyear, name = "total")) %>%
#   mutate(freq = n/total)

library("ggrepel")  
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

ggplot(df_finlocast,aes(x = monthyear, color = country)) + geom_bar() + facet_wrap(~ country)




ggplot(top_word_pairsold,aes(x = monthyear, y = n)) +  geom_col() +
  geom_text_repel(aes(x = monthyear, y = n, label = item2), vjust = -1) + 
 # geom_text(aes(label = item2), vjust = -1) + 
  facet_wrap(~country, dir = "v")

### ONLY COVID TERMS

df_finlocast <- df_finlocast %>% filter(str_detect(text,"corona|covid|sars|pandemic"))

df <- df_finlocast[df_finlocast$monthyear %in% c("07-2021"),] %>%
  unnest_tokens(word, text, token = "tweets") %>%
  filter(!word %in% stop_words$word,
         !word %in% str_remove_all(stop_words$word, "'")
  )


word_ratios <- df %>%
  #  filter(!str_detect(word, "^@")) %>%
  count(word, country) %>%
  group_by(word) %>%
  # filter(sum(n) >= 10) %>%
  ungroup() %>%
  pivot_wider(names_from = country, values_from = n, values_fill = 0) %>%
  mutate_if(is.numeric, list(~(. + 1) / (sum(.) + 1))) %>%
  mutate(logratio = log(Germany / Italy)) %>%
  arrange(desc(logratio))

word_ratios %>%
  group_by(logratio < 0) %>%
  slice_max(abs(logratio), n = 5) %>% 
  ungroup() %>%
  mutate(word = reorder(word, logratio)) %>%
  ggplot(aes(word, logratio, fill = logratio < 0)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  ylab("log odds ratio (Germany/Italy)") +
  scale_fill_discrete(name = "", labels = c("Germany", "Italy"))

## topic modelling

load("sample/stm_25.Rdata")
load("sample/dfm_cast.Rdata")
stm_m <- stm_25
stm_df <- quanteda::convert(dfm_cast,to = "stm")  
numm <- 25


sg <- labelTopics(stm_m,10)
sg_prob <- tibble(topic = 1:numm,sg$prob) # marginal probability
sg_frex <- tibble(topic = 1:numm,sg$frex) # marginal frex
# sg_prob_pol <- tibble(topic = 1:numm,sg$covnames[[1]],sg$cov.betas[[1]]$problabels) # POL prob
# sg_prob_ta <- tibble(topic = 1:numm,sg$covnames[[2]],sg$cov.betas[[2]]$problabels) # TA probl
# sg_prob_tu <- tibble(topic = 1:numm,sg$covnames[[3]],sg$cov.betas[[3]]$problabels) # TU prob
# sg_frex_pol <- tibble(topic = 1:numm,sg$covnames[[1]],sg$cov.betas[[1]]$frexlabels) # POL frex
# sg_frex_ta <- tibble(topic = 1:numm,sg$covnames[[2]],sg$cov.betas[[2]]$frexlabels) # TA frex
# sg_frex_tu <- tibble(topic = 1:numm,sg$covnames[[3]],sg$cov.betas[[3]]$frexlabels) # TU frex

t2 <- findThoughts(stm_m, texts = df_finlocast$text,n = 3, topics =2)$docs[[1]]
n <- 548
txx <-  print(c(paste0("ACTsj: ", df_finlocast[n,]$user_username," TXT: ",df_finlocast[n,]$text," DATE: ", df_finlocast[n,]$created_at,
                       " TWID: ",df_finlocast[n,]$tweet_id," CNID ",df_finlocast[n,]$conversation_id)))


thoughts <- list()
for (i in 1:numm){ # 
  # thg_det <- findThoughts(stm_m, texts = df_de$text,n = 3, topics =i)$docs[[1]]
  thought_text = list()
  dates <- findThoughts(stm_m, texts = df_finlocast$text,n = 3, topics =i)$index[[1]] # 
  for (n in dates) {
    txx <-  print(c(paste0(" ACT: ", df_finlocast[n,]$actor,
                           " ACTsj: ", df_finlocast[n,]$user_username," TXT: ",df_finlocast[n,]$text," DATE: ", df_finlocast[n,]$created_at,
                           " TWID: ",df_finlocast[n,]$tweet_id," CNID ",df_finlocast[n,]$conversation_id)))
    thought_text[[n]] <- txx
    thought_textfin <- do.call(rbind.data.frame, thought_text)
  }
  thoughts[[i]] <- thought_textfin
  
}
bind_rows(thoughts)
thoughts <- do.call(rbind.data.frame, lapply(thoughts,'[[',1))
colnames(thoughts) = c("1", "2","3") # columns where texts go
thoughts <- cbind(topic = 1:numm,thoughts)

report <- cbind(sg_prob,sg_frex,thoughts)
write.csv(report,file= "sample/reportstm_25.csv",row.names = F, col.names=T,  sep=";",  fileEncoding = "UTF-8")

## tf-idf

 df_words <- df_finlocast %>% 
   filter(!word %in% c("","#","_")) %>%
 count(country,quadrimester,dateyear,word,sort = TRUE)

total_words <- df_words %>% 
 group_by(country,quadrimester,dateyear) %>% 
 # group_by(country) %>%
  summarize(total = sum(n))

df_words <- left_join(df_words, total_words)

df_words$catlev <- paste0(df_words$country,"_",df_words$monthyear)
df_words$catquad <- paste0(df_words$country,"_",df_words$quadrimester,"_",df_words$dateyear)  
  
df_tf_idf <- df_words %>%
  bind_tf_idf(word,catquad, n)

df_tf_idf %>%  arrange(desc(tf_idf))

df_tf_idf[df_tf_idf$word == "corona",]

# df_tf_idf %>%
#   group_by(country) %>%
#   slice_max(tf_idf, n = 20) %>%
#   ungroup() %>%
#   ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = country)) +
#   geom_col(show.legend = FALSE) +
#   facet_wrap(~ country, ncol = 2, scales = "free") +
#   labs(x = "tf-idf", y = NULL) +
#   theme_bw()
# ggsave(file="figures/tfidf.jpg",width = 10, height = 5)

# tf-idf for correlations
df_tf_idf %>%
  group_by(country, monthyear) %>%
slice_max(tf_idf, n = 1) %>%
  group_by(catlev,tf_idf) %>%
  mutate(labelling = paste0(word,collapse = ", ")) %>%
# df_tf_idf %>%
#   group_by(country, monthyear) %>%
#   slice_max(tf_idf, n = 1) 
  ungroup() %>%
ggplot(aes(x = monthyear,y = n)) +
  geom_col() +
 # geom_jitter() +
 # geom_point() +
 # geom_line() + 
 #   geom_text_repel(aes(x = monthyear, y = tf_idf, label = word)) +
  geom_text(aes(label = labelling), hjust = 0) +
  coord_flip() +
  scale_x_discrete(limits=rev) +
  facet_wrap(~ country, dir = "h") +
  theme_bw()
ggsave("figures/tf_idf.jpg", width = 10,  height = 10)

#

df_tf_idf %>%
  filter(dateyear %in% c(2020,2021)) %>%
  filter(country == "Germany") %>%
  group_by(catquad) %>%
  slice_max(tf_idf, n = 5) %>%
 # group_by(catquad,tf_idf) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = catquad)) +
  geom_col(show.legend = FALSE) +
# coord_flip() +
  facet_wrap(~ catquad,scales = "free")

  























# keyness

dfcvd <- df_finlocast %>% filter(word %in% cvd)
df <- df_finlocast %>% filter(tweet_id %in% dfcvd$tweet_id)





word_ratios <- df %>%
  filter(!word %in% c(cvd,"solidarity","#solidarity","#thejoint","#grosseto","#stories",
                      "#tg24","brd_cuba","#d15","#webart","#caritaspadova","#chiciseparerà")) %>%
  
 # filter(!str_detect(word, "^@")) %>%
  count(word, country) %>%
  group_by(word) %>%
  filter(sum(n) >= 10) %>%
  ungroup() %>%
  filter(!word %in% c("","#")) %>%
  pivot_wider(names_from = country, values_from = n, values_fill = 0) %>%
  mutate_if(is.numeric, list(~(. + 1) / (sum(.) + 1))) %>%
  mutate(logratio = log(Germany / Italy)) %>%
  arrange(desc(logratio))

word_ratios %>%
  group_by(logratio < 0) %>%
  slice_max(abs(logratio), n = 15) %>% 
  ungroup() %>%
  mutate(word = reorder(word, logratio)) %>%
  ggplot(aes(word, logratio, fill = logratio < 0)) +
  geom_col() +
  coord_flip() +
  ylab("log odds ratio (Germany/Italy)") +
  scale_fill_discrete(name = "", labels = c("Germany", "Italy")) +
  theme_bw() 
#ggsave(file="figures/keyness_covidspecific.jpg",width = 10, height = 9)






