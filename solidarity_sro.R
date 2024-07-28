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


# empirical indicators (figure 1) ####
load("utils/df_eurostat.Rdata")
load("utils/whodf.Rdata")
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
ggsave(file="review2/review_2/submissionR2/figures/1_covidexp.jpg",width = 15,height = 9)

# upload data text-analysis ####

load("utils/df_finlocast.Rdata")
df_finlocast$text <- str_squish(df_finlocast$text)
# rem <- read.xls("utils/compound.xls",sheet = "rem", encoding = "latin1")[,1]
rem <- read_xls("utils/compound.xls",sheet = "rem")[,1]
rem <- as.character(rem$remove_list)
rem <- unique(rem)
rem <- gsub("#","",rem)
# cvd <- read.xls("utils/compound.xls",sheet = "cvd", encoding = "latin1")[,1] 
cvd <- read_xls("utils/compound.xls",sheet = "cvd")[,1] 
cvd <- as.character(cvd$cvd_list)
cvd <- gsub("#","",cvd)
rem_char <- c("http*","@*","€","+","|","s","faq","=","_","__","~","___")
# rem_de <- read.xls("utils/compound.xls",sheet = "rem_de", encoding = "latin1")[,1]
# rem_de <- unique(rem_de)
# rem_de <- gsub("#","",rem_de)

# clean duplicates
df_finlocast$selected <- 1
df_finlocast[11919,]$selected <- 0
df_finlocast <- df_finlocast[!duplicated(df_finlocast$original_language),]
df_finlocast <- df_finlocast %>% filter(! user_username %in% c("AntonioPinna11","AssGSintini","ThierryCuisine","FranceCarlucci_","StadtViersen")) # "StadtViersen"
df_finlocast <- df_finlocast %>% filter(selected == 1)

# delete tweets
# dl_tw <- read.xls("utils/compound.xls",sheet = "dl_tw", encoding = "latin1")[,1]
dl_tw <- read_xls("utils/compound.xls",sheet = "dl_tw")[,1]
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


# cmpd <- read.xls("utils/compound.xls",sheet = "cmpd", encoding = "latin1")[,1]
cmpd <- read_xls("utils/compound.xls",sheet = "cmpd")[1]
cmpd <- as.character(cmpd$cmpd_target)
cmpd <- paste0("\\b",cmpd,"\\b")
# cmpdsubstitute <- read.xls("utils/compound.xls",sheet = "cmpd", encoding = "latin1")[,2]
cmpdsubstitute <- read_xls("utils/compound.xls",sheet = "cmpd")[,2]
cmpdsubstitute <- as.character(cmpdsubstitute$cmpd_change)
names(cmpdsubstitute) <- cmpd
df_finlocast$text <- str_replace_all(df_finlocast$text,cmpdsubstitute)




# categorizations ####

dftop_ita <- read.csv("utils/Italy_cat.csv",sep = ",")
dftop_ita[dftop_ita$user_description == "Comitato aquilano nato a seguito del sisma del 6 aprile del 2009. Si batte per una ricostruzione sociale della città, giusta e partecipata.",]$user_username <- "3e32"  
  
dftop_deu <- read.csv("utils/Germany_cat.csv",sep = ",")
  
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



dftop <- dftop %>% filter(note == "keep") %>% filter(! detailed_description == "embassy")


# merge annotation to dataframe ####

df_finlocast <- merge(df_finlocast, dftop, by = c("user_username","country"))
# to filter out embassy and outplaced
df_finlocast <- df_finlocast %>% filter(note == "keep") %>% filter(! detailed_description == "embassy")



#df_finlocast$word <- gsub("#","",df_finlocast$word)

#### Top frequencies (figure 2)  https://www.tidytextmining.com/twitter.html ####

df_finlocast <- df_finlocast %>% filter(! user_username %in% dftop[dftop$note == "delete",]$user_username)


df_finlocast <- df_finlocast %>% unnest_tokens(word, text, token = stringr::str_split, pattern = " ")
df_finlocast$word <- str_remove_all(df_finlocast$word, "[^#_[:^punct:]]")
df_finlocast <- df_finlocast %>% filter(!word %in% stop_words$word,
                                        !word %in% rem_char,
                                        !word %in% stopwords("en"), 
                                        # !word %in% stopwords_en,
                                        !word %in% rem)


df_finlocast <- replace(df_finlocast, df_finlocast=='', NA)
df_finlocast <- df_finlocast %>% drop_na(word)

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



ggplot(dfs, aes(x = monthyear,y = n.x, color = country)) +
  geom_point(aes(color = country)) + 
  geom_line(aes(group = as.factor(country))) +
  ggrepel::geom_text_repel(aes(label = label), size = 4.5) +
  xlab("Time (month-year)") +
  ylab("Frequency") +
  labs(color = "Country") +
  ggtitle("Term #solidarity in Twitter and most associated words") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = -75, hjust = 0, size = 14), 
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 16),
        axis.title.x = element_text(size = 16),
        title = element_text(size = 17),
        legend.title =  element_text(size = 16),
        legend.text = element_text(size = 16),
        legend.position = "bottom")
ggsave(file="review2/review_2/submissionR2/figures/2_frequency.jpg",width = 12,height = 6)


# For covid-only tweets (keyness figure 3, figure 4,5,6,annex) ####

df_finlocast$text <- gsub("#","",df_finlocast$text) ###  delete hashtag
load("utils/tweet_cvd.Rdata")
tweet_cvd <- gsub("_","",tweet_cvd)
df_finlocast <- df_finlocast %>% filter(tweet_id %in% tweet_cvd)
# !! in word_embedding script this subsample is saved as df_we for shortness

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

# combine topic proportion (figure 4) ####
load("utils/gamma_terms_de.Rdata")
load("utils/gamma_terms_it.Rdata")

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

# topic modelling visualizations (figure 5) ####
 
load("utils/stm_de20.Rdata")
load("utils/dfm_desolcv.Rdata")
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


load("utils/stm_it25.Rdata")
load("utils/dfm_itsolcv.Rdata")
stm_dfIT <- quanteda::convert(dfm_itsolcv,to = "stm")  
numm <- 25

dfbIT <- df_finlocast %>% filter(country == "Italy")


prepIT <- estimateEffect(1:numm ~ s(datenum), stm_it25, metadata = stm_dfIT$meta, uncertainty = "Global")
tidystmIT <- tidy(stm_it25)

effects_intIT <- get_effects(estimates = prepIT,
                             variable = 'datenum',
                             type = 'continuous'  )  

#load("utils/6/6DE/de6_effects_int.Rdata")
#load("utils/6/6IT/it6_effects_int.Rdata")

#tidystm <- tidy(stm_it25)

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

# For Germany
numm <- 20
effects_int <- effects_intDE
tidystm <- tidy(stm_de20)
folder <- "DE/DE_20"

# For Italy
numm <- 25
effects_int <- effects_intIT
tidystm <- tidy(stm_it25)
folder <- "IT/IT_25"


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
ggsave(cm,file = paste0("review2/review_2/submissionR2/figures/",folder,"_",i,".jpg"),width = 18, height = 4)
 
 }
 

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


# pandemic economic indicators + prevalence (figure 5) ########
load("utils/df_eurostat.Rdata")
load("utils/whodf.Rdata")
df_eurostat$date <- as.character(df_eurostat$date)


eurostat.labs <- c("Debt Gross", "Inflation", "% Unemployment")
names(eurostat.labs) <- c("debtgross", "inflation", "unemployment")

who <- who %>% gather(variable, value, c(New_cases,New_deaths), factor_key=TRUE)

who.labs <- c("New cases", "New deaths")
names(who.labs) <- c("New_cases", "New_deaths")

norm_minmax <- function(x){
  (x- min(x)) /(max(x)-min(x))
}

# df_eurostat <- df_eurostat %>% gather(variable,value, c(unemployment, debtgross,inflation),factor_key = TRUE)

#who <- who %>% gather(variable, value, c(New_cases,New_deaths), factor_key=TRUE)
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

ggpubr::ggarrange(de_tm, it_tm, pandpic_de,pandpic_it)
ggsave(file="review2/review_2/submissionR2/figures/5_topicprev.jpg",width =13.5, height = 10)


# topic modelling actor categiries (figure 6) #####

load("utils/gamma_terms_catTOT.Rdata") 
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
ggsave(file = paste0("review2/review_2/submissionR2/figures/6_categoriestopic.jpg"),width = 23.1,height = 19)

# annex categories users #####
dftop %>% 
  group_by(country, description_4) %>% 
  summarise(sum_tot = sum(count_name_occurr),.groups = "drop") %>% 
  ggplot(aes(description_4,
             y = sum_tot,fill = description_4, label = sum_tot)) + 
  geom_col() +
  facet_wrap(~ country, scales = "free", dir = "v") +
  scale_y_continuous( labels = function(x) sprintf("%0.0f", x)) +
  coord_flip() +
  geom_text(size = 6) +
  ylab("count tweets") +
  xlab("Users' categories") +
  labs(fill = "Users Categories") +
  theme_bw() +
  theme(legend.position = "none",
        axis.text.y = element_text(size = 16),
        axis.text.x = element_text(size = 16),
        strip.text.x = element_text(size = 16),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        plot.margin = unit(c(0.2, 0.4, 0.2, 0.2), 
                           "inches")
  )
ggsave("review2/review_2/submissionR2/figures/sup_macrocat_descr.jpg", width = 9, height = 9)

# annex figure 6 #####

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
ggsave(file = paste0("review2/review_2/submissionR2/figures/sup_categoriestopic_",i,".jpg"),width = 21.5,height = 15)

}

