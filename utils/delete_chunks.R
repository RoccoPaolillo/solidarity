
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

# espic <- df_eurostat %>% filter(country %in% c("EU","Italy","Germany") & datenum >= 17897 & datenum <= 18993) %>%
#   ggplot(aes(x = datenum, y = value, color = country)) +
#           # geom_point() +
#            geom_line() +
#   scale_color_manual(values = c("EU" = "blue","Italy" = "red", "Germany" = "darkgreen"),
#                      name = "Country") +
#            facet_wrap(~  variable, scales = "free",
#                       labeller = labeller(variable = eurostat.labs) ) +
#   scale_x_continuous(breaks = c(17897,18262,18628,18993),
#                      labels = c("17897" = "JAN 19","18262" = "JAN 20","28628" = "JAN 21","18993" = "DEC 21")) +
#   geom_vline(xintercept = 18262, color = "gray") +
#   ggtitle("Economic indicators") +
#  # xlab("Time") +
#   theme_bw() + 
#   theme(axis.title.y = element_blank(),axis.title.x = element_blank(),
#         strip.text.x = element_text(size = 16),
#         axis.text.x = element_text(size = 13),
#         axis.text.y = element_text(size = 13),
#         plot.title = element_text(hjust = 0.5, size = 18),
#         legend.text = element_text(size = 14),
#         legend.title = element_text(size = 15),
#         plot.margin = unit(c(0.2, 0.3, 0.2, 0.2), 
#                            "inches"))
# 

# 
# whopic <- who %>% ggplot(aes(x = datenum, y = value, color = country)) +
#   geom_line() +
#   scale_color_manual(values = c("EU" = "blue","Italy" = "red", "Germany" = "darkgreen"),
#                      name = "Country") +
#   scale_x_continuous(breaks = c(18262,18628,18993),
#                      labels = c("18262" = "JAN 20","28628" = "JAN 21","18993" = "DEC 21")) +
#   facet_wrap(~ variable, scales = "free",
#              labeller = labeller(variable = who.labs) ) +
#   ggtitle("Pandemic indicators") +
#   # xlab("Time") +
#   theme_bw() +
#   theme(axis.title.y = element_blank(),axis.title.x = element_blank(),
#         strip.text.x = element_text(size = 16),
#         axis.text.x = element_text(size = 13),
#         axis.text.y = element_text(size = 13),
#         plot.title = element_text(hjust = 0.5, size = 18),
#         legend.text = element_text(size = 14),
#         legend.title = element_text(size = 15),
#         plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), 
#                            "inches"))
#   
# 
# ggpubr::ggarrange(espic,whopic, nrow  = 2, common.legend = TRUE, legend = "bottom")
# ggsave(file="review2/review_2/submissionR2/figures/covidexp.jpg",width = 15,height = 9)
# 2019-01-01 17897
# 2020-01-01 18262
# 2021-01-01 18628
# 2022-01-01 18993


# prep <- estimateEffect(1:numm ~ s(datenum), stm_m, metadata = stm_df$meta, uncertainty = "Global")

# de_effects_int <- get_effects(estimates = prep,
#                               variable = 'datenum',
#                               type = 'continuous'  )  


# load("utils/6/6DE/de6_effects_int.Rdata")
# load("utils/6/6IT/it6_effects_int.Rdata")

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

#

# deusers <- dftop %>%
#   filter(country == "Germany") %>%
#   top_n(30,count_name_occurr) %>%
#   ggplot(aes(x = fct_reorder(user_username, count_name_occurr) , y = count_name_occurr)) +
#   geom_col(aes(fill = description_4)) +
#   facet_wrap(~ country, scales = "free") +
#   coord_flip() +
#   ylab("count tweets")  + 
#   scale_fill_manual(values= c(
#     "private account" = "lightsalmon",
#     "media" = "lightskyblue",
#     "market sector" = "khaki",
#     "governance & politics" = "violet",
#     "association" = "palegreen"
#   )) +
#   theme_bw()  +
#   theme(
#     axis.title.y = element_blank(),
#     axis.text.y = element_blank(),
#     axis.ticks.y = element_blank()
#   ) +
#   guides(fill=guide_legend(title="Users Category")) 
# 
# itusers <- dftop %>%
#   filter(country == "Italy") %>%
#   top_n(30,count_name_occurr) %>%
#   ggplot(aes(x = fct_reorder(user_username, count_name_occurr) , y = count_name_occurr)) +
#   geom_col(aes(fill = description_4)) +
#   facet_wrap(~ country, scales = "free") +
#   coord_flip() +
#   ylab("count tweets") +
#   scale_fill_manual(values= c(
#     "private account" = "lightsalmon",
#     "media" = "lightskyblue",
#     "market sector" = "khaki",
#     "governance & politics" = "violet",
#     "association" = "palegreen"
#   )) + 
#   theme_bw()  +
#   theme(
#     axis.title.y = element_blank(),
#     axis.text.y = element_blank(),
#     axis.ticks.y = element_blank()
#   ) +
#   guides(fill=guide_legend(title="Users Category")) 
# 
# ggpubr::ggarrange(deusers,itusers, nrow =2)
# ggsave(file="review_1/sample_rv1/topusers30.jpg", height = 8, width = 8)
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



