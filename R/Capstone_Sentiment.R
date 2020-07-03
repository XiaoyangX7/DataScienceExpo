library(tidyverse)
library(dplyr)
library(stringr)
library(varhandle)
options(stringsAsFactors = FALSE)
library(ggplot2)
library(factoextra)
library(sentimentr)
library(cluster)

library(ggthemes)
library(glmnet)
theme_set(theme_bw())
library(Rtsne)
library(tidytext)
library(quanteda)

library(roxygen2)
library(usethis)
library(testthat)
library(R6)
library(jsonlite)
library(psych)
library(lubridate)

library(streamgraph)
library(plotly)
library(topicmodels)
set.seed(7)
library(cld3)

library(devtools)
devtools::install_github("ropensci/googleLanguageR")
library(googleLanguageR)

#df2 <- read.csv("ab_reviews.csv")


list <- unique(df3$listing_id)
lists <- list
list1 <- sample(lists,8051)
lists = lists[! lists %in% list1]
list2 <- sample(lists, 8051)
lists = lists[! lists %in% list2]
list3 <- sample(lists, 8050)
lists = lists[! lists %in% list3]
list4 <- sample(lists, 8051)
lists = lists[! lists %in% list4]
list5 <- sample(lists, 8051)
lists = lists[! lists %in% list5]




review_1 <- df3 %>%
  filter(listing_id %in% list1)
review_2 <- df3 %>%
  filter(listing_id %in% list2)
review_3 <- df3 %>%
  filter(listing_id %in% list3)
review_4 <- df3 %>%
  filter(listing_id %in% list4)
review_5 <- df3 %>%
  filter(listing_id %in% list5)

write.csv(review_1,"review_1.csv")
write.csv(review_2,"review_2.csv")
write.csv(review_3,"review_3.csv")
write.csv(review_4,"review_4.csv")
write.csv(review_5,"review_5.csv")

false <- read.csv("bq_false.csv")
fake <- read.csv("bq_fake.csv")
review1 <- read.csv("review_1.csv")


# 
# false_en <- false
# false_en$lang <- wraplang(false_en$comments)
# 
# ehh = as.vector(false_en$comments)
# 
# for (i in seq_along(ehh)) {
#    ehh[i]
#    false_en$label[i] = detect_language(ehh[i])
# }
# 
# false_en$label

languagewrapper <- function(x){
  x1 = as.vector(x)
  y = vector()
  for(i in seq_along(x1)){
    y[i] = detect_language(x1[i])
  }
  return (y)
}

k = languagewrapper(badb$comments)
badb$label = k
badb_eng = badb %>%
  filter(label %in% "en")

k = languagewrapper(big_sample$comments)
big_sample$label = k

e = languagewrapper(review1$comments)
review1$lang = e

sample_eng = big_sample %>%
  filter(label %in% "en")
fake_noneng = fake %>%
  filter(! label %in% "en")

key = "../ba888-cp-team2-befddf686c95.json"
gl_auth(key)

fake_noneng$comments = as.character(fake_noneng$comments)
trans_text = gl_translate(fake_noneng$comments, target = "en")
trans_text

review1_eng = review1 %>%
  filter(lang %in% "en")




# 
# 
# lapply(fake$comments, FUN= languagewrapper) -> label
# fake$label
# fake$comments
# 
# fake_en <- fake
# ehhh = as.vector(fake_en$comments)
# 
# for (i in seq_along(ehhh)) {
#   fake_en$label[i] = detect_language(ehhh[i])
# }
# 
# fake_eng = fake_en %>%
#   filter(label == "en")
# 
# fake_other = fake_en %>%
#   filter(label != "en")



bert_sentr = with(bert,
                  sentiment_by(get_sentences(comments),
                               list(listing_id,date,label)))

bert_sent_sampler = with(bert_sample,
                  sentiment_by(get_sentences(comments),
                               list(listing_id,date,label)))

fake_sentr = with(fake_en,
                  sentiment_by(get_sentences(comments),
                               list(listing_id,date,id)))
false_sentr = with(false_en,
                  sentiment_by(get_sentences(comments),
                               list(listing_id,date,id)))


# debate = corpus(review_1$comments)
# hist(ntoken(debate))
# 
# newsdfm = dfm(debate, 
#               remove_punct = T,
#               remove = stopwords(language = "en",source = "snowball"),
#               remove_numbers = T,
#               remove_symbols = T) %>% 
#   dfm_trim(min_termfreq = 2,
#            max_docfreq = .65,
#            docfreq_type = "prop")
# topfeatures(newsdfm)
# textplot_wordcloud(newsdfm, max_words = 70)
# dtm = convert(newsdfm, "topicmodels")
# newsmod = LDA(dtm, k=5, control = list(seed = 7))
# terms(newsmod, 10)
# reviewg = tidy(newsmod, matrix = "gamma")
# reviewb = tidy(newsmod, matrix = "beta")
# 
# review_top_terms <- reviewb %>%
#   group_by(topic) %>%
#   top_n(20,beta) %>%
#   ungroup %>%
#   arrange(topic, -beta)
# 
# review_top_terms %>%
#   mutate(term = reorder_within(term,beta,topic)) %>%
#   ggplot(aes(term,beta, fill = factor(topic))) +
#   geom_col(show.legend = FALSE) +
#   facet_wrap(~topic,scales = "free") +
#   coord_flip() +
#   scale_x_reordered()
# 
# 
# false_sentr = with(false,
#                      sentiment_by(get_sentences(comments),
#                                   list(listing_id,date,id)))
# highlight(false_sentr)
#                     

get_sentiments("loughran")
get_sentiments("bing")
get_sentiments("afinn")

sample = read.csv("sample_english.csv")

a1 = bert%>%
  select(listing_id,comments,label)
tidy_sample = a1 %>%
  filter(label == 0)%>%
  unnest_tokens(token,comments)
tidy_sample %>% 
  group_by(token) %>%
  count(sort = T)%>%
  print(n = 20)
tidy_sample1 = tidy_sample %>%
  anti_join(get_stopwords(), by=c("token" = "word"))
# tidy_sample2 %>%
#   group_by(token) %>%
#   count(sort = T)%>%
#   print(n = 50)
sw = c("ã","place","s","us","stay","de","really","ð","la","y","et","t","also")
tidy_sample2 = tidy_sample1 %>% filter(! token %in% sw)
sent_bing = inner_join(tidy_sample2, get_sentiments("bing"), 
                       by=c("token" = "word"))
head(sent_bing)
sent_bing2 = sent_bing %>% 
  count(listing_id, token, sentiment) %>% 
  pivot_wider(names_from = sentiment, 
              values_from = n,
              values_fill = list(n = 0))

sent_bing2 %>% 
  group_by(listing_id)%>%
  summarize(polarity = sum(positive) - sum(negative)) -> sent_bing3

sample_bing = sent_bing2 %>% 
  group_by(listing_id) %>% 
  summarise(pos = sum(positive),
            neg = sum(negative)) %>% 
  mutate(polarity = pos - neg)
ggplot(sample_bing,aes(polarity)) + geom_density()

afinn_bing = inner_join(tidy_sample2, get_sentiments("afinn"), 
                       by=c("token" = "word"))
head(afinn_bing)
afinn_bing%>% 
  select(token,value) %>%
  group_by(token,value) %>%
  count(sort = T) %>%
  print(n = 20)

afinn_bing2 = afinn_bing %>%
  count(listing_id) %>%
  inner_join(afinn_bing) %>%
  group_by(listing_id)%>%
  summarise(affin_polarity = sum(value)) 

ggplot(afinn_fake1,aes(x = affin_polarity)) + geom_histogram(bins = 20)
ggplot(afinn_bing2,aes(x = affin_polarity)) + geom_histogram(bins = 20)

loughran_fake = inner_join(sample_bad,get_sentiments("loughran"),
                           by = c("token" = "word"))
loughran_sample = inner_join(tidy_sample2,get_sentiments("loughran"),
                           by = c("token" = "word"))
loughran_fake %>%
  inner_join(fake) -> lf
lf %>%
  count(listing_id, token, sentiment) %>% 
  pivot_wider(names_from = sentiment, 
              values_from = n,
              values_fill = list(n = 0))


loughran_sample %>%
  inner_join(big_sample) %>%
  count(sentiment) %>%
  mutate(freq = n / sum(n))

loughran_fake %>%
  inner_join(bad) %>%
  count(sentiment) %>%
  mutate(freq = n / sum(n))

dev.new(width = 550, height = 330, unit = "px")
wordcloud(words = tidy_sample6$token,freq = tidy_sample6$n,min = 100,
          colors = c("#FF5A5F","#00A699","#FC642D","#484848","#767676"))

sample_bad = fake %>%
  select(listing_id,comments) %>%
  unnest_tokens(token,comments) %>%
  anti_join(get_stopwords(), by=c("token" = "word"))%>%
  filter(! token %in% sw) 
bad_tokens = sample_bad %>% count(token,sort = T)
dev.new(width = 550, height = 330, unit = "px")
wordcloud(words = bad_tokens$token,freq = bad_tokens$n,min = 30,max = 100,
          colors = c("#FF5A5F","#00A699","#FC642D","#484848","#767676"))



#-------------------------------------------------------------------------------
fake <- df %>% filter(attitude == 0)
fake_bigrams = fake %>%
  unnest_tokens(word, comments, token="ngrams", n=2)

fake_trigrams = fake %>%
  unnest_tokens(word, comments, token="ngrams", n=3)

fake_bigrams %>%
  count(word, sort=T) %>% 
  print(n=50) 

sample = read.csv("sample_english.csv")
kwic(fake$comments,"false",window = 5)

loughran_sample %>%
  filter(sentiment == "negative") %>%
  group_by(token) %>%
  count(sort = T) %>%
  print(n = 20)

bad_corpus = corpus(bad$comments)
bad_dfm = dfm(bad_corpus,
              remove_punct = T,
              remove = stopwords(), 
              stem = T, 
              remove_url = T,
              remove_symbols = T) %>% 
  dfm_trim(min_termfreq = 10, 
           termfreq_type = "count",
           max_docfreq = .7,
           docfreq_type = "prop")

topfeatures(bad_dfm)
textstat_frequency(bad_dfm, n = 20)
dtm = convert(bad_dfm, to = "topicmodels")
bad_topics = LDA(dtm, 6, control = list(seed=7))
bad_beta = tidy(bad_topics, matrix="beta")
bad_top15 =  bad_beta %>%
  group_by(topic) %>% 
  top_n(15, beta) %>% 
  ungroup() %>% 
  arrange(desc(beta)) 
bad_top15 %>%
  mutate(term = reorder_within(term, beta, topic))  %>% 
  ggplot(aes(term, beta)) +
  geom_col() +
  facet_wrap(~ topic, scales="free") +
  coord_flip() +
  scale_x_reordered()

bad_gamma = tidy(bad_topics, matrix="gamma")
badg_top15 =  bad_gamma %>%
  group_by(topic) %>% 
  top_n(15, gamma) %>% 
  ungroup() %>% 
  arrange(desc(gamma)) 
head(badg_top15)




sample_corpus = corpus(sample$comments)
sample_dfm = dfm(sample_corpus,
              remove_punct = T,
              remove = stopwords(), 
              stem = T, 
              remove_url = T,
              remove_symbols = T) %>% 
  dfm_trim(min_termfreq = 10, 
           termfreq_type = "count",
           max_docfreq = .7,
           docfreq_type = "prop")

topfeatures(sample_dfm)
textstat_frequency(sample_dfm, n = 20)
sam_dtm = convert(sample_dfm, to = "topicmodels")
sam_topics = LDA(sam_dtm, 6, control = list(seed=7))
sam_beta = tidy(sam_topics, matrix="beta")
sam_top15 =  sam_beta %>%
  group_by(topic) %>% 
  top_n(15, beta) %>% 
  ungroup() %>% 
  arrange(desc(beta)) 
sam_top15 %>%
  mutate(term = reorder_within(term, beta, topic))  %>% 
  ggplot(aes(term, beta)) +
  geom_col() +
  facet_wrap(~ topic, scales="free") +
  coord_flip() +
  scale_x_reordered()

names(bert_sentr)
