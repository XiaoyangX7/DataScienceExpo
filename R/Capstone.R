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

library(maps)
library(rgeos)
library("rnaturalearth")
library("rnaturalearthdata")

library(xgboost)
library(xgboostExplainer)
library(rpart)
library(randomForest)

library("car")
library("scatterplot3d")
library("rgl")


df1 <- read.csv("listings.csv")
df2 <- read.csv("ab_reviews.csv")
df3 <- read.csv("NYC_zipcodes.csv")
names(df1)
names(df2)
names(df3) = tolower(names(df3))
names(df3) = gsub("\\.","_",names(df3))

df3$zipcode = df3$jurisdiction_name
v = as.vector(df1$zipcode)
v1 = as.numeric(v)
which(is.na(v1))
v2 = sapply(v,str_remove(v,"NY "))
v3 = as.numeric(v2)
which(is.na(v3))
v2[48771]

df1$zipcode = unfactor(df1$zipcode)
df1$zipcode = as.numeric(df1$zipcode)
summary(df1$zipcode)
summary(df1)

t1 <- left_join(df1,df3)
str(df1$zipcode)
str(df3$zipcode)
names(df3)

devtools::install_github("dkahle/ggmap")
NYC <- c(lon = -74.0060,lat = 40.7128)
map1 <- get_map(location = NYC, zoom = 14, scale = 2)
map <- get_map(location = 'Europe', zoom = 4)

t1 = t1[-22047,]
t1$price = as.numeric(t1$price)
ggplot(t1, aes(price, fill = as.factor(neighbourhood_group_cleansed),group = as.factor(neighbourhood_group_cleansed))) + 
  geom_histogram(stat = 'count') -> p1
p1
t = as.vector(t1$price)


for (i in seq_along(t)) {
  t[i] = str_sub(t[i],2)
}
t1$price = t

for (i in seq_along(z)) {
  if(str_length(z[i] > 5)){
    z[i] = str_sub(z[i],4)
  }
 }


df4 <- read.csv("clean_abb.csv")
names(df4)
library(psych)
cor.plot(df4)
dplyr::select_if(df4, is.numeric) -> ndf
names(ndf)
ndf1 <- ndf %>%
  select(-id,-host_id,-price)
price <- ndf$price
ndf3 <- cbind(price,ndf1)
View(ndf3)
cor.plot(as.data.frame(abb_lasso_t),scale = TRUE, upper = FALSE,cex.axis = 0.6)
cor.plot(ndf3,main = "Correlation Plot among Listing Variables for NYC Airbnb",scale = TRUE,upper = FALSE,ylas = 2,
         cex.axis = 0.71,gr = colorRampPalette(c("#B52127", "white", "#2171B5"))) -> p1
df4$price = unfactor(df4$price)
tl = df4$price
for (i in seq_along(tl)) {
  tl[i] = str_sub(tl[i],2)
}
df4$price = as.numeric(tl)

df4$accommodates

ggplot(df4, aes(x = log(price),fill = "#a3b0c4")) + 
  geom_histogram() +
  theme_bw() +
  scale_fill_manual(values = c("#a3b0c4")) +
  theme(legend.position = "none") +
  labs(title = "Log Price Distribution of Airbnb Listings in NYC, Dec 2019")

ggplot(df4, aes(x = price,fill = "#8a9da8")) + 
  geom_histogram() +
  theme_bw() +
  scale_fill_manual(values = c("#8a9da8")) +
  theme(legend.position = "none") +
  labs(title = "Price Distribution of Airbnb Listings in NYC, Dec 2019")


ggplot(df4, aes(x = as.factor(cancellation_policy), y = review_scores_rating)) +
  geom_point()
ggplot(df4, aes(x = as.factor(room_type),y = price, fill = neighbourhood_group_cleansed)) +
  geom_boxplot() +
  theme_bw() +
  facet_wrap(~neighbourhood_group_cleansed, ncol = 2) +
  scale_fill_manual(values = c("#8aa88c","#4f7859","#a3b0c4","#8a9da8","#4f5878")) +
  labs(title = "Price Distribution of Listings by Room Type and Neighbourhood in NYC, Dec 2019")

ggplot(df4, aes(x = as.factor(neighbourhood_group_cleansed),y = log(price), fill = neighbourhood_group_cleansed)) +
  geom_boxplot() +
  theme_bw() +
  scale_fill_manual(values = c("#8aa88c","#4f7859","#678f89","#67838f","#4f5878")) +
  labs(title = "Log Price Distribution of Listings by Neighbourhood in NYC, Dec 2019")


ggplot(df4, aes(x = as.factor(room_type),y = log(price), fill = room_type)) +
  geom_boxplot() +
  theme_bw() +
  scale_fill_manual(values = c("#8aa88c","#4f7859","#678f89","#67838f","#4f5878")) +
  labs(title = "Log Price Distribution of Listings by Room Type in NYC, Dec 2019")



df4$lgp = log(df4$price)
ggplot(df4, aes(x = as.factor(lgp), y = as.factor(review_scores_rating))) +
         geom_point() + 
         theme_bw()
       


df4$listing_id = df4$id
dfr <- full_join(df4,df2,by = "listing_id")
View(dfr)

ggplot(dfr,aes(x = as.factor(host_is_superhost), y = log(price),fill = as.factor(host_is_superhost))) +
  geom_boxplot() +
  theme_bw() +
  scale_fill_manual(values = c("#8aa88c","#4f7859","#678f89","#67838f","#4f5878")) +
  labs(title = "Log Price Distribution of Listings by Neighbourhood in NYC, Dec 2019")

ggplot(df4, aes(x = number_of_reviews, y = log(price),color = as.factor(cancellation_policy))) +
  geom_jitter(alpha = 0.2) +
  theme_bw() +
  facet_wrap(~as.factor(cancellation_policy)) +
  labs(title = "Log Price Distribution of Listings by Neighbourhood in NYC, Dec 2019")


library(wordcloud)
library(quanteda)
nfl_corpus = corpus(dfr$comments)
nfl_dfm = dfm(nfl_corpus,
              remove_punct = T,
              remove_numbers = T,
              remove_twitter = T,
              remove = stopwords(),
              stem = T,
              remove_url = T,
              remove_symbols = T) %>%
  dfm_trim(min_termfreq = 10,
           termfreq_type = "count",
           max_docfreq = .7,
           docfreq_type = "prop")

textplot_wordcloud(nfl_dfm,max_words = 150,min_count = 90,rotation = 0.25, color = rev(RColorBrewer::brewer.pal(10, "RdBu")))

dtm = convert(nfl_dfm, to = "topicmodels")
abb_lda = LDA(dtm, k = 3, control = list(seed = 7))

corpus = corpus(df2$comments)
kwic(corpus, pattern = "fake", window = 3)





#--------------------------

data <- read.csv("ABBC.csv")
str(data)

## load the packages
library(tidyverse)
library(factoextra)
library(skimr)

## new packages
# install.packages("corrplot")
library("corrplot")
library(varhandle)

data$review_scores_accuracy = as.numeric(data$review_scores_accuracy)
str(data$review_scores_accuracy)
data$review_scores_rating = as.numeric(unfactor(data$review_scores_rating))
data$review_scores_cleanliness = as.numeric(unfactor(data$review_scores_cleanliness))
data$review_scores_checkin = as.numeric(unfactor(data$review_scores_checkin))
data$review_scores_communication = as.numeric(unfactor(data$review_scores_communication))
data$review_scores_location = as.numeric(unfactor(data$review_scores_location))
data$review_scores_value = as.numeric(unfactor(data$review_scores_value))

##
write.csv(data1,'airbb.csv')
##


z = unfactor(data$zipcode)

for (i in seq_along(z)){
  if(str_detect(z[i],'NY ')){ 
    z[i] = str_sub(z[i],4)
  }
  }
data$zip = z
library(fastDummies)

data$superhost = unfactor(data$host_is_superhost)
data$instant = unfactor(data$instant_bookable)

dummy_cols(data,select_columns = c("superhost","instant")) -> data
b = data$instant
b
for (i in seq_along(b)){
  if(b[i] == "f"){
    b[i] = 0
  }
  else{
    b[i] = 1
  }
}
data$instant = as.numeric(b)
h = data$superhost
for(i in seq_along(h)){
  if(h[i] == "f"){
    h[i] = 0
  }
  else if(h[i] == "t"){
    h[i] = 1
  }
  else{
    h[i] = NA
  }
}
data$superhost = as.numeric(h)
data1 = data[,1:41]
data1 = data1 %>%
  select(-host_is_superhost,-instant_bookable)
str(data1)

dplyr::select_if(data1,is.numeric) -> data2
data3 <- data2 %>%
  select(-id,-host_id)
d_cor = cor(data5)
corrplot(d_cor, 
         method = "color", 
         type="upper", 
         diag = F)

p = prcomp(data4)
data4 <- data3[complete.cases(data3),]
skimr::skim(data4)
price = data4$price
data4 %>%
  select(-price) -> data4
data5 <- cbind(price,data4)
names(data5)

summary(p)
p$center

fviz_screeplot(p)
get_eigenvalue(p)
fviz_pca_contrib(p, choice="var")


library(purrr)
library(cluster)

a = scale(data4)
k = kmeans(a,centers = 5, nstart = 25, iter.max = 25)
fviz_cluster(k,data = a)


#------------------------------------------------ 
# 2.6.2020
data <- read.csv('airbb.csv')
data1 <- data[complete.cases(data),]
data_inc <- data[!complete.cases(data),]


#------------------------------ Incomplete Data Checking
skimr::skim(data1)
skimr::skim(data_inc)
  
sec_dep_med <- median(data$security_deposit,na.rm = TRUE)
  
data %>%
  filter(price >= 8000) %>%
  ggplot(aes(x = 1, y =price,col = as.factor(neighbourhood_group_cleansed))) +
  geom_boxplot()
  
  
  

data_r <- read.csv('ab_reviews.csv')
data2 <- data1 %>%
  select(-zipcode,-longitude,-latitude,-X)
View(data2)
fastDummies::dummy_cols(data2,select_columns = c("cancellation_policy",
                                                 "bed_type","room_type","property_type","neighbourhood_group_cleansed",
                                                 "host_response_time"),remove_first_dummy = TRUE) -> data3

# Split

data_clean$train <- sample(c(0, 1), nrow(data_clean), replace = TRUE, prob = c(.3, .7)) 
abb_train <- data_clean %>% 
  filter(train == 1) %>% 
  select(-train,-id,-host_id,-longitude,-latitude,-X,-zipcode,-zip)
abb_test <- data_clean %>% 
  filter(train == 0) %>% 
  select(-train,-id,-host_id,-longitude,-latitude,-X,-zipcode,-zip)
names(abb_train)
write.csv(abb_train, 'abb_train.csv')
write.csv(abb_test, 'abb_test.csv')

##### Regressions

train_p <- abb_train$price
test_p <- abb_test$price

abb_train1 <- abb_train %>%
  select(-price)
abb_test1 <- abb_test %>%
  select(-price)

## Linear
m1 <- lm(price ~ . , data = abb_train)
summary(m1)

#-------------------------------------------------------------------------------------------
# Lasso
lambda_seq <- 10^seq(2, -2, by = -.1)
abb_lasso_t <- abb_train1 %>%
  select(-neighbourhood_cleansed)
abb_lasso_tr <- abb_test1 %>%
  select(-neighbourhood_cleansed)
  #%>%
  #select(-cancellation_policy,-host_response_time,-neighbourhood_cleansed,-neighbourhood_group_cleansed,-bed_type,-room_type,-property_type,-zip,-listing_id)
skimr::skim(abb_lasso_t)
abb_lasso_t <- fastDummies::dummy_cols(abb_lasso_t,select_columns = c("bed_type","cancellation_policy",
                                                        "host_response_time","neighbourhood_group_cleansed",
                                                        "property_type","room_type"))
abb_lasso_t <- abb_lasso_t %>% 
  select_if(is.numeric)
abb_lasso_tr <- abb_lasso_tr %>%
  select_if(is.numeric)
abb_lasso_t <- as.matrix(abb_lasso_t)
abb_lasso_tr <- as.matrix(abb_lasso_tr)
abb_lasso_tr <- fastDummies::dummy_cols(abb_lasso_tr,select_columns = c("bed_type","cancellation_policy",
                                                                       "host_response_time","neighbourhood_group_cleansed",
                                                                       "property_type","room_type"))
# abb_lasso_tr <- abb_test1 %>%
#   select(-cancellation_policy,-host_response_time,-neighbourhood_cleansed,-neighbourhood_group_cleansed,-bed_type,
#          -room_type,-property_type,-zip,-listing_id)
# abb_lasso_tr <- as.matrix(abb_lasso_tr)

lambda_seq <- seq(10, 0, by = -.05)
las <- glmnet(abb_lasso_t,train_p,alpha = 1,lambda = lambda_seq)
summary(las)
las$lambda

p_las_hat <- predict(las,abb_lasso_t)
mse_las_train <- vector()
for (i in 1:ncol(p_las_hat)){
  mse_las_train[i] <-  mean((train_p - p_las_hat[,i])^2)
}


p_las_hat_test <- predict(las,abb_lasso_tr)
mse_las_test <- vector()
for (i in 1:ncol(p_las_hat_test)){
  mse_las_test[i] <-  mean((test_p - p_las_hat_test[,i])^2)
}


lambda_las_mse_train <- mse_las_train[which.min(mse_las_train)] 
lambda_las_mse_test <- mse_las_test[which.min(mse_las_test)] 


coef(las, s = lambda_las_mse_train)
lasso <- as.matrix(lasso_table)

cv_output <- cv.glmnet(abb_lasso_t,train_p,alpha = 1,lambda = lambda_seq)
best_lam <- cv_output$lambda.min

lasso_b <- glmnet(abb_lasso_t,train_p,alpha = 1,lambda = 8.6)
coef(lasso_b)
summary(lasso_b)

p_las_hatb <- predict(lasso_b,abb_lasso_t)
mse_las_trainb <- vector()
for (i in 1:ncol(p_las_hatb)){
  mse_las_trainb[i] <-  mean((train_p - p_las_hatb[,i])^2)
}

xgboost(abb_lasso_t,label = train_p,nrounds = 200) -> xg1
dygraphs::dygraph(xg1$evaluation_log)
xg1pred <- predict(xg1,abb_lasso_t)

xgboost(abb_lasso_tr,label = test_p,nrounds = 300) -> xg2
dygraphs::dygraph(xg2$evaluation_log)

xgbtrain <- xgb.DMatrix(abb_lasso_t,label = train_p)
xgbtest <- xgb.DMatrix(abb_lasso_tr,label = test_p)
watchlist <- list(train = xgbtrain,
                  eval = xgbtest)
param <- list(eta = 1, verbose = 0, nthread = 2,
              objective = "reg:squarederror")
xg4 <- xgb.train(param,xgbtrain,nrounds = 100, watchlist)
xgb.plot.tree(model = xg4)
dygraphs::dygraph(xg4$evaluation_log)

xgb.train(param,xgbtrain,nrounds = 300, watchlist,
          booster = "gblinear", 
        eval.metric = "rmse") -> xg3
xgb.train(abb_lasso_t,label = train_p,booster = "gblinear",eta = 0.05,
          nrounds = 300,)
dygraphs::dygraph(xg3$evaluation_log)
summary(xg3)
xg3$feature_names

xgb.plot.tree(xg3)

dd_mse <- tibble(
  lambda = las$lambda,
  mse = mse_las_train,
)

#----------------------------------------------------------------------

# PCA & RTsne
data4 <- select_if(data2,is.numeric)
abb_tsne = Rtsne(data4,verbose=TRUE, max_iter = 500, check_duplicates=FALSE)
tsne_proj = abb_tsne$Y

tsne_df = as.data.frame(tsne_proj)
colnames(tsne_df) = c("dim1", "dim2")
plot(tsne_df$dim1, tsne_df$dim2, type="p", pch=19)


abb_tsne1 = Rtsne(data2,verbose=TRUE, max_iter = 500, check_duplicates=FALSE)
tsne_proj1 = abb_tsne1$Y
tsne_df1 = as.data.frame(tsne_proj1)
colnames(tsne_df1) = c("dim1", "dim2")
plot(tsne_df1$dim1, tsne_df1$dim2, type="p", pch=19)

data5 <- cbind(data2,tsne_df1)

ggplot(data5,aes(dim1,dim2)) +
  geom_point(aes(colour = as.factor(neighbourhood_group_cleansed)), alpha=.5) + 
  scale_colour_brewer(palette = "Set1") + 
  theme_bw() + 
  labs(title="Neighbourhood_Group")

ggplot(data5,aes(dim1,dim2)) +
  geom_point(aes(colour = as.factor(room_type)), alpha=.5) + 
  scale_colour_brewer(palette = "Set1") + 
  theme_bw() + 
  labs(title="Room_Type")

## back to regression
lmd <- cbind(abb_train1,train_p)
lmdt <- cbind(abb_test1,test_p)
m2 <- lm(train_p ~ . , data = lmd)
m2t <- lm(test_p~.,data = lmdt)
summary(m2)
summary(m2t)



## Doing some review joining, text analysis
data2$listing_id = data2$id
review <- semi_join(data_r,data2,by = "listing_id")

review_corpus <- corpus(review$comments)
kwic(review_corpus,pattern = phrase("fake")) -> fake
fake

review[816181,]
review[806392,]
review[651297,]
review[456115,]

data$X

m3 <- lm(adjusted_price ~ .,data = d)
summary(m3)

data2$adjusted_price = ((data2$price * data2$minimum_nights) + data2$cleaning_fee)/data2$minimum_nights
d <- data2 %>%
  select(-id,-host_id,-listing_id,-price,-zip,-neighbourhood_cleansed) 
d <- fastDummies::dummy_cols(d,select_columns = c("cancellation_policy",
                        "bed_type","room_type","property_type","neighbourhood_group_cleansed",
                        "host_response_time"),remove_first_dummy = TRUE)
names(d)
d <- d %>%
  select(-property_type,-room_type,-bed_type,-cancellation_policy,-neighbourhood_group_cleansed,-host_response_time)
d$accommodates
loadhistory(file = "cap")



#------------------------------2.10 Checking November Data

ggplot(data, aes(x = as.factor(property_type), y = price, col = as.factor(room_type))) +
  geom_boxplot() +
  facet_wrap(~property_type)

ggplot(data, aes(x = as.factor(accommodates), y = price, col = as.factor(room_type),
                 group = as.factor(room_type))) +
  geom_smooth(se = FALSE) +
  stat_summary()

  
nov_init <- read.csv("ABBNOV19.csv")
nov_init$host_response_rate = as.numeric(nov_init$host_response_rate)
com <- left_join(data,nov_init, by = "id")
names(data) -> names_list
names_list = names_list[1:37]
names_list = names_list[2:37]
nov_init1 <- nov_init %>%
  select(id,host_id,host_response_time,neighbourhood_group_cleansed,
                       zipcode,latitude,      
                      longitude,      property_type,               
                       room_type            ,        accommodates,                
                      bathrooms             ,       bedrooms ,                   
                       beds                 ,       bed_type,                    
                       price                 ,       security_deposit  ,          
                       cleaning_fee        ,        minimum_nights   ,           
                       maximum_nights    ,       availability_30      ,       
                       availability_60       ,     availability_90 ,            
                       availability_365     ,       number_of_reviews  ,         
                     number_of_reviews_ltm     , review_scores_rating   ,     
                       review_scores_accuracy  ,     review_scores_cleanliness  ,
                       review_scores_checkin   ,     review_scores_communication ,
                       review_scores_location  ,    review_scores_value         ,
                       cancellation_policy      ,    reviews_per_month )
class(names_list)

datax <- data %>%
  select(-X,-zip,-superhost,-instant)
com <- left_join(datax,nov_init1,by = "id")

skimr::skim(com)

data1 %>%
  group_by(property_type) %>%
  summarize(avg_dep = mean(security_deposit),
            avg_price = mean(price)) -> mean_sec_dep
mean_sec_dep

data_clean <- data 
data_clean$security_deposit[is.na(data_clean$security_deposit)] <- 0
data_clean$cleaning_fee[is.na(data_clean$cleaning_fee)] <- 0
data_clean$superhost[is.na(data_clean$superhost)] <- 0
data_clean$review_scores_rating[is.na(data_clean$review_scores_rating)] <- 0.00001
data_clean$review_scores_cleanliness[is.na(data_clean$review_scores_cleanliness)] <- 0.00001
data_clean$review_scores_checkin[is.na(data_clean$review_scores_checkin)] <- 0.00001
data_clean$review_scores_communication[is.na(data_clean$review_scores_communication)] <- 0.00001
data_clean$review_scores_location[is.na(data_clean$review_scores_location)] <- 0.00001
data_clean$review_scores_value[is.na(data_clean$review_scores_value)] <- 0.00001
data_clean$reviews_per_month[is.na(data_clean$reviews_per_month)] <- 0.00001

data_clean = data_clean %>%
  select(-zipcode)
data_clean_withreviews = data_clean[complete.cases(data_clean),]
data_clean_woreviews = data_clean[!complete.cases(data_clean),]

write.csv(data_clean,'complete_ab_Dec.csv')



#--------------------------------------------- Regression Analysis
data_lm <- data_clean %>%
  select(-X,-id,-host_id,-latitude,-longitude,-zip,-zipcode,-neighbourhood_cleansed)
lm1 <- lm(price ~ ., data_lm)
summary(lm1)




#------------------------------------------------ Topic Modeling
data_clean$listing_id = data_clean$id
with_reviews <- full_join(data_clean,df2,by = "listing_id")

r <- with_reviews %>%
  select(listing_id,date,comments,price,host_id)

r_ngram2 = r %>%
  unnest_tokens(token,comments,token = "ngrams",n = 2)
r_ngram2 %>%
  count(token, sort = T) %>%
  print(n = 20)

r_clean = r[complete.cases(r),]
r_clean = mutate(r_clean, comment_id = rownames(r_clean))
r_sample = r_clean[1:50000,]

r_tidy =  r_sample %>%
  unnest_tokens(tokens, comments, token="words",
                    drop=F) %>%
  anti_join(get_stopwords(source="smart"),
           by=c("tokens"="word"))

r_tidy %>%
  count(tokens, sort=T) %>% print(n=20)


get_sentiments("loughran")
get_sentiments("bing")
get_sentiments("afinn")


write.csv(r_clean,"clean_review.csv")
write.csv(r_tidy,"sample_review.csv")
# r_clean %>%
#   group_by(listing_id) %>%
#   summarize(avg_p = mean(price))

r_tidy <- read.csv("sample_review.csv")
r_clean <- read.csv("clean_review.csv")

sent_bing = inner_join(r_tidy,
                       get_sentiments("bing"),
                       by = c("tokens" = "word"))

sent_bing2 = sent_bing %>%
  count(comment_id,tokens,sentiment) %>%
  pivot_wider(names_from = sentiment,
              values_from = n,
              values_fill = list(n = 0))

abb_bing = sent_bing2 %>%
  group_by(comment_id) %>%
  summarize(pos = sum(positive),
            neg = sum(negative)) %>%
  mutate(polarity = pos - neg)

abb_bing1 = inner_join(r_sample,abb_bing)
ggplot(abb_bing1,aes(x = price,y = polarity)) + geom_smooth(se = FALSE)
abb_bing1$date = ymd(abb_bing1$date)

abb_bing1$month = month(abb_bing1$date)
abb_bing1$year = year(abb_bing1$date)
abb_bing1 %>%
  filter(year == 2019) %>%
  group_by(month,listing_id)%>%
  summarize(count = n()) -> month19
abb_bing1 %>%
  filter(year == 2019) %>%
  inner_join(month19) %>%
  ggplot(aes(x = date, y = polarity, color = log(price),size = count)) +
  geom_point(alpha = 0.5) %>%
  ggplot(aes(x = date, y = ))

ggplot(abb_bing1,aes(x = date, y = polarity, color = as.factor(polarity),size = count(comment_id))) +
  geom_point()



abb_bing1 %>%
  filter(listing_id %in% c(72265,74073,
                           74333,74680,
                           74821  ,74860 ,
                           75193 , 75635,
                           76761  ,77765,
                           77936 , 78919 ,
                           79782 , 80493 ,
                           80684)) %>%
  view()
  # ggplot(aes(x = date, y = log(price), color = as.factor(listing_id))) + geom_point()


sample_comments = r_sample$comments
sample_sentences = get_sentences(sample_comments)
sample_sentr = sentiment(sample_sentences)
abb_sentr = with(r_sample,
                 sentiment_by(get_sentences(comments),
                              list(listing_id,comment_id,price)))
write.csv(abb_sentr,"abb_sent.csv")
head(abb_sentr)
  
ggplot(abb_sentr, aes(x=word_count, y=ave_sentiment)) +
  geom_line() 

ggplot(abb_sentr,aes(x = word_count,y = ave_sentiment)) +
  geom_point(position = "jitter",alpha = 0.3)+
  geom_smooth(se = FALSE) +
  labs(title = "Most of the Reviews Are Short and Neutral",
       subtitle = "Longer Reviews Tend to be More Negative")

highlight(abb_sentr)



#-----------------------For new listings, should we ignore rating?

# lasso_cp <- data_clean_withreviews$price
# data_clean_las <- data_clean_withreviews %>%
#   select(-X,-id,-host_id,-price,-neighbourhood_cleansed,-longitude,-latitude)
# las_clean <- fastDummies::dummy_cols(data_clean_las,select_columns = c("bed_type","cancellation_policy",
#                                                                       "host_response_time","neighbourhood_group_cleansed",
#                                                                       "property_type","room_type"))
# las_clean = las_clean %>%
#   select_if(is.numeric) %>%
#   as.matrix()
# 
# lambda_seq <- seq(10, 0, by = -.05)
# las <- glmnet(las_clean,lasso_cp,alpha = 1,lambda = lambda_seq)
# summary(las)
# las$lambda
# 
# p_las_hat <- predict(las,las_clean)
# mse_las_train <- vector()
# for (i in 1:ncol(p_las_hat)){
#   mse_las_train[i] <-  mean((lasso_cp - p_las_hat[,i])^2)
# }
# lambda_las_mse_train <- mse_las_train[which.min(mse_las_train)] 
# 
# coef(las, s = lambda_las_mse_train)

#### Ignore rating...not significant in LASSO



#------------------------------------text analysis, back to it

fake <- read.csv("bq_fake.csv")
fake_sentr = with(fake,
                 sentiment_by(get_sentences(comments),
                              list(listing_id,date,id)))
ggplot(fake_sentr, aes(x=word_count, y=ave_sentiment)) +
  geom_line() 

ggplot(fake_sentr,aes(x = word_count,y = ave_sentiment)) +
  geom_point(position = "jitter")+
  geom_smooth(se = FALSE)

highlight(fake_sentr)


#--------------------------------------- breaking up reviews

list1 <- unique(df2$listing_id)[1:8076]
list2 <- unique(df2$listing_id)[8077:16153]
list3 <- unique(df2$listing_id)[16154:24230]
list4 <- unique(df2$listing_id)[24231:32306]
list5 <- unique(df2$listing_id)[32307:40379]

review_1 <- df2 %>%
  filter(listing_id %in% list1)

comp_list_id = unique(df2$listing_id)
sam1_id = sample(comp_list_id,100)
review_sample <- df2 %>%
  filter(listing_id %in% sam1_id)
write.csv(review_sample,"888_sample_review.csv")
# SPLITTING REVIEWS
list1 = sample(comp_list_id,8076)

# Handling other variables

# break verifications
length(strsplit(paste(df1$host_verifications[1], collapse = " "), ' ')[[1]])



### Mapping
sites <- data.frame(longitude = df1$longitude,
                    latitude = df1$latitude)
world <- ne_countries(scale = "medium", returnclass = "sf")
ggplot(data = world,lwd = 0.5) +
  geom_sf(fill = "white",lwd = 0.5) +
  geom_point(data = sites, aes(x = longitude, y = latitude), size = 0.5, 
             shape = 19, color = "#FF5A5F") +
  coord_sf(xlim = c(-74.3, -73.7), ylim = c(40.45,40.95), expand = FALSE) +
  labs(title = "Mapping of New York Airbnb Listings")

other_featrues <- data.frame(neighborhood = factor(df1$neighbourhood_group_cleansed),
                             scores = df1$review_scores_rating,
                             num_of_reviews = df1$number_of_reviews,
                             property_type = df1$property_type)
mapping = cbind(sites,other_featrues)

ggplot(data = world,lwd = 0.5) +
  geom_sf(fill = "white",lwd = 0.5) +
  geom_point(data = mapping, aes(x = longitude, y = latitude,group = neighborhood,color = neighborhood), size = 0.5, 
             shape = 19) +
  coord_sf(xlim = c(-74.3, -73.7), ylim = c(40.45,40.95), expand = FALSE) +
  labs(title = "Mapping of New York Airbnb Listings by Neighborhood") +
  scale_color_manual(values = c("#FF5A5F","#00A699","#FC642D","#484848","#b4bec2","#4c6a78"))


#### Random Forest
names(complete_entry) = gsub(" ","_",names(complete_entry))
names(complete_entry) = gsub("-","_",names(complete_entry))
names(complete_ntry) = gsub("/","_",names(complete_entry))
names(complete_entry) = gsub("()","",names(complete_entry))
names(complete_entry) = gsub(")","",names(complete_entry))
complete_entry1 = complete_entry %>%
  select(-`property_type_Casa_particular_(Cuba`)
write.csv(complete_entry1,"rf.csv")
rf1 <- randomForest(price ~ . , data = complete_entry1[1:10000,], importance = TRUE, proximity = TRUE)


###################### additional EDA
df1$p = substring(df1$price,2)
df1$p = gsub(",","",df1$p)
df1$price = as.numeric(df1$p)
df1 %>%
  select(price, availability_365,number_of_reviews,
         review_scores_rating,beds,bathrooms) -> EDA
EDA %>%
  ggplot() + geom_histogram() +
  facet_grid(~, scales = "free")

ggplot(df1,aes(x = factor(neighbourhood_group_cleansed), y = log(price),
               fill = factor(neighbourhood_group_cleansed))) +
  geom_boxplot()  +
  labs(title = "Log Price Distribution by Neighborhood") +
  scale_fill_manual(values = c("#FF5A5F","#00A699","#FC642D","#484848","#b4bec2","#4c6a78"))
       #geom_boxplot()
df1 %>%
  group_by(property_type) %>%
  tally() %>%
  filter(n > 100) -> property_list
df1 %>%
  filter(property_type %in% property_list$property_type) %>%
  ggplot(aes(x = log(price),fill = "#484848")) +
  geom_histogram() +
  facet_wrap(~ factor(property_type),nrow = 3) +
  theme(legend.position = 'none') +
  labs(title = "Property Type with More than 100 Observations")+
  scale_y_continuous(breaks=c(0,1,10,30,100,300,1000,6000), trans="log1p")


ggplot(df1,aes(x = review_scores_rating),fill = factor(host_is_superhost)) +
  geom_histogram(position = "dodge",binwidth = 10,aes(fill = factor(host_is_superhost))) +
  scale_fill_manual(values = c("#FF5A5F","#00A699","#FC642D","#484848","#b4bec2","#4c6a78")) +
  labs(title = "Most of the Reviews Score Above 80")



##### 
luxury %>%
  group_by(property_type) %>%
  tally()%>%
  arrange(-n)
summary(luxury)
skimr::skim(luxury)
skimr::skim(df1)
luxury_clean <- luxury
luxury_clean$security_deposit[is.na(luxury_clean$security_deposit)] <- 0
luxury_clean$cleaning_fee[is.na(luxury_clean$cleaning_fee)] <- 0
xgluxury = xgb.train(params = param,xgbtrain,nrounds = 300,watchlist = watchlist,verbose = 1, early_stopping_rounds = 5)

luxury_clean$review_scores_rating[is.na(luxury_clean$review_scores_rating)] <- 0
luxury_clean$review_scores_cleanliness[is.na(luxury_clean$review_scores_cleanliness)] <- 0
luxury_clean$review_scores_checkin[is.na(luxury_clean$review_scores_checkin)] <- 0
luxury_clean$review_scores_communication[is.na(luxury_clean$review_scores_communication)] <- 0
luxury_clean$review_scores_location[is.na(luxury_clean$review_scores_location)] <- 0
luxury_clean$review_scores_value[is.na(luxury_clean$review_scores_value)] <- 0
luxury_clean$reviews_per_month[is.na(luxury_clean$reviews_per_month)] <- 0
luxury_clean$review_scores_accuracy[is.na(luxury_clean$review_scores_accuracy)] <- 0

##################### More Cleaning and Summary Work
n = as.vector(names(df1))
n1 = str_detect(n,c('*url*','*scrape*'))
n = data.frame(n)
n$label = ifelse(n1 == FALSE,1,0)
df11 = rbind(df11,n1)          
#df11 = df11 %>%
 # select()

df_clean = df1 %>%
  select(id,host_id,host_since,host_response_time,
         host_response_rate,host_acceptance_rate,host_is_superhost,host_listings_count,host_total_listings_count,
         host_verifications,host_identity_verified,street,neighbourhood,neighbourhood_cleansed,
         neighbourhood_group_cleansed,city,zipcode,property_type,room_type,accommodates,
         bathrooms,bedrooms,beds,bed_type,amenities,square_feet,price,weekly_price,monthly_price,security_deposit,
         cleaning_fee,guests_included,extra_people,minimum_nights,maximum_nights,calendar_updated,has_availability,
         availability_30,avail
         ability_60,availability_90,availability_365,number_of_reviews,first_review,last_review,
         review_scores_accuracy,review_scores_checkin,review_scores_cleanliness,review_scores_communication,review_scores_location,
         review_scores_rating,review_scores_value,reviews_per_month,instant_bookable,cancellation_policy,require_guest_phone_verification,
         require_guest_profile_picture)

fake %>%
  filter(comments %in% 'Justice')



# df_clean operations, EDA
# -----------------------------------------------------
df_clean$id = as.numeric(df_clean$id)
df_clean$host_since = date(df_clean$host_since)

t = as.vector(df_clean$price)


for (i in seq_along(t)) {
  t[i] = str_sub(t[i],2)
}
df_clean$price = t
df_clean$price = as.numeric(df_clean$price)
df_clean = df_clean[complete.cases(df_clean$price),]

#-----------------small look up ------------------------------------
no_bedrooms = df_clean[!complete.cases(df_clean$bedroom),]
ggplot(no_bedrooms, aes(as.numeric(square_feet))) + geom_histogram()


#------------------continue cleaning
skimr::skim(df_clean)

t = as.vector(df_clean$security_deposit)


for (i in seq_along(t)) {
  t[i] = str_sub(t[i],2)
}
df_clean$security_deposit = t
df_clean$security_deposit = as.numeric(df_clean$security_deposit)
#df_clean = df_clean[complete.cases(df_clean$price),]


wrap_char_num <- function(x){
  t = as.vector(x)
  for (i in seq_along(t)) {
    if(t[i] != ""){
    t[i] = str_sub(t[i],2)
    t[i] = as.numeric(t[i])
    }
    else{
      t[i] = NA
    }
  }
  return(t)
}
t = wrap_char_num(df_clean$extra_people)
df_clean$extra_people = as.numeric(t)

df_clean$security_deposit[is.na(df_clean$security_deposit)] <- 0
df_clean$cleaning_fee[is.na(df_clean$cleaning_fee)] <- 0

t = as.vector(df_clean$cleaning_fee)


for (i in seq_along(t)) {
  t[i] = str_sub(t[i],2)
}
df_clean$cleaning_fee = t
df_clean$cleaning_fee = as.numeric(df_clean$cleaning_fee)


df_clean$cleaning_fee[is.na(df_clean$cleaning_fee)] <- 0
df_clean$bathrooms[is.na(df_clean$bathrooms)] <- 0
df_clean$bedrooms[is.na(df_clean$bedrooms)] <- 0
df_clean$beds[is.na(df_clean$beds)] <- 1
df_clean = df_clean %>%
  select(-square_feet,-zipcode,-neighbourhood)

df_clean$reviews_per_month[is.na(df_clean$reviews_per_month)] <- 0
df_clean$review_scores_rating[is.na(df_clean$review_scores_rating)] <- 0
df_clean$review_scores_cleanliness[is.na(df_clean$review_scores_cleanliness)] <- 0
df_clean$review_scores_checkin[is.na(df_clean$review_scores_checkin)] <- 00
df_clean$review_scores_communication[is.na(df_clean$review_scores_communication)] <- 0
df_clean$review_scores_location[is.na(df_clean$review_scores_location)] <- 0
df_clean$review_scores_value[is.na(df_clean$review_scores_value)] <- 0
df_clean$reviews_per_month[is.na(df_clean$reviews_per_month)] <- 0
df_clean$review_scores_accuracy[is.na(df_clean$review_scores_accuracy)] <- 0
df_clean$host_listings_count[is.na(df_clean$host_listings_count)] <- 0
df_clean$host_total_listings_count[is.na(df_clean$host_total_listings_count)] <- 0

df_clean$price = gsub("\\,","",df_clean$price)
df_clean$weekly_price = gsub("\\,","",df_clean$weekly_price)
df_clean$monthly_price = gsub("\\,","",df_clean$monthly_price)
df_clean$cleaning_fee = gsub("\\,","",df_clean$cleaning_fee)
df_clean$security_deposit = gsub("\\,","",df_clean$security_deposit)
df_clean$extra_people = gsub("\\,","",df_clean$extra_people)
df_clean$host_response_rate = gsub("\\%","",df_clean$host_response_rate)

df_clean$accommodates = as.numeric(df_clean$accommodates)

wrap_percent_num <- function(x){
  t = as.vector(x)
  for (i in seq_along(t)) {
    if(t[i] != "" & t[i] != "N/A"){
      t[i] = as.numeric(t[i])/100
    }
    else{
      t[i] = NA
    }
  }
  return(t)
}
t = wrap_percent_num(df_clean$host_response_rate)
df_clean$host_response_rate= as.numeric(t)
df_clean$require_guest_phone_verification = ifelse(df_clean$require_guest_phone_verification == "t",1,0)
df_clean$host_is_superhost = ifelse(df_clean$host_is_superhost == "t",1,0)
df_clean$host_identity_verified = ifelse(df_clean$host_identity_verified== "t",1,0)
df_clean$require_guest_profile_picture= ifelse(df_clean$require_guest_profile_picture== "t",1,0)
df_clean$instant_bookable = ifelse(df_clean$instant_bookable== "t",1,0)

wrap_length_num <- function(x){
  t = as.vector(x)
  y = vector()
  for (i in seq_along(t)){
    y[i] = lengths(strsplit(t[i],"\\,"))
  }
  return(y)
}
x = df_clean$host_verifications
x = wrap_length_num(x)
df_clean$host_verifications = x

wrap_date<- function(x){
  t = as.vector(x)
  y = vector()
  for (i in seq_along(t)){
    if (t[i] == ""){
      y[i] = NA
    } else{
    y[i] = as.Date(t[i])
    }
    print(y[i])
  }
  return(y)
}

x = df_clean$first_review[1:100]
x = wrap_date(x)


# --------------------cont eda
x = df_clean$accommodates
y = df_clean$bathrooms
z = df_clean$price
q = factor(df_clean$property_type)

fit <- lm(z ~ x + y)
grid.lines = 26
x.pred <- seq(min(x), max(x), length.out = grid.lines)
y.pred <- seq(min(y), max(y), length.out = grid.lines)
xy <- expand.grid( x = x.pred, y = y.pred)
z.pred <- matrix(predict(fit, newdata = xy), 
                 nrow = grid.lines, ncol = grid.lines)
# fitted points for droplines to surface
fitpoints <- predict(fit)

scatter3d(price ~ accommodates + bathrooms | neighbourhood_group_cleansed,data = df_c,pch = 18, cex = 2, 
          theta = 20, phi = 20, ticktype = "detailed",
          xlab = "accommodates", ylab = "bathrooms", zlab = "price",surface = F,
           main = "Airbnb NYC 2019")
df_c = df_clean %>%
  filter(price <= 1000)

df_clean = read.csv("888_clean.csv")

df_clean %>%
  filter(minimum_nights <= 30) %>%
  ggplot(aes(x = minimum_nights)) + geom_histogram()
df_clean %>%
  filter(minimum_nights > 14) %>%
  count()
  #ggplot(aes(x = minimum_nights)) + geom_histogram() + xlim(c(30,1000))

#---------------------  LASSO MAT

df_short = df_clean %>%
  select(-has_availability) 



#--------------------------------------------------------------
monthly <- df[complete.cases(df$monthly_price),]
weekly <- df[complete.cases(df$weekly_price),]

monthly %>% select(price,monthly_price)
summary(monthly$minimum_nights)
summary(df[!complete.cases(df$monthly_price),]$minimum_nights)

less <- df[df$minimum_nights < 30,]
more <- df[df$minimum_nights >= 30,]

less = less %>% select(-df.latitude,-df.longitude,-X,-X.1,
                       -has_availability,-host_since,-first_review,-last_review)


less = less %>% filter(!property_type %in% c("Barn","Bus","Cabin","Castle","Dorm",
                                             "Dome house","Farm stay","Houseboat",
                                             "Island","Yurt","Cave","Casa particular (Cuba)"))
less$train <- sample(c(0, 1), nrow(less), replace = TRUE, prob = c(.3, .7)) 
abb_train <- less %>% 
  filter(train == 1) 
abb_test <- less %>% 
  filter(train == 0) 
names(abb_train)
write.csv(abb_train, '888_train.csv')
write.csv(abb_test, '888_test.csv')

