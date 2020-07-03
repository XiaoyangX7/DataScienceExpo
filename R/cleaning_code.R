## df1 id supposed to be original listing file...IDK if "888.csv" works



df_clean = df1 %>%
  select(id,host_id,host_since,host_response_time,
         host_response_rate,host_acceptance_rate,host_is_superhost,host_listings_count,host_total_listings_count,
         host_verifications,host_identity_verified,street,neighbourhood,neighbourhood_cleansed,
         neighbourhood_group_cleansed,city,zipcode,property_type,room_type,accommodates,
         bathrooms,bedrooms,beds,bed_type,amenities,square_feet,price,weekly_price,monthly_price,security_deposit,
         cleaning_fee,guests_included,extra_people,minimum_nights,maximum_nights,calendar_updated,has_availability,
         availability_30,availability_60,availability_90,availability_365,number_of_reviews,first_review,last_review,
         review_scores_accuracy,review_scores_checkin,review_scores_cleanliness,review_scores_communication,review_scores_location,
         review_scores_rating,review_scores_value,reviews_per_month,instant_bookable,cancellation_policy,require_guest_phone_verification,
         require_guest_profile_picture)




df_clean$id = as.numeric(df_clean$id)
df_clean$host_since = date(df_clean$host_since)

t = as.vector(df_clean$price)


for (i in seq_along(t)) {
  t[i] = str_sub(t[i],2)
}
df_clean$price = t
df_clean$price = as.numeric(df_clean$price)
df_clean = df_clean[complete.cases(df_clean$price),]

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
df_clean$review_scores_checkin[is.na(df_clean$review_scores_checkin)] <- 0
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

### use wrap length on amenities as well
## deselect has availability

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