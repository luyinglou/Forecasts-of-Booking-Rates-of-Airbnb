data <- airbnb_test_x

data$X1=NULL

colnames(data)

#accomadates
table(data$accommodates)
typeof(data$accommodates)
data$accommodates=ifelse(is.na(data$accommodates),10,data$accommodates)
data$accommodates=ifelse(data$accommodates=='t',10,data$accommodates)
sum(is.na(data$accommodates))
data$accommodates=as.numeric(data$accommodates)
typeof(data$accommodates)

#availbility
data$availability_30=as.numeric(data$availability_30)
table(data$availability_30)
sum(is.na(data$availability_30))
data$availability_30=ifelse(is.na(data$availability_30),median(data$availability_30,na.rm = TRUE),data$availability_30)

data$availability_60=as.numeric(data$availability_60)
table(data$availability_60)
sum(is.na(data$availability_60))
data$availability_60=ifelse(is.na(data$availability_60),median(data$availability_60,na.rm = TRUE),data$availability_60)

data$availability_90=as.numeric(data$availability_90)
table(data$availability_90)
sum(is.na(data$availability_90))
data$availability_90=ifelse(is.na(data$availability_90),median(data$availability_90,na.rm = TRUE),data$availability_90)

data$availability_365=as.numeric(data$availability_365)
table(data$availability_365)
sum(is.na(data$availability_365))
data$availability_365=ifelse(is.na(data$availability_365),median(data$availability_365,na.rm = TRUE),data$availability_365)


##person/per bed
data$accommodates=as.numeric(data$accommodates)
data$beds=as.numeric(data$beds)
sum(is.na(data$beds))
data$beds=ifelse(is.na(data$beds),mean(data$beds,na.rm = TRUE),data$beds)

data$bed_person = data$accommodates/data$beds
sum(is.na(data$bed_person))
table(data$bed_person)
data$bed_person=ifelse(data$bed_person=='Inf',1,data$bed_person)


#share_bathroom
data$bathrooms=as.numeric(data$bathrooms)
sum(is.na(data$bathrooms))
data$bathrooms=ifelse(is.na(data$bathrooms),mean(data$bathrooms,na.rm = TRUE),data$bathrooms)

data$bedrooms=as.numeric(data$bedrooms)
data$bedrooms=ifelse(is.na(data$bedrooms),mean(data$bedrooms,na.rm = TRUE),data$bedrooms)
sum(is.na(data$bedrooms))

data$share_bathroom = ifelse(data$bathrooms<data$bedrooms,1,0)
table(data$share_bathroom)
sum(is.na(data$share_bathroom))

##reducing factors of cancellation policy

table(data$cancellation_policy )
data$cancel_policy = ifelse((data$cancellation_policy=="no_refunds") | (data$cancellation_policy=="strict") | (data$cancellation_policy=="super_strict_30") | (data$cancellation_policy=="super_strict_60"),1,ifelse(data$cancellation_policy=='moderate',2,3))
table(data$cancel_policy )
sum(is.na(data$cancel_policy))

###city

table(data$city_name)
data$city_popular = ifelse(data$city_name=="- Are you planning on filming or having a party here? Message us before booking. Filming rates start at $500 with minimal crew, but please let us know ahead of time or we will ask you to leave. Please no more people than you actually book the place for at any time. No smoking inside, keep the music down after 9pm. This is a peaceful neighborhood. It's important that we keep that vibe alive for our neighbors. Please keep noise down to a level that won't bother anyone. Thank you!",'Austin', data$city_name)

table(data$city_popular)
sum(is.na(data$city_popular))
data$city_popular = ifelse(is.na(data$city_popular),'Los Angeles',data$city_popular)
data$city_popular=ifelse(data$city_popular=='Asheville',1,
                                ifelse(data$city_popular=='Austin',2,
                                       ifelse(data$city_popular=='Boston',3,
                                              ifelse(data$city_popular=='Chicago',4,
                                                     ifelse(data$city_popular=='Denver',5,
                                                            ifelse(data$city_popular=='Los Angeles',6,
                                                                   ifelse(data$city_popular=='Nashville',7,
                                                                          ifelse(data$city_popular=='New Orleans',8,
                                                                                 ifelse(data$city_popular=='New York',9,
                                                                                        ifelse(data$city_popular=='Oakland',10,
                                                                                               ifelse(data$city_popular=='Portland',11,
                                                                                                      ifelse(data$city_popular=='San Diego',12,
                                                                                                             ifelse(data$city_popular=='San Francisco',13,
                                                                                                                    ifelse(data$city_popular=='Santa Cruz',14,
                                                                                                                           ifelse(data$city_popular=='Seattle',15,16
                                                                                                                                  
                                                                                                                           )))))))))))))))










##data$city_popular = ifelse(data$city_name %in% c("Nashville", "Los Angeles","San Diego","Washington DC","New Orleans","San Francisco","New York","Seattle","Chicago","Boston","Austin","Portland","Denver","Santa Cruz","Oakland","Asheville"),data$city_name,'Other')
###cleaning fee column
data$cleaning_fee = substr(data$cleaning_fee,2,1000)
data$cleaning_fee = as.numeric(data$cleaning_fee)
table(data$cleaning_fee)
data$cleaning_fee=ifelse(is.na(data$cleaning_fee),mean(data$cleaning_fee,na.rm = TRUE),data$cleaning_fee)
sum(is.na(data$cleaning_fee))

##description
description_len = c()

for (i in 1:nrow(data))
{
  m = length(strsplit(data$description[i], " ")[[1]])
  description_len = c(description_len,m)
  
}

data$description_len = description_len

table(data$description_len)
sum(is.na(data$description_len))

##extra people column
data$extra_people = substr(data$extra_people,2,1000)
data$extra_people=as.numeric(data$extra_people)

table(data$extra_people)
sum(is.na(data$extra_people))


##host

data$host_is_superhost = ifelse(data$host_is_superhost=='TRUE',1,0)
table(data$host_is_superhost)
sum(is.na(data$host_is_superhost))
data$host_is_superhost=ifelse(is.na(data$host_is_superhost),1,data$host_is_superhost)


#host listing count
table(data$host_listings_count)
sum(is.na(data$host_listings_count))
data$host_listings_count=as.numeric(data$host_listings_count)
data$host_listings_count = ifelse(data$host_listings_count=="The neighborhood is residential and you can walk to cafes, Whole Foods, coffee shops, bars, gyms. Walk or bike to Santa Monica Beach with dedicated bike lanes and sidewalks. Easy access to the 1-10 freeway to get anywhere. The best nearby nightlife is in Ocean Park (mostly along Main Street) and Venice (Abbott Kinney Blvd.). Also convenient to West Hollywood/Beverly Hills!",40, data$host_listings_count)

data$host_listings_count = ifelse(data$host_listings_count=="This house is located on Mt Washington 4 miles from downtown in a quiet neighborhood, with the property owners in very close proximity. Please inform us of anything which may cause them or other neighbors even a slight disturbance.",100,data$host_listings_count)
data$host_listings_count=ifelse(is.na(data$host_listings_count),mean(data$host_listings_count,na.rm = TRUE),data$host_listings_count)


##response
table(data$host_response_time)
data$host_response = ifelse(data$host_response_time=='within an hour',1,ifelse(data$host_response_time=='within a few hours',2,3))
sum(is.na(data$host_response))
table(data$host_response)

##impute response time
host_data = data[,c("host_is_superhost","host_listings_count","host_experience","host_response")]
library(mice)
init = mice(host_data, maxit=0) 
meth = init$method
predM = init$predictorMatrix
meth[c("host_is_superhost")] = ""
meth[c("host_listings_count")] = ""
meth[c("host_experience")] = ""
meth[c("host_response")] = "pmm"

set.seed(103)
imputed = mice(host_data, method=meth, predictorMatrix=predM, m=5)
imputed = complete(imputed)
host_data = imputed
sum(is.na(host_data$host_response))

data$host_response=host_data$host_response
sum(is.na(data$host_response))


#host experience
host_since_year = as.numeric(substr(data$host_since,1,4))
data$host_experience = 2019 - host_since_year
table(data$host_experience)
data$host_experience=ifelse(is.na(data$host_experience),6,data$host_experience)
sum(is.na(data$host_experience))

##instant bookaable

data$instant_bookable = ifelse(data$instant_bookable=='TRUE',1,0)
table(data$instant_bookable)
sum(is.na(data$instant_bookable))
data$instant_bookable=ifelse(is.na(data$instant_bookable),0,data$instant_bookable)


###price per person
data$price = gsub("\\$","",data$price)
data$price = gsub(",","",data$price)
data$price = as.numeric(data$price)
data$price_person = data$price/data$accommodates
table(data$price_person)
sum(is.na(data$price_person))
data$price_person=ifelse(data$price_person=='Inf',0,data$price_person)
data$price_person=ifelse(is.na(data$price_person),mean(data$price_person,na.rm = TRUE),data$price_person)

data$price_person =as.numeric(data$price_person )


#property type

table(data$property_type)

data$property_newtype = ifelse(data$property_type=='Apartment',1,ifelse(data$property_type=='House',2,ifelse(data$property_type=='Condominium',3,ifelse(data$property_type=='Townhouse',4,ifelse(data$property_type=='Loft',5,6)))))
table(data$property_newtype)
sum(is.na(data$property_newtype))
data$property_newtype=ifelse(is.na(data$property_newtype),3,data$property_newtype)


#resquire license

data$requires_license = ifelse(data$requires_license=='TRUE',1,0)
table(data$requires_license)
sum(is.na(data$requires_license))
data$requires_license=ifelse(is.na(data$requires_license),1,data$requires_license)


#room type

table(data$room_type)
sum(is.na(data$room_type))
data$room_type=ifelse(is.na(data$room_type),'Private room',data$room_type)
data$room_type=ifelse(data$room_type=='Entire home/apt',1,ifelse(data$room_type=='Private room',2,3))


#maximum night

data$maximum_nights = as.numeric(data$maximum_nights)
data$minimum_nights = as.numeric(data$minimum_nights)
data$maximum_night_range = ifelse(data$maximum_nights<=7,'1',
                                  ifelse(data$maximum_nights<=14,'2',
                                         ifelse(data$maximum_nights<=30,'3',
                                                ifelse(data$maximum_nights<=60,'4',
                                                       ifelse(data$maximum_nights<=90,'5',
                                                              ifelse(data$maximum_nights<=180,'6',
                                                                     ifelse(data$maximum_nights<=360,'7',
                                                                            ifelse(data$maximum_nights<=720,'8',
                                                                                   ifelse(data$maximum_nights<=1080,'9', '10')))))))))
table(data$maximum_night_range)
sum(is.na(data$maximum_night_range))
data$maximum_night_range=ifelse(is.na(data$maximum_night_range),5,data$maximum_night_range)


##minimum night

data$minimum_night_range = ifelse(data$minimum_nights<=3,'1',
                                  ifelse(data$minimum_nights<=7,'2',
                                         ifelse(data$minimum_nights<=10,'3',
                                                ifelse(data$minimum_nights<=14,'4',
                                                       ifelse(data$minimum_nights<=30,'5',
                                                              ifelse(data$minimum_nights<=60,'6','7'))))))
table(data$minimum_night_range)
sum(is.na(data$minimum_night_range))
data$minimum_night_range=ifelse(is.na(data$minimum_night_range),4,data$minimum_night_range)


##average bathroom
data$avg_bathrooms=data$bathrooms/data$accommodates
table(data$avg_bathrooms)
sum(is.na(data$avg_bathrooms))

#create a host_identity_verifie dummy variable(=1 if its verified)
table(data$host_identity_verified)
data$host_identity_verified=ifelse(data$host_identity_verified=="TRUE",1,0)
sum(is.na(data$host_identity_verified))
data$host_identity_verified=ifelse(is.na(data$host_identity_verified),1,data$host_identity_verified)


##location is exact 
data$is_location_exact=ifelse(data$is_location_exact=="TRUE",1,0)
sum(is.na(data$is_location_exact))
data$is_location_exact=ifelse(is.na(data$is_location_exact),0,data$is_location_exact)
table(data$is_location_exact)
#host response rate
data$host_response_rate = as.numeric(gsub("%.*","",data$host_response_rate))
data$host_response_rate=data$host_response_rate /100
sum(is.na(data$host_response_rate))
table(data$host_response_rate)

##impute response time
host_data2 = data[,c("host_is_superhost","host_listings_count","host_experience","host_response",'host_response_rate')]
library(mice)
init = mice(host_data2, maxit=0) 
meth = init$method
predM = init$predictorMatrix
meth[c("host_is_superhost")] = ""
meth[c("host_listings_count")] = ""
meth[c("host_experience")] = ""
meth[c("host_response")] = ""
meth[c("host_response_rate")] = "pmm"


set.seed(103)
imputed2 = mice(host_data2, method=meth, predictorMatrix=predM, m=5)
imputed2 = complete(imputed2)
host_data2 = imputed2
sum(is.na(host_data2$host_response_rate))

data$host_response_rate=host_data2$host_response_rate
sum(is.na(data$host_response_rate))




###data
colnames(data)

house2 = data[,c(2,4,5,6,7,15,20,26,27,28,32,38,41,58,59,70,71,72,73,74,75,76,77,78,79,80,81)]



colnames(house2)
colSums(is.na(house2))

hotel_data2=house2
###
hotel_data2$availability_30 = as.numeric(hotel_data2$availability_30)
hotel_data2$availability_60 = as.numeric(hotel_data2$availability_60)
hotel_data2$availability_90 = as.numeric(hotel_data2$availability_90)
hotel_data2$availability_365 = as.numeric(hotel_data2$availability_365)
hotel_data2$bed_person = as.numeric(hotel_data2$bed_person)
hotel_data2$host_experience = as.numeric(hotel_data2$host_experience)
hotel_data2$accommodates = as.numeric(hotel_data2$accommodates)
hotel_data2$cleaning_fee = as.numeric(hotel_data2$cleaning_fee)
hotel_data2$extra_people = as.numeric(hotel_data2$extra_people)
hotel_data2$description_len = as.numeric(hotel_data2$description_len)
hotel_data2$price_person = as.numeric(hotel_data2$price_person)
hotel_data2$host_listings_count = as.numeric(hotel_data2$host_listings_count)
hotel_data2$avg_bathrooms =  as.numeric(hotel_data2$avg_bathrooms )
hotel_data2$host_response_rate = as.numeric(hotel_data2$host_response_rate)


hotel_data2$host_is_superhost = as.numeric(hotel_data2$host_is_superhost)
hotel_data2$host_response = as.numeric(hotel_data2$host_response)
hotel_data2$instant_bookable = as.numeric(hotel_data2$instant_bookable)
hotel_data2$room_type = as.numeric(hotel_data2$room_type)
hotel_data2$property_newtype = as.numeric(hotel_data2$property_newtype)
##hotel_data2$high_booking_rate = as.factor(hotel_data2$high_booking_rate)
hotel_data2$requires_license = as.numeric(hotel_data2$requires_license)
hotel_data2$maximum_night_range = as.numeric(hotel_data2$maximum_night_range)
hotel_data2$minimum_night_range=as.numeric(hotel_data2$minimum_night_range)
hotel_data2$cancel_policy = as.numeric(hotel_data2$cancel_policy)
hotel_data2$city_popular = as.numeric(hotel_data2$city_popular)
hotel_data2$share_bathroom = as.numeric(hotel_data2$share_bathroom)
hotel_data2$host_identity_verified = as.numeric(hotel_data2$host_identity_verified)
hotel_data2$is_location_exact = as.numeric(hotel_data2$is_location_exact )


##split
test.data2 = as.matrix(hotel_data2)

xgb.pred2 = predict(xgb.fit,test.data2,reshape=T)
xgb.pred2 = as.data.frame(xgb.pred2)
class2 = ifelse(xgb.pred2[,2]>0.5,1,0)
View(xgb.pred2[,2] )
View(class2 )

write.csv(class2,"/Users/louluying/Documents/2020spring/T-datamining/project/group8_preds2_429.csv")



##boosting
boost_preds <- predict(boost.mod,newdata=house2,type='response',n.trees=1000)


