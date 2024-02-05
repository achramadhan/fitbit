library(dplyr)
library(janitor)
library(lubridate)
steps <- read.csv("dailySteps_merged.csv") %>% clean_names()
intensities <- read.csv("dailyIntensities_merged.csv") %>% clean_names()
colnames(intensites)
intensities <- intensities %>%
  mutate (total_active = round((sedentary_minutes+fairly_active_minutes+
                            lightly_active_minutes+very_active_minutes)/60,1))
 
calories <- read.csv("dailyCalories_merged.csv") %>% clean_names()
activity <- read.csv("dailyActivity_merged.csv") %>% clean_names()
activity <- activity %>% select(c(id,activity_date,total_distance)) 
sleep <- read.csv("sleepDay_merged.csv") %>% clean_names()

colnames(activity)
colnames(steps)
colnames(intensities)
colnames(sleep)
colnames(calories)

#joining the table
fitbit <- left_join(activity,steps, by = c('id'='id', 'activity_date'='activity_day'))
fitbit <- left_join(fitbit, calories, by = c('id'='id','activity_date'='activity_day'))
intensities <- intensities %>% select(c(id, activity_day, total_active))
fitbit <- left_join(fitbit,intensities, by = c('id'='id','activity_date'='activity_day'))
sleep <- sleep %>% select(-c(total_sleep_records)) 
#changing data type before joining table sleep, because different format
fitbit$activity_date <- mdy(fitbit$activity_date)
sleep$sleep_day <- mdy_hms(sleep$sleep_day)
fitbit <- left_join(fitbit, sleep, by = c('id'='id','activity_date'= 'sleep_day'))

#membuat nilai dummy untuk berat badan dan tinggi badan

# extract id from dataset
id_values <- unique(fitbit$id)

# Nilai minimum dan maksimum untuk setiap id
min_values <- c(50,80,66,64,46,102,84,74,86,44,58,68,70,100,65,75,54,86,66,76,56,65,84,94,86,96,74,104,56,66,76,74,84)
max_values <- c(52,82,68,66,48,104,86,78,88,48,60,70,72,102,67,77,56,88,67,78,58,67,86,96,88,98,76,106,58,68,78,76,86)
#nilai random untuk tinggi badan
height <-runif(33, min=155, max = 189)
#membuat data frame
df <- data.frame(id=id_values, min=min_values, max = max_values, height = height)
#menggabungkan 2 tabel

fitbit <- fitbit %>% left_join(df, by='id')
#membuat nilai dummy pada kolom weight dengan rentang nilai yang ditentukan min dan max
fitbit <- fitbit %>% mutate(weight = runif(n(), min=min, max=max)) %>% 
  select(-c(min,max))
#menambahkan BMI
fitbit$bmi<- fitbit$weight/(fitbit$height/100)^2
#Menambahkan kolom kategori BMI
fitbit$bmi_category <- cut(fitbit$bmi, 
                              breaks = c(-Inf, 18.5, 25, 30, Inf), 
                              labels = c("Underweight", "Normal weight", "Overweight", "Obesity"))
#mengatur digit pada kolom weight,height, bmi
fitbit$height<- round(fitbit$height, digits= 0)
fitbit$weight<- round(fitbit$weight, digits= 2)
fitbit$bmi<- round(fitbit$bmi, digits= 1)

#mengubah tipe data
fitbit$step_total <- as.numeric(fitbit$step_total)
fitbit$calories <- as.numeric(fitbit$calories)


#segmentation  
  
#activty of user
  type_of_usage_device <- fitbit %>% 
    group_by(id) %>% 
    summarise(activity = mean(total_active)) %>% 
    mutate(usage = case_when(
      activity<=10~"low",
      activity<=20~"moderate", 
      TRUE ~"high_use")) %>% 
    select(-c(activity)) %>% print(n=5)
  
  user_type <- fitbit %>% 
    group_by(id) %>% 
    summarise(mean_steps = mean(step_total)) %>% 
    mutate(user_type = case_when(
      mean_steps <= 5000 ~ "sendentary",
      mean_steps<7500~"lightly active",
      mean_steps<10000~"fairly active",
      TRUE ~ "very active"
    )) %>%select(-c(mean_steps)) %>% 
    print(n=5)
  fitbit <- left_join(fitbit, type_of_usage_device,by="id")
  fitbit <- left_join(fitbit, user_type ,by="id")

  #checking na values
  sapply(fitbit, function(x)sum(is.na(x)))
  str(fitbit)
  
#ANALISIS 

  #Quick Summary
summary(fitbit)



#distribution of User per BMI Category
library(ggplot2)

fitbit %>% group_by(bmi_category) %>% 
  summarise (total = n_distinct(id),.groups="drop")%>%
  ggplot(aes(bmi_category,total, fill=bmi_category))+
  geom_bar(stat="identity")+
  geom_text(aes(label = total))+
  labs(title="Total by BMI Category")+
  theme_minimal()
#usage
fitbit %>% group_by(bmi_category, usage) %>% 
  summarise (total = n_distinct(id),.groups="drop")%>%
  ggplot(aes(usage,total, fill=bmi_category))+
  geom_bar(stat="identity", position=position_dodge())+
  labs(title="Total by BMI Category",
       caption = "high use = average user using the apps more than 20 hours, moderate = average user using the apps from 11 to 20 hours")+
  theme_minimal()
#user_type
fitbit %>% group_by(bmi_category, user_type) %>% 
  summarise (total = n_distinct(id),.groups="drop")%>%
  ggplot(aes(user_type,total, fill=bmi_category))+
  geom_bar(stat="identity", position=position_dodge())+
  labs(title="Total by BMI Category",
       caption = "categorizes the average step per user, the value <=5000 is sendentary, <7500 is lightly active, <10000 is fairly active, else very active")+
  theme_minimal()

fitbit %>% 
  mutate(activity_date = wday(activity_date, label=TRUE)) %>% 
  group_by(activity_date, bmi_category) %>% 
  summarise(mean_distance = mean(total_distance),.groups = "drop") %>% 
  ggplot(aes(activity_date, mean_distance, group = bmi_category, color=bmi_category))+
  geom_line(size = 1.4)+
  theme_minimal()+
  labs(title ="Activity Average Distance by BMI Category",
    y = "Total Distance (KM)")

fitbit %>% mutate(activity_date = wday(activity_date, label=TRUE)) %>%
  group_by(activity_date, bmi_category) %>% 
  summarise(total_active = mean(total_active), .groups="drop") %>% 
  ggplot(aes(activity_date,total_active, group = bmi_category, color=bmi_category))+
  geom_line(size = 1.3)+
  labs(title = "Avg Tracking Active Apps in Hour",
       y = "hour",
       x= "Days of Week")


#time sleep vs total step
library(gridExtra)
grid.arrange(
{fitbit %>% mutate(activity_date = wday(activity_date, label=TRUE)) %>% 
  group_by(activity_date, bmi_category) %>% 
  summarise(mean_asleep = round(mean(total_minutes_asleep, na.rm = TRUE)/60,2),.groups = "drop") %>% 
  ggplot(aes(activity_date, mean_asleep, fill=bmi_category))+
  geom_bar(stat = 'identity', position = position_dodge())+
  geom_hline(yintercept = 8, color = "black")+
  geom_hline(yintercept = 6, color = "black")+
  labs(title = 'Mean Sleep By BMI Category',
       y = "Hour",
       x = "Day of Week")+
      theme(legend.position = "top")+
    guides(fill = guide_legend(nrow = 2, title = NULL))},
  
{fitbit %>% 
    ggplot(aes(step_total, total_minutes_asleep))+
    geom_jitter()+
    geom_smooth(color="red")+
  labs(title = 'Total Step vs Total Sleep',
       y = "Total Step",
       x = "Total Sleep",
       caption = "correlation :-0.1868665 ")},
ncol=2 
)
# Menghitung koefisien korelasi Pearson
cor(fitbit$step_total, fitbit$total_minutes_asleep, use = "pairwise.complete.obs")


#Memuat paket yang dibutuhkan
library(reshape2)
# Mengubah tabel fitbit menjadi format panjang (untuk corrHeatMap)
fitbit_melt <- fitbit %>% select(-c(bmi_category,id,activity_date, usage, user_type))
# Menghitung matriks korelasi
fitbit_cor <- round(cor(fitbit_melt, use = "pairwise.complete.obs"),2)
melt <- melt(fitbit_cor)
head(melt)
# Membuat plot heatmap
ggplot(melt, aes(x = Var1, y = Var2,fill = value)) +
  geom_tile(aes(), colour = "white") +
  scale_fill_gradient(low = "white", high = "red") +
  ggtitle("Heatmap Correlation Fitbit")+
  geom_text(aes(label = value))
#prediction distance by step total, weight, height
model <- lm(total_distance~step_total+weight+height, data =fitbit)
summary(model)
coba <- data.frame(step_total=c(12000),weight=c(80),height=c(171))
predictions <- predict(model, newdata=coba)
predictions

#prediction calories by distance, weight, height
model <- lm(calories~step_total+weight+height, data =fitbit)
summary(model)
coba <- data.frame(step_total=c(9000),weight=c(80),height=c(171))
predictions <- predict(model, newdata=coba)
predictions
