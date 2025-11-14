#####Loading packages 

#install.packages("readr")
#install.packages("dplyr")
##install.packages(tidyr)

library(dplyr)
library(readr)
library(tidyr)
df <-  read.csv('tips.csv')

View(df)
summary(df)

## Select

df$smoker

df$gender <- factor(df$gender)
df$smoker <- factor(df$smoker)
df$day <-  factor(df$day)
df$time <-  factor(df$time)

summary(df)

class(df$gender)

df$gender <- factor(df$gender, levels = c("Female", "Male"), ordered = TRUE)


### Pipe , ctrl + shift + M 
df$gender %>%  unique()

unique(df$gender)


df %>%  filter(gender == 'Male')

male_df <-  df %>%  filter(gender == 'Male')
View(male_df)

### Filtering
df %>%  filter(tip < 10) 

## And

f1 <-  df %>%  filter(tip < 10 & gender == "Male" ) 

## or 
f2 <-  df %>%  filter(tip < 10 | total_bill < 20 ) 

## rename
df %>%  rename(new_name = size)

# write codes with pipe 
df2 <-  df %>% rename(new_name = size ) 
df %>%  rename(new_name = size) %>%  filter(new_name == 2) %>%  filter(tip < 10)


### Check na's

df %>%  is.na() %>%  sum()

colSums(is.na(df))

### 
df %>%  slice(20:40)
df %>%  select(gender, size) 

df %>%  select(gender, size)   %>%  rename(new_size = size ) %>%  slice(20:40)
## row size 
df %>%  nrow()

## Col size 

df %>% ncol()


## Heading 
df %>%  head()

df %>%  glimpse()

df %>%  summary()

## Save data
write.csv(df2, "df2.csv")


## Extract from github 
df_new <-  read.csv('https://raw.githubusercontent.com/HumayDS/Data-Analytics-with-R-spring/main/car.csv')

df_new  %>%  glimpse()
is.na(df_new) %>%  sum()
df_new %>%  summary()
sum(is.na(df_new))

colSums(is.na(df_new))
### Calculate mean 

mean(df_new$Price , na.rm = TRUE)
hist(df_new$Price)
boxplot(df_new$Price)

### delete na's 

df_new %>%  drop_na()
df_new_omit <-  df_new %>%  drop_na()

# Filtering df_new

df_new %>% filter(Doors == 5) %>%  count()


df_new %>%  filter(Price > 20000)

df_new %>% filter((Price > 20000) | (Doors == 5))  #or

df_new %>% filter((Price > 20000)  & (Odometer < 80000))  #and
