#####Loading packages 

#install.packages("readr")
#install.packages("dplyr")
##install.packages(tidyr)

library(dplyr)
library(readr)
library(tidyr)



df <-  read.csv('https://raw.githubusercontent.com/humayhasilli101/E15---24.-Data-Analytics-with-R/refs/heads/main/car.csv')


## Glimpse 
df  %>%  glimpse()

# Check and sum na values 
is.na(df) %>%  sum()

## Summarise dataset and get descriptive stats.
df %>%  summary()


## lets get sum of all na values 

sum(is.na(df))

#shows how many missing values (NAs) are in each column

colSums(is.na(df))

# eliminare rows with na 
df %>%  drop_na()

## Filtering 
## Select only the rows where the Price column value is greater than 20,000.
df %>%  filter(Price > 20000)

##Select rows where either condition is true (logical OR |):
df %>% filter((Price > 20000) | (Doors == 5))  #or


colnames(df)

#Select only the cars that cost more than 20,000 and have mileage below 80,000
df %>% filter((Price > 20000)  & (Odometer..KM. < 80000))  #and


df %>%  summary()


## change to categoric  var
df$Make <-  factor(df$Make)
df$Colour <-  factor(df$Colour)
df$Doors <-  as.factor(df$Doors)


df %>%  summary()

df %>%  glimpse()



### getting correlation 

# Error
corr(df)

##Select only numeric variables
numeric_data <- select_if(df, is.numeric)


###Correlation --- Error again

numeric_data = na.omit(numeric_data)

cor(numeric_data)


##save dataset
write.csv(df,'data_123.csv', row.names = FALSE)




## Handling with na 
# location of na 

which(is.na(df$Price))

index_na  <-  which(is.na(df$Price))
index_na 




##show rows contain na
df[index_na, ]




## delete na's
df_na_omit <- na.omit(df)


## df mean imputation  Price

#calculate mean 
mean(df$Price , na.rm = T)
mean <-  mean(df$Price , na.rm = T)
#Copy df

df_clear <-  df 

#Mean imputation
df_clear$Price[index_na] <- mean


#Calculate mode

df_clear %>%
  count(Colour) %>%
  arrange(desc(n)) %>%
  slice(1)



#Imputate mode

df_clear$Colour[is.na(df_clear$Colour)] <-  'White'

is.na(df_clear) %>%  sum() 


colSums(is.na(df_clear))
## save data 

write.csv(df_clear, 'df.csv')


##Grouping
#mean
df_clear %>%
  group_by(Doors) %>%
  summarise(mean_price = mean(Price))



##Sum
df_clear %>%
  group_by(Make) %>%
  summarise(sum_price = sum(Price)) %>%  arrange(desc(mean_price))

#count

df_clear %>%
  group_by(Make) %>%
  summarise(count = n())

##mean and dsum


df_clear %>%
  group_by(Doors) %>%
  summarise(mean_price = mean(Price) , sum_price = sum(Price))


hist(df_clear$Price)
boxplot(df_clear$Price)




#Mutate 

df_clear %>%  head()

df_clear %>%  mutate(Price = Price*0.9)
df_clear %>%  mutate(Price90 = Price*0.9)
df_clear %>%  mutate(division =  Odometer/Price)
df_clear %>%  mutate(division =  Odometer/Price ,Price90 = Price*0.9 )

df_clear %>%  mutate(Doors  = case_when(  Price >  160000 ~ "Expensive",
                                          TRUE ~ "Cheap"
)) 

