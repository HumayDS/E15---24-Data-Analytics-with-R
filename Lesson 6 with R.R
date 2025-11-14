## Creare vectors

ages <- c(12, 26, 34, 44, 55)

#Extract indexes 


which(ages > 40)
indexes <- which(ages > 40)

##Filter

values <-  ages[indexes]

## Descriptive stats of values 

summary(values)


## extract second index

ages[2]
## Replace it  with 15


ages[2] <-  15

# select 2 and 3 indexes

ages[c(2,3)]

## Calculate the length of vector 
length(ages)

#install.packages('lubridate')
library(readr)
library(dplyr)
#install.packages('lubridate')
library(lubridate)
library(dplyr)
#Todays date
##Date
Sys.Date()
today <-  Sys.Date()
today
date1 = '06/06/2016'
class(date1)

date2 <-  as.Date(date1,format = '%d/%m/%Y' )
class(date2)


##TIME

Sys.time()


##create sequence
seq(from =1 , to = 100, by = 5)

seq(from = as.Date('12/12/2020' , format = '%d/%m/%Y') ,
    to = as.Date('22/12/2020' , format = '%d/%m/%Y') , by = 1)

seq(from = as.Date('12/12/2020' , format = '%d/%m/%Y') ,
    to = as.Date('22/12/2022' , format = '%d/%m/%Y') , by = 'year')


## Predict
#General term used in machine learning and statistics.
#It means estimating an outcome based on input data — past or present.
#Predictions can be for any kind of variable: sales, disease risk, customer churn, etc.
#Usually used when the target doesn’t depend on time.

#Forecast
#Used specifically for time series data.
#It means predicting future values based on patterns observed in the past.
#A forecast always involves time progression (e.g., next day, next month).
#Usually produced by time series models such as ARIMA, ETS, or Prophet.



##Read dataset

df <-  read_csv('https://raw.githubusercontent.com/humayhasilli101/E15---24.-Data-Analytics-with-R/refs/heads/main/datedata.csv')

df %>%  head()
df %>%  glimpse()

df <-  df %>%  select(-...1 )

df %>%  glimpse()
df %>% is.na() %>% sum()


## Rename 

df %>%
  rename(NewName = StockCode)
##Mutate 

#Create new column



df %>%
  mutate(rev = Price * 0.9)



df %>%
  mutate(quantify_cat = case_when(
    Quantity < 5 ~ "low",
    TRUE ~ "high"       # default for all other cases
  ))


#check uniques

unique(df$StockCode)


length(unique(df$StockCode))

unique(df$Country)

unique(df$Description) 

#convert type
df %>%  glimpse()


df$InvoiceDate <-  as.Date(df$InvoiceDate)

df$Country <-  as.factor(df$Country)
df$Customer.ID <-  as.character(df$Customer.ID)
df %>%  glimpse()


# Select only numeric columns
num_df <- df %>% select(where(is.numeric))

# Compute correlation matrix
corr_matrix <- cor(num_df, use = "complete.obs")
print(corr_matrix)

#install.packages('corrplot')
library(corrplot)

#Plot Heatmap
corrplot(corr_matrix, method = "color", type = "upper", 
         tl.col = "black", tl.srt = 35)


#get differences between days 

df$dayDifference <- as.numeric(difftime(Sys.Date(), df$InvoiceDate, units = "days"))

#Arrange by Date
df %>% arrange(InvoiceDate)

#get min and max date


min(df$InvoiceDate)
max(df$InvoiceDate)

length(df$InvoiceDate)
unique(df$InvoiceDate)

#filter by date

filtered_df <- df %>%
  filter(InvoiceDate > as.Date("2010-04-06"))

filtered_df2 <- df %>%
  filter(InvoiceDate > as.Date("2010-02-28") & InvoiceDate < as.Date("2011-07-01"))

##extraxt year, month,day

df$month<- month(df$InvoiceDate,label = TRUE)
df$week<- week(df$InvoiceDate)
df$year<-year (df$InvoiceDate)
df$day<-day(df$InvoiceDate)
df$weekday<-weekdays(df$InvoiceDate)

##Sys.setlocale("LC_TIME", "en_US.UTF-8")

#filter year == 2010

df %>%
  filter(year(InvoiceDate) == 2010)

df %>%  filter(year == 2011)

#mean
df %>%
  group_by(year) %>%
  summarise(mean_quantity = mean(Quantity))



# group by monthly and weekly
for_monthly<- df %>% group_by(month,year) %>% 
  summarise(sales= sum(Quantity,na.rm = TRUE)) %>% arrange(year,month)


for_weekly<- df %>% group_by(month,year, week) %>% 
  summarise(sales= sum(Quantity,na.rm = TRUE)) %>% arrange(year,month)



# Save dataset in working directory
write.csv(df,'for_monthly')

