### Matrix 

#a matrix is a two-dimensional data structure consisting of elements 
#of the same data type (numeric, character, logical, etc.).

#All elements must be the same type


matrix(21:30, nrow = 5, ncol = 2)
m <-  matrix(21:30, nrow = 5, ncol = 2)

#Multiplication 
m * 3

# Delete 2nd row
m[-2, ]

#delete the first column and the second row
m <- m[-2, -1]
m

summary(m)
#install.packages('lubridate')
library(lubridate)
library(dplyr)
library(readr)
##Read dataset

df <-  read_csv('https://raw.githubusercontent.com/humayhasilli101/E15---24.-Data-Analytics-with-R/refs/heads/main/datedata.csv')

df %>%  head()
df %>%  glimpse()

#Delete 1st row
df <-  df %>%  select(-...1 )

#Check isna
df %>% is.na() %>% sum()

## Rename 

df %>%
  rename(Stock_number = StockCode)# New_name = old_name 


##Mutate 

#Create new column
# Decrease 10% of price

df %>%
  mutate(rev = Price * 0.9)

df %>%  glimpse()
df %>%  unique()


# Create new column with condition 
df %>%
  mutate(quantify_cat = case_when(
    Quantity < 5 ~ "low",
    TRUE ~ "high"       # default for all other cases
  ))


#check uniques
# This will help you identify data types

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

#Extract and save date (years, month, weeks) from date column 
df$month<- month(df$InvoiceDate,label = TRUE)
df$week<- week(df$InvoiceDate)
df$year<-year (df$InvoiceDate)
df$day<-day(df$InvoiceDate)
df$weekday<-weekdays(df$InvoiceDate)


#filter year == 2010

df %>%
  filter(year(InvoiceDate) == 2010)

df %>%  filter(year == 2011)

# Simple filtering

df %>%  filter(Price < 10)


#mean
df %>%
  group_by(year) %>%
  summarise(mean_quantity = mean(Quantity))



# group by monthly and weekly
for_monthly<- df %>% group_by(month,year) %>% 
  summarise(sales= sum(Quantity,na.rm = TRUE)) %>% arrange(year,month)


for_weekly<- df %>% group_by(month,year, week) %>% 
  summarise(sales= sum(Quantity,na.rm = TRUE)) %>% arrange(year,month)


#Barplot


barplot(for_monthly$sales, 
        names.arg = paste(for_monthly$month, for_monthly$year, sep = "-"))


#Detailed bar plot 
        
        
barplot(for_monthly$sales, 
        names.arg = paste(for_monthly$month, for_monthly$year, sep = "-"),   # Combine month and year for labels
        col = "skyblue",                                                   # Set bar color
        main = "Monthly Sales by Year",                                     # Title of the plot
        xlab = "Month-Year",                                                # Label for the x-axis
        ylab = "Total Sales",                                               # Label for the y-axis
        las = 2,                                                            # Rotate x-axis labels for readability
        border = "white",                                                   # Remove bar borders
        cex.names = 0.7)   


colnames(df)

# Replace spaces in column names with underscores
colnames(df) <- gsub(" ", "_", colnames(df))
colnames(df)
colnames(df) <- gsub("\\.", "_", colnames(df))
colnames(df)


### Handling with Strings
#Combine cols

df$detailed_dec <-  paste(df$StockCode , '-' , df$Description)
df

paste(df$year , '-' , df$month)

# Add $ sign 

df$`Price$` <- paste(format(df$Price), "$", sep = "")


# Remove the dollar sign from the 'Price$' column
df$`Price$` <- gsub("\\$", "", df$`Price$`)


# Convert 'description' column to lowercase
df$Description <- tolower(df$Description)

# Convert 'description' column to uppercase
df$Description <- tolower(df$Description)


df$Description <- tools::toTitleCase(df$Description)





df %>% glimpse()

# Save dataset in working directory
write.csv(df,'df_new')

