options(scipen = 9999) #Not to have scientific number
#Extract dataset
df<- read.csv("https://raw.githubusercontent.com/HumayDS/E15---24-Data-Analytics-with-R/refs/heads/main/score.csv")



x <- c(168, 172, 165, 170, 175, 169, 173)
##CHeck normaliy
summary(x)
hist(x)
#Shapiro wilk test check if your dataset normally distributed or not
#H0 : data normally distributed
#H1: data is not normally distributed 
shapiro.test(x)  


# One sample t-test
#cover the 4 most essential hypothesis tests in R:

# One-sample t-test
# Independent two-sample t-test
# Paired t-test or dependent 
# Chi-square test



#1. One sample t-test

#Use this test when you want to compare your sample mean to a known value.

#Is the average height different from 170 cm?
#h0 : mu = 170
#h1 : mu != 170

# Sample data


x <- c(168, 172, 165, 170, 175, 169, 173)


# One-sample t-test
t.test(x, mu = 170)
t.test(df$Scores , mu = 55)




#t-value shows how many standard errors your sample mean is away from the mean in H₀.
#t = 0.22 means your sample mean is almost the same as the mean assumed in H₀.
#Wilcoxon Signed-Rank Test - non parametric 
#wilcox.test()




# Task for student 
# Apply one - sample t - test
#Test whether the mean score is different from 70:
#Interpret the result

scores <- c(65, 72, 68, 75, 70, 78, 74, 69, 71, 73, 66, 77, 72, 70, 74)









#2. Independent two -sample t-test
#Use this test when you want to compare two different groups
#(example: Male vs Female, Control vs Treatment, Class A vs Class B).
# Man - Whitney U test (Wilcoxon Rank-Sum Test)
#Wilcox.test()

groupA <- c(10, 12, 15, 14, 11)
groupB <- c(18, 20, 19, 21, 17)

t.test(groupA, groupB, paired = FALSE)

t.test(df$Hours , df$Scores, paired = FALSE)


# Task for student 
# State your hypothesis
# Run the independent two sample t test


classA <- c(78, 82, 75, 80, 77, 85, 79, 83, 76, 81)
classB <- c(72, 74, 70, 68, 75, 73, 71, 69, 74, 72)





# 3. Paired t -test 

#The same subjects are measured before and after
#data points are matched pairs
#Weight before vs. after diet
#Test score before training vs. after training
#Blood pressure before medicine vs. after medicine
#Wilcoxon Signed-Rank Test - non parametric version 
#wilcox.test(before, after, paired = TRUE)

before <- c(120, 130, 125, 140, 135)
after  <- c(115, 128, 120, 137, 130)
#Each element is the same person before and after.


t.test(before, after, paired = TRUE)


## Task for student 

before <- c(10.5, 11.2, 12.0, 9.8, 11.5, 10.9, 12.3, 11.0, 10.8, 11.7, 12.1, 10.4)
after  <- c(9.9, 10.8, 11.2, 9.4, 10.9, 10.2, 11.5, 10.6, 10.3, 11.0, 11.6, 9.8)








# chi squared test 
#Is there a relationship between two categorical variables?
# FIsher's Exact test -  non parametric version(approximation)

df2 <- data.frame(
  Smoking = c("Smoker", "Smoker", "Smoker", "Non-Smoker", "Non-Smoker", "Non-Smoker", "Smoker", "Non-Smoker", "Smoker", "Non-Smoker"),
  Disease = c("Yes",    "No",     "Yes",    "No",         "No",         "Yes",          "No",   "No",         "Yes",     "No")
)



tbl2 <- table(df2$Smoking, df2$Disease)
tbl2

chisq.test(tbl2)
fisher.test(tbl2)

# Task for students

## Apply Chi - square 
df_big <- data.frame(
  Method = c(
    rep("Group", 20),
    rep("Self", 20)
  ),
  Result = c(
    # Group study (20 students)
    "Pass","Pass","Pass","Pass","Pass",
    "Pass","Pass","Fail","Fail","Pass",
    "Pass","Fail","Pass","Pass","Fail",
    "Pass","Pass","Pass","Fail","Pass",
    
    # Self study (20 students)
    "Pass","Fail","Fail","Pass","Fail",
    "Pass","Fail","Fail","Pass","Fail",
    "Pass","Fail","Fail","Fail","Pass",
    "Fail","Fail","Pass","Fail","Fail"
  )
)


df <- data.frame(
  Hours = c(2, 3, 4, 5, 6, 7, 8, 9),
  Score = c(50, 55, 60, 65, 70, 72, 78, 85)
)




df <- read.csv("https://raw.githubusercontent.com/HumayDS/E15---24-Data-Analytics-with-R/refs/heads/main/score.csv")


cor(df$Hours,df$Scores)
#very strong positive relationship
#Correlation Test (Pearson for parametric)
#Correlation test (Spearman for non parametric )
cor.test(df$Hours, df$Scores) #Pearson

cor.test(df$Hours, df$Scores, method = "spearman")
#Means the correlation is statistically significant



#Task for students
#Find and test correlation 


sleep <- c(6,7,8,5,9,6,7,8,4,6,7,9,8,5,6)
concentration <- c(65,70,78,55,85,68,72,75,50,66,71,87,79,58,69)




##Linear regression 

df <- data.frame(
  Hours = c(2, 3, 4, 5, 6, 7, 8, 9),
  PracticeTests = c(0, 1, 1, 2, 2, 3, 3, 4),
  Score = c(50, 55, 60, 63, 68, 72, 78, 85)
)



model <- lm(Score ~ Hours + PracticeTests, data = df)
summary(model)


# Task for student 
#Build linear regression model and interpret it t
Size <- c(1200,1500,1700,2000,2200,2500,2600,2800,3000,3200,3500,3700)
Bedrooms <- c(2,3,3,3,4,4,4,4,5,5,5,5)
Price <- c(180,200,240,260,290,320,330,350,400,420,450,480)

df <- data.frame(Size, Bedrooms, Price)
df





