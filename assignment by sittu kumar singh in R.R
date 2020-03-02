# CALLING LIBRARIES
library(corrplot)
library(GGally)
library(dplyr)
library(ggthemes)
library(ggplot2)
library(naivebayes)

# GETTING DATA
DATA=read.csv(file.choose(),sep=',',header=T)

# DATA VIEWING
str(DATA)
colnames(DATA)
summary(DATA)
ncol(DATA)
nrow(DATA)
is.na(DATA)
nrow(DATA[!complete.cases(DATA),])

#Selecting columns

DATA = DATA %>%
  select(loan_amnt ,funded_amnt,term,int_rate, grade , emp_length , home_ownership , 
         annual_inc ,purpose,tot_coll_amt,tot_cur_bal,loan_status,annual_inc)
DATA=DATA[-10]
summary(DATA)
sapply(DATA , function(x) sum(is.na(x)))

#data cleaning

DATA = DATA %>%
  filter(!is.na(tot_cur_bal) , 
         emp_length != 'n/a')

sapply(DATA , function(x) sum(is.na(x)))

#Exploratory Data Analysis

#1)interest rate by grade

ggplot(DATA , aes(x = grade , y = int_rate , fill = grade)) + 
  geom_boxplot() + 
  theme_igray() + 
  labs(y = 'Interest Rate' , x = 'Grade')

#2)Why people want loan?
DATA %>%
  count(purpose) %>%
  ggplot(aes(x = reorder(purpose , desc(n)) , y = n , fill = n)) + 
  geom_col() + 
  coord_flip() + 
  labs(x = 'purpose' , y = 'Count')
#3)Total Loan Amount by diffrent home ownership
ggplot(DATA , aes(x = home_ownership  , y = loan_amnt , fill = home_ownership)) + 
  geom_bar(stat = "identity") + 
  theme_igray() + 
  labs(y = 'loan_amnt' , x = 'home_ownership')
#4)Annual Income by Loan Amount
ggplot(DATA, aes(x=annual_inc, y=loan_amnt, fill=annual_inc)) + 
  geom_line() +
  theme_bw() +
  labs(x = 'loan amouunt' , y = 'annual income')
 
#................................................................
#NAIVE BAYES CLASSIFACTION
#DATA SUBSETTING AND PREPARATION
set.seed(1234)
ind <- sample(2, nrow(DATA), replace = T, prob = c(0.7, 0.3))
TRAINING= DATA[ind==1,]
TESTING=  DATA[ind==2,]
str(TRAINING)
summary(TRAINING)
str(TESTING)
summary(TESTING)
# CREATING NAIVE BAYES MODEL
MODEL=naive_bayes(grade~.,data=TRAINING)
plot(MODEL)
PREDICTIONPROB=predict(MODEL,TESTING,type="prob")
head(cbind(PREDICTIONPROB,TESTING))

# PREDICTION
PREDICTION=predict(MODEL,TESTING)

# CROSS VALIDATION
(VALIDATION=table(TEST=TESTING$grade,PREDICTED=PREDICTION))

# CHECKING ACCURACY PERCENTAGE
(ACCURACY=sum(diag(VALIDATION))/sum(VALIDATION)*100)