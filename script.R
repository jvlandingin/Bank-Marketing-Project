##########################
#####  INTRODUCTION  #####
##########################

# The data to be used in this project is related with direct marketing campaigns of a Portuguese banking institution. 
# The marketing campaigns were based on phone calls. The goal in this project is to predict whether a client
# will subscribe to a bank term deposit. The dataset and details can be found on the website - 
# https://archive.ics.uci.edu/ml/datasets/bank+marketing


#We load the following libraries
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(ggpubr)) install.packages("ggpubr", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(scales)) install.packages("scales", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")
if(!require(Rborist)) install.packages("Rborist", repos = "http://cran.us.r-project.org")

#Importing dataset
dat <- read.csv(file = "bank-full.csv", header = T, sep = ";")
head(dat)

####################################
#### EXPLORATORY DATA ANALYSIS #####
####################################

# For this section, we explore our dataset and look for insights that could help in creating our model 
# in predicting whether a client will subscribe to a term deposit after a campaign

#####SUBSCRIBERS#####
# To get an initial idea, below is the distribution of clients who subscribed and did not subscribed.
dat %>%
  mutate(prop = round(ifelse(y == "yes", sum(y == "yes")/n(), sum(y == "no")/n()),2),
         prop = percent(prop),
         count = ifelse(y == "yes", sum(y == "yes"), sum(y == "no") + sum(y == "yes") + 2000)) %>%
  ggplot(aes(x = y, fill = y)) +
  geom_bar() +
  geom_text(aes(label = paste0("count: ",count," ", "percent: ", prop),
                y = count/2,
                group = y),
            vjust = 0.5,
            size = 4) +
  ylab("count")

# Overall, 5289 clients subscribed from the campaign, which is 12% of the total clients contacted
# from the campaign.

#####DEMOGRAPHICS#####

##Age##

#Summary of ages
summary(dat$age)
#The minimum age in the data is 18, while the maximum age is 95. The Median age is 39.

#Frequency of subscribers and non-subscribers among age groups
dat %>%
  mutate(ageGroup = ifelse(age %in% c(18:25), "18-25", 
                           ifelse(age %in% c(26:33), "26-33", 
                                  ifelse(age %in% c(34:41), "34-41",
                                         ifelse(age %in% c(34:41), "34-41",
                                                ifelse(age %in% c(42:49), "42-49",
                                                       ifelse(age %in% c(50:59), "50-59","60+"))))))) %>%
  ggplot(aes(ageGroup, fill = y)) +
  geom_bar() +
  ylab("Count") +
  xlab("Age Group")

#Based from the plot, most of the subscribers are aged between 26-33 and 34-41,
#while least of the subscribers are 18-25 and 60+

#Proportion of subscribers among age groups
fig1 <- dat %>%
  mutate(ageGroup = ifelse(age %in% c(18:25), "18-25", 
                           ifelse(age %in% c(26:33), "26-33", 
                                  ifelse(age %in% c(34:41), "34-41",
                                         ifelse(age %in% c(34:41), "34-41",
                                                ifelse(age %in% c(42:49), "42-49",
                                                       ifelse(age %in% c(50:59), "50-59","60+"))))))) %>%
  group_by(age, ageGroup) %>%
  summarize(count = n()) %>%
  ggplot(aes(age, count, fill = ageGroup)) +
  geom_col() +
  scale_x_continuous(breaks = seq(0, 90, 10)) +
  xlab("Age") +
  ylab("Count") +
  ggtitle("Age group sizes")

#Percent of clients that subscribed a term deposit by age group
fig2 <- dat %>%
  mutate(ageGroup = ifelse(age %in% c(18:25), "18-25", 
                           ifelse(age %in% c(26:33), "26-33", 
                                  ifelse(age %in% c(34:41), "34-41",
                                         ifelse(age %in% c(34:41), "34-41",
                                                ifelse(age %in% c(42:49), "42-49",
                                                       ifelse(age %in% c(50:59), "50-59","60+"))))))) %>%
  group_by(age, ageGroup) %>%
  summarize(prop = sum(y == "yes")/n()) %>%
  ggplot(aes(age, prop, fill = ageGroup)) +
  geom_col() +
  scale_x_continuous(breaks = seq(0, 90, 10)) +
  ylab("Subscribed (%)") +
  xlab("Age") +
  scale_y_continuous(labels = scales::percent) +
  ggtitle("Proportion of subscribers among age groups")

ggarrange(fig2, fig1, ncol = 1, nrow = 2, common.legend = TRUE, legend = "bottom")

#It can be seen from the plot that clients aged around 18-25 and 60+ 
#are more likely to subscribe from a campaign but their group sizes are few
#which is why most subscribers are still aged between 26-33.

##Job group##

dat %>%
  group_by(job) %>%
  mutate(yesnum = sum(y == "yes"),
         yesprcnt = sum(y == "yes")/n()) %>%
  ungroup() %>%
  ggplot(aes(x = reorder(job, yesnum), fill =y)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 40, hjust = 1)) +
  geom_text(aes(label = scales::percent(yesprcnt),
            y = yesnum/2),
            vjust = 0,
            size = 3) +
  xlab("Job Group") +
  ylab("Count")
# Based from the plot, most of the clients called were blue-collar workers, but majority who subscribed
# were working with management. Meanwhile, least of the subscribers were housemaids. With regards to
# proportion of subscribers per job group, the student and retired were most likely to be subscribed with
# 28.7% and 22.8% of the group size, while blue-collar workers were least likely to be subscribed, with
# only 7.28% of the group.

##Marital Status##

dat %>%
  group_by(marital) %>%
  mutate(yesnum = sum(y == "yes"),
         prop = ifelse(y == "yes", sum(y == "yes")/n(), sum(y == "no")/n()),
         count = ifelse(y == "yes", sum(y == "yes"), sum(y == "no") + sum(y == "yes"))) %>%
  ungroup() %>%
  ggplot(aes(x = marital, fill =y)) +
  geom_bar() +
  geom_text(aes(label = scales::percent(prop),
                y = count/2,
                group = y),
            vjust = 0,
            size = 4) +
  ylab("count")
# Based from the plot, majority of the subscribers are married,
# but only 10.1% of those married are subscribed. Moreover, single clients
# are more likely to be subscribers, comprising 14.9% of the single clients.


##Education##

dat %>%
  group_by(education) %>%
  mutate(yesnum = sum(y == "yes"),
         prop = ifelse(y == "yes", sum(y == "yes")/n(), sum(y == "no")/n()),
         count = ifelse(y == "yes", sum(y == "yes"), sum(y == "no") + sum(y == "yes"))) %>%
  ungroup() %>%
  ggplot(aes(x = education, fill =y)) +
  geom_bar() +
  geom_text(aes(label = scales::percent(prop),
                y = count/2,
                group = y),
            vjust = 0,
            size = 4) +
  ylab("count")
# Based from the plot, majority of the subscribers have secondary education level,
# but only 10.6% of those are subscribed. Furthermore, tertiary education level clients
# are more likely to be subscribers, comprising 15.0% of their group.

##Default##

dat %>%
  group_by(default) %>%
  mutate(yesnum = sum(y == "yes"),
         prop = ifelse(y == "yes", sum(y == "yes")/n(), sum(y == "no")/n()),
         count = ifelse(y == "yes", sum(y == "yes"), sum(y == "no") + sum(y == "yes") + 2000)) %>%
  ungroup() %>%
  ggplot(aes(x = default, fill =y)) +
  geom_bar() +
  geom_text(aes(label = scales::percent(prop),
                y = count/2,
                group = y),
            vjust = 0.5,
            size = 4) +
  ylab("count")
# Based from the plot, majority of the subscribers do not have credit in default, and
# are also more likely to be subscribed comprising 11.8% of their group.

##Loan##

fig3 <- dat %>%
  group_by(housing) %>%
  mutate(yesnum = sum(y == "yes"),
         prop = ifelse(y == "yes", sum(y == "yes")/n(), sum(y == "no")/n()),
         count = ifelse(y == "yes", sum(y == "yes"), sum(y == "no") + sum(y == "yes") + 2000)) %>%
  ungroup() %>%
  ggplot(aes(x = housing, fill =y)) +
  geom_bar() +
  geom_text(aes(label = scales::percent(prop),
                y = count/2,
                group = y),
            vjust = 0.5,
            size = 4) +
  ylab("count") +
  ggtitle("Housing Loan") +
  ylim(0, 40000)

fig4 <- dat %>%
  group_by(loan) %>%
  mutate(yesnum = sum(y == "yes"),
         prop = ifelse(y == "yes", sum(y == "yes")/n(), sum(y == "no")/n()),
         count = ifelse(y == "yes", sum(y == "yes"), sum(y == "no") + sum(y == "yes") + 2000)) %>%
  ungroup() %>%
  ggplot(aes(x = loan, fill =y)) +
  geom_bar() +
  geom_text(aes(label = scales::percent(prop),
                y = count/2,
                group = y),
            vjust = 0.5,
            size = 4) +
  ylab("count") +
  ggtitle("Personal Loan") +
  ylim(0, 40000)

ggarrange(fig3, fig4, nrow = 1, ncol = 2, common.legend = T, legend = "right")

# Based from the plot, majority of the clients had a housing loan
# but majority of the clients that subscribed did not have a
# housing loan. Furthermore, majority of the clients do not have
# a personal loan. Majority of the clients who also subscribed do
# not have a personal loan.

##Balance##

summary(dat$balance)

dat %>%
  filter(balance < 25000) %>%
  select(balance, y) %>%
  ggplot(aes(balance, fill = y)) +
  geom_histogram(color = "black", bins = 50) +
  scale_x_continuous(breaks = seq(-5000,25000,5000))

# We observe that most balances are between around -1000 euros to 10000 euros so we zoom a bit more in the
# code graph below. In building the algorithm, only balance within that threshold might be used to avoid
# outliers, or log transformation may be performed.

dat %>%
  filter(balance < 10000, balance > -1000) %>%
  select(balance, y) %>%
  ggplot(aes(balance, fill = y)) +
  geom_histogram(color = "black", bins = 50) +
  scale_x_continuous(breaks = c(0,seq(-500,10000,1000)))

# As observed from the graph, balanced of the clients are mostly between -500 euros to 1500 euros.
# Summary statistic tells us that the median is 448 euros.


#####CONTACT DETAILS#####

##Contact##

dat %>%
  group_by(contact) %>%
  mutate(yesnum = sum(y == "yes"),
         prop = ifelse(y == "yes", sum(y == "yes")/n(), sum(y == "no")/n()),
         count = ifelse(y == "yes", sum(y == "yes"), sum(y == "no") + sum(y == "yes") + 1000)) %>%
  ungroup() %>%
  ggplot(aes(x = contact, fill =y)) +
  geom_bar() +
  geom_text(aes(label = scales::percent(prop),
                y = count/2,
                group = y),
            vjust = 0.5,
            size = 4) +
  ylab("count")

# Based from the graph, most of the clients contacted was by cellular. Most clients who also subscribed
# were also contacted by cellular, having as well the highest proportion of the group size that subscribed.

##Date##
fig5 <- dat %>%
  mutate(date = as.Date(paste0(month, day), "%B%d")) %>%
  ggplot(aes(x = date, fill =y)) +
  geom_histogram(color = "black") +
  ylab("count") +
  scale_x_date(date_breaks = "1 month", labels = date_format("%b")) +
  ggtitle("Bar Graph")


fig6 <- dat %>%
  mutate(date = as.Date(paste0(month, day), "%B%d")) %>%
  ggplot(aes(x = date, fill =y)) +
  geom_density(alpha = 0.5) +
  scale_x_date(date_breaks = "1 month", labels = date_format("%b")) +
  ggtitle("Density Plot")

ggarrange(fig5, fig6, nrow = 2, ncol = 1)

# It seems that most of the clients are contacted between May to June. From the density plot,
# it can be seen that most subscribers are also within those months, followed by from July to September.

# Below we add date to the dataset
dat <- dat %>%
  mutate(date = as.Date(paste0(month, day), "%B%d")) %>%
  select(-month, -day)

##Duration##
summary(dat$duration)

dat %>%
  filter(duration < 1000) %>%
  mutate(duration = duration/60) %>%
  ggplot(aes(x = duration, fill =y)) +
  geom_histogram(color = "black", bins = 10) +
  scale_x_continuous(breaks = c(seq(0,100,1)))

# From the bar graph, it can be observed that most calls are between 2 minutes. But call above 3
# minutes look to have higher rate of client subscription than below 2 mins.

#####CAMPAIGN DETAILS#####
##Campaign##
head(dat)
summary(dat$campaign)
sum(dat$campaign)
# A total of 124956 contacts were made. The median number of contacts per client is 2.

dat %>%
  ggplot(aes(x = campaign, fill =y)) +
  geom_histogram(color = "black", bins = 20) +
  scale_x_continuous(breaks = c(1,2,3,10,30), trans = "log2")

# Most of the subscribers were contacted only once during the campaign

##pdays##
dat %>%
  filter(pdays > 0) %>%
  ggplot(aes(x = pdays, fill =y)) +
  geom_histogram(color = "black", bins = 50) +
  scale_x_continuous(breaks = c(0,100,200,300,400)) +
  coord_cartesian(xlim = c(-1, 400))
# Clients contacted just around less than 100 days since last contact were the most common among
# clients that were previously contacted. They also looks to have highest subscriber to non-subscriber ratio

# It does not look like there is a significant pattern shown in pdays so we explore further the proportion
# of subscribers in the below graph.

dat %>%
  group_by(pdays) %>%
  summarize(percent = mean(y == "yes")) %>%
  ggplot(aes(pdays, percent)) +
  geom_smooth() +
  geom_point(alpha = 0.2)

# Upon inspection, pdays do not seem to uncover a much meaningful insight or possible generalization,
# thus, this will not be used in building the algorithm.

##previous##
dat %>%
  group_by(previous) %>%
  filter(previous < 50) %>%
  summarize(percent = mean(y == "yes")) %>%
  ggplot(aes(previous, percent)) +
  geom_smooth() +
  geom_point()

# Upon inspection, it looks like the more number of contacts performed before this
# campaign and for this client, the less likely they will subscribe.But the larger
# previous numbers does not give much insight, and looks to show a lot of noise.
# Does this will not be used.

##poutcome##
dat %>%
  group_by(poutcome) %>%
  mutate(yesnum = sum(y == "yes"),
         prop = ifelse(y == "yes", sum(y == "yes")/n(), sum(y == "no")/n()),
         count = ifelse(y == "yes", sum(y == "yes"), sum(y == "no") + sum(y == "yes") + 2000)) %>%
  ungroup() %>%
  ggplot(aes(x = poutcome, fill =y)) +
  geom_bar() +
  geom_text(aes(label = scales::percent(prop),
                y = count/2,
                group = y),
            vjust = 0.5,
            size = 4) +
  ylab("count")

# based from the graph, most of the clients had unknown outcome from previous marketing campaign. But
# upon inspection, a successful outcome from previous marketing campaign shows a better chance of their
# subscription for a new campaign. Also,a previous failure shows the worst chance that they will subscribe.

###################
#### MODELING #####
###################

# In this section, we create an algorithm to predict if the client will subscribe a term deposit.

# First we divide the dataset into training and test sets.
test_index <- createDataPartition(y = dat$y, times = 1, p = 0.2, list = F)
train_set <- dat[-test_index,]
test_set <- dat[test_index,]

head(dat)
head(train_set)

# Logistics regression is done below as a benchmark as to whether a more complex model will be necessary.

# Logistics Regression

train_glm <- train(y ~ age + job + marital + education + default + balance + housing + loan + date +
                     contact + duration + campaign + poutcome, method = "glm", data = train_set, 
                   trControl = trainControl(method="cv", number = 5))

modelLookup("rf")

train_glm$results$Accuracy
max(train_rf_sample$results$Accuracy)

#  random forest sample

train_rf_sample <- train(y ~ age + job + marital + education + default + balance + housing + loan + date +
                           contact + duration + campaign + poutcome, method = "rf", data = train_set,
                         ntree = 150,
                         trControl = trainControl(method="cv", number = 5),
                         tuneGrid = data.frame(mtry = c(1, 5, 10, 15, 20, 25, 30)),
                         nsamp = 5000)
train_rf_sample
varImp(train_rf_sample)

# Based from the run of the code above, it seems that highest accuracy is found between mtry = 20 to 
# mtry = 30. Also, varImp shows that only duration, date, balance, age, campaign, housing, and contact
# have the highest importance values with numbers above 4. Below we rerun the code with variables having
# importances above 4 if accuracy is better.

train_rf_sample <- train(y ~ age + balance + housing  + date + contact + duration + campaign, 
                         method = "rf", 
                         data = train_set,
                         ntree = 150,
                         trControl = trainControl(method="cv", number = 5),
                         tuneGrid = data.frame(mtry = c(1, 5, 10, 15, 20, 25, 30)),
                         nsamp = 5000)
train_rf_sample

# mtry  Accuracy   Kappa    
# 1    0.8830181  0.0000000
# 5    0.8992480  0.4574513
# 10    0.8980038  0.4575283
# 15    0.8977826  0.4570911
# 20    0.8967318  0.4521092
# 25    0.8972848  0.4522970
# 30    0.8976167  0.4565980

varImp(train_rf_sample)
# duration         100.000
# date              72.754
# balance           48.805
# age               41.249
# campaign          12.448
# housingyes         5.248
# contactunknown     4.619
# contacttelephone   0.000


# accuracy is a bit worse compared to using all variables. Below we try with variables having
# importances above 10 if accuracy is better.

train_rf_sample <- train(y ~ age + balance  + date + duration + campaign, 
                         method = "rf", 
                         data = train_set,
                         ntree = 150,
                         trControl = trainControl(method="cv", number = 5),
                         tuneGrid = data.frame(mtry = c(1, 5, 10, 25, 50, 100)),
                         nsamp = 5000)
train_rf_sample

# mtry  Accuracy   Kappa    
# 1    0.8890457  0.2045489
# 5    0.8947411  0.4205695
# 10    0.8948241  0.4213362
# 15    0.8949071  0.4205364
# 20    0.8955153  0.4260244
# 25    0.8956260  0.4278819
# 30    0.8958194  0.4258057

# Accuracy is worse now with lesser predictors, thus we stick with using all predictors in the original 
# algorithm.

train_rf_sample <- train(y ~ age + job + marital + education + default + balance + housing + loan + date +
                           contact + duration + campaign + poutcome, method = "rf", data = train_set,
                         ntree = 150,
                         trControl = trainControl(method="cv", number = 5),
                         tuneGrid = data.frame(mtry = c(1, 5, 10, 25, 50, 100)),
                         nsamp = 5000)
train_rf_sample

train_rf_sample$bestTune$mtry

# mtry  Accuracy   Kappa    
# 1   0.8830181  0.0000000
# 5   0.9033953  0.4118114
# 10   0.9056902  0.4724902
# 25   0.9035612  0.4858299
# 50   0.9039759  0.4892939
# 100   0.9048607  0.4950827

# mtry = 10 gave the highest accuracy
# Upon checking, using random forest has a better accuracy than just using logistics regression

#Fitting final model
col_index_remove <- which(colnames(train_set) %in% c("pdays", "previous", "y"))
y_col_index <- which(colnames(train_set) == "y")

fit_rf <- randomForest(train_set[,-col_index_remove], factor(train_set[,y_col_index]),
                       minNode = train_rf_sample$bestTune$mtry)
y_hat_fitrf <- predict(fit_rf, test_set[,-col_index_remove])
confusionMatrix(y_hat_fitrf, factor(test_set[,y_col_index]))


plot(fit_rf)
# plot also shows that it look like we have used enough trees to get the best result out of the
# random forest