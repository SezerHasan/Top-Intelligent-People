topIntelligent<-read.csv("top_intelligent.csv")

str(topIntelligent)
glimpse(topIntelligent)
colnames(topIntelligent)
library(tidyverse)
library(dplyr)

#  ---------------               1. DATA CLEANING              ----------------------

sapply(topIntelligent, function(x) sum(x == "N/A"))
table(topIntelligent$Awards) # Awards have 1249 NAs in string format

# Drop NAs in Awards
topIntelligent_cleaned<-subset(topIntelligent, Awards !="N/A")

# Alert: Sample size decreases substantially when NAs in Awards are removed. Use original data set
# when not analyzing Awards! Use cleaned version when working on Awards!



#  ---------------               2. DESCRIPTIVE STATISTICS               ----------------------

# 2.1. Distribution of awards by each country 

Awards_by_Country<-topIntelligent_cleaned%>%
  count(Country,Awards)

Awards_by_Country

ggplot(data = Awards_by_Country, aes(x=Country, y=n, fill=Awards))+
  geom_bar(stat = "identity", position = "dodge")+
  scale_y_continuous(labels = scales::label_number()) +
  labs(title = "Number of Awards by Country",
       x = "Country",
       y = "Count") +
  theme_minimal()

# 2.2 The country with the most Nobel Prize 

NobelPrize<-Awards_by_Country%>%
  filter(Awards=="Nobel Prize")

ggplot(data=NobelPrize, aes(x=Country, y=n))+
  geom_bar(stat = "identity", position = "dodge", fill="Skyblue")+
  scale_y_continuous(labels = scales::label_number()) +
  labs(title="Number of Nobel Prize by Country",
      x="Country",
      y="Count")+
  theme_minimal()

# 2.3 The country with the most awards

sum_country_awards <- topIntelligent %>%
  group_by(Country) %>%
  summarize(Total_Awards = n())

sum_country_awards %>%
  arrange(desc(Total_Awards))

ggplot(data=sum_country_awards, aes(x=Country, y=Total_Awards, fill=Country))+
  geom_bar(stat = "identity", position = "dodge")+
  scale_y_continuous(labels = scales::label_number()) +
  labs(title="Total Awards by Country",
       x="Country",
       y="Count")+
  theme_minimal()


# 2.2 IQ Analysis

summary(topIntelligent$IQ)

# IQ by Country= countries have very close IQs. 


IQ_by_Country <- topIntelligent %>%
  group_by(Country) %>%
  summarize(Average_IQ=mean(IQ, na.rm = TRUE))


IQ_by_Country

ggplot(data=IQ_by_Country, aes(x=Country, y=Average_IQ, fill=Country)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::label_number()) +
  labs(title="Average IQ by Country",
       x="Country",
       y="Average IQ") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# IQ and Education 

table(topIntelligent$Education) # Education is not very insightful as most of them have PhD degrees. 
IQ_by_Education <- topIntelligent %>%
  group_by(Education) %>%
  summarise(Avr_IQ=mean(IQ, na.rm = T))

IQ_by_Education

# IQ and Influence

table(topIntelligent$Influence)

IQ_by_Influence <-topIntelligent %>%
  group_by(Influence) %>%
  summarize(Average_IQ=mean(IQ, na.rm = TRUE))

IQ_by_Influence

# IQ and Awards

IQ_by_Awards <- topIntelligent_cleaned %>%
  group_by(Awards) %>%
  summarise(Avr.IQ=mean(IQ, na.rm = T))

IQ_by_Awards

# Intepretation: IQ variable is not very insightful as descriptive bivariable analysis with it yield very close IQ average. 



# 2.3 Fields Analysis

colnames(topIntelligent)

award_by_field <- topIntelligent_cleaned %>%
  count(Field.of.Expertise,Awards)
award_by_field

# Field with the most awards

field_with_most_awards <- award_by_field %>%
  group_by(Field.of.Expertise) %>%
  summarise(total_awards=sum(n))

field_with_most_awards

ggplot(data=field_with_most_awards, aes(x=Field.of.Expertise, y=total_awards))+
  geom_bar(stat = "identity", position="dodge", fill="orange")+
  scale_y_continuous(labels = scales::label_number()) +
  labs(title="Total Awards by Fields",
       x="Field of Expertise",
       y="Count")+
  theme_minimal()

# Fields by awards

field_by_award <- topIntelligent_cleaned %>%
  count(Field.of.Expertise,Awards)

field_by_award

ggplot(data=field_by_award, aes(x=Field.of.Expertise, y=n, fill=Awards))+
  geom_bar(stat = "identity", position="dodge",)+
  scale_y_continuous(labels = scales::label_number()) +
  labs(title="Total Awards by Fields",
       x="Field of Expertise",
       y="Count")+
  theme_minimal()

colnames(topIntelligent)
levels(topIntelligent$Awards)


#  -----   # How can one get a Nobel Prize: Logistic Regression Analysis   ----------------------

# I will try to predict what factors contribute to receiving a Nobel Prize. 

# Creating a new column "Nobel Prize" and dummy variables therein 
# where 0 is the reference category of not getting a Nobel Prize, and 1 is for Nobel Prize. 


topIntelligent_cleaned$NobelPrize <-ifelse(topIntelligent_cleaned$Awards=="Nobel Prize",1,0)

str(topIntelligent_cleaned) # looks good! But, many categorical variables are in string format!

topIntelligent_cleaned$Country<-as.factor(topIntelligent_cleaned$Country)
topIntelligent_cleaned$Field.of.Expertise<-as.factor(topIntelligent_cleaned$Field.of.Expertise)
topIntelligent_cleaned$Achievements<-as.factor(topIntelligent_cleaned$Achievements)
topIntelligent_cleaned$Gender<-as.factor(topIntelligent_cleaned$Gender)
topIntelligent_cleaned$Education<-as.factor(topIntelligent_cleaned$Education)
topIntelligent_cleaned$Influence<-as.factor(topIntelligent_cleaned$Influence)

# I will evaluate the model fit using a backward selection method, where I feed all the variables
# into the model and remove one a time by checking model fit using AIC and Deviance. 

backward_model<-glm(NobelPrize~Country+Field.of.Expertise+Achievements+Gender+Education+Influence,data = topIntelligent_cleaned, family = binomial())

stepwise_backward_model<-step(backward_model,direction = "backward")
# Interpretation: No variables are significant in their relation to Nobel Prize: 
# Removing each does not degrade the model fit (does not increase AIC)

summary(backward_model) # Wald statistical significance test for Logistic regression 

summary(stepwise_backward_model) 

#  ---------  Checking why no predictor is significant  ----------------------

confint.default(backward_model)

exp(coef(backward_model))

library(broom)
tidy(stepwise_backward_model, exponentiate = TRUE)

# Checking Multicollinearity
install.packages("car")
library(car)

vif(backward_model)

# Interpretation: VIFs are lower than 10, so variables in the model do not seem to to correlated




