library(tidyverse)

link <- 'https://maven-datasets.s3.amazonaws.com/Telecom+Customer+Churn/Telecom+Customer+Churn.zip'
temp <- tempfile()
download.file(link, temp)

churn <- read.csv(unz(temp, 'telecom_customer_churn.csv'))
zipcode_pop <- read.csv(unz(temp, 'telecom_zipcode_population.csv'))
data_dict <- read.csv(unz(temp, 'telecom_data_dictionary.csv'))

unlink(temp)

churn <- churn %>% mutate(is_churned = as.logical(replace(
  churn$Customer.Status, churn$Customer.Status == 'Churned', TRUE)))
churn$is_churned[is.na(churn$is_churned) == TRUE] <- FALSE

churn <- churn %>% inner_join(zipcode_pop, by = 'Zip.Code')

summary(churn)

cor(select(churn, Age, Number.of.Dependents, Number.of.Referrals, 
           Tenure.in.Months, Avg.Monthly.Long.Distance.Charges, 
           Avg.Monthly.GB.Download, Monthly.Charge, Total.Charges,
           Total.Refunds, Total.Extra.Data.Charges, Total.Long.Distance.Charges,
           Total.Revenue, Population))

model <- glm(is_churned ~ Age + Number.of.Dependents + Number.of.Referrals +
               Tenure.in.Months + Monthly.Charge + Total.Refunds + 
               Total.Extra.Data.Charges + Population, data = churn, family = 'binomial')
summary(model)

model2 <- glm(is_churned ~ scale(Age) + scale(Number.of.Dependents) + 
                scale(Number.of.Referrals) + scale(Tenure.in.Months) + 
                scale(Monthly.Charge) + scale(Total.Refunds) +
                scale(Total.Extra.Data.Charges) + scale(Population),
              data = churn, family = 'binomial')
summary(model2)

churn %>% filter(Customer.Status == 'Churned' & Tenure.in.Months <= 12) %>%
  count() / churn %>% filter(Customer.Status == 'Churned') %>% count()

churn$Offer[is.na(churn$Offer) == TRUE] <- 'None'

churn %>% filter(Customer.Status == 'Churned') %>% group_by(Offer) %>% 
  count() %>% full_join(churn %>% group_by(Offer) %>% count(), by = 'Offer') %>%
  mutate(percent_churned = n.x/n.y) %>% arrange(percent_churned)

select(churn, Offer, Customer.Status, Tenure.in.Months) %>% 
  filter(Offer == 'Offer E') %>% arrange(desc(Tenure.in.Months)) %>% head(5)

select(churn, Offer, Customer.Status, Tenure.in.Months) %>% 
  filter(Offer == 'Offer E') %>% arrange(Tenure.in.Months) %>% head(5)

select(churn, Offer, Customer.Status, Tenure.in.Months) %>% 
  filter(Offer == 'Offer A') %>% arrange(desc(Tenure.in.Months)) %>% head(5)

select(churn, Offer, Customer.Status, Tenure.in.Months) %>% 
  filter(Offer == 'Offer A') %>% arrange(Tenure.in.Months) %>% head(5)