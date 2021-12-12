library(tidyverse)
library(magrittr)
library(grid)
library(gtable)
library(lattice)
library(gridExtra)
library(lme4)
library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(stats)
library(arm)
library(lmerTest)
library(stringr)

# read the data set into R
raw_data <- read.csv('train_data.csv')

# drop the observations that loss information
data <- raw_data %>% filter(enrollee_id != '', city != '', relevent_experience != '',
                        gender != '', city_development_index != '', education_level != '',
                        enrolled_university != '', education_level != '',
                        major_discipline != '', experience != '', last_new_job != '',
                        training_hours != '')

# switch the character variable to factor variable
tidy_data <- raw_data %>% mutate_if(is.character, as.factor)




# EDA

# sample size helper function
check_size <- function(n){
  moded <- n %/% 50
  if (moded <= 6 & moded >= 1) {
    return(paste('Between', moded * 50, 'and', (moded + 1) * 50))
  } else if (moded < 1) {
    return('Smaller Than 50')
  } else {
    return('Larger Than 300')
  }
}

# get tidy group-level data
group_level <- data %>% group_by(city) %>% summarize(n = n())
group_level <- group_level %>%
  mutate(scale = apply(as.data.frame(group_level$n), 1, FUN = check_size))

# plot the size of groups
group_level %>% group_by(scale) %>% summarize(n = n()) %>% ggplot(aes(x = scale, y = n)) +
  geom_col(aes(fill = scale)) + geom_text(aes(label = n)) +
  theme(axis.text.x = element_text(angle = 15))


## sample size versus development index
p1 <- data %>% group_by(city) %>% summarize(dev_index = mean(city_development_index), n = n()) %>%
  ggplot() + geom_point(aes(x = n, y = dev_index)) +
  labs(x = 'sample size', y = 'development index', title = 'group size versus city development index')


## gender proportion across cities
p2 <- data %>% ggplot() + geom_bar(aes(x = gender, fill = gender)) +
  labs(title = 'number of observation in each gender group')
p3 <- data %>% group_by(city) %>% ggplot() + geom_bar(aes(x = city, fill = gender), stat = 'count',
                                                position = 'fill') +
  labs(x = NULL, y = 'proportion', title = 'gender proportion across cities')
grid.arrange(p2, p3, ncol = 2)

## education level across cities
data %>% group_by(city) %>% ggplot() + 
  geom_bar(aes(x = city, fill = education_level), stat = 'count', position = 'fill') +
  labs(x = NULL, y = 'proportion', title = 'education across cities')

temp <- filter(data, gender == 'Male') %>% mutate(training_hours = training_hours + 1)




# Model establishment
fit1 <- glmer(target ~ education_level + training_hours + gender +
               (1 + experience| city), family = binomial(link = 'logit'), data = sliced_city)




fit3 <- glmer(target ~ education_level + log(training_hours) + 
                (1 + education_level + log(training_hours)| city), 
              family = binomial(link = 'logit'), data = data)

fit2 <- glm(target ~ education_level + training_hours,
            family = binomial(link = 'logit'), data = temp)

theme_set(theme_sjplot())

plot_model(fit1, vline.color = "red", sort.est = TRUE, show.values = TRUE,
           show.p = FALSE, title = "estimates of coefficients", ylim = 0.5)

summary(fit1)

# ?
plot(fit1)
binnedplot(fitted(fit1),resid(fit1))
qqmath(fit3)
qqnorm(resid(fit1))
plot(resid(fit1))+ abline(h = 0)


plot(fit2)
qqmath(fit2)
qqnorm(resid(fit2))


# relationship between city and job search condition
data %>% 
  group_by(city,target) %>%
  summarise(count = n())%>%
  mutate(ratio = count / sum(count),
         label = 100 * (ratio %>% round(2))) %>% 
  filter(city %in% c("city_103","city_21","city_16","city_114",
                     "city_160","city_136","city_67",
                     "city_75","city_102","city_104")) %>% 
  ggplot(aes(x=reorder(city,-count),
             y=ratio,
             fill=target)) + 
  geom_bar(stat='identity', color = 'black') +
  scale_fill_manual(values = c('skyblue', 'tomato'),
                    labels = c("Not searching for a new job", 
                               "Searching for a new job")) +
  theme(axis.text.x = element_text(vjust = 5, 
                                   hjust = 0.5, 
                                   size = 12)) +
  scale_y_continuous(labels = percent) +
  theme_hc() +
  theme(plot.title=element_text(size=30, face="bold", hjust = 0.5),
        plot.subtitle=element_text(size=22, hjust = 0.5),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(vjust = 5, 
                                   hjust = 0.5, 
                                   size = 14),
        axis.text.y = element_text(size = 16),
        legend.position = "top",
        legend.title=element_blank(),
        legend.text=element_text(size=18)) +
  labs(
    title = 
      "Relationship between city and job search"
  )


# The sample size of each education level
tidy_data %>% ggplot()+
  geom_bar(mapping = aes(x = education_level, fill=education_level),show.legend = TRUE)+
  ggtitle("sample size of each education level")

AIC(fit1)


```{r}


# exclude index
fit1.2 <- glmer(target ~ enrolled_university + education_level + last_new_job + log(training_hours) + experience + company_size + company_type + (1|city), family = binomial(link = 'logit'), data = model_train)

# city_index in random
fit2 <- glmer(target ~ experience + enrolled_university + education_level  + training_hours + company_type + company_size + last_new_job + (1 + city_development_index|city), family = binomial(link = 'logit'), data = model_train)

# company_size in random
fit3 <- glmer(target ~ city_development_index + enrolled_university + education_level + last_new_job + log(training_hours) + experience + company_type + (1 + company_size|city), family = binomial(link = 'logit'), data = model_train)


# company type in random
fit4 <- glmer(target ~ city_development_index + enrolled_university + education_level + last_new_job + log(training_hours) + experience + company_size + (1 + company_type|city), family = binomial(link = 'logit'), data = model_train)


# enrolled uni in random
fit5 <- glmer(target ~ city_development_index + education_level + last_new_job + log(training_hours) + experience + company_size + company_type + (1 + enrolled_university|city), family = binomial(link = 'logit'), data = model_train)


# educa level in random
fit6 <- glmer(target ~ city_development_index + enrolled_university + last_new_job + log(training_hours) + experience + company_size + company_type + (1 + education_level|city), family = binomial(link = 'logit'), data = model_train)


# last job in random
fit7 <- glmer(target ~ city_development_index + enrolled_university + log(training_hours) + experience + company_size + company_type + education_level + (1 + last_new_job|city), family = binomial(link = 'logit'), data = model_train)
```


