library(ggplot2)
library(dplyr)
library(tidyverse)

df<-read.csv("E:/netflix_titles.csv", na.strings = c("","NA"))

missing.values <- df %>%
  gather(key = "key", value = "val") %>%
  mutate(isna = is.na(val)) %>%
  group_by(key) %>%
  mutate(total = n()) %>%
  group_by(key, total, isna) %>%
  summarise(num.isna = n()) %>%
  mutate(pct = num.isna / total * 100)


levels <-
  (missing.values  %>% filter(isna == T) %>% arrange(desc(pct)))$key

percentage.plot <- missing.values %>%
  ggplot() +
  geom_bar(aes(x = reorder(key, desc(pct)), 
               y = pct, fill=isna), 
           stat = 'identity', alpha=0.8) +
  scale_x_discrete(limits = levels) +
  scale_fill_manual(name = "", 
                    values = c('#b20710', '#221f1f'), labels = c("Present", "Missing")) +
  coord_flip() +
  labs(title = "Percentage of missing values", x =
         'Variable', y = "% of missing values")

percentage.plot

# Replacments
calc_mode <- function(x){
  # List the distinct / unique values
  distinct_values <- unique(x)
  
  # Count the occurrence of each distinct value
  distinct_tabulate <- tabulate(match(x, distinct_values))
  
  # Return the value with the highest occurrence
  distinct_values[which.max(distinct_tabulate)]
}
df %>% 
  mutate(country = if_else(is.na(country), 
                         calc_mode(country), 
                         country))

df %>% 
  mutate(cast = if_else(is.na(cast), 
                           "No Data", 
                           cast))

df %>% 
  mutate(director = if_else(is.na(director), 
                        "No Data", 
                        director))


# Drops

df_2 <- na.omit(df)

# Drop Duplicates

df_2 <- df_2 %>% distinct()

# dealing datetime
df_2$date_added <- lubridate::mdy(df_2$date_added)

df_2$month_added = month(df_2$date_added)
df_2$MonthName_Added <- month.abb[df_2$month_added]
df_2$year_added = year(df_2$date_added)

count<- function(x){
  # List the distinct / unique values
  distinct_values <- unique(x)
  
  # Count the occurrence of each distinct value
  distinct_tabulate <- tabulate(match(x, distinct_values))
  distinct_tabulate
}
count_year_added<-count(df_2$year_added)
year<-distinct_values <- unique(df_2$year_added)
year_added_count<-data.frame(df_2$type,df_2$year_added)

ggplot(year_added_count, aes(x=df_2.year_added, fill=df_2.type)) +
  geom_area(stat ="bin",binwidth=1)
# Use semi-transparent fill
p<-ggplot(year_added_count, aes(x=df_2.year_added, fill=df_2.type)) +
  geom_area(stat ="bin", binwidth=1)
  
p<-p+scale_fill_manual(values=c('#b20710', '#221f1f')) 
p

df_2 <- df_2 %>% mutate(MonthName_Added=factor(MonthName_Added, 
                                      levels=c("Jan", "Feb", "Mar",
                                               "Apr", "May", "Jun", 
                                               "Jul", "Aug", "Sep", 
                                               "Oct", "Nov", "Dec"), ordered=TRUE))
q = ggplot(df_2, aes(x=MonthName_Added))+
  geom_bar(stat="count", width=0.7)+theme_light() +
  scale_fill_gradient(low='#221f1f', high='#b20710', limits=c(300,500)) +
  theme(axis.title.y=element_text(angle=0))
q + theme(axis.text.x = element_text(angle=45, vjust = 1, hjust=1))
q + coord_polar()

for (i in 1: 4808){
  df_2$firstCountry[i] = str_split(df_2$country[i], ",")[[1]][1]
}

for (i in 1: 4808){
  df_2$genre[i] = str_split(df_2$listed_in[i], ",")[[1]][1]
}


df_genre<-data.frame(df_2$firstCountry, df_2$genre)
  # List the distinct / unique values
distinct_values_2 <- unique(df_genre$df_2.firstCountry)
  
  # Count the occurrence of each distinct value
distinct_tabulate_2 <- tabulate(match(df_genre$df_2.firstCountry, distinct_values))
df_country_10<-data.frame(distinct_values_2, distinct_tabulate_2)
  
df_genre_10 <- df_country_10 %>%                                      # Top N highest values by group
  arrange(desc(df_country_10$distinct_tabulate_2))
df_genre_10<- head(df_genre_10, 6)

df_genre_fix<-df_genre[(df_genre$df_2.firstCountry == "United States"
                        | df_genre$df_2.firstCountry == "India"
                        | df_genre$df_2.firstCountry == "United Kingdom"
                        |df_genre$df_2.firstCountry == "Canada"
                        |df_genre$df_2.firstCountry == "France"
                        |df_genre$df_2.firstCountry == "Spain"),]

df_genre_fix %>%
  group_by(df_2.firstCountry) %>%
  ggplot(., aes(x = factor(df_2.firstCountry), y = df_2.genre)) +
  geom_tile(aes(alpha = 0.7, fill = df_2.genre), color = "white") +
  scale_alpha(range = c(0.1, 1)) +
  theme(legend.position = "none")

df_hist <- data.frame(df_2$duration)

for (i in 1: 4808){
  df_hist$df_2.duration[i] = str_split(df_hist$df_2.duration[i], ",")[[1]][1]
  if (grepl("Season", df_hist$df_2.duration[i][[1]])){
    df_hist$df_2.duration[i] = NA
  }
}
df_hist_1 = na.omit(df_hist)
for (i in 1: 4673){
  df_hist_1$df_2.duration[i] = str_split(df_hist_1$df_2.duration[i], " ")[[1]][1]
}


# Density plots with semi-transparent fill
ggplot(df_hist_1, aes(x=df_2.duration, fill="count")) + geom_density(alpha=.3, , color="#b20710")

