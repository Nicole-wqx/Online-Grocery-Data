setwd("/Users/wqx/Desktop/IMC Data Management and Visualization/Grocery Data R")
lookup <- read.csv("lookup.csv")
od <- read.csv("order_detail_sample.csv")
od <- detail %>% select(-cnsm_id) 
oh <- read.csv("order_header_sample.csv")
session <- read.csv("session_sample.csv")
customer <- read.csv("customer_sample.csv")
library(dplyr) 
library(ggplot2)
library(sqldf)

#Purchase behavior-Frequency

joined <- sqldf("
  SELECT 
    oh.cnsm_id AS cnsm_id,
    SUM(od.tot_pr_qy) AS monetary,
    COUNT(DISTINCT oh.ord_id) AS frequency
  FROM oh
  JOIN od ON oh.ord_id = od.ord_id
  GROUP BY oh.cnsm_id
") 

joined$rank <- cut(
  joined$monetary,
  breaks = quantile(joined$monetary, probs = seq(0, 1, 0.2), na.rm = TRUE),
  labels = FALSE,
  include.lowest = TRUE
)

mon <- sqldf("
  SELECT rank, AVG(monetary) AS Meanmon
  FROM joined
  GROUP BY rank
  ORDER BY rank
")

ggplot(mon, aes(y=Meanmon, x=rank)) +
  geom_bar(position='dodge', stat='identity', fill='lightgreen', color='black') +
  ggtitle('Mean monetary by monetary decile') +
  geom_text(aes(label=format(round(Meanmon, 0), nsmall=0, big.mark=',')), vjust=1.5, color='grey2')

# Filter only Rank 5 customers
rk5 <- sqldf("
  SELECT *
  FROM rank5_customers r5
  JOIN oh ON r5.cnsm_id = oh.cnsm_id
  JOIN od ON oh.ord_id = od.ord_id
  JOIN lookup ON od.pod_id = lookup.pod_id
")

# Minor Categories
category <- sqldf("
  SELECT minor_cat, COUNT(*) AS count
  FROM rk5
  GROUP BY minor_cat
  ORDER BY count DESC
  LIMIT 20
")

category$color <- with(category, 
                       ifelse(grepl("^FRESH MARKET", minor_cat), "lightgreen",  # Specific first
                              ifelse(grepl("^GROC BEVERAGE", minor_cat), "yellow", 
                                     ifelse(grepl("^GROC", minor_cat), "darkgreen", 
                                            ifelse(grepl("^FROZEN", minor_cat), "lightblue", 
                                                   ifelse(grepl("^DAIRY", minor_cat), "pink", "gray")))))
)

ggplot(category, aes(x = reorder(minor_cat, -count), y = count, fill = color)) + 
  geom_bar(position = 'dodge', stat = 'identity', color = 'black') +
  scale_fill_manual(values = c("lightgreen" = "lightgreen", "yellow" = "yellow", "darkgreen"="darkgreen", "lightblue" = 'lightblue', 'pink'='pink')) +
  labs(x = "Category", y = "Count", title = "Minor Categories") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# Age groups
age <- sqldf("
  SELECT c.cnsm_id, c.age
  FROM customer c
  JOIN rank5_customers r5 ON c.cnsm_id = r5.cnsm_id
")

age$age_group <- with(age, case_when(
  age < 18 ~ "under18",
  age >= 18 & age <= 30 ~ "18-30",
  age >= 31 & age <= 45 ~ "31-45",
  age >= 46 & age <= 60 ~ "46-60",
  age >= 61 & age <= 75 ~ "61-75",
  age > 75 ~ "over75"
))

age_count <- sqldf("
  SELECT age_group, COUNT(DISTINCT cnsm_id) AS count
  FROM age
  WHERE age_group IS NOT NULL
  GROUP BY age_group
")

age_count$percentage <- (age_count$count / sum(age_count$count)) * 100

# Plot the bar chart
age_count <- age_count %>%
  mutate(percentage = (count / sum(count)) * 100)

ggplot(age_count, aes(x = age_group, y = count)) +
  geom_bar(stat = 'identity', fill = 'lightgreen', color = 'black') +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), vjust = -0.5, size = 4) +  # Add percentage labels
  labs(x = "Age Group", y = "Distinct Count of cnsm_id", title = "Consumers by Age Group") +
  theme_minimal()

        
# Location
customer$zip6 <- sprintf("%05d", as.numeric(customer$zip5))

location <- sqldf("
  SELECT r5.cnsm_id, c.zip5 AS zip6
  FROM rank5_customers r5
  JOIN customer c ON r5.cnsm_id = c.cnsm_id
")

location$zip6 <- sprintf("%05d", as.numeric(location$zip6))
location$begin <- substr(location$zip6, 1, 2)

location2 <- sqldf("
  SELECT begin, COUNT(DISTINCT cnsm_id) AS number
  FROM location
  GROUP BY begin
")

library(ggplot2)

ggplot(location2, aes(y = number, x = begin, fill = ifelse(begin %in% c("02", "06", "11", "20", "60"), "green", "gray"))) +
  geom_bar(position = 'dodge', stat = 'identity', color = 'black') +
  geom_text(aes(label = number), vjust = -0.5, size = 3) +  # Add labels above bars
  scale_fill_manual(values = c("green" = "lightgreen", "gray" = "gray")) +  # Define colors
  theme_minimal() +  # Cleaner theme
  labs(x = "State", y = "Number") +
  theme(legend.position = "none") 


#################################################################################