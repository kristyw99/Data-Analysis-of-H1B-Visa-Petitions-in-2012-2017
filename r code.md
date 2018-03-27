---
title: "
H1B Visa Petition Analysis"
author: "Tingting Wang" 
---


> **Context**:  The H-1B program allows foreign workers to be temporarily employed by U.S. employers in occupations that require the theoretical and practical application. The disclosure application data can be accessed to perform in-depth analysis to get the key insights of this program. In particular, the data in the period of 2012 -2017 is used here to analyze which employers, occupations and states have the most applicants, how the prevailing wage varies across different employers and occupations over 2012-2017, and the corresponding information of possible data science related occupations.



```{r packages, echo=FALSE, message = FALSE, warning = FALSE}
# Load all of the packages used in the following analysis 

library(dplyr)
library(ggplot2)
library(readxl)
library(hashmap)
library(ggmap)
library(tidyr)
library(RColorBrewer)
library(knitr)
library(gridExtra)

#Empty data frame
h1b_df = data.frame()
new_df = data.frame()

```
# Data Preparation

The overview of each year’s raw dataset downloaded from OFLC provides the following preliminary information: in the period of 2012-2017, each dataset includes 40 columns in each year's records and the column names completely changed after 2015. To get the information of the H1B petition information over years, we should merge the separated dataset into one file. To do that, the first step before performing the data analysis should be rename the relevant columns in records from 2012-2014 to match with the newer records from 2015-2017. Then the “YEAR” column was added and the columns which are relevant to our analysis were selected and merged into one dataset. 

The columns in the dataset include:

1) `CASE_STATUS`: Status associated with the last significant event or decision. Valid values include “Certified,” “Certified-Withdrawn,” Denied,” and “Withdrawn”. This feature will help us analyze what share of the H-1B visa is taken by different employers / job positions. 

2) `EMPLOYER_NAME`: Name of employer submitting the H1-B application. Used in comparing salaries and number of applications of various employers.

3) `JOB_TITLE`: Title of the job using which we can filter specific job positions for e.g., Data Scientist, Data Engineer etc.

4) `FULL_TIME_POSITION`: Y = Full Time Position; N = Part Time Position

5) `PREVAILING_WAGE`: The prevailing wage for a job position is defined as the average wage paid to similarly employed workers in the requested occupation in the area of intended employment. The prevailing wage is based on the employer’s minimum requirements for the position. (Source). This column will be one of the key metrics of the data analysis.

6) `WORKSITE_CITY`: WORKSITE_STATE: The foreign worker’s intended area of employment. We will explore the relationship between prevailing wage for Data Scientist position across different locations.

7) `SOC_NAME`: Occupational name associated with the SOC_CODE. SOC_CODE is the occupational code associated with the job being requested for temporary labor condition, as classified by the Standard Occupational Classification (SOC) System.

Among these attributes of the dataset, the main features of interest are the prevailing wage and quantity of H1B petitions. The following analysis will focus on which employers, occupations and states have the most applicants, how the prevailing wage varies across different employers and occupations over 2012-2017, and the corresponding information of Data Science related occupations.

```{r Load_the_Data, echo=FALSE}
# Load each year's data 
for(year in seq(2017,2012)) {
  print(paste0("Year=", as.character(year)))  
  raw_data_path = paste0("~/Downloads/capstone project/DATA/",year,"rawdata.xlsx")
  raw_data_path 
  new_df = read_excel(raw_data_path)
  print(paste0( as.character(year), " Raw data size: ", as.character(dim(new_df))))  
  
# Changing column names of data before 2015
if(year != 2015 & year != 2016 & year != 2017){   
    new_df = new_df %>%
      mutate(CASE_NUMBER = LCA_CASE_NUMBER,
             CASE_STATUS = STATUS,
             EMPLOYER_NAME = LCA_CASE_EMPLOYER_NAME,
             SOC_NAME = LCA_CASE_SOC_NAME,
             SOC_CODE = LCA_CASE_SOC_CODE,
             JOB_TITLE = LCA_CASE_JOB_TITLE,
             FULL_TIME_POSITION = FULL_TIME_POS,
             PREVAILING_WAGE = PW_1,
             PW_UNIT_OF_PAY = PW_UNIT_1,
             WORKSITE_CITY = LCA_CASE_WORKLOC1_CITY,
             WORKSITE_STATE = LCA_CASE_WORKLOC1_STATE)
}
  
  #write.csv(new_df,"~/Downloads/capstone project/DATA/2011rawdatadeal.csv")
    # Adding Year column to dataframe
    new_df = new_df %>%
      mutate(YEAR = as.character(year))
    
    print(paste0( as.character(year), " Mutated data size: ", as.character(dim(new_df))))  
    
    # Selecting only the relevant columns
    new_df = new_df %>%
      select(CASE_NUMBER,
             CASE_STATUS,
             EMPLOYER_NAME,
             SOC_NAME,
             SOC_CODE,
             JOB_TITLE,
             FULL_TIME_POSITION,
             PREVAILING_WAGE,
             PW_UNIT_OF_PAY,
             WORKSITE_CITY,
             WORKSITE_STATE,
             YEAR)
    
    
    # Merging data with already transformed data
    h1b_df = rbind(h1b_df, new_df)
    print(paste0( as.character(year), " Merged data size: ",as.character(dim(h1b_df))))
}

write.csv(h1b_df,"~/Downloads/capstone project/DATA/h1bdata.csv")

    # Saving read data frame
    saveRDS(h1b_df,"h1b_df_no_transform.rds")
    
    # h1b_df_tx will undergo all transformations
    h1b_df_tx <- h1b_df
    colnames(h1b_df_tx)
    glimps(h1b_df_tx)
```

---

# Data Analysis

#### 1. H-1B Petition Case Status
The dataset prepared for analysis has more than 3,000,000 records which means more than 3 million applicants filed the H-1B petition over the past six years. So the first issue which needs to be considered is how many petitions are certified each year. 

```{r H-1B Petition Case Status, echo=FALSE}

# Convert all the lowercase characters to uppercase
h1b <- data.frame(lapply(h1b_df_tx, function(v) {
  if (is.character(v)) return(toupper(v))
  else return(v)
}))

h1b <- tbl_df(h1b)
glimpse(h1b)

# Keep only "CERTIFIED" H1B cases
certified_h1b <- h1b %>%
  filter(CASE_STATUS == "CERTIFIED")

glimpse(certified_h1b)

# Count H1B petitions filed in each year
case_quantity_per_year <- certified_h1b %>%
  group_by(YEAR) %>%
  summarise(Certified = n())

# Bar plot showing the H1B quantities in each year
ggplot(case_quantity_per_year, aes(y = Certified, x = YEAR, fill = YEAR)) + 
  geom_bar(stat = "identity", alpha = 0.7, width = 0.5) + 
  scale_y_continuous(limits = c(0, 570000), 
                     breaks = seq(0, 570000, 100000),
                     labels = scales::comma) + 
  ggtitle("Certified H-1B Petitios in Each Year")+
  theme(
    plot.title = element_text(size = rel(1.3)),
    panel.background = element_rect(fill = '#f0f0f0'),
    legend.position = "none"
  )

    

```

#### 2. Top 10 occupations with the most H-1B applicants

The next question that comes to mind naturally is what kinds of jobs the applicants are doing and which jobs have the most H-1B applications.  

```{r Top 10 occupations with the most H-1B applicants, echo=FALSE}
# Function to return top N occupations that have the most H1B applicants
top_N_SOC <- function(num) {
  certified_h1b %>%
    filter(!is.na(certified_h1b$SOC_NAME)) %>%
    group_by(SOC_NAME) %>%
    summarise(num_apps = n()) %>%
    arrange(desc(num_apps)) %>%
    slice(1:num)
}

# Bar plot to show the top 10 H1B occupations 
ggplot(top_N_SOC(10), 
       aes(x = reorder(SOC_NAME, num_apps), y = num_apps)) +
  geom_bar(stat = "identity", alpha = 0.9, fill = "orange", width = 0.7) +
  coord_flip() +
  scale_y_continuous() +
  geom_text(aes(label = num_apps), hjust = -0.2, size = 2) +
  ggtitle("Top 10 Occupations with Most H1B Petitions in 2012-2017 Period") +
  theme(plot.title = element_text(size = rel(1)),
        axis.text.y = element_text(size = rel(0.8))) +
  labs(x = "SOC Name", y = "No. of Applications")

```

#### 3. Top 10 Employers with the most H-1B applicants

If we make a list of which employers file the most number of H-1B visa applications in period of 2012-2017, we can see that the Indian IT companies dominate the top 10. Infosys has filed more than 150,000 petitions in the past six years, which is almost two times of the company on the second position. 

```{r Top 10 Employers with the most H-1B applicants, echo=FALSE}

# Function to return the top N employers that have the most H1B workers
top_N_employers <- function(num_emp) {
  certified_h1b %>%
    group_by(EMPLOYER_NAME) %>%
    summarise(num_apps = n()) %>%
    arrange(desc(num_apps)) %>%
    slice(1:num_emp)
}


# Bar plot to show the top 10 employers who filed the most h1b visa applications
ggplot(top_N_employers(10), 
       aes(x = reorder(EMPLOYER_NAME, num_apps), y = num_apps)) +
  geom_bar(stat = "identity", alpha = 0.9, fill = "sky blue", width = 0.7) +
  coord_flip() +
  scale_y_continuous(limits = c(0, 160000), breaks = seq(0, 140000, 20000)) +
  geom_text(aes(label = num_apps), hjust = -0.2, size = 2) +
  ggtitle("Top 10 Employers with most applications") +
  theme(plot.title = element_text(size = rel(1)),
        axis.text.y = element_text(size = rel(0.8))) +
  labs(x = "Employer Name", y = "No. of Applications")


```

#### 4. H-1B petitions by states


After investigated the employers and occupations with the most applicants, we then would like to find out the relations between locations and H-1B petitions. So we investigated the top 10 states which provides the most job opportunities for H-1B applicants. Clearly, California take the lead with more than 500,000 applications in the past six years, followed by Texas and New York. This result is expected because Silicon Valley is located in California, the petroleum and emerging IT industry is located in Texas, and New York has always been the finance center of the U.S. 

```{r H-1B petitions by states, echo=FALSE}

# Count H1B petitions filed by each state
petition_by_state <- certified_h1b %>%
  filter(WORKSITE_STATE != "NA") %>%
  group_by(region = tolower(WORKSITE_STATE)) %>%
  summarise(no_petitions = n()) %>%
  arrange(desc(no_petitions))
 
g <- ggplot(petition_by_state[1:10,], aes(x=reorder(region,no_petitions),y=no_petitions)) +
  geom_bar(stat = "identity", fill = "blue") + coord_flip() +
  ggtitle("H1B petitions by state in 2012-2017 Period")+
  get_theme() +  
  labs(x = "Number of petitions", y = "State") + scale_color_discrete()

g


```
If we take a look at the application data in these three states in each year, the same trends can be found as the whole application presents above. The filed H-1B applications in these three states all gradually increased from 2012-2016, and then decreased slightly in 2017. With stricter policy coming out, we can expect the continuing decrease in H-1B applications next year.



```{r H1B petitions in CA, NY and TX , echo=FALSE}

# Count H1B petitions in CA, NY and TX in each year
cnt_case_per_year <- certified_h1b %>%
  filter(WORKSITE_STATE %in% c("CA", "NY", "TX")) %>%
  group_by(YEAR, WORKSITE_STATE) %>%
  summarise(count = n()) %>%
  arrange(YEAR, WORKSITE_STATE) 

# Bar plot showing H1B quantities of each state in each year
ggplot(cnt_case_per_year, aes(x = YEAR, y = count, fill = WORKSITE_STATE)) +
  geom_bar(stat = "identity", position = position_dodge(), alpha = 0.8,
           color = "grey") +
  ggtitle("Quantity of H1B cases in California, New York and Texas from 2012 to 2017") +
  theme(legend.position = "bottom",
        plot.title = element_text(size = rel(1.3)))

```
#### 5.Prevailing wages distribution in the top 10 employers

Next, let’s take a look at the prevailing wage distributions in the top 10 employers. From the boxplot we can see that the median wage for the high-applicant companies is around $70,000 whereas the median wage for Microsoft, one of the biggest tech companies, is around $90,000, which exceeds all other major sponsor companies. Also, Tata Consultancy has the the smallest interquantile range of prevailing wages compared to that of other companies. In other words, Tata Consultancy has the least variation in wages for its middle 50% of H-1B workers.


```{r Top 10 occupations with the highest wages, echo=FALSE}

# Top 10 employers who filed the most H1B petitions
top_10_employers <- certified_h1b %>%
  group_by(EMPLOYER_NAME) %>%
  summarise(num_apps = n()) %>%
  arrange(desc(num_apps)) %>%
  slice(1:10) %>%
  select(EMPLOYER_NAME)

employers_boxplot_df <- certified_h1b %>%
  filter(EMPLOYER_NAME %in% top_10_employers$EMPLOYER_NAME)

# Boxplot showing the wage distribution of each employer
ggplot(employers_boxplot_df, aes(y = PREVAILING_WAGE, x = EMPLOYER_NAME, 
                                 fill = EMPLOYER_NAME, notch = TRUE, notchwidth = .3)) + 
  geom_boxplot(notch = TRUE) + 
  #scale_y_continuous(limits = c(0, 150000), 
                     #breaks = seq(0, 150000, 10000)) + 
  ggtitle("Wages for H1B cases in top 10 companies")+
  theme(
    plot.title = element_text(size = rel(1.3)),
    panel.background = element_rect(fill = '#f0f0f0'),
    axis.text.x=element_blank(),
    legend.position = "bottom", 
    legend.title = element_text(size = rel(0.7)),
    legend.text = element_text(size = rel(0.4)), 
    panel.grid.major = element_line(colour = '#f0f0f0'),
    panel.grid.major.x = element_line(linetype = 'blank'),
    panel.grid.minor = element_line(linetype = 'blank')  
  )


```

#### 6.Top 10 occupations with highest median prevailing wages

The next question which needs to be solved is which occupations have the highest average wages. The median wage is used here as the metric to compare prevailing wages of different occupations, which will help reduce distortion and provide a better picture. We can find the occupation having the highest median wage is Physicians and surgeons. Out of the top 10 high income occupations, 8 are in the medical and health care field, which consistent with our common sense because these jobs are known to make more money. Also, it can be easily found out that none of the top 10 occupations with the most H-1B petitions (computer system analysts, software developers, etc.), is included in the top 10 high income ones. 

```{r Top 10 occupations with highest median prevailing wages, echo=FALSE}

# Top 10 occupations with the highest wages
top_10_soc_with_highest_wage <- certified_h1b %>%
  group_by(SOC_NAME) %>%
  summarise(median_wage = median(PREVAILING_WAGE)) %>%
  arrange(desc(median_wage)) %>%
  slice(1:10) %>%
  select(SOC_NAME, median_wage)

# Bar plot showing median wages for each occupation
ggplot(top_10_soc_with_highest_wage, aes(y = median_wage, x = reorder(SOC_NAME, median_wage))) + 
  geom_bar(stat = "identity", fill = "blue", alpha = 0.7, width = 0.7) + 
  #scale_y_continuous(limits = c(0, 150000), 
  #breaks = seq(0, 150000, 5000)) + 
  ggtitle("Top 10 occupations with highest median prevailing wages") +
  coord_flip() +
  theme(
    plot.title = element_text(size = rel(1)),
    axis.text.x=element_text(size = rel(0.8)),
    legend.position = "bottom"
  ) +
  labs(x = "Occupational Name")


knitr::kable(top_10_soc_with_highest_wage)

```

#### 7.Prevailing wages trend in the period of 2012-2017


We then depicts the trend of mean, median, 10th percentile and 90th percentile of wages from 2012 to 2017. In this period, wages for H-1B applicants are increasing gradually at a relatively low rate. The mean wages being larger than the median wages every year indicates that the wage distribution is right skewed and these outliers are so extreme that they drag the mean wages up.


```{r Prevailing wages trend in the period of 2012-2017, echo=FALSE}

set.seed(123)

# Filter out the top 1% outliers in the prevailing_wage variable
wage_year_sample <- subset(certified_h1b[sample(1:nrow(certified_h1b), 300000), 
                                         c(8, 12)], 
                           !is.na(PREVAILING_WAGE) & 
                             PREVAILING_WAGE <= quantile(certified_h1b$PREVAILING_WAGE, 
                                                         0.99))

wage_year_sample <- wage_year_sample %>%
  group_by(YEAR) %>%
  mutate(mean_wage = mean(PREVAILING_WAGE),
         median_wage = median(PREVAILING_WAGE),
         '10th_percentile' = quantile(PREVAILING_WAGE, 0.1),
         '90th_percentile' = quantile(PREVAILING_WAGE, 0.9))

wage_year_stats <- wage_year_sample %>%
  distinct(mean_wage, median_wage, `10th_percentile`, `90th_percentile`)

wage_year_stats <- wage_year_stats[order(wage_year_stats$YEAR), 
                                   c(ncol(wage_year_stats), 
                                     1:(ncol(wage_year_stats) - 1))]

# From wide to long format
wage_year_stats_long <- gather(wage_year_stats, statistics, values, 
                               mean_wage, median_wage,`10th_percentile`, `90th_percentile`,
                               factor_key = TRUE)

# Trend of median, mean, 10th percentile and 90th percentile of wages
wage_trend <- 
  ggplot(wage_year_stats_long , 
                     aes(x = YEAR, y = values, group = statistics)) +
  geom_line(aes(color = statistics), lineend = "round", size = 1) +
  expand_limits(y = 0) +
  scale_y_continuous(breaks = seq(0, 120000, 10000), labels = scales::comma) +
  ggtitle("Wage Trend from 2011 to 2016: Line Chart") +
  labs(y = "wage / $") +
  theme(plot.title = element_text(size = rel(1.3)),
        legend.position = "bottom")

wage_trend


```

