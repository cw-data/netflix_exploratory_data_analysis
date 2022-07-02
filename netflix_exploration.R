
library(tidyverse)
library(janitor)
library(jtools)
library(gt)
library(dplyr)

# setwd("C:/Users/wainr/OneDrive/Documents/netflix_exploratory_data_analysis")
# https://www.kaggle.com/code/svitlanabozhenko/netflix

################################## Netflix Credits ################################## 
#### load data and wrangle
getwd() # confirm my current working directory
credits <- read.csv("credits.csv") # read the csv from our working directory
credits <- credits %>%
    clean_names() # janitor::clean_names() changes column names to snake_case (to match the tidyverse style guide)
glimpse(credits) # tidyverse alternative to head() that includes df dimensions (rows and cols) and data types (e.g., <int> or <chr>)
summary(credits) # Besides giving quick access to column-wise summary statistics, summary() me if there are NAs in a column.

# Double-checking whether there are NAs:
credit_colnames <- colnames(credits) # create a vector of column names from credit df
list_of_na <- vector(mode = "list", length = (n_distinct(credit_colnames))) #empty list to receive a dataframe of NAs; one dataframe for each column
names(list_of_na) <- credit_colnames #name elements of list_of_na to match colnames from df credit
for(i in 1:n_distinct(credit_colnames)){ # specify loop length to match count of colnames
    list_of_na[[i]] <- subset(credits, is.na(credit_colnames[i])) # make a dataframe that contains only NAs in each column of df credits
} # confirmed, there are no NAs in any column of credit

#### which actors are in the most Netflix titles?
# since there are 77k+ rows and none of the columns are numbers (person_id is really a factor), I want to know how many unique values we have for each column. Are there lots of repeats?
list_of_uniques <- vector(mode = "list", length = (n_distinct(credit_colnames))) #empty list to receive a counts of unique values in each column
names(list_of_uniques) <- credit_colnames #name elements of list_of_na to match colnames from df credit
for(i in 1:n_distinct(credit_colnames)){ # specify loop length to match count of colnames
    list_of_uniques[[i]] <- n_distinct(credits[i]) # extract n_distinct for each column in credits dataframe
} # confirmed, there are no NAs in any column of credit

# histogram to show distribution
credit_counts <- credits %>%
    count(name, sort = TRUE)
credit_counts$director <- credits %>%
    filter(role=="ACTOR") %>%
    count(name, sort = TRUE)


n_in_fig <- 50
test2 <-
head(credits, n = n_in_fig) %>%
    count(name, role) %>%
    pivot_wider(names_from = role, values_from = n) %>%
    clean_names()

df_plot <- head(credit_counts, n = n_in_fig)
df_plot$actor <- test2$actor

text_size <- 10
df_plot %>%
    ggplot(aes(x = reorder(name,-n), y = n)) +
    geom_col() +
    geom_text(aes(label = name), hjust = 1.2, vjust = 0.35, colour = "white", angle = 90, size = text_size) +
    theme_bw() +
    labs(
        title = paste0("Top ", n_in_fig, " actors and directors on Netflix"),
        x = "Name",
        y = "Count of appearances"
    )+
    theme(
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = text_size+5),
        axis.title = element_text(size = text_size+10),
        title = element_text(size = text_size+15)
    )


ggplot(data = head(test, n = 200), aes(x = name, y = n)) +
    geom_col() # since I did the counting beforehand, I use geom_col because it defaults to stat_identity (not stat_count)


colnames(test)
# How many people are dual threats? (i.e., they played both ACTOR and DIRECTOR at some point)
unique(credits$role)

################################## Netflix Titles ################################## 
titles <- read.csv("titles.csv")
