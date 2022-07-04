
library(tidyverse)
library(janitor)
library(jtools)
library(gt)
library(dplyr)
library(scales)
library(shadowtext)
library(moments)

# setwd("C:/Users/wainr/OneDrive/Documents/netflix_exploratory_data_analysis")
# https://www.kaggle.com/code/svitlanabozhenko/netflix

################################## Credits data exploration ################################## 
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
} # still found no NAs
# however, looking (with my eyes) through the data shows that there are lots of blanks (i.e., "") in $character

#### which actors are in the most Netflix titles?
# since there are 77k+ rows and none of the columns are numbers (person_id is really a factor), I want to know how many unique values we have for each column. Are there lots of repeats?
list_of_uniques <- vector(mode = "list", length = (n_distinct(credit_colnames))) #empty list to receive a counts of unique values in each column
names(list_of_uniques) <- credit_colnames #name elements of list_of_na to match colnames from df credit
for(i in 1:n_distinct(credit_colnames)){ # specify loop length to match count of colnames
    list_of_uniques[[i]] <- n_distinct(credits[i]) # extract n_distinct for each column in credits dataframe
} # confirmed, there are no NAs in any column of credit

# frequency bar chart histogram showing distribution of credit
credit_counts <- credits %>%
    count(person_id, sort = TRUE) # count the number of times a person_id appears in df credits
count_freqs <- credit_counts %>%
    count(n, sort = TRUE, name = "count") # count the number of times a number of appearances appears in credit_counts
credits_mean <- mean(credit_counts$n)
credits_median <- median(credit_counts$n)
credits_skewness <- skewness(credit_counts$n)
credits_kurtosis <- kurtosis(credit_counts$n)
n_in_fig <- 50
hjust <- 0
x <- 15
color_fig <- "black"

top_annotate <- 0.4
credit_counts %>%
    ggplot(aes(x = n)) +
    geom_density(adjust = 5, alpha = 0.5) +
    # geom_histogram(bins = nbins, fill = "gray", color = "black") + 
    scale_y_continuous(expand = c(0,0)) +
    scale_fill_brewer(palette = "Paired") +
    geom_vline(
        xintercept = credits_mean,
        na.rm = FALSE,
        color = "red",
        linetype = "dashed",
        size = 2
    ) +
    geom_vline(
        xintercept = credits_median,
        na.rm = FALSE,
        color = "black",
        linetype = "solid",
        size = 2
    ) +
    theme_classic() +
    labs(
        title = paste0("Distribution of appearances by actors and directors in Netflix programs"),
        subtitle = paste0("This dataset includes about ", comma(round(nrow(credit_counts),-3)), " actors and directors"),
        y = "Observation density",
        x = "Count of appearances",
        fill = "Role"
    )  +
    theme(
        axis.text = element_text(size = text_size + 5),
        axis.title = element_text(size = text_size + 10),
        plot.title = element_text(size = text_size + 15),
        plot.subtitle = element_text(size = text_size + 5),
        legend.text = element_text(size = text_size + 5)
    ) +
    annotate("text", x = x, y = top_annotate, size = text_size-3, label = "Distribution summary statistics") +
    annotate("text", x = x, y = top_annotate - 0.05*top_annotate, size = text_size-5, label = paste0("Kurtosis: ", round(credits_kurtosis,2), " and skewness: ", round(credits_skewness,2))) +
    # annotate("text", x = x, y = top_annotate - 0.1*top_annotate, size = text_size-5, label = paste0("Skewness: ", round(credits_skewness,2))) +
    annotate("text", x = x, y = top_annotate - 0.1*top_annotate, size = text_size-5, label = paste0("Mean appearances: ", round(credits_mean,2), " (red dashed line)")) +
    annotate("text", x = x, y = top_annotate - 0.15*top_annotate, size = text_size-5, label = paste0("Median appearances: ", credits_median, " (black solid line)"))

# bar chart to showing most prolific names
credit_counts <- credits %>%
    group_by(person_id, role) %>% # group df credits by person_id and role
    count(name, sort = TRUE) # count how many times a unique combination of name and role appears
makelabels <- credit_counts %>%
    group_by(person_id) %>% # group df credits by person_id and role
    summarize(
        label = sum(n))
credit_counts <- left_join(credit_counts, makelabels, by="person_id")
head(credit_counts)
n_in_fig <- 50 # it doesn't make sense (visually) to plot all of the names and their credit counts, so I'm choosing to plot the top x most prolific names
df_plot <- head(credit_counts, n = n_in_fig) # subset all credit counts to only view the top n_in_fig credited people
df_plot$role <- str_to_sentence(df_plot$role) # change $role to mixed case for plot aesthetics
text_size <- 10 # choose a font size so if we need to scale plot text, everything scales relative to one number

df_plot %>%
    ggplot(aes(x = reorder(name,n), y = n, fill = role)) +
    geom_col(position="dodge", color = "black") + # since I did the counting beforehand, I use geom_col because it defaults to stat_identity (not stat_count)
    coord_flip() +
    geom_shadowtext(
        aes(label = name),
        vjust = 0.35,
        y = 0, # left-align text in bar chart text labels:  https://community.rstudio.com/t/how-to-start-text-label-from-extreme-left-in-bar-plot-using-geom-col-and-geom-text-in-ggplot2/77256/2
        hjust = 0,
        colour = "white",
        size = text_size - 5
        ) + 
    geom_shadowtext(
        aes(label = n),
        vjust = 0.35,
        #y = 10, # left-align text in bar chart text labels:  https://community.rstudio.com/t/how-to-start-text-label-from-extreme-left-in-bar-plot-using-geom-col-and-geom-text-in-ggplot2/77256/2
        hjust = 1,
        colour = "white",
        size = text_size - 5
    ) +
    scale_fill_brewer(palette = "Paired") + # https://r-graph-gallery.com/ggplot2-color.html
    scale_y_continuous(expand = c(0,0)) +
    theme_classic() +
    labs(
        title = paste0("Top ", n_in_fig, " actors and directors on Netflix"),
        x = "Name",
        y = "Count of appearances",
        fill = "Role"
    ) +
    theme(
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = text_size + 5),
        axis.title = element_text(size = text_size + 10),
        title = element_text(size = text_size + 15),
        legend.text = element_text(size = text_size + 5),
        axis.ticks.y = element_blank()
    )

# test case to work out the logic for stacked & color-coded bars annotated with with left-aligned names, right-aligned sums
# testdf <- data.frame(role = c(rep("Actor",3),rep("Director",3)),
#                      name = c(rep("Jim Halpert",2),rep("Michael Scott",3), rep("Phyllis Lapin", 1)))
# credit_counts <- testdf %>%
#     group_by(name, role) %>% # group df credits by person_id and role
#     count(name, sort = TRUE)
# # credit_counts <- credit_counts %>%
# #     group_by(name) %>% # group df credits by person_id and role
# #     summarize(
# #         label = sum(n))
# test2 <- credit_counts %>%
#     group_by(name) %>% # group df credits by person_id and role
#     summarize(
#         label = sum(n))
# test3 <- left_join(credit_counts, test2, by="name")
# test3
# 
# test3 %>%
#     ggplot(aes(x = reorder(name,n), y = n, fill = role)) +
#     geom_col(position="stack", color = "black") + # since I did the counting beforehand, I use geom_col because it defaults to stat_identity (not stat_count)
#     coord_flip() +
#     geom_shadowtext(
#         aes(label = name),
#         vjust = 0.35,
#         y = 0, # left-align text in bar chart text labels:  https://community.rstudio.com/t/how-to-start-text-label-from-extreme-left-in-bar-plot-using-geom-col-and-geom-text-in-ggplot2/77256/2
#         hjust = 0,
#         colour = "white",
#         size = text_size - 5
#     ) + 
#     geom_shadowtext(
#         aes(label = label),
#         vjust = 0.35,
#         y = test3$label, # left-align text in bar chart text labels:  https://community.rstudio.com/t/how-to-start-text-label-from-extreme-left-in-bar-plot-using-geom-col-and-geom-text-in-ggplot2/77256/2
#         hjust = 1,
#         colour = "white",
#         size = text_size - 5
#     ) +
#     scale_fill_brewer(palette = "Paired") + # https://r-graph-gallery.com/ggplot2-color.html
#     scale_y_continuous(expand = c(0,0)) +
#     theme_bw() +
#     labs(
#         title = paste0("Top ", n_in_fig, " actors and directors on Netflix"),
#         x = "Name",
#         y = "Count of appearances",
#         fill = "Role"
#     ) +
#     theme(
#         axis.text.y = element_blank(),
#         axis.text.x = element_text(size = text_size + 5),
#         axis.title = element_text(size = text_size + 10),
#         title = element_text(size = text_size + 15),
#         legend.text = element_text(size = text_size + 5)
#     )

# How many people are dual threats? (i.e., they played both ACTOR and DIRECTOR at some point)
unique(credits$role)
actors <- subset(credits, role == "ACTOR")
directors <- subset(credits, role == "DIRECTOR")
colnames(actors)
double_threats <- inner_join(actors, directors, by = "person_id", keep = FALSE)
nrow(double_threats)


# bar chart to showing most prolific names
double_threats_counts <- double_threats %>%
    group_by(person_id) %>% # group df credits by person_id and role
    count(name, sort = TRUE) # count how many times a unique combination of name and role appears
makelabels <- credit_counts %>%
    group_by(person_id) %>% # group df credits by person_id and role
    summarize(
        label = sum(n))
credit_counts <- left_join(credit_counts, makelabels, by="person_id")
head(credit_counts)
n_in_fig <- 50 # it doesn't make sense (visually) to plot all of the names and their credit counts, so I'm choosing to plot the top x most prolific names
df_plot <- head(credit_counts, n = n_in_fig) # subset all credit counts to only view the top n_in_fig credited people
df_plot$role <- str_to_sentence(df_plot$role) # change $role to mixed case for plot aesthetics
text_size <- 10 # choose a font size so if we need to scale plot text, everything scales relative to one number

df_plot %>%
    ggplot(aes(x = reorder(name,n), y = n, fill = role)) +
    geom_col(position="dodge", color = "black") + # since I did the counting beforehand, I use geom_col because it defaults to stat_identity (not stat_count)
    coord_flip() +
    geom_shadowtext(
        aes(label = name),
        vjust = 0.35,
        y = 0, # left-align text in bar chart text labels:  https://community.rstudio.com/t/how-to-start-text-label-from-extreme-left-in-bar-plot-using-geom-col-and-geom-text-in-ggplot2/77256/2
        hjust = 0,
        colour = "white",
        size = text_size - 5
    ) + 
    geom_shadowtext(
        aes(label = n),
        vjust = 0.35,
        #y = 10, # left-align text in bar chart text labels:  https://community.rstudio.com/t/how-to-start-text-label-from-extreme-left-in-bar-plot-using-geom-col-and-geom-text-in-ggplot2/77256/2
        hjust = 1,
        colour = "white",
        size = text_size - 5
    ) +
    scale_fill_brewer(palette = "Paired") + # https://r-graph-gallery.com/ggplot2-color.html
    scale_y_continuous(expand = c(0,0)) +
    theme_classic() +
    labs(
        title = paste0("Top ", n_in_fig, " actors and directors on Netflix"),
        x = "Name",
        y = "Count of appearances",
        fill = "Role"
    ) +
    theme(
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = text_size + 5),
        axis.title = element_text(size = text_size + 10),
        title = element_text(size = text_size + 15),
        legend.text = element_text(size = text_size + 5),
        axis.ticks.y = element_blank()
    )


################################## Titles data exploration ################################## 
titles <- read.csv("titles.csv")




################################## Who is (are) the best actor(s) and director(s) on Netflix? ################################## 
