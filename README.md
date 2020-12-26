California COVID-19 Sex Demographic Analysis
================
Bowie Lam
12/25/2020

## Contents:

  - [Contents](#contents)
  - [Introduction](#introduction)
  - [Setting Up](#setting-up)
  - [Observing Imported Data](#observing-imported-data)
  - [Cleaning Data](#cleaning-data)
  - [Exploring Data](#exploring-data)
  - [Plotting Data](#plotting-data)

## Introduction

For this mini-project, I want to observe:

1.  the trend of COVID-19 cases overtime for the male and female sexes,
    and
2.  any discrepancies in cases between both sexes

The below tasks are not intended to provide extensive knowledge into the
subject matter, rather they simply showcase a glimpse into the general
picture of the data. The observational texts that I provide for each
task are meant to be rudimentary. I may conduct further
research/analysis into this data set, but for now I merely intend to
acquire a (very) brief look into the data.

Thank you to [California Open Data](https://data.ca.gov/) for the data.
The direct link to the particular data set used and its dictionary can
be found [here](https://data.ca.gov/dataset/covid-19-cases).

## Setting Up

``` r
# load packages
library(ggplot2)
library(dplyr, warn.conflicts = FALSE)
library(here)
```

    ## here() starts at /Users/bowielam/Desktop/GitHub/COVID-19

``` r
# import data
df <- read.csv(here("raw_data", "case_demographics_sex.csv"), na.strings = c("", ".", "NA", " ", "None"))
head(df)
```

    ##       sex totalpositive2       date case_percent deaths deaths_percent
    ## 1  Female           5015 2020-04-02           NA     NA             NA
    ## 2    Male           5547 2020-04-02           NA     NA             NA
    ## 3 Unknown            139 2020-04-02           NA     NA             NA
    ## 4  Female           5674 2020-04-03           NA     NA             NA
    ## 5    Male           6202 2020-04-03           NA     NA             NA
    ## 6 Unknown            150 2020-04-03           NA     NA             NA
    ##   ca_percent
    ## 1         NA
    ## 2         NA
    ## 3         NA
    ## 4         NA
    ## 5         NA
    ## 6         NA

## Observing Imported Data

``` r
# structure of data set
str(df)
```

    ## 'data.frame':    655 obs. of  7 variables:
    ##  $ sex           : Factor w/ 4 levels "Female","Male",..: 1 2 4 1 2 4 1 2 4 1 ...
    ##  $ totalpositive2: int  5015 5547 139 5674 6202 150 6349 6876 213 6740 ...
    ##  $ date          : Factor w/ 217 levels "2020-04-02","2020-04-03",..: 1 1 1 2 2 2 3 3 3 4 ...
    ##  $ case_percent  : num  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ deaths        : num  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ deaths_percent: num  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ ca_percent    : num  NA NA NA NA NA NA NA NA NA NA ...

``` r
# view the different factors of `sex`
df$sex %>% unique()
```

    ## [1] Female      Male        Unknown     Transgender
    ## Levels: Female Male Transgender Unknown

``` r
# view the count of each unique `sex`
df %>% group_by(sex) %>% count()
```

    ## # A tibble: 4 x 2
    ## # Groups:   sex [4]
    ##   sex             n
    ##   <fct>       <int>
    ## 1 Female        218
    ## 2 Male          218
    ## 3 Transgender     1
    ## 4 Unknown       218

## Cleaning Data

``` r
# add a 'month' column
df$month <-as.numeric(substr(df$date, 6, 7))

# add a 'day' column
df$day <-as.numeric(substr(df$date, 9, 10))
```

``` r
# remove the following characteristics: 
# 'Transgender' and 'Unknown' from the 'sex' column; 
# 'November' from the 'month' column
df <- df %>% filter(sex != "Transgender", sex != "Unknown", month != 11)

# remove unneeded column
df$date <- NULL
df$case_percent <- NULL
df$deaths <- NULL
df$deaths_percent <- NULL
df$ca_percent <- NULL

# rename 'totalpositive2' column
colnames(df)[names(df) == "totalpositive2"] <- "total_positive"

# arrange by 'day', then arrange by 'month' (note: important for when diff() is used later on)
df <- df %>% arrange(day) %>% arrange(month)

head(df)
```

    ##      sex total_positive month day
    ## 1 Female           5015     4   2
    ## 2   Male           5547     4   2
    ## 3 Female           5674     4   3
    ## 4   Male           6202     4   3
    ## 5 Female           6349     4   4
    ## 6   Male           6876     4   4

## Exploring Data

### (+ continuation of any necessary data cleansing/manipulations)

``` r
# explore a particular month of the data frame e.g. where 'month' is '4'
april <- df %>% filter(month == 4)

april
```

    ##       sex total_positive month day
    ## 1  Female           5015     4   2
    ## 2    Male           5547     4   2
    ## 3  Female           5674     4   3
    ## 4    Male           6202     4   3
    ## 5  Female           6349     4   4
    ## 6    Male           6876     4   4
    ## 7  Female           6740     4   5
    ## 8    Male           7296     4   5
    ## 9  Female           7600     4   6
    ## 10   Male           7957     4   6
    ## 11 Female           8108     4   7
    ## 12   Male           8488     4   7
    ## 13 Female           8776     4   8
    ## 14   Male           9130     4   8
    ## 15 Female           9387     4   9
    ## 16   Male           9745     4   9
    ## 17 Female           9981     4  10
    ## 18   Male          10330     4  10
    ## 19 Female           9981     4  10
    ## 20   Male          10330     4  10
    ## 21 Female          10571     4  11
    ## 22   Male          10926     4  11
    ## 23 Female          10877     4  12
    ## 24   Male          11256     4  12
    ## 25 Female          11352     4  13
    ## 26   Male          11772     4  13
    ## 27 Female          11934     4  14
    ## 28   Male          12265     4  14
    ## 29 Female          12771     4  15
    ## 30   Male          13169     4  15
    ## 31 Female          13424     4  16
    ## 32   Male          13865     4  16
    ## 33 Female          14185     4  17
    ## 34   Male          14535     4  17
    ## 35 Female          14902     4  18
    ## 36   Male          15185     4  18
    ## 37 Female          15224     4  19
    ## 38   Male          15507     4  19
    ## 39 Female          16363     4  20
    ## 40   Male          16641     4  20
    ## 41 Female          17411     4  21
    ## 42   Male          17718     4  21
    ## 43 Female          18395     4  22
    ## 44   Male          18690     4  22
    ## 45 Female          19394     4  23
    ## 46   Male          19577     4  23
    ## 47 Female          20395     4  24
    ## 48   Male          20459     4  24
    ## 49 Female          20908     4  25
    ## 50   Male          20957     4  25
    ## 51 Female          21562     4  26
    ## 52   Male          21600     4  26
    ## 53 Female          22384     4  27
    ## 54   Male          22337     4  27
    ## 55 Female          23154     4  28
    ## 56   Male          23045     4  28
    ## 57 Female          24247     4  29
    ## 58   Male          24372     4  29
    ## 59 Female          24973     4  30
    ## 60   Male          25167     4  30

Notice that there are duplicate rows in the `april` data frame
e.g. where month = 4 and day = 10.

Create a function:

  - to remove duplicate rows
  - to double check for duplicates

<!-- end list -->

``` r
#' @title Duplicate Row Remover
#' @description removes duplicated rows from a data frame
#' @param dataframe is a table or two-dimensional array-like structure/object
#' @return a data frame that no longer has duplicate rows

remove_dup <- function(dataframe) {
  return(dataframe[!duplicated(dataframe), ])
}
```

``` r
#' @title Duplicate Row Checker
#' @description double checks that are no duplicate rows in the data frame
#' @param dataframe is a table or two-dimensional array-like structure/object
#' @return a character/string message indicating if there are any duplicates in the data frame

check_for_dups <- function(dataframe) {
  len = dataframe %>% group_by(day) %>% count() %>% pull(n) %>% unique() %>% length()
  if (len != 1) {
    print("Oh, no! There are duplicates in this dataset! :( ")
  } else {
    print("Yay! There are no duplicates in this dataset! :) ")
  }
}
```

Test the function `remove_dup()`.

``` r
# this data frame has duplicate rows
female_4 <- april %>% filter(sex == "Female")

# this data frame does not have duplicate rows
female_4_cleaned <- remove_dup(female_4)

check_for_dups(female_4)
```

    ## [1] "Oh, no! There are duplicates in this dataset! :( "

``` r
check_for_dups(female_4_cleaned)
```

    ## [1] "Yay! There are no duplicates in this dataset! :) "

Remove duplicate rows from the entire `df` data frame using
`remove_dup()`.

``` r
# apply 'remove_dup()' to the data frame
df <- remove_dup(df)
head(df)
```

    ##      sex total_positive month day
    ## 1 Female           5015     4   2
    ## 2   Male           5547     4   2
    ## 3 Female           5674     4   3
    ## 4   Male           6202     4   3
    ## 5 Female           6349     4   4
    ## 6   Male           6876     4   4

## Plotting Data

### Task 1

**Task 1A:** Plot in one graph the total number of positive cases for
the first available month in the data set for all sexes.

``` r
# first available 'month' is '4'
ggplot(data = df %>% filter(month == 4), 
       aes(x = day, y = total_positive, color = sex, group = sex)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = seq(min(df$day), max(df$day), 2)) +
  labs(title = "Trend of cases for the month of April", x = "Day of the Month", y = "Number of Positive Cases") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5))
```

![](exploratory-data-analysis_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

**Task 1B:** Plot in one graph the total number of positive cases for
the last available month in the data set for all sexes.

``` r
# last available 'month' is '10'
ggplot(data = df %>% filter(month == 10), 
       aes(x = day, y = total_positive, color = sex, group = sex)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = seq(min(df$day), max(df$day), 2)) +
  labs(title = "Trend of cases for the month of October", x = "Day of the Month", y = "Number of Positive Cases") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5))
```

![](exploratory-data-analysis_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

**Task 1 Observations:** Upon first look at both graphs, it’s clear that
there is a difference in the number of total cases between sexes when
comparing the first month, April, to the last month, October. In Task
1A, the graph displays a similar increase in the number of positive
cases between males and females; however, in October, that gap has
widened to the point where it’s more than obvious that females are
testing positive more often than males.

### Task 2

**Task 2A:** Create a data frame for all sexes for individual analysis.

``` r
# data frame where 'sex' is 'Female'
females_df <- df %>% filter(sex == "Female")

# data frame where 'sex' is 'Male'
males_df <- df %>% filter(sex == "Male")
```

**Task 2B:** Plot in one graph the number of cases for each month for
the female sex.

``` r
# displays the trend of cases for each month for 'Female'
options(scipen = 5)
ggplot(data = females_df, 
       aes(x = day, y = total_positive, color = as.factor(month), group = month)) +
  geom_line() +
  labs(title = "Trend of cases for each month", subtitle = "for Females", x = "Day of the Month", y = "Number of Positive Cases") +
  scale_x_continuous(breaks = seq(min(df$day), max(df$day), 2)) +
  theme_minimal() +
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 12, face = "bold", hjust = 0.5)) +
  scale_colour_discrete("Month")
```

![](exploratory-data-analysis_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

**Task 2C:** Plot in one graph the number of cases for each month for
the male sex.

``` r
# displays the trend of cases for each month for 'Male'
options(scipen = 5)
ggplot(data = males_df, 
       aes(x = day, y = total_positive, color = as.factor(month), group = month)) +
  geom_line() +
  labs(title = "Trend of cases for each month", subtitle = "for Males", x = "Day of the Month", y = "Number of Positive Cases") +
  scale_x_continuous(breaks = seq(min(df$day), max(df$day), 2)) +
  theme_minimal() +
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5), 
        plot.subtitle = element_text(size = 12, face = "bold", hjust = 0.5)) +
  scale_colour_discrete("Month")
```

![](exploratory-data-analysis_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

**Task 2 Observations:** For both sexes, it appears they share similar
trends in the number of positive cases for each month. Besides that,
there is not much useful information that these two graphs can provide.

### Task 3

**Task 3A:** Use the `diff()` function to find the difference in the
number of cases from one day to the next.

Note: I separated `df` into different sex data frames, `females_df` and
`males_df`, because if `diff()` is used on `df` where the rows alternate
between `Male` and `Female`, the `difference` column produced will be
incorrect. Thus, in this section I: 1. separate the main dataframe by
`sex` 2. compute `diff()` on each dataframe 3. bind, using `rbind()`,
the two data frames and reassign it to the main dataframe, `df`

``` r
# add a 'difference' column to `females_df` dataframe
females_df$difference <- c(0, diff(females_df$total_positive))
head(females_df)
```

    ##      sex total_positive month day difference
    ## 1 Female           5015     4   2          0
    ## 2 Female           5674     4   3        659
    ## 3 Female           6349     4   4        675
    ## 4 Female           6740     4   5        391
    ## 5 Female           7600     4   6        860
    ## 6 Female           8108     4   7        508

``` r
# add a 'difference' column to `males_df` dataframe
males_df$difference <- c(0, diff(males_df$total_positive))
head(males_df)
```

    ##    sex total_positive month day difference
    ## 1 Male           5547     4   2          0
    ## 2 Male           6202     4   3        655
    ## 3 Male           6876     4   4        674
    ## 4 Male           7296     4   5        420
    ## 5 Male           7957     4   6        661
    ## 6 Male           8488     4   7        531

**Task 3B:** Plot in one graph the fluctuation in cases between days for
each month for the female sex.

``` r
# plot `difference` in cases between days for each month for `Female`
ggplot(data = females_df, 
       aes(x = day, y = difference, color = as.factor(month), group = month)) +
  geom_line() +
  labs(title = "Fluctuations in Positive Cases On a Daily Basis", subtitle = "for Females", x = "Day of the Month", y = "Difference") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5), 
        plot.subtitle = element_text(size = 12, face = "bold", hjust = 0.5)) +
  scale_colour_discrete("Month")
```

![](exploratory-data-analysis_files/figure-gfm/unnamed-chunk-20-1.png)<!-- -->

**Task 3C:** Plot in one graph the fluctuation in cases between days for
each month for the male sex.

``` r
# plot `difference` in cases between days for each month for `Male`
ggplot(data = males_df, 
       aes(x = day, y = difference, color = as.factor(month), group = month)) +
  geom_line() +
  labs(title = "Fluctuations in Positive Cases On a Daily Basis", subtitle = "for Males", x = "Day of the Month", y = "Difference") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5), 
        plot.subtitle = element_text(size = 12, face = "bold", hjust = 0.5)) +
  scale_colour_discrete("Month")
```

![](exploratory-data-analysis_files/figure-gfm/unnamed-chunk-21-1.png)<!-- -->

**Task 3 Observations:** In both graphs for females and males, the
months of July and August appear to containing the highest differences
among all months. This may indicate a summer spike where people
quarantined less than when the pandemic first started in the earlier
months. This could be due to many reasons, such as adaptation to the new
norm, thus resulting in people taking the pandemic less seriously.

### Task 4

**Task 4A:** Combine `females_df` and `males_df` into `df`.

``` r
# combine `females_df` and `males_df` and reassign to `df` dataframe
df <- rbind(females_df, males_df)
```

**Task 4B:** Plot in one graph the difference in the number of cases for
the first available month in the data set for all sexes.

``` r
# first available 'month' is '4'
ggplot(data = df %>% filter(month == 4), 
       aes(x = day, y = difference, color = sex, group = sex)) +
  geom_line() +
  geom_point() +
  labs(title = "Fluctuations in Positive Cases On a Daily Basis", subtitle = "in the month of April", x = "Day of the Month", y = "Difference") +
  scale_x_continuous(breaks = seq(min(df$day), max(df$day), 2)) +
  theme_minimal() +
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5), 
        plot.subtitle = element_text(size = 12, face = "bold", hjust = 0.5))
```

![](exploratory-data-analysis_files/figure-gfm/unnamed-chunk-23-1.png)<!-- -->

**Task 4C:** Plot in one graph the difference in the number of cases for
the last available month in the data set for all sexes.

``` r
# last available 'month' is '10'
ggplot(data = df %>% filter(month == 10), 
       aes(x = day, y = difference, color = sex, group = sex)) +
  geom_line() +
  geom_point() +
  labs(title = "Fluctuations in Positive Cases On a Daily Basis", subtitle = "in the month of April", x = "Day of the Month", y = "Difference") +
  scale_x_continuous(breaks = seq(min(df$day), max(df$day), 2)) +
  theme_minimal() +
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5), 
        plot.subtitle = element_text(size = 12, face = "bold", hjust = 0.5))
```

![](exploratory-data-analysis_files/figure-gfm/unnamed-chunk-24-1.png)<!-- -->

**Task 4 Observations:** Although there are some instances where one sex
had a larger difference than the other, both experienced very similar
fluctuations in positive cases, whether that be in April or October.
However, it does appear that female have larger differences than males
more often. Similar to Task 3, this may indicate a larger problem in
society that’s resulting in the disproportionatlity.
