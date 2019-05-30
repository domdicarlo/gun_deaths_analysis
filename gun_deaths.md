Exploring Gun Deaths in America
================
Dominic DiCarlo
May 26, 2019

``` r
library(tidyverse) # for tidy data functions
```

    ## -- Attaching packages ------------------------------------------------------------------------------------------------------------------ tidyverse 1.2.1 --

    ## v ggplot2 3.1.1       v purrr   0.3.2  
    ## v tibble  2.1.1       v dplyr   0.8.0.1
    ## v tidyr   0.8.3       v stringr 1.4.0  
    ## v readr   1.3.1       v forcats 0.4.0

    ## -- Conflicts --------------------------------------------------------------------------------------------------------------------- tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(rcfss) # library for Computing for Social Sciences Course
               # at the University of Chicago
library(knitr) # load in knitr packages for kable
```

In this R markdown analysis, I look at some data on gun deaths in America published by the data journalism site [FiveThirtyEight](http://fivethirtyeight.com/). This data contains all gun deaths in the United States from 2012-2014. The dataset is actually loaded in from the `rcfss` library, this analysis was originally a homework assignment for the Computing for Social Sciences class at the University of Chicago (and this library was used in the course).

### Generate a data frame that summarizes the number of gun deaths per month. Print the data frame as a formatted `kable()` table.

``` r
# the dataset is known as "gun_deaths" in the rcfss library, we  
# need to load it in like so:
data("gun_deaths")

deaths_per_month <- gun_deaths %>%
  group_by(month) %>% # select months
  summarize(n()) # count the instances of each month

# make a pretty version of the table 
kable(deaths_per_month, format = 'markdown', 
      col.names = c('Month', 'Deaths'), 
      caption = 'Gun Deaths per Month')
```

|  Month|  Deaths|
|------:|-------:|
|      1|    8273|
|      2|    7093|
|      3|    8289|
|      4|    8455|
|      5|    8669|
|      6|    8677|
|      7|    8989|
|      8|    8783|
|      9|    8508|
|     10|    8406|
|     11|    8243|
|     12|    8413|

### Generate a bar chart for gun deaths per month

``` r
ggplot(deaths_per_month, aes(month, `n()`)) +
  geom_col() + # bar chart
  scale_x_discrete(limits=month.abb[]) + # add in abbrev labels
  labs(title = "Gun deaths per month", y = "deaths") +  # labels  
  theme_classic() # adds a simple graph design
```

![](gun_deaths_files/figure-markdown_github/unnamed-chunk-2-1.png)

### Generate a sorted bar chart that identifies the number of gun deaths associated with each type of intent cause of death.

``` r
deaths_per_intent <- gun_deaths %>%
  group_by(intent) %>% # select by intent
  summarize(n()) # count the instances of each death by intent

# standard bar graph - reorder function sorts the bars from hi to lo
# the negative sign in front of the `n()` variable reorders from 
# greatest to least.
ggplot(deaths_per_intent, aes(x = reorder(intent, -`n()`), `n()`)) +
  geom_col() +
  # labels
  labs(title = "Gun deaths and cause", y = "deaths", x = "cause") +
  theme_classic() # adds a simple graph design
```

![](gun_deaths_files/figure-markdown_github/unnamed-chunk-3-1.png)

### Generate a boxplot visualizing the age of gun death victims, by sex. Print the average age of female gun death victims.

``` r
# map variable to color
ggplot(gun_deaths, aes(x = sex, y = age, fill = sex)) +
  geom_boxplot() + # box plot 
  # labels
  labs(title = "Gun victim age and sex", y = "age (years)", x="sex") +
  theme_classic() +
  theme(legend.position = "none")
```

    ## Warning: Removed 18 rows containing non-finite values (stat_boxplot).

![](gun_deaths_files/figure-markdown_github/unnamed-chunk-4-1.png)

``` r
# getting the female gun gun victim average age
female_avg <- gun_deaths %>%
  filter(sex == "F") %>% # take just the females
  summarize(mean(age, na.rm = TRUE)) # take the mean, remove NA vals

# print
print(female_avg[[1]])
```

    ## [1] 43.69507

Which season of the year has the most gun deaths?
-------------------------------------------------

Assume that:

-   Winter = January-March
-   Spring = April-June
-   Summer = July-September
-   Fall = October-December

``` r
# Lets find the answer to this one via a bargraph, similar to 
# the way we made the graph for deaths by intent

# character vector for seasons
seasons <- c("Winter", "Spring", "Fall", "Summer")

# convert a continuous variable into a categorical variable
gun_deaths$season <- gun_deaths$month %>%
  # make a break at every 3 months for a season
  cut(breaks = c(0,3,6,9,12), labels = seasons[])

# let's group by season now
deaths_per_season <- gun_deaths %>%
  group_by(season) %>% # select by season
  summarize(n()) # count the instances of each death by season

# performing a reorder again so we can see the clear winner 
# just by looking to the rightmost of the graph
ggplot(deaths_per_season, aes(reorder(season, `n()`), `n()`,
                              fill = season)) +
  geom_col() + # bar graph
  # axislabels  
  labs(title = "Gun deaths per season", y = "deaths", x = "season") + 
  # custom color coding for a seasonal vibe
  scale_fill_manual(values=c("Winter"="#3933ff", "Spring"="#fffc33", 
                            "Summer"="#33ff99", "Fall"="#ffbe33")) +
  theme_classic() # adds a simple graph deign
```

![](gun_deaths_files/figure-markdown_github/unnamed-chunk-5-1.png)

Are whites who are killed by guns more likely to die because of suicide or homicide? How does this compare to blacks and hispanics?
===================================================================================================================================

``` r
bl_his_wh_cide <- gun_deaths %>%
  # take hispanic, black, and white values with -cide
   filter(race == "Black" | race == "Hispanic" | race == "White",
          intent == "Suicide" | intent == "Homicide")

ggplot(bl_his_wh_cide, aes(intent)) +
  geom_bar() +
  # axis labels
  labs(title = "Gun suicides and homocides by race", y = "deaths",
       x = "cause") +
  facet_wrap(. ~race) + # side by side graphs with race
  theme_classic() # add a nice theme
```

![](gun_deaths_files/figure-markdown_github/unnamed-chunk-6-1.png)

### Are police-involved gun deaths significantly different from other gun deaths? Assess the relationship between police involvement and age, police involvement and race, and the intersection of all three variables.

``` r
# Two subsets of data:

police_deaths <- filter(gun_deaths, police == 1)

non_police_deaths <- filter(gun_deaths, police == 0)

# First, let's look at police involvement as a function of age

ggplot(police_deaths, aes(age)) +
  # open histogram, with bins from 0 to 100 by 5
  geom_freqpoly(breaks=seq(0,100,by=5)) + 
  labs(title = "Police involved gun deaths and age", y = "deaths",
       x = "age (years)") 
```

![](gun_deaths_files/figure-markdown_github/unnamed-chunk-7-1.png)

``` r
ggplot(non_police_deaths, aes(age)) +
  # open histogram, with bins from 0 to 100 by 5
  geom_freqpoly(breaks=seq(0,100,by=5)) + 
  labs(title = "Non-police involved gun deaths and age", y="deaths",
       x = "age (years)") 
```

    ## Warning: Removed 18 rows containing non-finite values (stat_bin).

![](gun_deaths_files/figure-markdown_github/unnamed-chunk-7-2.png)

Mid-twenties appears to be an at risk age group in all gun deaths, with mid 50s being another at risk group in non-police gun deaths. Otherwise, likelihood of gun death goes down with increase in age.

Now let's look at police involvement and race.

``` r
ggplot(police_deaths, aes(race)) +
  geom_bar() +
   labs(title = "Police involved gun deaths and race", y="deaths",
       x = "race") 
```

![](gun_deaths_files/figure-markdown_github/unnamed-chunk-8-1.png)

``` r
ggplot(non_police_deaths, aes(race)) +
  geom_bar() +
    labs(title = "Non-police involved gun deaths and race", y="deaths",
       x = "race") 
```

![](gun_deaths_files/figure-markdown_github/unnamed-chunk-8-2.png)

From these two graphs, it appears there is a higher proportion of non-whites to whites being killed in police involved incidences than in non police incidences.

Now let's look at police involvement and age across race.

``` r
ggplot(police_deaths, aes(age)) +
  # open histogram, with bins from 0 to 100 by 5
  geom_freqpoly(breaks=seq(0,100,by=5)) + 
  labs(title = "Police involved gun deaths and age", y = "deaths",
       x = "age (years)") +
  facet_wrap(. ~race)
```

![](gun_deaths_files/figure-markdown_github/unnamed-chunk-9-1.png)

The above graph doesn't appear to show any strong relations between age, police involvement, and race.

``` r
ggplot(non_police_deaths, aes(age)) +
  # open histogram, with bins from 0 to 100 by 5
  geom_freqpoly(breaks=seq(0,100,by=5)) + 
  labs(title = "Non-police involved gun deaths and age", y = "deaths",
       x = "age (years)") +
  facet_wrap(. ~race)
```

    ## Warning: Removed 18 rows containing non-finite values (stat_bin).

![](gun_deaths_files/figure-markdown_github/unnamed-chunk-10-1.png)

However, this graph illustrates that non-white gun deaths without police involvement occur at higher frequencies at younger ages than white gun deaths.

``` r
# color the fill by police/non and outline white
ggplot(gun_deaths, aes(x=race, y=age)) +
  geom_boxplot() +
  # compare side by side police and non-police data in two graphs
  facet_wrap(. ~police,
             labeller=labeller(police = c(`0`="Non-police",
                                          `1`="Police"))) +
  # rotate long labels 90 degrees so they are legible
  theme(axis.text.x=element_text(angle = -90, hjust = 0),
        legend.position = "none") +
  labs(title = "Gun deaths and age", y="age (years)",
       x = "") 
```

    ## Warning: Removed 18 rows containing non-finite values (stat_boxplot).

![](gun_deaths_files/figure-markdown_github/unnamed-chunk-11-1.png)

This trend is more clear side by side in box plots, which shows blacks and hispanics die from gun related incidents with police involvement at higher average ages than in non-police incidences. This trend is reversed for all other races.

Session info
------------

``` r
# don't modify this chunk
devtools::session_info()
```

    ## - Session info ----------------------------------------------------------
    ##  setting  value                       
    ##  version  R version 3.5.3 (2019-03-11)
    ##  os       Windows 10 x64              
    ##  system   x86_64, mingw32             
    ##  ui       RTerm                       
    ##  language (EN)                        
    ##  collate  English_United States.1252  
    ##  ctype    English_United States.1252  
    ##  tz       America/Chicago             
    ##  date     2019-05-30                  
    ## 
    ## - Packages --------------------------------------------------------------
    ##  package     * version date       lib source                        
    ##  assertthat    0.2.1   2019-03-21 [1] CRAN (R 3.5.3)                
    ##  backports     1.1.3   2018-12-14 [1] CRAN (R 3.5.2)                
    ##  broom         0.5.2   2019-04-07 [1] CRAN (R 3.5.3)                
    ##  callr         3.2.0   2019-03-15 [1] CRAN (R 3.5.3)                
    ##  cellranger    1.1.0   2016-07-27 [1] CRAN (R 3.5.3)                
    ##  cli           1.1.0   2019-03-19 [1] CRAN (R 3.5.3)                
    ##  colorspace    1.4-1   2019-03-18 [1] CRAN (R 3.5.3)                
    ##  crayon        1.3.4   2017-09-16 [1] CRAN (R 3.5.3)                
    ##  desc          1.2.0   2018-05-01 [1] CRAN (R 3.5.3)                
    ##  devtools      2.0.2   2019-04-08 [1] CRAN (R 3.5.3)                
    ##  digest        0.6.18  2018-10-10 [1] CRAN (R 3.5.3)                
    ##  dplyr       * 0.8.0.1 2019-02-15 [1] CRAN (R 3.5.3)                
    ##  evaluate      0.13    2019-02-12 [1] CRAN (R 3.5.3)                
    ##  forcats     * 0.4.0   2019-02-17 [1] CRAN (R 3.5.3)                
    ##  fs            1.2.7   2019-03-19 [1] CRAN (R 3.5.3)                
    ##  generics      0.0.2   2018-11-29 [1] CRAN (R 3.5.3)                
    ##  ggplot2     * 3.1.1   2019-04-07 [1] CRAN (R 3.5.3)                
    ##  glue          1.3.1   2019-03-12 [1] CRAN (R 3.5.3)                
    ##  gtable        0.3.0   2019-03-25 [1] CRAN (R 3.5.3)                
    ##  haven         2.1.0   2019-02-19 [1] CRAN (R 3.5.3)                
    ##  highr         0.8     2019-03-20 [1] CRAN (R 3.5.3)                
    ##  hms           0.4.2   2018-03-10 [1] CRAN (R 3.5.3)                
    ##  htmltools     0.3.6   2017-04-28 [1] CRAN (R 3.5.3)                
    ##  httr          1.4.0   2018-12-11 [1] CRAN (R 3.5.3)                
    ##  jsonlite      1.6     2018-12-07 [1] CRAN (R 3.5.3)                
    ##  knitr       * 1.22    2019-03-08 [1] CRAN (R 3.5.3)                
    ##  labeling      0.3     2014-08-23 [1] CRAN (R 3.5.2)                
    ##  lattice       0.20-38 2018-11-04 [2] CRAN (R 3.5.3)                
    ##  lazyeval      0.2.2   2019-03-15 [1] CRAN (R 3.5.3)                
    ##  lubridate     1.7.4   2018-04-11 [1] CRAN (R 3.5.3)                
    ##  magrittr      1.5     2014-11-22 [1] CRAN (R 3.5.3)                
    ##  memoise       1.1.0   2017-04-21 [1] CRAN (R 3.5.3)                
    ##  modelr        0.1.4   2019-02-18 [1] CRAN (R 3.5.3)                
    ##  munsell       0.5.0   2018-06-12 [1] CRAN (R 3.5.3)                
    ##  nlme          3.1-137 2018-04-07 [2] CRAN (R 3.5.3)                
    ##  pillar        1.3.1   2018-12-15 [1] CRAN (R 3.5.3)                
    ##  pkgbuild      1.0.3   2019-03-20 [1] CRAN (R 3.5.3)                
    ##  pkgconfig     2.0.2   2018-08-16 [1] CRAN (R 3.5.3)                
    ##  pkgload       1.0.2   2018-10-29 [1] CRAN (R 3.5.3)                
    ##  plyr          1.8.4   2016-06-08 [1] CRAN (R 3.5.3)                
    ##  prettyunits   1.0.2   2015-07-13 [1] CRAN (R 3.5.3)                
    ##  processx      3.3.0   2019-03-10 [1] CRAN (R 3.5.3)                
    ##  ps            1.3.0   2018-12-21 [1] CRAN (R 3.5.3)                
    ##  purrr       * 0.3.2   2019-03-15 [1] CRAN (R 3.5.3)                
    ##  R6            2.4.0   2019-02-14 [1] CRAN (R 3.5.3)                
    ##  rcfss       * 0.1.5   2019-04-11 [1] Github (uc-cfss/rcfss@63f9833)
    ##  Rcpp          1.0.1   2019-03-17 [1] CRAN (R 3.5.3)                
    ##  readr       * 1.3.1   2018-12-21 [1] CRAN (R 3.5.3)                
    ##  readxl        1.3.1   2019-03-13 [1] CRAN (R 3.5.3)                
    ##  remotes       2.0.2   2018-10-30 [1] CRAN (R 3.5.3)                
    ##  rlang         0.3.4   2019-04-07 [1] CRAN (R 3.5.3)                
    ##  rmarkdown     1.12    2019-03-14 [1] CRAN (R 3.5.3)                
    ##  rprojroot     1.3-2   2018-01-03 [1] CRAN (R 3.5.3)                
    ##  rstudioapi    0.10    2019-03-19 [1] CRAN (R 3.5.3)                
    ##  rvest         0.3.2   2016-06-17 [1] CRAN (R 3.5.3)                
    ##  scales        1.0.0   2018-08-09 [1] CRAN (R 3.5.3)                
    ##  sessioninfo   1.1.1   2018-11-05 [1] CRAN (R 3.5.3)                
    ##  stringi       1.4.3   2019-03-12 [1] CRAN (R 3.5.3)                
    ##  stringr     * 1.4.0   2019-02-10 [1] CRAN (R 3.5.3)                
    ##  tibble      * 2.1.1   2019-03-16 [1] CRAN (R 3.5.3)                
    ##  tidyr       * 0.8.3   2019-03-01 [1] CRAN (R 3.5.3)                
    ##  tidyselect    0.2.5   2018-10-11 [1] CRAN (R 3.5.3)                
    ##  tidyverse   * 1.2.1   2017-11-14 [1] CRAN (R 3.5.3)                
    ##  usethis       1.5.0   2019-04-07 [1] CRAN (R 3.5.3)                
    ##  withr         2.1.2   2018-03-15 [1] CRAN (R 3.5.3)                
    ##  xfun          0.6     2019-04-02 [1] CRAN (R 3.5.3)                
    ##  xml2          1.2.0   2018-01-24 [1] CRAN (R 3.5.3)                
    ##  yaml          2.2.0   2018-07-25 [1] CRAN (R 3.5.2)                
    ## 
    ## [1] C:/Users/eugene/Documents/R/win-library/3.5
    ## [2] C:/Program Files/R/R-3.5.3/library
