---
title: "tidy_data"
output: 
  html_document: 
    keep_md: yes
date: "2024-03-12"
---



## Loading the Libraries


```r
library(tidyverse)
```

```
## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
## ✔ dplyr     1.1.4     ✔ readr     2.1.4
## ✔ forcats   1.0.0     ✔ stringr   1.5.1
## ✔ ggplot2   3.4.4     ✔ tibble    3.2.1
## ✔ lubridate 1.9.3     ✔ tidyr     1.3.0
## ✔ purrr     1.0.2     
## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
## ✖ dplyr::filter() masks stats::filter()
## ✖ dplyr::lag()    masks stats::lag()
## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
```

```r
library(janitor)
```

```
## 
## Attaching package: 'janitor'
## 
## The following objects are masked from 'package:stats':
## 
##     chisq.test, fisher.test
```

```r
library(naniar)
library(ggmap)
```

```
## ℹ Google's Terms of Service: <https://mapsplatform.google.com>
##   Stadia Maps' Terms of Service: <https://stadiamaps.com/terms-of-service/>
##   OpenStreetMap's Tile Usage Policy: <https://operations.osmfoundation.org/policies/tiles/>
## ℹ Please cite ggmap if you use it! Use `citation("ggmap")` for details.
```

```r
library(shiny)
library(shinydashboard)
```

```
## 
## Attaching package: 'shinydashboard'
## 
## The following object is masked from 'package:graphics':
## 
##     box
```

## Loading the Data Set  


```r
dog <- read_csv(file = "../../data/dogbreeddataset.xlsx - A.csv")
```

```
## New names:
## Rows: 1299 Columns: 25
## ── Column specification
## ──────────────────────────────────────────────────────── Delimiter: "," chr
## (25): Data S1. Modern dog breeds dataset. Related to Figures 1 and S4, ....
## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
## Specify the column types or set `show_col_types = FALSE` to quiet this message.
## • `` -> `...2`
## • `` -> `...3`
## • `` -> `...4`
## • `` -> `...5`
## • `` -> `...6`
## • `` -> `...7`
## • `` -> `...8`
## • `` -> `...9`
## • `` -> `...10`
## • `` -> `...11`
## • `` -> `...12`
## • `` -> `...13`
## • `` -> `...14`
## • `` -> `...15`
## • `` -> `...17`
## • `` -> `...18`
## • `` -> `...19`
## • `` -> `...20`
## • `` -> `...21`
## • `` -> `...22`
## • `` -> `...23`
## • `` -> `...24`
## • `` -> `...25`
```

## Fixing the Headers

### Renaming the headers as the values from the first row  


```r
names(dog) <- dog[1,]
```

```
## Warning: The `value` argument of `names<-()` must be a character vector as of tibble
## 3.0.0.
## This warning is displayed once every 8 hours.
## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
## generated.
```

### Removing the first row in the data set  


```r
dog <- dog %>% 
        filter(!row_number() %in% 1) 
```

## Cleaning up the variable names   


```r
dog <- clean_names(dog)
```

## Determining if the data is tidy and cleaning up the data  


```r
summary(dog)
```

```
##   sample_id            breed               type           body_mass_kg      
##  Length:1298        Length:1298        Length:1298        Length:1298       
##  Class :character   Class :character   Class :character   Class :character  
##  Mode  :character   Mode  :character   Mode  :character   Mode  :character  
##   height_cm             sex            coverage_all        coverage_x       
##  Length:1298        Length:1298        Length:1298        Length:1298       
##  Class :character   Class :character   Class :character   Class :character  
##  Mode  :character   Mode  :character   Mode  :character   Mode  :character  
##  bio_project         bio_sample         longitude           latitude        
##  Length:1298        Length:1298        Length:1298        Length:1298       
##  Class :character   Class :character   Class :character   Class :character  
##  Mode  :character   Mode  :character   Mode  :character   Mode  :character  
##  igf1_as_genotype   used_for_gwas     
##  Length:1298        Length:1298       
##  Class :character   Class :character  
##  Mode  :character   Mode  :character  
##  group_in_phylogenetic_analyses_d_statistics chr15_41216098    
##  Length:1298                                 Length:1298       
##  Class :character                            Class :character  
##  Mode  :character                            Mode  :character  
##  chr15_41216597     chr15_41217964     chr15_41217985     chr15_41219654    
##  Length:1298        Length:1298        Length:1298        Length:1298       
##  Class :character   Class :character   Class :character   Class :character  
##  Mode  :character   Mode  :character   Mode  :character   Mode  :character  
##  chr15_41221438     chr15_41227725     chr15_41228068     chr15_41229597    
##  Length:1298        Length:1298        Length:1298        Length:1298       
##  Class :character   Class :character   Class :character   Class :character  
##  Mode  :character   Mode  :character   Mode  :character   Mode  :character  
##  chr15_41229800    
##  Length:1298       
##  Class :character  
##  Mode  :character
```

#### We notice that all the variables are of data class character, which is not desired for variables including `body_mass_kg`, `height_cm`.   

## Where are the NA's?   


```r
miss_var_summary(dog)
```

```
## # A tibble: 25 × 3
##    variable       n_miss pct_miss
##    <chr>           <int>    <dbl>
##  1 bio_sample        486    37.4 
##  2 height_cm         277    21.3 
##  3 sex               270    20.8 
##  4 body_mass_kg      243    18.7 
##  5 longitude         163    12.6 
##  6 latitude          163    12.6 
##  7 chr15_41217964    158    12.2 
##  8 chr15_41217985    146    11.2 
##  9 chr15_41221438    120     9.24
## 10 chr15_41228068    120     9.24
## # ℹ 15 more rows
```

## Changing the data class of certain variables.   


```r
dog <- dog %>% 
        mutate(body_mass_kg = as.numeric(body_mass_kg)) %>% 
        mutate(height_cm = as.numeric(height_cm)) %>% 
        mutate(latitude = as.numeric(latitude)) %>% 
        mutate(longitude = as.numeric(longitude))
```

```
## Warning: There was 1 warning in `mutate()`.
## ℹ In argument: `latitude = as.numeric(latitude)`.
## Caused by warning:
## ! NAs introduced by coercion
```

```
## Warning: There was 1 warning in `mutate()`.
## ℹ In argument: `longitude = as.numeric(longitude)`.
## Caused by warning:
## ! NAs introduced by coercion
```

## Saving the cleaned up data set as new file     


```r
#write_csv(dog, "clean_dog_data.csv") #saving the clean data
```

## Creating and saving new long format object   


```r
dog_long <- dog %>%
        mutate(height_category = case_when(height_cm <= 20.0 ~ "small",
                                           height_cm > 20.0 & height_cm < 73.0 ~ "medium",
                                           height_cm >= 73.0 ~ "large")) %>% 
        filter(height_category == "small" | height_category == "large") %>% 
        pivot_longer(cols = starts_with("chr15"),
                     names_to = "chr_location",
                     values_to = "marker_alleles_data")
```


```r
#write_csv(dog, "dog_long.csv") #saving the clean data
```

