Visualization
================
Polly Wu (rw3031)
2024-11-07

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.3     ✔ tidyr     1.3.1
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(readxl)
library(ggplot2)
library(rworldmap)
```

    ## Loading required package: sp
    ## ### Welcome to rworldmap ###
    ## For a short introduction type :   vignette('rworldmap')

``` r
data_extract = 
  read_excel("./Data Extraction Sheet.xlsx")|>
  janitor::clean_names()
```

# the plot for year of publication

``` r
year_plot =
data_extract|>
  ggplot(aes(x=year))+geom_histogram( fill = "#08519c")+
  theme_classic()+
  labs(x = "Year of Publication",
       title = "Distribution of Year of Publication")

year_plot
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](visualization-for-result_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
ggsave("year_visualization.png", plot = year_plot)
```

    ## Saving 7 x 5 in image
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

# the plot for geographic region of publication

``` r
country_count =
data_extract|>
  select(country_region)|>
  mutate(country_region = gsub("10 clinics across ", "", country_region)) |>
  separate_rows(country_region, sep = ", ") |>
  mutate(country_region = trimws(country_region))|>
  mutate(country_region = str_replace(country_region, "Hong Kong/China", "Hong Kong"),
         country_region = str_replace(country_region, "and Sweden", "Sweden"),
         country_region = str_replace(country_region, "US", "United States")
         )|>
  group_by(country_region)|>
  summarise(count=n())
```

``` r
world_map <- joinCountryData2Map(country_count, joinCode = "NAME", nameJoinColumn = "country_region")
```

    ## 17 codes from your data successfully matched countries in the map
    ## 1 codes from your data failed to match with a country code in the map
    ## 226 codes from the map weren't represented in your data

``` r
blue_palette <- rev(c("#08306b", "#08519c", "#2171b5", "#4292c6", "#6baed6", "#9ecae1", "#c6dbef", "#deebf7"))

png("map_visualization.png", width = 800, height = 600)

mapCountryData(world_map, nameColumnToPlot = "count", catMethod = "fixedWidth", colourPalette = blue_palette, mapTitle = "Geographic Distribution regarding Publications")
```

    ## Warning in rwmGetColours(colourPalette, numColours): 8 colours specified and 7
    ## required, using interpolation to calculate colours

``` r
dev.off()
```

    ## png 
    ##   2

# type of interventions

``` r
data_extract|>
  group_by(intervention_type)|>
  summarise(count = n())|>
  arrange(desc(count))
```

    ## # A tibble: 6 × 2
    ##   intervention_type                   count
    ##   <chr>                               <int>
    ## 1 mobile application                      8
    ## 2 telemedicine                            8
    ## 3 website/online platform                 8
    ## 4 electronic questionnaire/assessment     2
    ## 5 wearable device                         2
    ## 6 virtual reality                         1

``` r
data_extract|>
  select(id,intervention_type)|>
  arrange(desc(intervention_type))
```

    ## # A tibble: 29 × 2
    ##       id intervention_type      
    ##    <dbl> <chr>                  
    ##  1    16 website/online platform
    ##  2     4 website/online platform
    ##  3    22 website/online platform
    ##  4     8 website/online platform
    ##  5    20 website/online platform
    ##  6     7 website/online platform
    ##  7    18 website/online platform
    ##  8     1 website/online platform
    ##  9    29 wearable device        
    ## 10    15 wearable device        
    ## # ℹ 19 more rows

## settings

``` r
data_extract|>
  group_by(setting)|>
  summarise(count = n())|>
  arrange(desc(count))
```

    ## # A tibble: 3 × 2
    ##   setting        count
    ##   <chr>          <int>
    ## 1 home-based        23
    ## 2 clinical based     4
    ## 3 both               2

``` r
data_extract|>
  filter(setting == "home-based")
```

    ## # A tibble: 23 × 32
    ##       id title   year objective study_design intervention who_is_delivering_th…¹
    ##    <dbl> <chr>  <dbl> <chr>     <chr>        <chr>        <chr>                 
    ##  1    26 Age-R…  2007 To asses… "Prospectiv… Daily teleh… nurses                
    ##  2     5 Why D…  2014 Explore … "Qualitativ… Web-based Q… Physician             
    ##  3    16 Commu…  2014 To devel… "Qualitativ… A website d… oncologists, psycholo…
    ##  4     4 Co-cr…  2016 To devel… "User-cente… A telehealt… physiotherapists, sur…
    ##  5    17 The E…  2016 To evalu… "Retrospect… Internet-ba… emergency department …
    ##  6     9 Exper…  2017 Tests ne… "survey, qu… Patients co… Lung cancer nurse spe…
    ##  7    14 Impro…  2017 To evalu… "Retrospect… self-report… Physician             
    ##  8    21 Rando…  2017 To asses… "Multicente… Web-mediate… Physician             
    ##  9    22 Reduc…  2017 To asses… "Pooled ana… The CHESS+C… Physician             
    ## 10    23 Autom…  2017 To deter… "Longitudin… Daily autom… nurses                
    ## # ℹ 13 more rows
    ## # ℹ abbreviated name: ¹​who_is_delivering_the_intervention
    ## # ℹ 25 more variables: setting <chr>, length_of_interventions <chr>,
    ## #   frequency_of_interventions <chr>,
    ## #   theory_or_non_theory_based_is_it_guided_by_a_specific_framework <chr>,
    ## #   country_region <chr>, patient_demographics <chr>, age_range <chr>,
    ## #   stage_of_cancer <chr>, stage_2 <chr>, …

## stage

``` r
data_extract|>
  select(id,stage_2)|>
  arrange(desc(stage_2))
```

    ## # A tibble: 29 × 2
    ##       id stage_2             
    ##    <dbl> <chr>               
    ##  1    26 local and metastatic
    ##  2     5 local and metastatic
    ##  3    14 local and metastatic
    ##  4    21 local and metastatic
    ##  5    23 local and metastatic
    ##  6    24 local and metastatic
    ##  7     3 local and metastatic
    ##  8    20 local and metastatic
    ##  9     7 local and metastatic
    ## 10    25 local and metastatic
    ## # ℹ 19 more rows

``` r
data_extract|>
  select(id,stage_2)|>
  group_by(stage_2)|>
  summarise(n())
```

    ## # A tibble: 4 × 2
    ##   stage_2                    `n()`
    ##   <chr>                      <int>
    ## 1 Advanced stage, metastatic     8
    ## 2 NA                             8
    ## 3 local                          1
    ## 4 local and metastatic          12

## intervention design \* type

``` r
data_extract|>
  group_by(intervention_type,commercial_tool_designed_specifically_for_the_study)|>
  summarise(count=n())|>
  pivot_wider(
    values_from = count,
    names_from = commercial_tool_designed_specifically_for_the_study
  )|>knitr::kable()
```

    ## `summarise()` has grouped output by 'intervention_type'. You can override using
    ## the `.groups` argument.

| intervention_type                   | designed for the study | commercial tool | not specified |
|:------------------------------------|-----------------------:|----------------:|--------------:|
| electronic questionnaire/assessment |                      2 |              NA |            NA |
| mobile application                  |                      6 |               1 |             1 |
| telemedicine                        |                      5 |               3 |            NA |
| virtual reality                     |                      1 |              NA |            NA |
| wearable device                     |                     NA |               2 |            NA |
| website/online platform             |                      5 |               2 |             1 |

``` r
data_extract|>
  select(id,pre_post,intervention_type)|>
  mutate(
    Pre_treatment = ifelse(str_detect(pre_post, "Pre-treatment"), 1, 0),
    During_treatment = ifelse(str_detect(pre_post, "During-treatment"), 1, 0),
    Post_treatment = ifelse(str_detect(pre_post, "Post-treatment"), 1, 0)
  )|>
  group_by(intervention_type,pre_post)|>
  summarise(count=n())|>
  pivot_wider(
    values_from = count,
    names_from = pre_post
  )
```

    ## `summarise()` has grouped output by 'intervention_type'. You can override using
    ## the `.groups` argument.

    ## # A tibble: 6 × 7
    ## # Groups:   intervention_type [6]
    ##   intervention_type   `During-treatment` `Post-treatment` During-treatment,Pos…¹
    ##   <chr>                            <int>            <int>                  <int>
    ## 1 electronic questio…                  2               NA                     NA
    ## 2 mobile application                   5                3                     NA
    ## 3 telemedicine                         4               NA                      2
    ## 4 virtual reality                      1               NA                     NA
    ## 5 wearable device                      2               NA                     NA
    ## 6 website/online pla…                  5                1                     NA
    ## # ℹ abbreviated name: ¹​`During-treatment,Post-treatment`
    ## # ℹ 3 more variables: `Pre-treatment,During-treatment` <int>,
    ## #   `Pre-treatment,During-treatment,Post-treatment` <int>,
    ## #   `Pre-treatment` <int>

``` r
data_extract|>
  select(id,pre_post,intervention_type)|>
  separate_rows(pre_post, sep = ",")|>
  ggplot(aes(x=intervention_type,fill=pre_post))+geom_bar()+
  theme(legend.position = "bottom")
```

![](visualization-for-result_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

``` r
data_extract|>
  select(theme,intervention_type)|>
  separate_rows(theme, sep = ",")|>
  ggplot(aes(x=intervention_type,fill=theme))+geom_bar()+
  theme(legend.position = "bottom")
```

![](visualization-for-result_files/figure-gfm/unnamed-chunk-11-2.png)<!-- -->

# intervention type over pre_post

``` r
data_extract|>
  select(id,pre_post,intervention_type)|>
  separate_rows(pre_post, sep = ",")|>
  group_by(intervention_type,pre_post)|>
  summarise(count = n())|>
  pivot_wider(
    names_from = pre_post,
    values_from = count
  )|>
  knitr::kable()
```

    ## `summarise()` has grouped output by 'intervention_type'. You can override using
    ## the `.groups` argument.

| intervention_type                   | During-treatment | Post-treatment | Pre-treatment |
|:------------------------------------|-----------------:|---------------:|--------------:|
| electronic questionnaire/assessment |                2 |             NA |            NA |
| mobile application                  |                5 |              3 |            NA |
| telemedicine                        |                8 |              3 |             2 |
| virtual reality                     |                1 |             NA |            NA |
| wearable device                     |                2 |             NA |            NA |
| website/online platform             |                6 |              1 |             2 |

# intervention type over theme

``` r
data_extract|>
  select(theme,intervention_type)|>
  separate_rows(theme, sep = ",")|>
  group_by(intervention_type,theme)|>
  summarise(count = n())|>
  pivot_wider(
    names_from = theme,
    values_from = count
  )|>
  knitr::kable()
```

    ## `summarise()` has grouped output by 'intervention_type'. You can override using
    ## the `.groups` argument.

| intervention_type                   | communication and coordination | monitoring and tracking | decision support and guidance | education and information delivery | promotion of healthy behavior | emotional and social support |
|:------------------------------------|-------------------------------:|------------------------:|------------------------------:|-----------------------------------:|------------------------------:|-----------------------------:|
| electronic questionnaire/assessment |                              2 |                       2 |                            NA |                                 NA |                            NA |                           NA |
| mobile application                  |                              6 |                       8 |                             2 |                                  1 |                             1 |                           NA |
| telemedicine                        |                              7 |                       4 |                             1 |                                  3 |                             3 |                            2 |
| virtual reality                     |                             NA |                      NA |                            NA |                                 NA |                            NA |                            1 |
| wearable device                     |                             NA |                       2 |                            NA |                                 NA |                             1 |                           NA |
| website/online platform             |                              5 |                       3 |                             1 |                                  5 |                             1 |                            5 |

``` r
data_extract|>
  select(theme,intervention_type, pre_post)|>
  separate_rows(theme, sep = ",")|>
  group_by(intervention_type,theme,pre_post)|>
  summarise(count = n())|>
  pivot_wider(
    names_from = pre_post,
    values_from = count
  )|>
  knitr::kable()
```

    ## `summarise()` has grouped output by 'intervention_type', 'theme'. You can
    ## override using the `.groups` argument.

| intervention_type                   | theme                              | During-treatment | Post-treatment | During-treatment,Post-treatment | Pre-treatment,During-treatment | Pre-treatment,During-treatment,Post-treatment | Pre-treatment |
|:------------------------------------|:-----------------------------------|-----------------:|---------------:|--------------------------------:|-------------------------------:|----------------------------------------------:|--------------:|
| electronic questionnaire/assessment | communication and coordination     |                2 |             NA |                              NA |                             NA |                                            NA |            NA |
| electronic questionnaire/assessment | monitoring and tracking            |                2 |             NA |                              NA |                             NA |                                            NA |            NA |
| mobile application                  | communication and coordination     |                4 |              2 |                              NA |                             NA |                                            NA |            NA |
| mobile application                  | decision support and guidance      |                2 |             NA |                              NA |                             NA |                                            NA |            NA |
| mobile application                  | education and information delivery |                1 |             NA |                              NA |                             NA |                                            NA |            NA |
| mobile application                  | monitoring and tracking            |                5 |              3 |                              NA |                             NA |                                            NA |            NA |
| mobile application                  | promotion of healthy behavior      |               NA |              1 |                              NA |                             NA |                                            NA |            NA |
| telemedicine                        | communication and coordination     |                4 |             NA |                               1 |                              1 |                                             1 |            NA |
| telemedicine                        | decision support and guidance      |               NA |             NA |                               1 |                             NA |                                            NA |            NA |
| telemedicine                        | education and information delivery |                1 |             NA |                               1 |                             NA |                                             1 |            NA |
| telemedicine                        | emotional and social support       |               NA |             NA |                               1 |                             NA |                                             1 |            NA |
| telemedicine                        | monitoring and tracking            |                3 |             NA |                               1 |                             NA |                                            NA |            NA |
| telemedicine                        | promotion of healthy behavior      |                1 |             NA |                               1 |                             NA |                                             1 |            NA |
| virtual reality                     | emotional and social support       |                1 |             NA |                              NA |                             NA |                                            NA |            NA |
| wearable device                     | monitoring and tracking            |                2 |             NA |                              NA |                             NA |                                            NA |            NA |
| wearable device                     | promotion of healthy behavior      |                1 |             NA |                              NA |                             NA |                                            NA |            NA |
| website/online platform             | communication and coordination     |                3 |              1 |                              NA |                              1 |                                            NA |            NA |
| website/online platform             | decision support and guidance      |               NA |             NA |                              NA |                             NA |                                            NA |             1 |
| website/online platform             | education and information delivery |                3 |             NA |                              NA |                              1 |                                            NA |             1 |
| website/online platform             | emotional and social support       |                3 |              1 |                              NA |                              1 |                                            NA |            NA |
| website/online platform             | monitoring and tracking            |                3 |             NA |                              NA |                             NA |                                            NA |            NA |
| website/online platform             | promotion of healthy behavior      |                1 |             NA |                              NA |                             NA |                                            NA |            NA |

``` r
data_extract|>
  select(id, theme,intervention_type, pre_post)|>
  separate_rows(theme, sep = ",")|>
  group_by(intervention_type,theme,pre_post)|>
  summarise(ids = paste(unique(id), collapse = ", ")) |>
  pivot_wider(
    names_from = pre_post,
    values_from = ids
  )|>
  knitr::kable()
```

    ## `summarise()` has grouped output by 'intervention_type', 'theme'. You can
    ## override using the `.groups` argument.

| intervention_type                   | theme                              | During-treatment  | Post-treatment | During-treatment,Post-treatment | Pre-treatment,During-treatment | Pre-treatment,During-treatment,Post-treatment | Pre-treatment |
|:------------------------------------|:-----------------------------------|:------------------|:---------------|:--------------------------------|:-------------------------------|:----------------------------------------------|:--------------|
| electronic questionnaire/assessment | communication and coordination     | 17, 24            | NA             | NA                              | NA                             | NA                                            | NA            |
| electronic questionnaire/assessment | monitoring and tracking            | 17, 24            | NA             | NA                              | NA                             | NA                                            | NA            |
| mobile application                  | communication and coordination     | 26, 3, 28, 12     | 13, 6          | NA                              | NA                             | NA                                            | NA            |
| mobile application                  | decision support and guidance      | 28, 12            | NA             | NA                              | NA                             | NA                                            | NA            |
| mobile application                  | education and information delivery | 28                | NA             | NA                              | NA                             | NA                                            | NA            |
| mobile application                  | monitoring and tracking            | 26, 11, 3, 28, 12 | 5, 13, 6       | NA                              | NA                             | NA                                            | NA            |
| mobile application                  | promotion of healthy behavior      | NA                | 5              | NA                              | NA                             | NA                                            | NA            |
| telemedicine                        | communication and coordination     | 23, 25, 2, 19     | NA             | 14                              | 27                             | 9                                             | NA            |
| telemedicine                        | decision support and guidance      | NA                | NA             | 14                              | NA                             | NA                                            | NA            |
| telemedicine                        | education and information delivery | 19                | NA             | 10                              | NA                             | 9                                             | NA            |
| telemedicine                        | emotional and social support       | NA                | NA             | 14                              | NA                             | 9                                             | NA            |
| telemedicine                        | monitoring and tracking            | 23, 25, 2         | NA             | 14                              | NA                             | NA                                            | NA            |
| telemedicine                        | promotion of healthy behavior      | 19                | NA             | 10                              | NA                             | 9                                             | NA            |
| virtual reality                     | emotional and social support       | 21                | NA             | NA                              | NA                             | NA                                            | NA            |
| wearable device                     | monitoring and tracking            | 29, 15            | NA             | NA                              | NA                             | NA                                            | NA            |
| wearable device                     | promotion of healthy behavior      | 29                | NA             | NA                              | NA                             | NA                                            | NA            |
| website/online platform             | communication and coordination     | 4, 22, 18         | 20             | NA                              | 16                             | NA                                            | NA            |
| website/online platform             | decision support and guidance      | NA                | NA             | NA                              | NA                             | NA                                            | 8             |
| website/online platform             | education and information delivery | 22, 7, 1          | NA             | NA                              | 16                             | NA                                            | 8             |
| website/online platform             | emotional and social support       | 4, 18, 1          | 20             | NA                              | 16                             | NA                                            | NA            |
| website/online platform             | monitoring and tracking            | 4, 7, 18          | NA             | NA                              | NA                             | NA                                            | NA            |
| website/online platform             | promotion of healthy behavior      | 1                 | NA             | NA                              | NA                             | NA                                            | NA            |
