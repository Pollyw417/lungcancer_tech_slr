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

country_count|>
  knitr::kable()
```

| country_region | count |
|:---------------|------:|
| Belgium        |     1 |
| China          |     1 |
| Estonia        |     1 |
| Finland        |     1 |
| France         |     4 |
| Greece         |     1 |
| Hong Kong      |     1 |
| Japan          |     1 |
| Korea          |     1 |
| Netherlands    |     4 |
| Norway         |     1 |
| Portugal       |     1 |
| Scotland       |     1 |
| Spain          |     2 |
| Sweden         |     1 |
| Taiwan         |     1 |
| United Kingdom |     1 |
| United States  |    11 |

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
    ##  1     5 website/online platform
    ##  2     6 website/online platform
    ##  3    11 website/online platform
    ##  4    16 website/online platform
    ##  5    17 website/online platform
    ##  6    18 website/online platform
    ##  7    22 website/online platform
    ##  8    24 website/online platform
    ##  9    20 wearable device        
    ## 10    28 wearable device        
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

    ## # A tibble: 23 × 33
    ##       id title   year objective study_design intervention who_is_delivering_th…¹
    ##    <dbl> <chr>  <dbl> <chr>     <chr>        <chr>        <chr>                 
    ##  1     1 Telem…  2023 We inves… "a parallel… 12-week int… trainer               
    ##  2     2 Testi…  2024 Test the… "Structured… A Lalaby Ap… Research team         
    ##  3     3 Mobil…  2019 determin… "prospectiv… A comprehen… Physician             
    ##  4     4 Co-cr…  2016 To devel… "User-cente… A telehealt… physiotherapists, sur…
    ##  5     5 Why D…  2014 Explore … "Qualitativ… Web-based Q… Physician             
    ##  6     6 Evalu…  2022 Evaluate… "Mixed-meth… Digital pla… oncologists, nurses, …
    ##  7     8 Pilot…  2020 Assess t… "Pilot stud… Telehealth … Physical and occupati…
    ##  8     9 Exper…  2017 Tests ne… "survey, qu… Patients co… Lung cancer nurse spe…
    ##  9    12 Teleh…  2024 To evalu… "Multisite … Early palli… Physician             
    ## 10    14 Impro…  2017 To evalu… "Retrospect… self-report… Physician             
    ## # ℹ 13 more rows
    ## # ℹ abbreviated name: ¹​who_is_delivering_the_intervention
    ## # ℹ 26 more variables: setting <chr>, length_of_interventions <chr>,
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
    ##  1     1 local and metastatic
    ##  2     3 local and metastatic
    ##  3     5 local and metastatic
    ##  4     6 local and metastatic
    ##  5     7 local and metastatic
    ##  6    14 local and metastatic
    ##  7    20 local and metastatic
    ##  8    21 local and metastatic
    ##  9    23 local and metastatic
    ## 10    24 local and metastatic
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

## treatment type

``` r
data_extract|>
  select(treatment_2,id)|>
  separate_rows(treatment_2, sep = ",")|>
  group_by(treatment_2)|>
  summarise(id=paste(unique(id),collapse = ","), count = n())
```

    ## # A tibble: 7 × 3
    ##   treatment_2        id                                    count
    ##   <chr>              <chr>                                 <int>
    ## 1 chemotherapy       1,2,3,5,6,7,9,10,12,14,15,21,23,24,26    15
    ## 2 immunotherapy      6,7,29                                    3
    ## 3 not specified      11,13,16,18,19,20,22                      7
    ## 4 palliative/hospice 27,28                                     2
    ## 5 radiotherapy       1,9,14,21,24,25                           6
    ## 6 surgery            1,4,5,8,9,14,17,21,24,25                 10
    ## 7 targeted therapy   7,21                                      2

## challenge faced by patients in different treatment regimens

``` r
data_extract|>
  select(challenge_recode, treatment_2, id)|>
  separate_rows(challenge_recode, sep = ",")|>
  group_by(challenge_recode)|>
  summarise(count = n(),
            id=paste(unique(id),collapse = ","))|>
  arrange(desc(count))
```

    ## # A tibble: 6 × 3
    ##   challenge_recode                              count id                        
    ##   <chr>                                         <int> <chr>                     
    ## 1 symptom distress/side-effect management          23 1,2,3,4,6,7,8,10,11,12,14…
    ## 2 emotional distress/anxiety/depression            12 1,5,6,8,10,12,16,17,19,24…
    ## 3 accurate and in-time reporting of symptoms       10 2,6,7,13,14,21,22,23,27,28
    ## 4 communication with providers                      8 2,4,6,7,9,14,15,25        
    ## 5 adherence to rehabilitation/physical activity     7 1,3,4,8,18,24,29          
    ## 6 preparedness for treatment                        3 5,8,16

``` r
data_extract|>
  select(challenge_recode, treatment_2, id)|>
  separate_rows(challenge_recode, sep = ",")|>
  separate_rows(treatment_2, sep = ",")|>
  group_by(challenge_recode, treatment_2)|>
  summarise(count = n(),
            id=paste(unique(id),collapse = ","))|>
  arrange(treatment_2, desc(count))
```

    ## `summarise()` has grouped output by 'challenge_recode'. You can override using
    ## the `.groups` argument.

    ## # A tibble: 33 × 4
    ## # Groups:   challenge_recode [6]
    ##    challenge_recode                              treatment_2   count id         
    ##    <chr>                                         <chr>         <int> <chr>      
    ##  1 symptom distress/side-effect management       chemotherapy     13 1,2,3,6,7,…
    ##  2 accurate and in-time reporting of symptoms    chemotherapy      6 2,6,7,14,2…
    ##  3 communication with providers                  chemotherapy      6 2,6,7,9,14…
    ##  4 emotional distress/anxiety/depression         chemotherapy      6 1,5,6,10,1…
    ##  5 adherence to rehabilitation/physical activity chemotherapy      3 1,3,24     
    ##  6 preparedness for treatment                    chemotherapy      1 5          
    ##  7 accurate and in-time reporting of symptoms    immunotherapy     2 6,7        
    ##  8 communication with providers                  immunotherapy     2 6,7        
    ##  9 emotional distress/anxiety/depression         immunotherapy     2 6,29       
    ## 10 symptom distress/side-effect management       immunotherapy     2 6,7        
    ## # ℹ 23 more rows

## intervention design \* type

``` r
data_extract|>
  select(id,intervention_type,commercial_tool_designed_specifically_for_the_study)|>
  group_by(intervention_type,commercial_tool_designed_specifically_for_the_study)|>
  summarise(id=paste(unique(id),collapse = ","))|>
  pivot_wider(
    values_from = id,
    names_from = commercial_tool_designed_specifically_for_the_study
  )
```

    ## `summarise()` has grouped output by 'intervention_type'. You can override using
    ## the `.groups` argument.

    ## # A tibble: 6 × 3
    ## # Groups:   intervention_type [6]
    ##   intervention_type                   `designed for the study` `commercial tool`
    ##   <chr>                               <chr>                    <chr>            
    ## 1 electronic questionnaire/assessment 7,13                     <NA>             
    ## 2 mobile application                  2,3,4,14,21,23,27        15               
    ## 3 telemedicine                        1,12,19,26               8,9,25,29        
    ## 4 virtual reality                     <NA>                     10               
    ## 5 wearable device                     28                       20               
    ## 6 website/online platform             6,11,16,17,18,22,24      5

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

## plot for pre-post over intervention type

``` r
library(viridis)
```

    ## Loading required package: viridisLite

``` r
data_extract |>
  select(id, pre_post, intervention_type) |>
  separate_rows(pre_post, sep = ",") |>
  ggplot(aes(x = intervention_type, fill = pre_post)) +
  geom_bar() +
  scale_fill_viridis_d() +
  theme(
      legend.position = "bottom",
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )
```

![](visualization-for-result_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

## plot for theme over intervention type

``` r
theme_intervention =
  data_extract |>
    select(theme, intervention_type) |>
    separate_rows(theme, sep = ",") |>
    ggplot(aes(x = intervention_type, fill = theme)) +
    geom_bar() +
    scale_fill_viridis_d() +
    theme(
      legend.position = "bottom",
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )

theme_intervention
```

![](visualization-for-result_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

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

| intervention_type | During-treatment | Post-treatment | Pre-treatment |
|:---|---:|---:|---:|
| electronic questionnaire/assessment | 2 | NA | NA |
| mobile application | 5 | 3 | NA |
| telemedicine | 8 | 3 | 2 |
| virtual reality | 1 | NA | NA |
| wearable device | 2 | NA | NA |
| website/online platform | 6 | 1 | 2 |

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

| intervention_type | communication and coordination | monitoring and tracking | decision support and guidance | education and information delivery | promotion of healthy behavior | emotional and social support |
|:---|---:|---:|---:|---:|---:|---:|
| electronic questionnaire/assessment | 2 | 2 | NA | NA | NA | NA |
| mobile application | 6 | 8 | 2 | 1 | 1 | NA |
| telemedicine | 7 | 4 | 1 | 3 | 3 | 2 |
| virtual reality | NA | NA | NA | NA | NA | 1 |
| wearable device | NA | 2 | NA | NA | 1 | NA |
| website/online platform | 5 | 3 | 1 | 5 | 1 | 5 |

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

| intervention_type | theme | During-treatment | Post-treatment | During-treatment,Post-treatment | Pre-treatment,During-treatment | Pre-treatment,During-treatment,Post-treatment | Pre-treatment |
|:---|:---|---:|---:|---:|---:|---:|---:|
| electronic questionnaire/assessment | communication and coordination | 2 | NA | NA | NA | NA | NA |
| electronic questionnaire/assessment | monitoring and tracking | 2 | NA | NA | NA | NA | NA |
| mobile application | communication and coordination | 4 | 2 | NA | NA | NA | NA |
| mobile application | decision support and guidance | 2 | NA | NA | NA | NA | NA |
| mobile application | education and information delivery | 1 | NA | NA | NA | NA | NA |
| mobile application | monitoring and tracking | 5 | 3 | NA | NA | NA | NA |
| mobile application | promotion of healthy behavior | NA | 1 | NA | NA | NA | NA |
| telemedicine | communication and coordination | 4 | NA | 1 | 1 | 1 | NA |
| telemedicine | decision support and guidance | NA | NA | 1 | NA | NA | NA |
| telemedicine | education and information delivery | 1 | NA | 1 | NA | 1 | NA |
| telemedicine | emotional and social support | NA | NA | 1 | NA | 1 | NA |
| telemedicine | monitoring and tracking | 3 | NA | 1 | NA | NA | NA |
| telemedicine | promotion of healthy behavior | 1 | NA | 1 | NA | 1 | NA |
| virtual reality | emotional and social support | 1 | NA | NA | NA | NA | NA |
| wearable device | monitoring and tracking | 2 | NA | NA | NA | NA | NA |
| wearable device | promotion of healthy behavior | 1 | NA | NA | NA | NA | NA |
| website/online platform | communication and coordination | 3 | 1 | NA | 1 | NA | NA |
| website/online platform | decision support and guidance | NA | NA | NA | NA | NA | 1 |
| website/online platform | education and information delivery | 3 | NA | NA | 1 | NA | 1 |
| website/online platform | emotional and social support | 3 | 1 | NA | 1 | NA | NA |
| website/online platform | monitoring and tracking | 3 | NA | NA | NA | NA | NA |
| website/online platform | promotion of healthy behavior | 1 | NA | NA | NA | NA | NA |

``` r
data_extract|>
  select(id, theme, intervention_type, pre_post)|>
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

| intervention_type | theme | During-treatment | Post-treatment | During-treatment,Post-treatment | Pre-treatment,During-treatment | Pre-treatment,During-treatment,Post-treatment | Pre-treatment |
|:---|:---|:---|:---|:---|:---|:---|:---|
| electronic questionnaire/assessment | communication and coordination | 7, 13 | NA | NA | NA | NA | NA |
| electronic questionnaire/assessment | monitoring and tracking | 7, 13 | NA | NA | NA | NA | NA |
| mobile application | communication and coordination | 2, 15, 23, 27 | 14, 21 | NA | NA | NA | NA |
| mobile application | decision support and guidance | 23, 27 | NA | NA | NA | NA | NA |
| mobile application | education and information delivery | 23 | NA | NA | NA | NA | NA |
| mobile application | monitoring and tracking | 2, 3, 15, 23, 27 | 4, 14, 21 | NA | NA | NA | NA |
| mobile application | promotion of healthy behavior | NA | 4 | NA | NA | NA | NA |
| telemedicine | communication and coordination | 12, 19, 26, 29 | NA | 9 | 25 | 8 | NA |
| telemedicine | decision support and guidance | NA | NA | 9 | NA | NA | NA |
| telemedicine | education and information delivery | 29 | NA | 1 | NA | 8 | NA |
| telemedicine | emotional and social support | NA | NA | 9 | NA | 8 | NA |
| telemedicine | monitoring and tracking | 12, 19, 26 | NA | 9 | NA | NA | NA |
| telemedicine | promotion of healthy behavior | 29 | NA | 1 | NA | 8 | NA |
| virtual reality | emotional and social support | 10 | NA | NA | NA | NA | NA |
| wearable device | monitoring and tracking | 20, 28 | NA | NA | NA | NA | NA |
| wearable device | promotion of healthy behavior | 20 | NA | NA | NA | NA | NA |
| website/online platform | communication and coordination | 6, 11, 22 | 17 | NA | 5 | NA | NA |
| website/online platform | decision support and guidance | NA | NA | NA | NA | NA | 16 |
| website/online platform | education and information delivery | 11, 18, 24 | NA | NA | 5 | NA | 16 |
| website/online platform | emotional and social support | 6, 22, 24 | 17 | NA | 5 | NA | NA |
| website/online platform | monitoring and tracking | 6, 18, 22 | NA | NA | NA | NA | NA |
| website/online platform | promotion of healthy behavior | 24 | NA | NA | NA | NA | NA |

# pre_treatment support

``` r
data_extract|>
  filter(str_detect(pre_post, regex("pre-treatment", ignore_case = TRUE)))|>
  select(id, theme,intervention_type,impact_of_technology_on_treatment_outcomes, intervention)|>
  separate_rows(theme, sep = ",")|>
  arrange(theme)|>
  knitr::kable()
```

| id | theme | intervention_type | impact_of_technology_on_treatment_outcomes | intervention |
|---:|:---|:---|:---|:---|
| 5 | communication and coordination | website/online platform | Improved decision-making, emotional support, reduced anxiety. | Web-based Q&A platform on DLIC website. |
| 8 | communication and coordination | telemedicine | High adherence to the program; improved functional capacity, improving trajectory for patient distress | Telehealth sessions with PT/OT to improve physical activity before and after surgery. |
| 25 | communication and coordination | telemedicine | Time to treatment initiation did not differ between telemedicine and in-person visits across all treatment modalities, for patients who are newly diagnosied the median time from referral to initial visit were shorter among the telemedicine group | Telemedicine visits via phone or video conference |
| 16 | decision support and guidance | website/online platform | Website improved patient understanding of SABR and helped reduce anxiety based on the qualitative outcome | A website designed to provide treatment information for early-stage lung cancer patients considering SABR |
| 5 | education and information delivery | website/online platform | Improved decision-making, emotional support, reduced anxiety. | Web-based Q&A platform on DLIC website. |
| 8 | education and information delivery | telemedicine | High adherence to the program; improved functional capacity, improving trajectory for patient distress | Telehealth sessions with PT/OT to improve physical activity before and after surgery. |
| 16 | education and information delivery | website/online platform | Website improved patient understanding of SABR and helped reduce anxiety based on the qualitative outcome | A website designed to provide treatment information for early-stage lung cancer patients considering SABR |
| 5 | emotional and social support | website/online platform | Improved decision-making, emotional support, reduced anxiety. | Web-based Q&A platform on DLIC website. |
| 8 | emotional and social support | telemedicine | High adherence to the program; improved functional capacity, improving trajectory for patient distress | Telehealth sessions with PT/OT to improve physical activity before and after surgery. |
| 8 | promotion of healthy behavior | telemedicine | High adherence to the program; improved functional capacity, improving trajectory for patient distress | Telehealth sessions with PT/OT to improve physical activity before and after surgery. |

# during treatment

``` r
data_extract|>
  filter(str_detect(pre_post, regex("during-treatment", ignore_case = TRUE)))|>
  select(id, theme, intervention_type, impact_of_technology_on_treatment_outcomes, intervention)|>
  separate_rows(theme, sep = ",")|>
  group_by(theme)|>
  arrange(theme)|>
  knitr::kable()
```

| id | theme | intervention_type | impact_of_technology_on_treatment_outcomes | intervention |
|---:|:---|:---|:---|:---|
| 2 | communication and coordination | mobile application | report of relevant symptoms after chemotherapy | A Lalaby App that require patient to report activities/symptoms twice a day, and complete a questionnaire on QOL weekly |
| 5 | communication and coordination | website/online platform | Improved decision-making, emotional support, reduced anxiety. | Web-based Q&A platform on DLIC website. |
| 6 | communication and coordination | website/online platform | Improved communication, enhanced engagement, better psychosocial needs identification, easier planning of care activites, high reponse rate for patients during follow up | Digital platform for patient reported outcome monitoring and follow-up. |
| 7 | communication and coordination | electronic questionnaire/assessment | 90.9% compliance rate for lung cancer patients, enhanced communication, symptom management, and care adjustments. | Patients completed HRQoL questionnaires before each visit using tablets or computers. |
| 8 | communication and coordination | telemedicine | High adherence to the program; improved functional capacity, improving trajectory for patient distress | Telehealth sessions with PT/OT to improve physical activity before and after surgery. |
| 9 | communication and coordination | telemedicine | Improved identification of patient needs and patient report improved quality of life at follow up assessment, emotional support, 75% reduction in high concerns | Patients completed eSPARC electronic questionnaires monthly, combined with telephone consultations. |
| 11 | communication and coordination | website/online platform | Reduced symptom distress in CHESS arm versus Internet arm at 4 and 6 months, with possible survival benefit for CHESS users | Online support system (CHESS) providing lung cancer information, caregiver resources, and communication with clinicians and social networks |
| 12 | communication and coordination | telemedicine | Telehealth visits were found equivalent to in-person care in maintaining quality of life, no difference in satisfaction with care, anxiety and depression symptoms, use of approach-oriented or avoidant coping strategies, or perceptions of the primary goal of treatment and curability of their cancer | Early palliative care via secure video |
| 13 | communication and coordination | electronic questionnaire/assessment | Improved patient awareness of symptoms and facilitated communication with healthcare providers; advantages of electronic assessment mentioned in interview: clear presentation, expedited assessment, user-friendliness | Tablet PC-assisted symptom assessment targeting dyspnoea, fatigue, pain, and anxiety |
| 15 | communication and coordination | mobile application | Improved QoL at week 12, but not week 6 or week 18, improved patient-HCP communication | Digital monitoring tool (Kaiku Health DPM) for symptom reporting and management |
| 19 | communication and coordination | telemedicine | patients reported symptom relief, lifestyle improvements, improved self efficacy | 7-session, hour-long series on topics like mindful movement, acupuncture, culinary medicine, and stress management delivered via telehealth |
| 22 | communication and coordination | website/online platform | Patients in the CHESS+CR group had improved symptoms reported more often than those in CHESS-only (53% vs. 26%), web-based reporting let to more timely symptom management | The CHESS+CR (Clinician Report) eHealth system designed for caregivers with a symptom-reporting system with alerts sent to clinicians for severe patient symptoms |
| 23 | communication and coordination | mobile application | SCH participants had lower symptom severity, fewer severe/moderate symptom days, and more mild/no symptom days compared to UC. | Daily automated symptom reporting, self-management coaching, automated alert for poorly controlled symptom, and nurse practitioner (NP) follow-up using a decision support system |
| 25 | communication and coordination | telemedicine | Time to treatment initiation did not differ between telemedicine and in-person visits across all treatment modalities, for patients who are newly diagnosied the median time from referral to initial visit were shorter among the telemedicine group | Telemedicine visits via phone or video conference |
| 26 | communication and coordination | telemedicine | Older adults reported greater increase in HRQOL than younger adults, the increase are statistically signficant when comparing baseline with 6 months | Daily telehealth interactions with a care coordinator (registered nurse) to manage chemotherapy-related symptoms |
| 27 | communication and coordination | mobile application | Reduction in reported symptom severity in the SCH group compared to the control group, days reporting one or more moderate-to-severe patient symptom reduce by 38% | SCH automated symptom reporting system with caregiver coaching and nurse notifications based on severity of symptoms |
| 29 | communication and coordination | telemedicine | A signifcant 2.1 point decrease in fatigue level comparing pre- and post programme scores. Patients show increase in moderate physical acvtivity time, and intrinsic motivation to practice PA | Six-month videoconference-based PA program combining supervised and autonomous sessions targeting fatigue reduction |
| 9 | decision support and guidance | telemedicine | Improved identification of patient needs and patient report improved quality of life at follow up assessment, emotional support, 75% reduction in high concerns | Patients completed eSPARC electronic questionnaires monthly, combined with telephone consultations. |
| 23 | decision support and guidance | mobile application | SCH participants had lower symptom severity, fewer severe/moderate symptom days, and more mild/no symptom days compared to UC. | Daily automated symptom reporting, self-management coaching, automated alert for poorly controlled symptom, and nurse practitioner (NP) follow-up using a decision support system |
| 27 | decision support and guidance | mobile application | Reduction in reported symptom severity in the SCH group compared to the control group, days reporting one or more moderate-to-severe patient symptom reduce by 38% | SCH automated symptom reporting system with caregiver coaching and nurse notifications based on severity of symptoms |
| 1 | education and information delivery | telemedicine | intervention participants had statistically significant and clinically meaningful improved HRQL (SGRQ total, symptom, and impact scores) (standardized effect size: -1.03 to -1.30). | 12-week intervention, delivered via telemedicine, consisted of exercise training (IMT + walking), education, and behavior change support. |
| 5 | education and information delivery | website/online platform | Improved decision-making, emotional support, reduced anxiety. | Web-based Q&A platform on DLIC website. |
| 8 | education and information delivery | telemedicine | High adherence to the program; improved functional capacity, improving trajectory for patient distress | Telehealth sessions with PT/OT to improve physical activity before and after surgery. |
| 11 | education and information delivery | website/online platform | Reduced symptom distress in CHESS arm versus Internet arm at 4 and 6 months, with possible survival benefit for CHESS users | Online support system (CHESS) providing lung cancer information, caregiver resources, and communication with clinicians and social networks |
| 18 | education and information delivery | website/online platform | High potential for supporting self-management of chronic breathlessness if implemented, though outcomes not directly measured | SELF-BREATHE online self-management intervention targeting breathlessness in patients with chronic conditions |
| 23 | education and information delivery | mobile application | SCH participants had lower symptom severity, fewer severe/moderate symptom days, and more mild/no symptom days compared to UC. | Daily automated symptom reporting, self-management coaching, automated alert for poorly controlled symptom, and nurse practitioner (NP) follow-up using a decision support system |
| 24 | education and information delivery | website/online platform | 61% (17/28) reported that this information enhanced knowledge of their disease and 43% (12/28) indicated that it enhanced their sense of control over their disease, no improvement of patient outcome over time | MyAVL portal providing patient education, appointment overview, EMR access, PROs with feedback, and tailored physical activity support |
| 29 | education and information delivery | telemedicine | A signifcant 2.1 point decrease in fatigue level comparing pre- and post programme scores. Patients show increase in moderate physical acvtivity time, and intrinsic motivation to practice PA | Six-month videoconference-based PA program combining supervised and autonomous sessions targeting fatigue reduction |
| 5 | emotional and social support | website/online platform | Improved decision-making, emotional support, reduced anxiety. | Web-based Q&A platform on DLIC website. |
| 6 | emotional and social support | website/online platform | Improved communication, enhanced engagement, better psychosocial needs identification, easier planning of care activites, high reponse rate for patients during follow up | Digital platform for patient reported outcome monitoring and follow-up. |
| 8 | emotional and social support | telemedicine | High adherence to the program; improved functional capacity, improving trajectory for patient distress | Telehealth sessions with PT/OT to improve physical activity before and after surgery. |
| 9 | emotional and social support | telemedicine | Improved identification of patient needs and patient report improved quality of life at follow up assessment, emotional support, 75% reduction in high concerns | Patients completed eSPARC electronic questionnaires monthly, combined with telephone consultations. |
| 10 | emotional and social support | virtual reality | patients had an altered perception of time, no signficant difference in symptom distress, no cybersickness | VR distraction with scenarios including deep sea diving, art museum exploration, ancient worlds, and solving a mystery |
| 22 | emotional and social support | website/online platform | Patients in the CHESS+CR group had improved symptoms reported more often than those in CHESS-only (53% vs. 26%), web-based reporting let to more timely symptom management | The CHESS+CR (Clinician Report) eHealth system designed for caregivers with a symptom-reporting system with alerts sent to clinicians for severe patient symptoms |
| 24 | emotional and social support | website/online platform | 61% (17/28) reported that this information enhanced knowledge of their disease and 43% (12/28) indicated that it enhanced their sense of control over their disease, no improvement of patient outcome over time | MyAVL portal providing patient education, appointment overview, EMR access, PROs with feedback, and tailored physical activity support |
| 2 | monitoring and tracking | mobile application | report of relevant symptoms after chemotherapy | A Lalaby App that require patient to report activities/symptoms twice a day, and complete a questionnaire on QOL weekly |
| 3 | monitoring and tracking | mobile application | improvement in excersice capacity, decrease pain severity at 6 weeks, improve in anxiety and depression at 12 week, no change on QoL, reduction in unexpected visit to ED | A comprehensive mobile health care app with a pulmonary rehabilitation program and symptom management resources and a Internet of Things wearable device |
| 6 | monitoring and tracking | website/online platform | Improved communication, enhanced engagement, better psychosocial needs identification, easier planning of care activites, high reponse rate for patients during follow up | Digital platform for patient reported outcome monitoring and follow-up. |
| 7 | monitoring and tracking | electronic questionnaire/assessment | 90.9% compliance rate for lung cancer patients, enhanced communication, symptom management, and care adjustments. | Patients completed HRQoL questionnaires before each visit using tablets or computers. |
| 9 | monitoring and tracking | telemedicine | Improved identification of patient needs and patient report improved quality of life at follow up assessment, emotional support, 75% reduction in high concerns | Patients completed eSPARC electronic questionnaires monthly, combined with telephone consultations. |
| 12 | monitoring and tracking | telemedicine | Telehealth visits were found equivalent to in-person care in maintaining quality of life, no difference in satisfaction with care, anxiety and depression symptoms, use of approach-oriented or avoidant coping strategies, or perceptions of the primary goal of treatment and curability of their cancer | Early palliative care via secure video |
| 13 | monitoring and tracking | electronic questionnaire/assessment | Improved patient awareness of symptoms and facilitated communication with healthcare providers; advantages of electronic assessment mentioned in interview: clear presentation, expedited assessment, user-friendliness | Tablet PC-assisted symptom assessment targeting dyspnoea, fatigue, pain, and anxiety |
| 15 | monitoring and tracking | mobile application | Improved QoL at week 12, but not week 6 or week 18, improved patient-HCP communication | Digital monitoring tool (Kaiku Health DPM) for symptom reporting and management |
| 18 | monitoring and tracking | website/online platform | High potential for supporting self-management of chronic breathlessness if implemented, though outcomes not directly measured | SELF-BREATHE online self-management intervention targeting breathlessness in patients with chronic conditions |
| 19 | monitoring and tracking | telemedicine | patients reported symptom relief, lifestyle improvements, improved self efficacy | 7-session, hour-long series on topics like mindful movement, acupuncture, culinary medicine, and stress management delivered via telehealth |
| 20 | monitoring and tracking | wearable device | Degree of physical activity is correlated with patient reported outcome (brief fatigue inventory, MD Anderson Symptom Inventory) | Patients wore a Garmin vívofit 3® wristband for 28 days to monitor physical activity and sleep duration |
| 22 | monitoring and tracking | website/online platform | Patients in the CHESS+CR group had improved symptoms reported more often than those in CHESS-only (53% vs. 26%), web-based reporting let to more timely symptom management | The CHESS+CR (Clinician Report) eHealth system designed for caregivers with a symptom-reporting system with alerts sent to clinicians for severe patient symptoms |
| 23 | monitoring and tracking | mobile application | SCH participants had lower symptom severity, fewer severe/moderate symptom days, and more mild/no symptom days compared to UC. | Daily automated symptom reporting, self-management coaching, automated alert for poorly controlled symptom, and nurse practitioner (NP) follow-up using a decision support system |
| 26 | monitoring and tracking | telemedicine | Older adults reported greater increase in HRQOL than younger adults, the increase are statistically signficant when comparing baseline with 6 months | Daily telehealth interactions with a care coordinator (registered nurse) to manage chemotherapy-related symptoms |
| 27 | monitoring and tracking | mobile application | Reduction in reported symptom severity in the SCH group compared to the control group, days reporting one or more moderate-to-severe patient symptom reduce by 38% | SCH automated symptom reporting system with caregiver coaching and nurse notifications based on severity of symptoms |
| 28 | monitoring and tracking | wearable device | Higher activity (angle and spin values) correlated with survival, wearable device successfully evaluate prognosis of patients and predict survival outcomes for patients in hospice care | Continuous monitoring of patients’ hand movements with wearable actigraphy devices |
| 1 | promotion of healthy behavior | telemedicine | intervention participants had statistically significant and clinically meaningful improved HRQL (SGRQ total, symptom, and impact scores) (standardized effect size: -1.03 to -1.30). | 12-week intervention, delivered via telemedicine, consisted of exercise training (IMT + walking), education, and behavior change support. |
| 8 | promotion of healthy behavior | telemedicine | High adherence to the program; improved functional capacity, improving trajectory for patient distress | Telehealth sessions with PT/OT to improve physical activity before and after surgery. |
| 20 | promotion of healthy behavior | wearable device | Degree of physical activity is correlated with patient reported outcome (brief fatigue inventory, MD Anderson Symptom Inventory) | Patients wore a Garmin vívofit 3® wristband for 28 days to monitor physical activity and sleep duration |
| 24 | promotion of healthy behavior | website/online platform | 61% (17/28) reported that this information enhanced knowledge of their disease and 43% (12/28) indicated that it enhanced their sense of control over their disease, no improvement of patient outcome over time | MyAVL portal providing patient education, appointment overview, EMR access, PROs with feedback, and tailored physical activity support |
| 29 | promotion of healthy behavior | telemedicine | A signifcant 2.1 point decrease in fatigue level comparing pre- and post programme scores. Patients show increase in moderate physical acvtivity time, and intrinsic motivation to practice PA | Six-month videoconference-based PA program combining supervised and autonomous sessions targeting fatigue reduction |

# post_treatment

``` r
data_extract|>
  filter(str_detect(pre_post, regex("post-treatment", ignore_case = TRUE)))|>
  select(id, theme,intervention_type, impact_of_technology_on_treatment_outcomes, intervention)|>
  separate_rows(theme, sep = ",")|>
  group_by(theme)|>
  arrange(theme)|>
  knitr::kable()
```

| id | theme | intervention_type | impact_of_technology_on_treatment_outcomes | intervention |
|---:|:---|:---|:---|:---|
| 8 | communication and coordination | telemedicine | High adherence to the program; improved functional capacity, improving trajectory for patient distress | Telehealth sessions with PT/OT to improve physical activity before and after surgery. |
| 9 | communication and coordination | telemedicine | Improved identification of patient needs and patient report improved quality of life at follow up assessment, emotional support, 75% reduction in high concerns | Patients completed eSPARC electronic questionnaires monthly, combined with telephone consultations. |
| 14 | communication and coordination | mobile application | Significant improvement in survival with median survival of 22.4 months in the experimental group versus 16.7 months in the control group | self-reported symptom assessment via web application, alerts to oncologist for early intervention based on predefined criteria |
| 17 | communication and coordination | website/online platform | Improved quality of life (SF-36 scores) and high patient satisfaction in the remote group compared to conventional care group | Internet-based pain management intervention, including remote pain control, lifestyle advice, and psychological support via smartphones and internet |
| 21 | communication and coordination | mobile application | Improved overall survival and better performance status at relapse in the intervention group, reduced imaging needs | Web-mediated follow-up using an e-follow up application for weekly self-scored symptoms, triggering alerts for oncologists based on abnormal results |
| 9 | decision support and guidance | telemedicine | Improved identification of patient needs and patient report improved quality of life at follow up assessment, emotional support, 75% reduction in high concerns | Patients completed eSPARC electronic questionnaires monthly, combined with telephone consultations. |
| 1 | education and information delivery | telemedicine | intervention participants had statistically significant and clinically meaningful improved HRQL (SGRQ total, symptom, and impact scores) (standardized effect size: -1.03 to -1.30). | 12-week intervention, delivered via telemedicine, consisted of exercise training (IMT + walking), education, and behavior change support. |
| 8 | education and information delivery | telemedicine | High adherence to the program; improved functional capacity, improving trajectory for patient distress | Telehealth sessions with PT/OT to improve physical activity before and after surgery. |
| 8 | emotional and social support | telemedicine | High adherence to the program; improved functional capacity, improving trajectory for patient distress | Telehealth sessions with PT/OT to improve physical activity before and after surgery. |
| 9 | emotional and social support | telemedicine | Improved identification of patient needs and patient report improved quality of life at follow up assessment, emotional support, 75% reduction in high concerns | Patients completed eSPARC electronic questionnaires monthly, combined with telephone consultations. |
| 17 | emotional and social support | website/online platform | Improved quality of life (SF-36 scores) and high patient satisfaction in the remote group compared to conventional care group | Internet-based pain management intervention, including remote pain control, lifestyle advice, and psychological support via smartphones and internet |
| 4 | monitoring and tracking | mobile application | Positive intentions from HCPs and patients to use the application; improved confidence in recovery and reduced insecurity about symptoms and rehabilitation progress. | A telehealthcare application consisting of symptom and physical activity monitoring using sensors, and a web-based physical exercise program. |
| 9 | monitoring and tracking | telemedicine | Improved identification of patient needs and patient report improved quality of life at follow up assessment, emotional support, 75% reduction in high concerns | Patients completed eSPARC electronic questionnaires monthly, combined with telephone consultations. |
| 14 | monitoring and tracking | mobile application | Significant improvement in survival with median survival of 22.4 months in the experimental group versus 16.7 months in the control group | self-reported symptom assessment via web application, alerts to oncologist for early intervention based on predefined criteria |
| 21 | monitoring and tracking | mobile application | Improved overall survival and better performance status at relapse in the intervention group, reduced imaging needs | Web-mediated follow-up using an e-follow up application for weekly self-scored symptoms, triggering alerts for oncologists based on abnormal results |
| 1 | promotion of healthy behavior | telemedicine | intervention participants had statistically significant and clinically meaningful improved HRQL (SGRQ total, symptom, and impact scores) (standardized effect size: -1.03 to -1.30). | 12-week intervention, delivered via telemedicine, consisted of exercise training (IMT + walking), education, and behavior change support. |
| 4 | promotion of healthy behavior | mobile application | Positive intentions from HCPs and patients to use the application; improved confidence in recovery and reduced insecurity about symptoms and rehabilitation progress. | A telehealthcare application consisting of symptom and physical activity monitoring using sensors, and a web-based physical exercise program. |
| 8 | promotion of healthy behavior | telemedicine | High adherence to the program; improved functional capacity, improving trajectory for patient distress | Telehealth sessions with PT/OT to improve physical activity before and after surgery. |

# challenge

``` r
data_extract|>
  separate_rows(challenge_theme, sep = ",")|>
  filter(challenge_theme != "NA")|>
  select(id, challenge_theme, challenges_of_technology_use)|>
  arrange(challenge_theme)|>
  knitr::kable()
```

| id | challenge_theme | challenges_of_technology_use |
|---:|:---|:---|
| 15 | compliance | frequency of questionnaire in relation to adherence; some patients needed help navigating the tool |
| 6 | compliance | Unclear low response rate in some patients, a small proportion of patient lack digital skill to fill out the questionnaire |
| 7 | compliance | Physicians reviewed HRQoL data in only 73.1% of visits; organizational constrant: lack of human and financial resource, institutional support, unfamiliarity with technology |
| 11 | compliance | Internet cost (in the study reimbursed), availability of digital device, motivation for frequent use |
| 12 | compliance | Reduced caregiver participation in telehealth compared to in-person visits |
| 18 | compliance | Internet access, technological literacy, and low self-motivation among some patients |
| 26 | compliance | Some patients had limited cooperation |
| 28 | compliance | Data gaps from device-sync issues, battery limitations, patients’ data privacy and security and patient compliance |
| 5 | confusion and tmi | Possible confusion, overwhelming information, lack of face-to-face consultation. |
| 8 | confusion and tmi | Some participants dropped out due to being overwhelmed or lack of interest; post-discharge activity levels dropped significantly. |
| 20 | device malfunction and sync issue | Some data gaps due to sync issues, device malfunctions |
| 28 | device malfunction and sync issue | Data gaps from device-sync issues, battery limitations, patients’ data privacy and security and patient compliance |
| 17 | favor of in person care | compliance with remote instructions, patients favorable in person care versus internet based care |
| 5 | favor of in-person care | Possible confusion, overwhelming information, lack of face-to-face consultation. |
| 1 | internet and tech access | Unreliable phone signal, unacceptable space for completing the excercise, technical challenges for video telemedicne sessions |
| 11 | internet and tech access | Internet cost (in the study reimbursed), availability of digital device, motivation for frequent use |
| 18 | internet and tech access | Internet access, technological literacy, and low self-motivation among some patients |
| 21 | internet and tech access | Requires internet access and basic familiarity with email for patients or relatives |
| 22 | internet and tech access | Caregivers may rate symptoms lower to avoid triggering alerts; technology access and comfort |
| 23 | internet and tech access | Requires access to a telephone and ability to participate in calls |
| 25 | internet and tech access | technology accessibility, the use of telemedicine was forced by the pandemic setting |
| 29 | internet and tech access | access to technology for teleconferencing and internet required for participation |
| 24 | lack of content | Complicated login process, limited accessibility for non-Windows OS users, low content update frequency |
| 1 | low digital literacy | Unreliable phone signal, unacceptable space for completing the excercise, technical challenges for video telemedicne sessions |
| 4 | low digital literacy | complex visual representation of symptom data, difficulty navigating the exercise module, and the need for integration with electronic patient records |
| 6 | low digital literacy | Unclear low response rate in some patients, a small proportion of patient lack digital skill to fill out the questionnaire |
| 7 | low digital literacy | Physicians reviewed HRQoL data in only 73.1% of visits; organizational constrant: lack of human and financial resource, institutional support, unfamiliarity with technology |
| 9 | low digital literacy | not all patient comfortable with thechnology, |
| 13 | low digital literacy | 3 participants required assistance when the device was unresponsive, perceived barrier by patients as lack of competence and lack of reliability |
| 14 | low digital literacy | patients without digital experience may find difficult to participate |
| 15 | low digital literacy | frequency of questionnaire in relation to adherence; some patients needed help navigating the tool |
| 16 | low digital literacy | patients with low literacy may experience more difficulty in understanding the contents |
| 18 | low digital literacy | Internet access, technological literacy, and low self-motivation among some patients |
| 19 | low digital literacy | Some required initial setup support |
| 21 | low digital literacy | Requires internet access and basic familiarity with email for patients or relatives |
| 24 | low digital literacy | Complicated login process, limited accessibility for non-Windows OS users, low content update frequency |
| 28 | privacy | Data gaps from device-sync issues, battery limitations, patients’ data privacy and security and patient compliance |
| 2 | reliability regarding reporting | Patients score for the EORTC QLQ-C30 questionnaire/reported symptoms and perceived QoL didn’t align; patient found it difficult to choose the most suitable description of the performance status |
| 4 | reliability regarding reporting | complex visual representation of symptom data, difficulty navigating the exercise module, and the need for integration with electronic patient records |
| 13 | reliability regarding reporting | 3 participants required assistance when the device was unresponsive, perceived barrier by patients as lack of competence and lack of reliability |
| 22 | reliability regarding reporting | Caregivers may rate symptoms lower to avoid triggering alerts; technology access and comfort |

``` r
data_extract|>
  separate_rows(challenge_theme, sep = ",")|>
  filter(challenge_theme != "NA")|>
  select(id, challenge_theme, challenges_of_technology_use)|>
  group_by(challenge_theme)|>
  summarise(count=n())|>
  knitr::kable()
```

| challenge_theme                   | count |
|:----------------------------------|------:|
| compliance                        |     1 |
| compliance                        |     7 |
| confusion and tmi                 |     2 |
| device malfunction and sync issue |     2 |
| favor of in person care           |     1 |
| favor of in-person care           |     1 |
| internet and tech access          |     8 |
| lack of content                   |     1 |
| low digital literacy              |    13 |
| privacy                           |     1 |
| reliability regarding reporting   |     4 |
