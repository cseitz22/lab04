Lab 04 - La Quinta is Spanish for next to Denny’s, Pt. 1
================
Cat Seitz
Jan. 24, 2023

### Load packages and data

``` r
library(tidyverse) 
library(dsbox) 
```

``` r
states <- read_csv("data/states.csv")
```

### Exercise 1

``` r
nrow(dennys)
```

    ## [1] 1643

``` r
ncol(dennys)
```

    ## [1] 6

There are 1643 rows and 6 columns. Each row is a Dennys address, and the
variables include the address, city, state, zip code, longitude, and
latitude.

### Exercise 2

``` r
nrow(laquinta)
```

    ## [1] 909

``` r
ncol(laquinta)
```

    ## [1] 6

There are 909 rows and 6 columns. This dataset has the same variables as
the Dennys dataset, but each row is a La Quinta address.

### Exercise 3

La Quinta are located in Canada, Mexico, China, New Zealand, Turkey,
United Arab Emirates, Chile, and Colombia. Dennys are only located in
the US, in every state except Delaware.

### Exercise 4

Since country is not a variable, we could potentially use the state or
zip code variables to distinguish between within or outside the US. If
the dataset used specific state codes for outside countries or if zip
codes for other countries follow a different pattern than the US, then
we can use this information to create a new column titled something like
“in_usa” and have responses be “yes” or “no.” Or, we can do what is
suggested in the lab (which seems easier) and filter items that have a
state not in the state dataframe.

### Exercise 5

``` r
dennys %>%
  filter(!(state %in% states$abbreviation))
```

    ## # A tibble: 0 × 6
    ## # … with 6 variables: address <chr>, city <chr>, state <chr>, zip <chr>,
    ## #   longitude <dbl>, latitude <dbl>

There are no Denny’s location outside the U.S., which confirms what we
found on the website in exercise 3.

### Exercise 6

``` r
dennys <- dennys %>%
  mutate(country = "United States")
```

### Exercise 7

``` r
laquinta %>%
  filter(!(state %in% states$abbreviation))
```

    ## # A tibble: 14 × 6
    ##    address                                     city  state zip   longi…¹ latit…²
    ##    <chr>                                       <chr> <chr> <chr>   <dbl>   <dbl>
    ##  1 Carretera Panamericana Sur KM 12            "\nA… AG    20345  -102.    21.8 
    ##  2 Av. Tulum Mza. 14 S.M. 4 Lote 2             "\nC… QR    77500   -86.8   21.2 
    ##  3 Ejercito Nacional 8211                      "Col… CH    32528  -106.    31.7 
    ##  4 Blvd. Aeropuerto 4001                       "Par… NL    66600  -100.    25.8 
    ##  5 Carrera 38 # 26-13 Avenida las Palmas con … "\nM… ANT   0500…   -75.6    6.22
    ##  6 AV. PINO SUAREZ No. 1001                    "Col… NL    64000  -100.    25.7 
    ##  7 Av. Fidel Velazquez #3000 Col. Central      "\nM… NL    64190  -100.    25.7 
    ##  8 63 King Street East                         "\nO… ON    L1H1…   -78.9   43.9 
    ##  9 Calle Las Torres-1 Colonia Reforma          "\nP… VE    93210   -97.4   20.6 
    ## 10 Blvd. Audi N. 3 Ciudad Modelo               "\nS… PU    75010   -97.8   19.2 
    ## 11 Ave. Zeta del Cochero No 407                "Col… PU    72810   -98.2   19.0 
    ## 12 Av. Benito Juarez 1230 B (Carretera 57) Co… "\nS… SL    78399  -101.    22.1 
    ## 13 Blvd. Fuerza Armadas                        "con… FM    11101   -87.2   14.1 
    ## 14 8640 Alexandra Rd                           "\nR… BC    V6X1…  -123.    49.2 
    ## # … with abbreviated variable names ¹​longitude, ²​latitude

Create a tibble of the location of La Quintas outside the US. Looks like
there are only 14 locations.

### Exercise 8

``` r
laquinta <- laquinta %>%
  mutate(country = case_when(
    state %in% states$abbreviation     ~ "United States",
    state %in% c("ON", "BC") ~ "Canada",
    state == "ANT"           ~ "Colombia",
    state %in% c("QR", "AG", "CH", "NL", "VE", "PU", "SL") ~ "Mexico",
    state ==  "FM" ~ "Honduras"
  ))
```

Got the code to work, but the locations we have in this dataset do not
line up with what La Quinta has on their website. This could be because
the dataset has not been updated since new locations were introduced.

### Exercise 9

``` r
laquinta <- laquinta %>%
  filter(country =="United States")
```

``` r
dennys %>%
  count(state) %>%
  inner_join(states, by = c("state" = "abbreviation"))
```

    ## # A tibble: 51 × 4
    ##    state     n name                     area
    ##    <chr> <int> <chr>                   <dbl>
    ##  1 AK        3 Alaska               665384. 
    ##  2 AL        7 Alabama               52420. 
    ##  3 AR        9 Arkansas              53179. 
    ##  4 AZ       83 Arizona              113990. 
    ##  5 CA      403 California           163695. 
    ##  6 CO       29 Colorado             104094. 
    ##  7 CT       12 Connecticut            5543. 
    ##  8 DC        2 District of Columbia     68.3
    ##  9 DE        1 Delaware               2489. 
    ## 10 FL      140 Florida               65758. 
    ## # … with 41 more rows

``` r
laquinta %>%
  count(state) %>%
  inner_join(states, by = c("state" = "abbreviation"))
```

    ## # A tibble: 48 × 4
    ##    state     n name           area
    ##    <chr> <int> <chr>         <dbl>
    ##  1 AK        2 Alaska      665384.
    ##  2 AL       16 Alabama      52420.
    ##  3 AR       13 Arkansas     53179.
    ##  4 AZ       18 Arizona     113990.
    ##  5 CA       56 California  163695.
    ##  6 CO       27 Colorado    104094.
    ##  7 CT        6 Connecticut   5543.
    ##  8 FL       74 Florida      65758.
    ##  9 GA       41 Georgia      59425.
    ## 10 IA        4 Iowa         56273.
    ## # … with 38 more rows

I’m somewhat surprised there are more Dennys in California than Texas,
but more La Quintas in Texas than California. I also expected the
numbers to line up better due to the theory that these two
establishments are grouped together.

### Exercise 10

``` r
dennys <- dennys %>%
  mutate(establishment = "Denny's")
laquinta <- laquinta %>%
  mutate(establishment = "La Quinta")

dn_lq <- bind_rows(dennys, laquinta)
```

``` r
ggplot(dn_lq, mapping = aes(x = longitude,
                            y = latitude,
                            color = establishment)) +
  geom_point()
```

![](lab-04_files/figure-gfm/plot%20locations-1.png)<!-- -->

### Exercise 11

``` r
nc <- dn_lq %>%
  filter(state =="NC")

ggplot(nc, mapping = aes(x = longitude,
                            y = latitude,
                            color = establishment)) +
  geom_point(alpha=.5)+
  scale_color_manual(values=c("#69b3a2", "purple"))+
  theme_light()+
  labs(title="Denny's and La Quinta Locations in North Carolina", color ="Establishment")+
  xlab("Longitude")+
  ylab("Latitude")
```

![](lab-04_files/figure-gfm/filter%20for%20NC%20data%20only-1.png)<!-- -->

The joke does not seem to hold true in North Carolina. It seem that only
1 location is overlapping and 1 more where a Denny’s and La Quinta are
right next to each other.

### Exercise 12

``` r
tx <- dn_lq %>%
  filter(state =="TX")

ggplot(tx, mapping = aes(x = longitude,
                            y = latitude,
                            color = establishment)) +
  geom_point(alpha=.3)+
  scale_color_manual(values=c("#69b3a2", "purple"))+
  theme_light()+
  labs(title="Denny's and La Quinta Locations in Texas", color ="Establishment")+
  xlab("Longitude")+
  ylab("Latitude")
```

![](lab-04_files/figure-gfm/filter%20for%20Texas%20data%20only-1.png)<!-- -->

The joke seems to hold more true for establishments in Texas compared to
North Carolina. Specifically, there are very few stand alone Denny’s in
Texas and a lot of La Quintas that overall with them.
