# Requesting the libraries 'gapminder', 'dplyr' and ''ggplot2':

library('gapminder')
library('dplyr')
library('ggplot2')

# Storing the database 'gapminder':

df <- gapminder

attach(df)

# CHAPTER 1 - DATABASE WRANGLING

##Filtering the database by:

### Rows that contains the years 1957, 1987, 2007:

df_f1 <- df %>% filter(year == 1957)
df_f1

df_f2 <- df %>% filter(year == 1987)
df_f2

df_f3 <- df %>% filter(year == 2007)
df_f3

### Rows that contains the year 2002 and the country 'Uruguay':

df_f4 <- df %>% filter(year == 2002, country == 'Uruguay')
df_f4

## Arranging the database by life expectancy:

### Descending order:

df_a1 <- df %>% arrange(desc(lifeExp))
df_a1 

### Ascending order:

df_a2 <- df %>% arrange(lifeExp)
df_a2

## Filter for the year 1957 and arrange in descending order of population:

t1 <- df %>% filter(year == 1967) %>% arrange(desc(pop))
t1

## Transforming column values with mutate function:

### Adding a column with the variable 'Life Expectancy' in months:

df_m1 <- df %>% mutate(lifeExpMonths = lifeExp * 12)
df_m1

## Find the country with the highest life expectancy (in months) in 2007:

t2 <- df %>% filter(year == 2007) %>% mutate(lifeExpMonths = lifeExp * 12) %>%
        arrange(desc(lifeExpMonths))
t2

# CHAPTER 2 - DATA VISUALIZATION

## Filter the dataset for the year of 1952:

dff1 <- df %>% filter(year == 1952)
dff1

## Plot a scatter plot:

### Population x GDP per capita:

p1 <- ggplot(dff1, aes(x = pop, y = gdpPercap)) + geom_point()
p1

### Population x Life Expectancy:

p2 <- ggplot(dff1, aes(x = pop, y = lifeExp)) + geom_point()
p2


### Log10(population) x Life Expectancy:

p3 <- ggplot(dff1, aes(x = pop, y = lifeExp)) + geom_point() +
       scale_x_log10()
p3

### Log10(population) x Log10(GDP per capita):

p4 <- ggplot(dff1, aes(x = pop, y = gdpPercap)) + geom_point() +
        scale_x_log10() +scale_y_log10()
p4

### Population x Life Expectancy:
#### Add colors to categorize continents:

p5 <- ggplot(dff1, aes(x = pop, y = lifeExp, color = continent)) + 
        geom_point() + scale_x_log10() 
p5

### Population x Life Expectancy:
#### Add colors to categorize continents:
#### Highlight countries with greater GDP per capita (bigger points):

p6 <- ggplot(dff1, aes(x = pop, y = lifeExp, color = continent, 
        size = gdpPercap)) + geom_point() + 
          scale_x_log10() 
p6

### Population x Life Expectancy by continent:

p7 <- ggplot(dff1, aes(x = pop, y = lifeExp)) + geom_point() + 
        scale_x_log10() + facet_wrap(~ continent)
p7

### log10(GDP per capita) x Life Expectancy:
#### Add colors to categorize continents:
#### Highlight countries with greater population (bigger points):
#### Make scatter plotter from 1952 to 2007:

p8 <- ggplot(df, aes(x = gdpPercap, y = lifeExp, color = continent,
        size = pop)) + geom_point() + scale_x_log10() + facet_wrap(~ year)
p8


# CHAPTER 3 - GROUPING AND SUMMARIZING

## summarize the life expectancy to the median:

s1 <- df %>% summarise(median(lifeExp))

## summarize the life expectancy to the median in 1957:

s2 <- df %>% filter(year == 1957) %>% summarise(medianLifeExp = median(lifeExp))

## summarize the life expectancy to the median in 1957:
## summarize the GDP per capita to the maximum value in 1957:

s3 <- df %>% filter(year == 1957) %>% 
          summarise(medianLifeExp = median(lifeExp), 
                      maxGdpPercap = max(gdpPercap)) 

s3

## summarize the life expectancy by year:
## summarize the GDP per capita to the maximum value by year:

s4 <- df %>% group_by(year) %>%
        summarise(medianLifeExp = median(lifeExp),
            maxGdpPercap = max(gdpPercap))
              
## summarize the life expectancy in 1957 by continent:
## summarize the GDP per capita to the maximum value in 1957 by continent:

s5 <- df %>% filter(year == 1957) %>% group_by(continent) %>%
          summarise(medianLifeExp = median(lifeExp),
               maxGdpPercap = max(gdpPercap))

## summarize the life expectancy by year and by continent:
## summarize the GDP per capita to the maximum value by year and by continent:


s6 <- df %>% group_by(continent, year) %>%
          summarise(medianLifeExp = median(lifeExp),
              maxGdpPercap = max(gdpPercap))


## make a scatter plot: variable grouped by year  x life expectancy median:

p9 <- ggplot(s4, aes(x = year, y= medianLifeExp)) + geom_point() +
          expand_limits(y = 0)

p9

## make a scatter plot: variable grouped by year  x GDP per capita median: 

s7 <- df %>% group_by(continent, year) %>%
          summarise(medianGdpPercap = median(gdpPercap))

p10 <- ggplot(s7, aes(x = year, y = medianGdpPercap, color = continent)) +
          geom_point() + expand_limits(y = 0 )

p10

## Make a scatter plot: GDP per capita median x life expectancy median:
###Filter the dataset of the year 2007:
### categorize the continents by colors:

s8 <- df %>% filter(year == 2007) %>% group_by(continent) %>%
        summarise(medianGdpPercap = median(gdpPercap), 
          medianLifeExp = median(lifeExp))

p11 <- ggplot(s8, aes(x = medianGdpPercap, y = medianLifeExp,
              color = continent)) + geom_point() + expand_limits(y = 0)

p11

# CHAPTER 4 - TYPES OF VISUALIZATIONS

## Make a line plot: variable grouped by year x median GDP per capita:

s9 <- df %>% group_by(year) %>%
                    summarise(medianGdpPercap = median(gdpPercap))

p12 <- ggplot(s9, aes(x = year, y = medianGdpPercap)) + geom_line() + 
          expand_limits(y = 0)

p12

## Make a line plot: variable grouped by year x median GDP per capita:
### categorize continents by colors:

s10 <- df %>% group_by(year, continent) %>%
          summarise(medianGdpPercap = median(gdpPercap))

p13 <- ggplot(s10, aes(x = year, y = medianGdpPercap, color = continent)) +
          geom_line() + expand_limits(y = 0)
p13

## Make a bar plot for median GDP Per capita in 1952 by continent:

s11 <- df %>% filter(year == 1952) %>%
            group_by(continent) %>%
              summarise(medianGdpPercap = median(gdpPercap))

p14 <- ggplot(s11, aes(x = continent, y = medianGdpPercap)) + geom_col()
p14 


## Make a bar plot for median GDP Per capita, in 1952, by country in Oceania:

s12 <- df %>% filter(continent == 'Oceania', year == 1952) 
              

p15 <- ggplot(s12, aes(x = country, y = gdpPercap)) + geom_col()
p15 

## Make a histogram of population (by million) by country in 1952:

s13 <- df %>% filter(year == 1952) %>%
            mutate(pop_by_mil = pop / 1000000)

h1 <- ggplot(s13, aes(x = pop_by_mil)) + geom_histogram(bins = 50)
h1

## Make a histogram of log10(population) by country in 1952:

s15 <- df %>% filter(year == 1952)


h2 <- ggplot(s15, aes(x = pop)) + geom_histogram() +
        scale_x_log10()
h2


## Make a boxplot: log10(GDP Per Capita) by country in 1952

p16 <- ggplot(s15, aes(x = continent, y = gdpPercap)) + geom_boxplot() +
            scale_y_log10() + 
                ggtitle('Comparing GDP per capita across continents.') +
                    theme(plot.title = element_text(hjust = 0.5))
  
  
p16


