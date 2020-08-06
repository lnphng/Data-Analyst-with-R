
## Introduction to the Tidyverse_Datacamp

Description:

- Focused on a powerful set of tools, Tidyverse.
- Learn the intertwined processes of data manipulation and visualization using the tools dplyr and ggplot2. 
- Learn to manipulate data by filtering, sorting, and summarizing a real dataset of historical country data in order to answer exploratory questions. 
- Learn to turn this processed data into informative line plots, bar plots, histograms, and more with the ggplot2 package.

### 1. Data wrangling


```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(gapminder)
library(dplyr)
library(knitr)
```

#### a. Filtering in R
**Dataset gapminder**

```{r echo = FALSE, results='asis'}
kable(gapminder[1:5,])
```

**Filtering 1 year**
```{r echo = FALSE, results='asis'}
kable(head(gapminder %>%
  filter(year == 1957),5))
```

**Filtering for one country and one year**

```{r echo = FALSE, results='asis'}
kable(head(gapminder %>%
  filter(country == "China", year == 2002),5))
```

Sort in ascending order of lifeExp

```{r echo=FALSE, results='asis'}

# Sort in ascending order of lifeExp
kable(head(gapminder %>%
arrange(lifeExp),5))
```

Sort in descending order of lifeExp

```{r echo=FALSE, results='asis'}  
kable(head(gapminder %>%
arrange(desc(lifeExp)),5))
```

Filter for the year 1957, then arrange in descending order of population

```{r echo=FALSE, results='asis'} 
kable(head(gapminder %>%
  filter(year == 1957) %>%
  arrange(desc(pop)),5))
```

Use mutate to create a new column called lifeExpMonths

```{r echo = FALSE, results= 'asis'}
kable(head(gapminder %>%
  mutate(lifeExpMonths = 12*lifeExp),5))
```

# Filter, mutate, and arrange the gapminder dataset

```{r echo = FALSE, results='asis'}
kable(head(gapminder %>%
  filter(year == 2007) %>%
  mutate(lifeExpMonths =12*lifeExp) %>%
  arrange(desc(lifeExpMonths)),5))

```

### 2. Visualizing with ggplot2
```{r}
library(ggplot2)
```

```{r}
gapminder_1952 <- gapminder %>%
  filter(year == 1952)
ggplot(gapminder_1952, aes(x = gdpPercap, y = lifeExp)) +
  geom_point()
```

Scatterplot for population and dgpPercap

```{r}
ggplot(gapminder_1952, aes(x = pop, y = gdpPercap)) +
  geom_point()
```

**Putting the x-axis on a log scale**
Since population is spread over several orders of magnitude, with some countries having a much higher population than others, it's a good idea to put the x-axis on a log scale.

```{r}
ggplot(gapminder_1952, aes(x = gdpPercap, y = lifeExp)) +
geom_point() + scale_x_log10()
```

Scatter plot comparing pop and gdpPercap, with both axes on a log scale

```{r}
ggplot(gapminder_1952, aes(x=pop, y= gdpPercap))+ geom_point()+ scale_x_log10()+scale_y_log10()
```
# Scatter plot comparing pop and lifeExp, with color representing continent

```{r}
ggplot(gapminder_1952, aes(x=pop, y=lifeExp, color = continent)) +geom_point()+ scale_x_log10()
```

the scatter plot so that the size of the points represents each country's GDP per capita

```{r}
ggplot(gapminder_1952, aes(x = pop, y = lifeExp, color = continent,  size = gdpPercap)) +
  geom_point() +
  scale_x_log10()
```

### Faceting

```{r}
# Scatter plot comparing pop and lifeExp, faceted by continent
ggplot(gapminder_1952, aes(x = pop, y = lifeExp)) +
geom_point() +
scale_x_log10() +
facet_wrap(~ continent)

```

Scatter plot comparing gdpPercap and lifeExp, with color representing continent and size representing population, faceted by year

```{r}
ggplot(gapminder, aes(x=gdpPercap, y=lifeExp, color =continent, size=pop))+geom_point()+scale_x_log10()+facet_wrap(~year)
```

### The summarize verb

Summarize to find the mian life expectancy
```{r}

gapminder %>%
summarize(meanLifeExp=median(lifeExp))

```

Filter for 1957 then summarize the median life expectancy

```{r}
gapminder %>%
filter(year == 1957) %>%
summarize(medianLifeExp=median(lifeExp))
```

Filter for 1957 then summarize the median life expectancy and the maximum GDP per capita

```{r}
gapminder %>% 
filter(year==1957) %>%
summarize(medianLifeExp = median(lifeExp), maxGdpPercap=max(gdpPercap))
```

Find median life expectancy and maximum GDP per capita in each year

```{r}
gapminder %>%
  group_by(year) %>%
    summarize(medianLifeExp =median(lifeExp), maxGdpPercap=max(gdpPercap))
```
# Find median life expectancy and maximum GDP per capita in each continent in 1957


```{r}
gapminder %>%
  filter(year==1957) %>%
    group_by(continent)%>%
      summarize(medianLifeExp=median(lifeExp), maxGdpPercap=max(gdpPercap))
```

# Find median life expectancy and maximum GDP per capita in each continent/year combination
```{r}
gapminder %>%
  group_by(continent, year) %>%
    summarize(medianLifeExp=median(lifeExp), maxGdpPercap=max(gdpPercap))
```

# Plot the change in medianGdpPercap in each continent over time
```{r}

by_year_continent <- gapminder %>%
                      group_by(continent, year) %>%
                        summarize(medianGdpPercap= median(gdpPercap))

ggplot(by_year_continent, aes(x=year, y=medianGdpPercap, color=continent)) +geom_point() + expand_limits(y=0)
```

Use a scatter plot to compare the median GDP and median life expectancy

```{r}
by_continent_2007 <- gapminder %>%
                        filter(year==2007)%>%
                        group_by(continent)%>%
                        summarize(medianLifeExp=median(lifeExp),           medianGdpPercap=median(gdpPercap))

ggplot(by_continent_2007, aes(x=medianGdpPercap, y=medianLifeExp, color=continent))+geom_point()+expand_limits(y=0)

```

### Line plot
Create a line plot showing the change in medianGdpPercap over time
```{r}
by_year <- gapminder %>%
            group_by(year)%>%
            summarize(medianGdpPercap= median(gdpPercap))

# 
ggplot(by_year, aes(x=year, y=medianGdpPercap))+geom_line()+ expand_limits(y=0)

```



```{r}
by_year_continent <- gapminder %>%
                      group_by(year, continent) %>%
                      summarize(medianGdpPercap=median(gdpPercap))


# Create a line plot showing the change in medianGdpPercap by continent over time
ggplot(by_year_continent, aes(x=year, y=medianGdpPercap, color =continent)) + geom_line()+expand_limits(y=0)
```

Create a bar plot showing medianGdp by continent

```{r}
# Summarize the median gdpPercap by continent in 1952
by_continent <- gapminder %>%
                  filter(year==1952)%>%
                  group_by(continent)%>%
                    summarize(medianGdpPercap=median(gdpPercap))
                  
# Create a bar plot showing medianGdp by continent
ggplot(by_continent, aes(x=continent, y=medianGdpPercap)) + geom_col()
```

Create a bar plot of gdpPercap by country

```{r}
# Filter for observations in the Oceania continent in 1952
oceania_1952 <- gapminder %>%
                  filter(year ==1952, continent == "Oceania") 

# Create a bar plot of gdpPercap by country
ggplot(oceania_1952, aes(x=country, y=gdpPercap)) +geom_col()
```

```{r}
gapminder_1952 <- gapminder %>%
  filter(year == 1952) %>%
  mutate(pop_by_mil = pop / 1000000)

# Create a histogram of population (pop_by_mil)
ggplot(gapminder_1952, aes(x=pop_by_mil))+ geom_histogram(bins=50)

```
```{r}
gapminder_1952 <- gapminder %>%
  filter(year == 1952)

# Create a histogram of population (pop), with x on a log scale
ggplot(gapminder_1952, aes(x=pop)) + geom_histogram()+scale_x_log10()
```

Create a boxplot comparing gdpPercap among continents

```{r}
gapminder_1952 <- gapminder %>%
  filter(year == 1952)

# Create a boxplot comparing gdpPercap among continents
ggplot(gapminder_1952, aes(x=continent, y=gdpPercap))+geom_boxplot()+scale_y_log10()
```

# Add a title to this graph: "Comparing GDP per capita across continents"
```{r}
ggplot(gapminder_1952, aes(x = continent, y = gdpPercap)) +
  geom_boxplot() +
  scale_y_log10() +
  ggtitle("Comparing GDP per capita across continents")
```