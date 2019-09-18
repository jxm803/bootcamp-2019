
# Importing California energy data
generation <- read.csv("data/ca_energy_generation.csv", sep = ",", stringsAsFactors=FALSE)
imports <- read.csv("data/ca_energy_imports.csv", sep = ",", stringsAsFactors=FALSE)

# Explore data
str(generation)
str(imports)

# Lubridate
## to convert character variables into datetime format using the as_date() function. another way to accomplish this is to import via read_csv()
library(lubridate)
generation$datetime <- as_datetime(generation$datetime)
imports$datetime <- as_datetime(imports$datetime)
class(imports$datetime)

# Reshape 2
## melt function makes data long
library(reshape2)
long_gen <- melt(generation, id.vars="datetime", variable.name="source", value.name="usage")
## order by date time
head(long_gen[order(long_gen$datetime),])

# Merging 
## join generation and imports dataframes  
merged_energy <- merge(generation, imports, by = "datetime")

## melt the merged set 
long_merged_energy <- melt(merged_energy, id.vars="datetime", variable.name="source", value.name="usage")

# Dplyr to chain together a few, simple functions
library(tidyverse)
## select to subset variables, pulls only the chosen columns or columns containing the selected strings
names(select(merged_energy, biogas, biomass, geothermal, solar))
names(select(merged_energy, contains("hydro"), starts_with("bio")))
## filter to subset observatoins based on one or many conditions
nrow(filter(merged_energy, imports > 7000))
nrow(filter(merged_energy, imports > 7000, natural_gas < 7000))
head(filter(merged_energy, imports > 7000, natural_gas < 7000))
## mutate to add new variables
head(mutate(long_merged_energy, log_usage = log(usage)))
## summarize to reduce multiple observations to a single value (ie find the mean)
### calculate total energy consumption
summarize(long_merged_energy, total=sum(usage, na.rm=T))
sum(long_merged_energy$usage)
### calculate mean energy consumption along with total energy consumption 
summarize(long_merged_energy, mean=mean(usage, na.rm=T), total=sum(usage, na.rm=T))


# The Pipe %>% to chain together functions 
### calculate the mean log usage of geothermal energy 
long_merged_energy %>%
    filter(source == "geothermal") %>%
    select(-datetime) %>%
    mutate(log_usage = log(usage)) %>%
    summarize(mean_log_usage = mean(log_usage, na.rm=T))

## while piping the piped dataframe is not changed 
merged_energy %>%
    select(-datetime) %>%
    mutate(total_usage = rowSums(., na.rm=T)) %>%
    summarize(total_usage=sum(total_usage, na.rm=T))

### calculate mean usage for total hydro
merged_energy %>%
    select(contains ("hydro")) %>% 
    mutate(total_hydro = rowSums(., na.rm=T)) %>% 
    summarize(mean_usage = mean(total_hydro, na.rm=T))

# Group_by to perform operations by groups of observations 
long_merged_energy %>% 
    group_by(source) %>% 
    summarize(sum_usage = sum(usage, na.rm=T), mean_usage = mean(usage, na.rm=T))

### calculate mean usage for hydro and bio sources
merged_energy %>% 
    select(datetime, contains("hydro"), contains("bio")) %>% 
    melt(id.vars="datetime", variable.name="source", value.name="usage") %>% 
    group_by(source) %>% 
    summarize(mean_usage=mean(usage, na.rm=T))

long_merged_energy %>% 
    group_by(source) %>% 
    summarize(mean_usage = mean(usage, na.rm=T)) %>% 
    filter(source %in% c("large_hydro", "small_hydro", "biogas", "biomass"))

long_merged_energy %>% 
    group_by(source) %>% 
    summarize(mean_usage = mean(usage, na.rm=T)) %>% 
    filter(source == "small_hydro" | source == "large_hydro" | source =="biogas" | source == "biomass") 

# Data.table for a compact, syntax-based, enhanced version of data.frames
library(data.table)
## data tables are data frames == dt[i, j, by] where i is row filter, j is column operations, by is group by
generation_df <- read.csv("data/ca_energy_generation.csv", stringsAsFactors = F)
generation_dt <- fread("data/ca_energy_generation.csv")
class(generation_df)
class(generation_dt)
### calculate the number of rows
generation_dt[,.N]
generation_dt[,.N, by=biogas]
## filter by row 
generation_dt[wind>4400 & mday(datetime)==7]
generation_dt[natural_gas <= 5000 & large_hydro > 2000]
generation_dt[coal >10 & solar > median(solar)]
## column operations where the first adds a new column in place and the second exports the new column by returning data used to do something else 
generation_dt[,newcol := 3*wind + solar*biogas/2]
generation_dt[,.(newcol = 3*wind + solar*biogas/2)]
generation_dt[,totalhydro := small_hydro + large_hydro]
generation_dt[,.(biogas=mean(biogas), nuclear=mean(nuclear))]
generation_dt[solar==0, .(datetime, total_thermal=natural_gas + coal)]
## group by
generation_dt[hour(datetime) > 19, .(mean_nuc=mean(nuclear), mean_wind=mean(wind), by=mday(datetime))]
### find median solar generation by hour
generation_dt[,.(solar=median(solar)), by=hour(datetime)]
### find the maximum natural gas generation by day for hours when the solar generation is greater than zero
generation_dt[solar>0,max(natural_gas), by=hour(datetime)]

## dplyr to data.table
long_merged_energy %>% 
    mutate(day=as_date(datetime), log_output=log(usage)) %>% 
    group_by(day) %>% 
    mutate(total_daily_output=sum(usage, na.rm=T)) %>% 
    ungroup() %>% 
    mutate(per_output = usage/total_daily_output)

