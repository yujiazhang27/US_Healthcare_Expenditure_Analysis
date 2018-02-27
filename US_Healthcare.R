## Import
library(ggplot2)
library(reshape2)
library(scales)
library(plyr)


## Function
calculate_growth_rate <- function(df){
  for (i in 2:ncol(df)){  # calculate the growth rate
    growthRate <- c(0)
    tempList = as.vector(df[i])
    for (j in 1:nrow(tempList)-1){
      rate = (tempList[j+1,]-tempList[j,])/tempList[j,]   # rate = (later-former)/former
      growthRate <- c(growthRate, rate)
    }
    growthRate <- as.data.frame(growthRate)
    df[i] <- growthRate
  }
  return(df)
}

calculate_pct_for_each_category <- function(tempDf){
  tempDf <- dcast(tempDf, Region_Name~Item)   # reshape it like a table
  
  rownames(tempDf) <- tempDf[,1]    # get the row names
  tempDf <- tempDf[,-1]     # set the row names
  
  pctDf <- cbind(prop.table(as.matrix(tempDf), margin = 1))
  return(pctDf)
}

convert_year_to_integer <- function(df){
  tempYear <- df['variable']    # extract the "year" column
  tempYear <- df[,2]
  charYear <- as.character(tempYear)    # a list of years as charactors
  intYear <- c()    # initialize an empty list for year as integer
  for (e in charYear){
    e <- substring(e, 2, 5)   # remove the 'Y'
    intYear <- c(intYear, e)
  }
  intYear <- as.integer(intYear)    # convert year to integer
  return(intYear)
}


## Read row data into dataframe
rawExpenditure <- read.csv('/Users/pollyzhang/Desktop/US_PER_CAPITA14.csv')

rawExpenditure2 <- rawExpenditure[rawExpenditure[, "Group"] == "Region",]

expenditure <- subset(rawExpenditure2, select=-c(Code, Group, Region_Number, State_Name, Average_Annual_Percent_Growth))


## Region State
region_state <- subset(rawExpenditure, select=c(Region_Name, State_Name))
region_state <- region_state[region_state[, "State_Name"] != "",]

## Sum by region 
sum_by_region <- aggregate(. ~ Region_Name, expenditure, sum)   # sum by region
sum_by_region <- subset(sum_by_region, select=-c(Item))  # remove unused columns
sum_by_region <- melt(sum_by_region, id=c("Region_Name"))   # reshape the dataframe

year <- convert_year_to_integer(sum_by_region)

sum_by_region$intTime = year
sum_by_region <- subset(sum_by_region, select=c(Region_Name, intTime, value))

ggplot(sum_by_region, aes(intTime,value)) + 
  geom_line(aes(colour = Region_Name)) +
  scale_x_continuous(breaks = seq(1991, 2014, by=1), limits=c(1991,2014)) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  scale_y_continuous(labels = scales::dollar) +
  xlab("Time (Year)") +
  ylab("Total Expenditure (in US Dollar)") +
  ggtitle("Expenditures for Each Region Summed by Category over Year") +
  scale_fill_discrete(name = "Region")


## Growth rate by region
region_by_time <- dcast(sum_by_region, intTime~Region_Name)  # reshape the dataframe

regionGrowthRate <- region_by_time   # create a new dataframe with the same shape

regionGrowthRate <- calculate_growth_rate(regionGrowthRate)

regionGrowthRate<- melt(regionGrowthRate[2:nrow(regionGrowthRate),], id=c("intTime"))   # reshape the dataframe

ggplot(regionGrowthRate, aes(intTime,value)) +         # plot
  geom_line(aes(colour = variable)) +
  geom_point(aes(colour = variable)) +
  facet_grid(variable ~ .) +
  scale_x_continuous(breaks = seq(1991, 2014, by=1), limits=c(1991,2014)) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  scale_y_continuous(labels = scales::percent) + 
  xlab("Time (Year)") +
  ylab("Annual Growth Rate (%)") +
  ggtitle("Annual Growth Rate over Year") +
  scale_fill_discrete(name = "Region")


## Percentage of each category for each region
# 1991
temp <- subset(expenditure, select=c(Item, Region_Name, Y1991))
pct1991 <- calculate_pct_for_each_category(temp)
# 2000
temp <- subset(expenditure, select=c(Item, Region_Name, Y2000))
pct2000 <- calculate_pct_for_each_category(temp)
# 2009
temp <- subset(expenditure, select=c(Item, Region_Name, Y2009))
pct2009 <- calculate_pct_for_each_category(temp)
# 2014
temp <- subset(expenditure, select=c(Item, Region_Name, Y2014))
pct2014 <- calculate_pct_for_each_category(temp)
# combine three dataframes 
pct1991 <- melt(pct1991, id=c(rownames(pct1991)))
colnames(pct1991)[3] <- 'Y1991'
pct2000 <- melt(pct2000, id=c(rownames(pct2000)))
colnames(pct2000)[3] <- 'Y2000'
pct2009 <- melt(pct2009, id=c(rownames(pct2009)))
colnames(pct2009)[3] <- 'Y2009'
pct2014 <- melt(pct2014, id=c(rownames(pct2014)))
colnames(pct2014)[3] <- 'Y2014'

pctExpenditure <- join_all(list(pct1991,pct2000,pct2009,pct2014), by=c('Var1','Var2'), type = 'full')
colnames(pctExpenditure)[1] <- 'Region_Name'
colnames(pctExpenditure)[2] <- 'Item'

pctExpenditureTemp <- subset(pctExpenditure, select=-c(Y2000,Y2009))

pctExpenditureNew <- melt(pctExpenditureTemp, id=c("Item","Region_Name"))   # reshape the dataframe
#pctExpenditure['Year'] <- convert_year_to_integer(pctExpenditure)
colnames(pctExpenditureNew)[3] <- 'Year'
colnames(pctExpenditureNew)[4] <- 'Percentage'

ggplot(pctExpenditureNew, aes(x=Region_Name, y=Percentage)) + 
  geom_bar(aes(fill = Item), stat = "identity") +
  facet_grid(Year ~ .) +
  scale_y_continuous(labels = scales::percent) +
  coord_flip() +
  scale_fill_brewer(palette="Paired") + 
  xlab("Region") +
  ylab("Percentage") +
  ggtitle("Percentage of Each Category by Region over Year")

## Pertentage change
difference <- pctExpenditure
difference[,3] <- 0
difference[,4] <- pctExpenditure[,4]-pctExpenditure[,3] # 2000-1991
difference[,5] <- pctExpenditure[,5]-pctExpenditure[,4] # 2009-2000
difference[,6] <- pctExpenditure[,6]-pctExpenditure[,5] # 2014-2009
difference <- subset(difference, select=-c(Y1991))

differenceNew <- melt(difference, id=c("Item","Region_Name"))   # reshape the dataframe

positiveDat <- subset(differenceNew, value>= 0)
negativeDat <- subset(differenceNew,value < 0)

ggplot() + 
  geom_bar(data=positiveDat, aes(x=variable, y=value),stat = "identity") +
  geom_bar(data=negativeDat, aes(x=variable, y=value),stat = "identity") +
  facet_grid(Region_Name ~ Item) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  scale_y_continuous(labels = scales::percent) +
  xlab("Year") +
  ylab("Change in Percentage") +
  ggtitle("Change in Percentage over Year for each Cagegory and Region")














### Plots are not included in the research

## Sum by item
sum_by_item <- aggregate(. ~ Item, expenditure, sum)    # sum by item
sum_by_item <- subset(sum_by_item, select=-c(Region_Name, Average_Annual_Percent_Growth))   # remove unused colums
sum_by_item <- melt(sum_by_item, id=c("Item"))   # reshape the dataframe

year <- convert_year_to_integer(sum_by_item)  # convert year to integer

sum_by_item$intTime = year     # add the integer year as a new column
sum_by_item <- subset(sum_by_item, select=c(Item, intTime, value))    # remove unused columns

ggplot(sum_by_item, aes(intTime,value)) + 
  geom_line(aes(colour = Item)) +
  scale_x_continuous(breaks = seq(1991, 2009, by=1), limits=c(1991,2009)) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  scale_y_continuous(labels = scales::dollar) +
  xlab("Time (Year)") +
  ylab("Total Expenditure (in US Dollar)") +
  ggtitle("Expenditures for Each Category Summed by Region over Year") +
  scale_fill_discrete(name = "Category")
