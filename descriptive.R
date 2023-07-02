#descriptive
library(readxl)
census_2011_literacy <- read_excel("census-2011-literacy.xlsx")
df=census_2011_literacy



Population=df$Population
StateName=df$StateName
barplot(Population,names.arg=StateName,las=2,ylim =c(0,209812341),main='Statewise population barplo')







# Assign population and state names to variables

Population <- df$Population
StateName <- df$StateName

# Define colors for each state
colors <- c("red", "blue", "green", "orange", "purple", "pink", "brown", "gray", "black", "yellow")

# Set bottom margin to fit all x-axis labels
par(mar = c(16, 10, 4, 2) + 0.1)
options(scipen = 999)

# Create barplot with colored bars
barplot(Population, 
        names.arg = StateName, 
        las = 2, 
        ylim = c(0, 209812341), 
        col = colors,
        main = 'Statewise population barplot')






Literate=df$Literate
barplot(Literate,names.arg=StateName,las=2,ylim =c(0,209812341),main='Statewise population barplot')

LiteracyRate=(Literate/Population)*100
barplot(LiteracyRate,names.arg=StateName,las=2,ylim =c(0,100),main='Statewise population barplot')


x=(df$MaleLiterate)/(df$Male)
y=(df$FemaleLiterate)/(df$Female)
d1=data.frame(x,y)
d2=x-y







library(readxl)
population_density <- read_excel("population-density.xlsx")
View(population_density)
df2=population_density
total_density=df2$Total
StateName1=df2$State
par(mar = c(14, 5, 4, 2) + 0.1)
barplot(total_density,names.arg=StateName1,las=2)



# Load the readxl library and read in the Excel file
library(readxl)
population_density <- read_excel("population-density.xlsx")

# Extract the Total and State columns from the data frame
total_density <- population_density$Total
StateName1 <- population_density$State

# Define a color palette for the bars
colors <- c("#4B9CD3", "#E47243", "#F4A7B9", "#D97E68", "#B2B2B2")

# Create the bar plot with color
par(mar = c(14, 5, 4, 2) + 0.1)
barplot(total_density, 
        names.arg = StateName1, 
        las = 2, 
        col = colors)

# Add a title to the plot
title(main = "Population Density by State")










library(readxl)
literacy_rate <- read_excel("literacy-rate.xlsx")
literacy_rate

colors <- c("red", "blue", "green", "orange", "purple", "pink", "brown", "gray", "black", "yellow")

# Set bottom margin to fit all x-axis labels
par(mar = c(16, 10, 4, 2) + 0.1)

# Create barplot with colored bars
barplot(literacy_rate$literacyRate, 
        names.arg = literacy_rate$States, 
        las = 2, 
        ylim = c(0, 100), 
        col = colors,)




# Read in data
library(readxl)
literacy_rate <- read_excel("literacy-rate.xlsx")

# Load the grDevices package
library(grDevices)

# Define custom color palette based on range of data values
color_palette <- colorRamp2(0, 100, c("blue", "red"))


# Create barplot with custom color palette
barplot(literacy_rate$literacyRate, 
        names.arg = literacy_rate$States, 
        las = 2, 
        ylim = c(0, 100), 
        col = color_palette(literacy_rate$literacyRate),
        main = 'Statewise population barplot')





# Read in data
library(readxl)
literacy_rate <- read_excel("literacy-rate.xlsx")

# Sort data by literacy rate in descending order
sorted_data <- literacy_rate[order(literacy_rate$literacyRate, decreasing = TRUE),]

# Define custom color palette based on range of data values
color_palette <- colorRampPalette(c("blue", "purple", "red"))(length(sorted_data$literacyRate))

# Create barplot with custom color palette
barplot(sorted_data$literacyRate, 
        names.arg = sorted_data$States, 
        las = 2, 
        ylim = c(0, 100), 
        col = color_palette)


summary(literacy_rate$literacyRate)
boxplot(literacy_rate$literacyRate)








## Testing
library(readxl)
male_female_literacy <- read_excel("male-female-literacy.xlsx")
View(male_female_literacy)

male_lit_ratio=male_female_literacy$Male
female_lit_ratio=male_female_literacy$Female



hist(female_lit_ratio)
hist(male_lit_ratio)
var(male_lit_ratio)
var(female_lit_ratio)
t.test(male_lit_ratio,female_lit_ratio, paired=FALSE)  

## Shaprio wiki

shapiro.test(male_lit_ratio)
shapiro.test(female_lit_ratio)

## t test
t.test(male_lit_ratio,female_lit_ratio)


#anova
# Conduct the ANOVA
myanova <- aov(male_lit_ratio~female_lit_ratio)

# Print the ANOVA table
summary(myanova)




###gpt

library(ggplot2)

# Create a data frame with the two variables
lit_ratio_df <- data.frame(
  gender = rep(c("Male", "Female"), each = length(male_lit_ratio)),
  lit_ratio = c(male_lit_ratio, female_lit_ratio)
)

# Create a box plot
ggplot(lit_ratio_df, aes(x = gender, y = lit_ratio, fill = gender)) +
  geom_boxplot() +
  labs(x = "Gender", y = "Literacy Rate", fill = "Gender")

# Perform an ANOVA
anova(lm(lit_ratio ~ gender, data = lit_ratio_df))






###gpt

var(male_lit_ratio)
var(female_lit_ratio)


## Age group

Age_Group_0_29
Age_Group_30_49
Age_Group_50

#interms of percentages
Age_Group_0_29=data$Age_Group_0_29/data$Population
Age_Group_30_49=data$Age_Group_30_49/data$Population
Age_Group_50=data$Age_Group_50/data$Population

ageGroup=data.frame(Age_Group_0_29,Age_Group_30_49,Age_Group_50)
colnames(ageGroup) <- c("<29", "30-49",">50")
stack(ageGroup[, 1:3])


library(ggplot2)

ggplot(stack(ageGroup[, 1:3]), aes(x = ind, y = values)) +
  geom_boxplot(fill = "#0099f8") +
  labs(
    title = "Age group population",
    x = "Age group",
    y = "Population"
  ) +
  scale_y_continuous(label = function(x) format(x, scientific = FALSE)) +
  theme_classic()


anova <- aov(values ~ ind, data = stack(ageGroup[, 1:3]))
summary(anova)





# Time series
data1=New_Microsoft_Excel_Worksheet
totalLit=data.frame(data1$Year,data1$`Total Literacy Rate`)

plot.ts(data1$`Total Literacy Rate`, main = "Literacy Rate",ylab='Total Literacy Rate')

# library required for forecasting
library(forecast)

# forecasting model using arima model
fit <- auto.arima(data1$`Total Literacy Rate`)

# Next 5 forecasted values
forecast(fit, 5)

# 5 weekly forecasted values
plot(forecast(fit, 5),  col.main ="darkgreen")




# Create a time series dataset with unevenly spaced data
dates <- as.numeric(c('1951', '1961' ,'1971',' 1981', '1991', '1997',' 2001',' 2005',' 2011'))
values <- data1$`Total Literacy Rate`
ts_data <- zoo::zoo(values, dates)

# Plot the time series
plot(ts_data, main = "Unevenly Spaced Data")

# Resample the time series to a regular frequency
ts_data_regular <- zoo::na.approx(ts_data, xout = seq(start(ts_data), end(ts_data), by = "Year"))

# Decompose the time series into seasonal, trend, and residual components
decomposed <- decompose(ts_data)





data2=data.frame(data1$`Male literacy`,data1$`Female literacy`)
plot.ts(data2)

# Time series forecasting using ARIMA for males
library(forecast)
dates <- as.numeric(c('1951', '1961' ,'1971',' 1981', '1991', '1997',' 2001',' 2005',' 2011'))
values <- data1$`Male literacy`
ts_data <- zoo::zoo(values, dates)
model2 <- auto.arima(ts_data)
forecast <- forecast(model2, h = 12)


df=New_Microsoft_Excel_Worksheet

#new 10 year evenly spaced

totalRate=df$`Total Literacy Rate`
ts_data<- ts(totalRate, start = 1951, frequency = 2)
ts_data
plot.ts(ts_data,main = "Total Literacy rate", xlab = "Year", ylab = "Literacy rate")
deComp <- decompose(ts_data) #****

plot(ts_data)
abline(reg=lm(ts_data~time(ts_data))) #Plot a Trendline on the Original Dataset

model1 <- auto.arima(ts_data)
model1

plot.ts(model1$residuals)

forecast1 <- forecast(model1, level=c(95), h=50*1/10)
plot(forecast1)












# Create a data frame for the density and literacy data
density_literacy_df <- data.frame(
  state = c("Andaman and Nicobar Islands", "Andhra Pradesh", "Assam", "Bihar", "Chandigarh", "Dadra and Nagar Haveli", "Daman and Diu", "Delhi", "Goa", "Gujarat", "Haryana", "Himachal Pradesh", "Jammu and Kashmir", "Jharkhand", "Karnataka", "Kerala", "Lakshadweep", "Madhya Pradesh", "Maharashtra", "Manipur", "Meghalaya", "Mizoram", "Nagaland", "Odisha", "Puducherry", "Punjab", "Rajasthan", "Sikkim", "Tamil Nadu", "Tripura", "Uttar Pradesh", "Uttarakhand", "West Bengal"),
  density = c(46, 308, 398, 1106, 9258, 700, 2191, 11320, 394, 308, 573, 123, 56, 414, 319, 860, 2149, 236, 365, 115, 132, 52, 119, 270, 2547, 551, 200, 86, 555, 350, 829, 189, 1028),
  literacy = c(86.63, 67.41, 72.19, 61.8, 86.05, 76.24, 87.1, 86.21, 88.7, 78.03, 75.55, 82.8, 67.16, 66.41, 75.36, 94, 91.85, 69.32, 82.34, 76.94, 74.43, 91.33, 79.55, 72.87, 85.85, 75.84, 66.11, 81.42, 80.09, 87.22, 67.68, 78.82, 76.26)
)

# Create the vectors for density and literacy with only matching states
matching_states <- intersect(density_literacy_df$state, rownames(density_literacy_df))
density <- density_literacy_df[density_literacy_df$state %in% matching_states, "density"]
literacy <- density_literacy_df[density_literacy_df$state %in% matching_states, "literacy"]
