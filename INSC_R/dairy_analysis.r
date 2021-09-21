# First, import the dataset from excel and necessary packages
dairy = read.csv(file = "dairy_data.csv")
library(ggplot2)

# view columns and data to see structure, number of farms, etc
head(dairy, n = 15)
tail(dairy, n = 15)
## see farms 1 through 247 with data from years 93 to 97

# you can remove columns to clean up the dataset if you want. Example: remove the land and labor column
# there are two ways to do this depending on how many columns you want to remove:
dairy$LAND = NULL
# dairy$LABOR = NULL

# dairy = dairy[-c(5,6)]

head(dairy)
colnames(dairy)

# Determining whether you have NA values in your dataset
summary(is.na(dairy))
# See there are no NA values in the dataset

# Questions we can ask about this dataset:
    # 1. Are there changes in milk production over the years?
    # 2. Is there an effect of feed amount on milk production?
        # Average amount of feed fed per cow vs average milk produced per cow
   # 3. Look at farm profits
        # ie. See if they feed less and get less milk, but if they make more overall profit
        # Assume farmers make $5.50 per liter of milk, pay $0.79 per pound of feed, and pay each employee $36,000 per year 

########################################################################################################
################# Question 1: Are there differences in milk production over the years? #################
########################################################################################################

# Using the aggregate function to calculates the total amount of milk produced per year by all the farms
milk = dairy$MILK
year = dairy$YEAR
total_MPY = aggregate(milk ~ year, FUN = sum) # FUN = sum creates a sum of the values specified per year
total_MPY
# Plotting the total milk produced per year on a line graph (type = lets you choose between points only (p), lines only (l), or both (o))
jpeg("total_MPY_line.jpeg")
    plot(total_MPY$milk ~ total_MPY$year,type = "o",col = "blue", xlab = "Year", ylab = "Liters of Milk Produced", 
        main = "Total milk produced per year")
dev.off()

# Can also use hex codes for specific colors
jpeg("total_MPY_line.jpeg")
    plot(total_MPY$milk ~ total_MPY$year,type = "l",col = "#e75480", xlab = "Year", ylab = "Liters of Milk Produced", 
        main = "Total milk produced per year")
dev.off()

####### Could this increase in milk production be attributed to an increase in cow numbers? #######

# Repeating the above with cow numbers
cows = dairy$COWS
year = dairy$YEAR
total_CPY = aggregate(cows ~ year, FUN = sum)

jpeg("total_CPY_bar.jpeg")
    barplot(total_CPY$cows ~ total_CPY$year, col = "dark green", xlab = "Year", ylab = "Cows Milked", 
        main = "Total cows milked per year")
dev.off()


# Can see that both milk production and cow numbers increase each year. Do they increase proportionally?

# Calculating percent increase in milk production from 1993 to 1998:
# referencing the aggregate dataframes and specifying what cells we want to use in our equation. [row, column]
percent_milk_increase = ((total_MPY[6,2] - total_MPY[1,2])/total_MPY[1,2]) * 100
percent_milk_increase

# Calculating percent increase in cow number from 1993 to 1998:
percent_cow_increase = ((total_CPY[6,2] - total_CPY[1,2])/total_CPY[1,2]) * 100
percent_cow_increase

# Milk production increases 55% while cow numbers only increase 25%.... What else could be going on?
# Farmers are producing more milk per cow
# Verifying this using a line graph
# Creating a new column that calculates the average milk produced per cow per year
avg_MPC = dairy$MILK/dairy$COWS
dairy$MILKperCOW = avg_MPC
#checking to make sure it was added
head(dairy)

# Calculating the average milk produced per cow per year
MPC = dairy$MILKperCOW
year = dairy$YEAR
avg_MPC = aggregate(MPC ~ year, FUN = mean) # FUN = mean creates an average of the values specified per year
avg_MPC
#plotting the information into a violin plot with mean

# x values need to be factors
dairy$YEAR = as.factor(dairy$YEAR)

# creating the plot
jpeg("MPC_vio.jpeg")
ggplot(dairy, aes(x = YEAR, y = MILKperCOW)) +
geom_violin(color = "darkgray", trim = FALSE) +
stat_summary(fun=mean, geom="point", shape=23, size=2)
dev.off()

# See that the average milk production per cow per year has increased 21% from 93 to 98
percent_MPC_increase = ((avg_MPC[6,2] - avg_MPC[1,2])/avg_MPC[1,2]) * 100
percent_MPC_increase

# We see a trend, but is it statistically significant?
# Perform an ANOVA
MPC_aov = aov(dairy$MILKperCOW ~ as.factor(dairy$YEAR), data=dairy)
print(MPC_aov)
MPC_aov_sum= summary(MPC_aov)
print(MPC_aov_sum)

# p value is 0.0000000000000002
# Run a Tukey's test to see which years are different
TukeyHSD(MPC_aov)
# Years 93 and 98 are statistically significant (p = 0.0000000)

# We see that the increase from 93 to 98 is statistically significant

# Need to check that the data is normally distributed
jpeg("MPC_aov_res.jpeg")
plot(MPC_aov,1)
dev.off()

# No relationship between residuals and fitted values - can assume data is normally distributed
# Can also visualize the data using a histogram to look and see what shape it takes
jpeg("MPC_hist.jpeg")
ggplot(dairy, aes(x = MILKperCOW)) +
  geom_histogram(fill = "white", colour = "black", binwidth = 50) +
  facet_grid(YEAR ~ .)
dev.off()

# What could be causing the increase in milk production per cow? Are they being fed more? Are they more efficient?

##############################################################################################
################# Question 2: Is there an effect of feed on milk production? #################
##############################################################################################

# Creating a new column with average feed per cow per year
avg_FPC = dairy$FEED/dairy$COWS
dairy$FEEDperCOW = avg_FPC

# Checking to see that it was added
head(dairy)

# Looking at the boxplot of pounds of feed per cow per year
FPC = dairy$FEEDperCOW
year = dairy$YEAR
avg_FPC = aggregate(FPC ~ year, FUN = mean)

jpeg("FPC_box.jpeg")
ggplot(dairy, aes(group = year, x=year, y=FEEDperCOW)) + 
    geom_boxplot() +
    geom_jitter(color = "#e75480")
dev.off()

# if jiter is too chaotic
jpeg("FPC_box.jpeg")
ggplot(dairy, aes(group = year, x=year, y=FEEDperCOW)) + 
    geom_boxplot() +
    geom_point()
dev.off()

#Calculating the percent increase in feed per cow
FPC = dairy$FEEDperCOW
year = dairy$YEAR
avg_FPC = aggregate(FPC ~ year, FUN = mean)

percent_FPC_increase = ((avg_FPC[6,2] - avg_FPC[1,2])/avg_FPC[1,2]) * 100
percent_FPC_increase

# See a 37% increase in feed fed per cow
# We see an increase in feed fed per cow AND an increase in milk produced per cow, is there a relationship between feed fed and milk produced per cow?

# Perform linear regression on feed per cow and milk per cow
lm_MPF = lm(dairy$MILKperCOW ~ dairy$FEEDperCOW, data = dairy)

# look at summary and confidence interval of MPF linear model
lm_MPF_sum = summary(lm_MPF)
lm_MPF_sum
lm_MPF_cf = confint(lm_MPF)
lm_MPF_cf

## creating a plot of the data with a line of best fit, pvalue, and Rsquared 
## defining p value and rsquared
my.p = lm_MPF_sum$coefficients[2,4]
r2 = lm_MPF_sum$adj.r.squared
#  creating a vector with 2 expressions(rp) and then defining what expression 1 (rp[1]) and expression 2(rp[2]) will be (r2 and my.p, respectively)
rp = vector('expression',2)
rp[1] = substitute(expression(italic(R)^2 == MYVALUE), 
		list(MYVALUE = format(r2,dig=3)))[2]
rp[2] = substitute(expression(italic(p) == MYOTHERVALUE), 
		list(MYOTHERVALUE = format(my.p, digits = 2)))[2]

# creating a graph of Milk per Feed
jpeg('MPF_lm.jpg')
plot(dairy$MILKperCOW ~ dairy$FEEDperCOW, data = dairy,
    main = "Avg Milk per Feed per Cow", xlab = "Avg Feed per Cow", ylab = "Avg Milk per Cow",
    col = "#006400")
# adding the regression line
abline(lm_MPF)
#adding the pvalue and R squared created earlier
legend('topright', legend = rp, bty = 'n')
dev.off()

# Don't forget to look at the residuals
jpeg("MPF_lm_res.jpeg")
plot(lm_MPF$residuals)
dev.off()

# Can change size and color of dots
jpeg("MPF_lm_res.jpeg")
plot(lm_MPF$residuals, pch = 20, col = "gray")
dev.off()

# Can also change the dots to characters
jpeg("MPF_lm_res.jpeg")
plot(lm_MPF$residuals, pch = "s")
dev.off()

###############################################################################################
######################### Question 3: What farms are more profitable? #########################
###############################################################################################

# Assume farmers make $5.50 per liter of milk, pay $0.79 per pound of feed,$19 per cow per year (vaccinations, etc), and pay each employee $36,000 per year

# Calculating profit
profit = (dairy$MILK * 5.50) - ((dairy$FEED * 0.79) + (dairy$LABOR * 36000) + (dairy$COWS * 19))
dairy$PROFIT = profit

# look at the data to see if it was added
head(dairy)

# going to perform a categorical x categorical analysis to see if relationship between farm size and profit
# calculating summary statistics to find median
summary(dairy$PROFIT)
summary(dairy$COWS)

# dividing farms by median cow number and median profit
# farms less than or equal to 20 cows will be considered small, and greater than 20 cows will be large
# farms making less than or equal to 505425 dollars per year will be low income(low) and greater than 505425 will be high income(high) 
# adding farm size column to dataset

#creating an empty vector and adding it to the dataframe
size = rep(NA,length(dairy$COWS))

dairy$SIZE = size

# determining whether a farm is small or large based on number of cows
# Can account for NA values here 
for (i in seq_along(dairy$COWS))
if (!is.na(dairy$COWS[i]) && dairy$COWS[i] <= 20.0) {
   dairy$SIZE[i] = "small"
} else if (!is.na(dairy$COWS[i]) && dairy$COWS[i] > 20.0) {
   dairy$SIZE[i] = "large"
} else {dairy$SIZE[i] = "NA"
}
   
#repeating with income level
income = rep(NA,length(dairy$PROFIT))

dairy$INCOME = income

for (i in seq_along(dairy$PROFIT))
if (dairy$PROFIT[i] <= 505425.0) {
   dairy$INCOME[i] = "low"
} else {dairy$INCOME[i] = "high"
}

# Checking to ensure they were added and changed
head(dairy, n = 10)
# Creating a contingency table and performing a chi squared analysis

# Can add data to a text file using sink
sink(file = "dairy_contingency_chi.txt", append = TRUE, type = c("output"))

title = "Farm Size x Income"
print(title)
print("contingency table")

# Can account for NA values here
SxI_cont=table(dairy$SIZE, dairy$INCOME, exclude = "NA")
print(SxI_cont)

SxI_cs = chisq.test(dairy$SIZE, dairy$INCOME)
print(SxI_cs)
if (SxI_cs$p.value < 0.05) {
  print("Farm Size and Income are Dependent values")
} else {
  print("Farm Size and Income are Independent values")
}

print("-----------------------------------------------------------")

sink()

# Small farms make a lower profit than large farms

################################################################################################
############################################ SUMMARY ###########################################
################################################################################################

# Question 1: Is there a difference in milk production?
## Yes, milk production increases AND total cow number increases
## Line graph, Barplot, Violin Plot, Histogram
## ANOVA, Tukeys, Normality Check
# Question 2: Is there an effect of feed on milk production?
## Yes, cattle fed more produce more milk
## Linear Modelling, Plotting the lm, residual checks
# Question 3: Which farm size is more profitable?
## The larger farms
## Contingency table, chi squared test