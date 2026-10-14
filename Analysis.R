# Analysis of Measles Dataset
# Load and inspect data
measles <- read.csv("reported-cases-of-measles.csv")

head(measles)
str(measles)
summary(measles)

# Clean and Filter the data
# Rename the 4th column 
names(measles)[4] <- "Measles_cases"
names(measles)

#Select countries and years
countries <- c("Netherlands", "Belgium", "Germany", "France", "Italy" )
color_map <- c("Netherlands" = "orange", "Belgium" = "red", "Germany" = "black", "France" = "blue", "Italy" = "green")
measles_selected <- measles[measles$Entity %in% countries & measles$Year >= 2015 & measles$Year <= 2024,]

# Sort data by year so the plot lines draw correctly from left to right
measles_selected <- measles_selected[order(measles_selected$Year), ]

print(measles_selected)

# Analyze outbreaks
total_cases <- aggregate(Measles_cases ~ Entity, data = measles_selected, FUN = sum)

print(total_cases)

# Identify the country with the highest case count
highest_cases <- max(total_cases$Measles_cases)
highest_country  <- total_cases[total_cases$Measles_cases == highest_cases, ]

print(highest_country)

# Create a plot for measles causes from 2015 to 2024
plot(NA, xlim = c(2015, 2024), ylim = c(0, max(measles_selected$Measles_cases)), xlab = "Year", ylab = "Reported measles cases", main = "Reported Measles Cases in countries")

# Add line for each country
for (i in 1:length(countries)) {
    country_data <- measles_selected[measles_selected$Entity == countries[i], ]
    lines(country_data$Year, country_data$Measles_cases, type = "o", col = color_map[countries[i]])
}
# Add a legend
legend("topright", legend = countries, col = color_map[countries], lty =1, pch = 1)

# Create a bar plot of total measles cases for countries
barplot(total_cases$Measles_cases, names.arg = total_cases$Entity, xlab = "Country", ylab = "Total reported measles cases", main = "Total reported measles cases from 2015 to 2024", col= color_map[total_cases$Entity])


# Find the highest number of cases for each country
peak_cases_per_country <- aggregate(Measles_cases~ Entity, data = measles_selected, FUN = max)

# Find the year with the highest number of ocurrences
peak_data <- merge(peak_cases_per_country, measles_selected, by = c("Entity", "Measles_cases"))

peak_data <- peak_data[ , c("Entity", "Year", "Measles_cases")]

# Print peak data
print(peak_data)

# Print final results
print(total_cases)
print(highest_country)
print(peak_data)