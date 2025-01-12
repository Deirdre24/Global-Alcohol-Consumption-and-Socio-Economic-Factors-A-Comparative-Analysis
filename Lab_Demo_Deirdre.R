# installing pacakges
install.packages("plotly")
install.packages("shiny")

#Loading Libraries
library(tidyverse)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(shiny) # For building dashboards
library(plotly)

file_path <- "Lab_Assessment_Dataset.xls"

# Get the names of all sheets in the Excel file
sheet_names <- excel_sheets(file_path)
print(sheet_names)

lab_assessment_data <- read_excel(file_path, sheet = "Lab_Assessment_Dataset")
gapminder_alcohol_data <- read_excel(file_path, sheet = "gapminder_alcohol")

# Exploring the data
glimpse(gapminder_alcohol_data)
head(gapminder_alcohol_data)
view(gapminder_alcohol_data)
summary(gapminder_alcohol_data)

# missing Values
missing_values_gapminder <- colSums(is.na(gapminder_alcohol_data))
print(missing_values_gapminder)

boxplot_alcconsumption <- ggplot(data = gapminder_alcohol_data, aes(y = alcconsumption)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "Boxplot of Alcohol Consumption",
       y = "Alcohol Consumption (litres)") +
  theme_minimal()

print(boxplot_alcconsumption)

# Identify outliers based on the boxplot
Q1 <- quantile(gapminder_alcohol_data$alcconsumption, 0.25, na.rm = TRUE)
Q3 <- quantile(gapminder_alcohol_data$alcconsumption, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1

# Define lower and upper bounds for outliers
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

# Identify countries that are outliers
outliers <- gapminder_alcohol_data %>%
  filter(alcconsumption < lower_bound | alcconsumption > upper_bound)

print("Identified Outliers in Alcohol Consumption:")
print(outliers)

# Filter for Moldova and other countries for comparison
comparison_data <- gapminder_alcohol_data %>%
  filter(country == "Moldova" | incomeperperson < 1000) 

# Create a scatter plot comparing alcohol consumption and income per person
ggplot(data = comparison_data, aes(x = incomeperperson, y = alcconsumption)) +
  geom_point(color = ifelse(comparison_data$country == "Moldova", "red", "blue"), 
             size = 3, alpha = 0.7) +  # Adjust size and transparency
  geom_text(aes(label = country), vjust = -0.5, size = 3, check_overlap = TRUE) + 
  labs(title = "Alcohol Consumption vs Income Per Person",
       x = "Income Per Person",
       y = "Alcohol Consumption (litres)") +
  scale_x_log10() +  # Use log scale for better visualization
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),  # Center title
        axis.text.x = element_text(angle = 45, hjust = 1))


names(gapminder_alcohol_data)


#Handling missing values
gapminder_alcohol_data %>%
  select(country, alcconsumption, incomeperperson, suicideper100th, employrate,
  urbanrate)%>%
  filter(!complete.cases(.))

# Drop rows with excessive missing values 4 or more columns have NA
gapminder_cleaned <- gapminder_alcohol_data %>%
  filter(rowSums(is.na(.)) < 4)  # Keeps rows with fewer than 4 NA values
view(gapminder_cleaned)

# Impute missing values for specific columns
gapminder_cleaned <- gapminder_cleaned %>%
    mutate(
        incomeperperson = ifelse(is.na(incomeperperson),
                                 median(incomeperperson, na.rm = TRUE), incomeperperson),
        alcconsumption =  ifelse(is.na(alcconsumption),
                                 median(alcconsumption, na.rm = TRUE), alcconsumption),
        employrate = ifelse(is.na(employrate),
                            mean(employrate, na.rm = TRUE), employrate),
        suicideper100th = ifelse(is.na(suicideper100th),
                                 mean(suicideper100th, na.rm = TRUE), suicideper100th),
        urbanrate = ifelse(is.na(urbanrate),
                           mean(urbanrate, na.rm = TRUE), urbanrate)
    )
view(gapminder_cleaned)

#Check missing values
remaining_missing_values <- colSums(is.na(gapminder_cleaned))
print("Remaining Missing Values After Cleaning:")
print(remaining_missing_values)

# Create income categories based on income per person
gapminder_cleaned <- gapminder_cleaned %>%
  mutate(IncomeCategory = case_when(
    incomeperperson < 1000 ~ "Low",
    incomeperperson >= 1000 & incomeperperson < 10000 ~ "Medium",
    TRUE ~ "High"
  ))


# Display a few entries from the transformed dataset including IncomeCategory
view(table(head((gapminder_cleaned %>% select(country, incomeperperson, IncomeCategory)))))


# Simulate some time series data for demonstration
set.seed(123)  # For reproducibility
years <- 2000:2020
alc_consumption <- cumsum(runif(21, min = 0, max = 5))  # Cumulative sum to simulate increasing alcohol consumption

# Create a data frame
line_plot_data <- data.frame(
  year = years,
  alc_consumption = alc_consumption
)

# Create the line plot with updated linewidth aesthetic
line_plot <- ggplot(line_plot_data, aes(x = year, y = alc_consumption)) +
  geom_line(linewidth = 1, color = "blue") + 
  geom_point(color = "red", size = 2) + 
  labs(title = "Hypothetical Alcohol Consumption Over Years", 
       x = "Year", 
       y = "Alcohol Consumption (litres)") +
  theme_minimal()
print(line_plot)


# Calculate average alcohol consumption by income category
average_consumption <- gapminder_cleaned %>%
  group_by(IncomeCategory) %>%
  summarise(AverageAlcoholConsumption = mean(alcconsumption, na.rm = TRUE))

# Create a bar plot of average alcohol consumption by income category
ggplot(data = average_consumption, aes(x = IncomeCategory, y = AverageAlcoholConsumption)) +
  geom_bar(stat = "identity", fill = "blue", color= "black") +
  labs(title = "Average Alcohol Consumption by Income Category",
       x = "Income Category",
       y = "Average Consumption (litres)") +
  theme_minimal()

ggplot(data = gapminder_cleaned, aes(x = incomeperperson, y = alcconsumption)) +
  geom_point(color = "blue", alpha = 0.6) +  
  labs(title = "Alcohol Consumption vs Income Per Person",
       x = "Income Per Person (USD)",
       y = "Alcohol Consumption (litres)") +
  scale_x_log10() + # Use log scale for better visualization
  theme_minimal()




# Calculate average urbanization rates by income category
urbanization_summary <- gapminder_cleaned %>%
  group_by(IncomeCategory) %>%
  summarise(AverageUrbanRate = mean(urbanrate, na.rm = TRUE))

# Create a bar plot of average urbanization rates by income category
ggplot(data = urbanization_summary, aes(x = IncomeCategory, y = AverageUrbanRate)) +
  geom_bar(stat = "identity", fill = "purple", color= "black") +
  labs(title = "Average Urbanization Rate by Income Category",
       x = "Income Category",
       y = "Average Urbanization Rate (%)") +
  theme_minimal()

# Scatter plot of Alcohol Consumption vs Urbanization Rate
ggplot(data = gapminder_cleaned, aes(x = urbanrate, y = alcconsumption)) +
  geom_point(alpha = 0.6, color = "purple") +  # Points representing each country
  labs(title = "Alcohol Consumption vs Urbanization Rate",
       x = "Urbanization Rate (%)",
       y = "Alcohol Consumption (litres)") +
  theme_minimal()




# Calculate average suicide rates by income category
suicide_summary <- gapminder_cleaned %>%
  group_by(IncomeCategory) %>%
  summarise(AverageSuicideRate = mean(suicideper100th, na.rm = TRUE))

# Bar chart of average suicide rates by income category
ggplot(data = suicide_summary, aes(x = IncomeCategory, y = AverageSuicideRate)) +
  geom_bar(stat = "identity", fill = "red", color ="black") +
  labs(title = "Average Suicide Rates by Income Category",
       x = "Income Category",
       y = "Average Suicide Rate (per 100,000)") +
  theme_minimal()


# Scatter plot of Alcohol Consumption vs Suicide Rates
ggplot(data=gapminder_cleaned, aes(x=suicideper100th, y=alcconsumption)) +
  geom_point(alpha=0.6, color="red") +
  labs(title="Alcohol Consumption vs Suicide Rates",
       x="Suicide Rate (per 100,000)",
       y="Alcohol Consumption (litres)") +
  theme_minimal()


# histogram of alcohol consumption
ggplot(gapminder_cleaned, aes(x = alcconsumption)) +
  geom_histogram(binwidth = 1, fill = "green", color = "black") +
  labs(title = "Distribution of Alcohol Consumption",
       x = "Alcohol Consumption (litres)",
       y = "Frequency") +
  theme_minimal()



ggplot(data=gapminder_cleaned, aes(x=incomeperperson, y=alcconsumption)) +
  geom_point() +
  facet_wrap(~ IncomeCategory) + 
  labs(title="Alcohol Consumption vs Income Per Person by Income Category", 
       x="Income Per Person (USD)", y="Alcohol Consumption (litres)") +
  theme_minimal()

identify_outliers <- function(data) {
  data %>%
    group_by(IncomeCategory) %>%
    mutate(Q1 = quantile(alcconsumption, 0.25),
           Q3 = quantile(alcconsumption, 0.75),
           IQR = Q3 - Q1,
           lower_bound = Q1 - 1.5 * IQR,
           upper_bound = Q3 + 1.5 * IQR,
           is_outlier = alcconsumption < lower_bound | alcconsumption > upper_bound) %>%
    filter(is_outlier)
}

outlier_countries <- identify_outliers(gapminder_cleaned)

print(outlier_countries)

gapminder_cleaned <- gapminder_cleaned %>%
  mutate(is_outlier = ifelse(country %in% c("Belarus", "Moldova", "Nigeria", "Uganda"), TRUE, FALSE))

# Plot with highlighted outliers
ggplot(data=gapminder_cleaned, aes(x=incomeperperson, y=alcconsumption)) +
  geom_point(aes(color = is_outlier), size = 3) +
  facet_wrap(~ IncomeCategory) + 
  labs(title="Alcohol Consumption vs Income Per Person by Income Category", 
       x="Income Per Person (USD)", y="Alcohol Consumption (litres)") +
  scale_color_manual(values = c("black", "red"), labels = c("Not Outlier", "Outlier")) +
  theme_minimal()


#interactive visualizations

p <- ggplot(data=average_consumption, aes(x=IncomeCategory,
                                          y=AverageAlcoholConsumption)) +
  geom_bar(stat="identity", fill="yellow", color = "black") +
  labs(title="Average Alcohol Consumption by Income Category", 
       x="Income Category", y="Average Consumption (litres)")

ggplotly(p) %>% layout(hovermode="closest")


p_histogram <- ggplot(gapminder_cleaned, aes(x = alcconsumption)) +
  geom_histogram(binwidth = 1, fill = "green", color = "black") +
  labs(title = "Distribution of Alcohol Consumption",
       x = "Alcohol Consumption (litres)",
       y = "Frequency") 

ggplotly(p_histogram) %>% layout(hovermode="closest")

# just to veryfy
countries_with_6_litres <- gapminder_cleaned %>%
  filter(alcconsumption >= 5.5 & alcconsumption <= 6.5)

print(countries_with_6_litres)


p_facet <- ggplot(data=gapminder_cleaned, aes(x=incomeperperson, y=alcconsumption)) +
  geom_point() + 
  facet_wrap(~ IncomeCategory) + 
  labs(title="Alcohol Consumption vs Income Per Person by Income Category")

ggplotly(p_facet) %>% layout(hovermode="closest")

summary_metrics <- gapminder_cleaned %>%
  summarise(TotalAlcoholConsumption=sum(alcconsumption),
            AverageAlcoholConsumption=mean(alcconsumption))
print(summary_metrics)


# library(shiny)

names(gapminder_cleaned)

install.packages("corrplot")

library(corrplot)


# Define UI
ui <- fluidPage(
  titlePanel("Alcohol Consumption Dashboard"),
  sidebarLayout(
    sidebarPanel(
      selectInput("income_category", "Select Income Category:",
                  choices = unique(gapminder_cleaned$IncomeCategory)),
      numericInput("top_n", "Top N Countries:", value = 5, min = 1)
    ),
    mainPanel(
      fluidRow(
        column(6,
               plotOutput("barPlot")),
        column(6,
               plotOutput("scatterPlot"))
      ),
      fluidRow(
        column(6,
               plotOutput("suicideRatePlot")), # New plot for alcohol vs suicide rates
        column(6,
               plotOutput("correlationPlot")) # Correlation plot
      ),
      tableOutput("summaryTable")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  filtered_data <- reactive({
    gapminder_cleaned %>%
      filter(IncomeCategory == input$income_category) %>%
      arrange(desc(alcconsumption)) %>%
      head(input$top_n)
  })
  
  output$barPlot <- renderPlot({
    ggplot(filtered_data(), aes(x = reorder(country, -alcconsumption), y = alcconsumption)) +
      geom_bar(stat = 'identity', fill = 'blue') +
      labs(title = 'Top Alcohol Consumption by Country', x = 'Country', y = 'Alcohol Consumption (litres)') +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$scatterPlot <- renderPlot({
    ggplot(filtered_data(), aes(x = incomeperperson, y = alcconsumption)) +
      geom_point(color = 'red', size = 3) +
      labs(title = 'Alcohol Consumption vs Income Per Person',
           x = 'Income Per Person (USD)', y = 'Alcohol Consumption (litres)') +
      theme_minimal()
  })
  
  output$suicideRatePlot <- renderPlot({
    ggplot(filtered_data(), aes(x = alcconsumption, y = suicideper100th)) +
      geom_point(color='purple', size=3) +
      labs(title='Alcohol Consumption vs Suicide Rates',
           x='Alcohol Consumption (litres)', y='Suicide Rate (per 100k)') +
      theme_minimal()
  })
  
  output$summaryTable <- renderTable({
    filtered_data() %>%
      summarise(TotalConsumption = sum(alcconsumption),
                AverageConsumption = mean(alcconsumption),
                AverageIncomePerPerson = mean(incomeperperson),
                AverageSuicideRate = mean(suicideper100th))
  })
  
  output$correlationPlot <- renderPlot({
    # Calculate correlation matrix for the filtered data
    cor_data <- filtered_data()[ , c("alcconsumption", "incomeperperson", "suicideper100th", "employrate", "urbanrate")]
    cor_matrix <- cor(cor_data)
    
    # Correlation plot
    corrplot(cor_matrix, method="circle",
             title="Correlation Matrix",
             tl.col="black", tl.srt=45)
  })
}

library(maps)              # Load the ggmap library
library(leaflet) # For creating interactive maps
library(ggrepel)

city_data <- read_excel(file_path, sheet = "Lab_Assessment_Dataset")

# View the first few rows of the data to ensure it's loaded correctly
print(head(city_data))

bot_map <- map_data("world", region = "Botswana")  # Get map data for Botswana

leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap tiles
  setView(lng = 24, lat = -22, zoom = 6) %>%  # Center on Botswana
  addCircleMarkers(data = city_data, 
                   lng = ~lng, 
                   lat = ~lat, 
                   label = ~city,  # Use city names as labels
                   color = 'red', 
                   radius = 5,
                   fillOpacity = 0.5) %>%
  addLegend("bottomright", 
            colors = "red", 
            labels = "City Locations", 
            title = "Legend") %>%
  addProviderTiles(providers$CartoDB.Positron)



# Simulate some time series data for demonstration
set.seed(123)  # For reproducibility
years <- 2000:2020
alc_consumption <- cumsum(runif(21, min = 0, max = 5))  # Cumulative sum to simulate increasing alcohol consumption


library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(tidyr)
library(zoo)  # Load zoo for moving averages

# Define UI for application
ui <- dashboardPage(
  dashboardHeader(title = "Alcohol Consumption Analysis"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      selectInput("income_category", "Select Income Category:",
                  choices = unique(gapminder_cleaned$IncomeCategory)),
      textInput("country_search", "Search for Country:", placeholder = "Type country name..."),
      selectInput("country_select", "Select Countries to Compare:", 
                  choices = unique(gapminder_cleaned$country), 
                  selected = NULL, 
                  multiple = TRUE),  # Allow multiple selections
      actionButton("search_button", "Search")  # Place search button beneath country selection
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard",
              fluidRow(
                valueBoxOutput("total_consumption"),
                valueBoxOutput("average_consumption"),
                valueBoxOutput("average_income"),
                valueBoxOutput("average_suicide_rate")
              ),
              fluidRow(
                column(6, plotOutput("barPlotAlcohol", height = "300px")),  # Horizontal bar graph for alcohol consumption
                column(6, plotOutput("urbanRatePlot", height = "300px"))  # Bar chart for income category by urban rate
              ),
              plotOutput("barPlotComparison", height = "300px"),  # Vertical bar graph for comparison
              plotOutput("moving_average_plot", height = "400px")  # Plot for moving averages
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  filtered_data <- reactive({
    req(input$income_category)  # Ensure income category is selected
    data <- gapminder_cleaned %>%
      filter(IncomeCategory == input$income_category)
    
    # Filter by country search input if provided
    if (input$country_search != "") {
      data <- data %>%
        filter(grepl(input$country_search, country, ignore.case = TRUE))
    }
    
    return(data)
  })
  
  # Summary metrics in infoboxes
  output$total_consumption <- renderValueBox({
    total <- sum(filtered_data()$alcconsumption, na.rm = TRUE)
    valueBox(value = total, subtitle = "Total Alcohol Consumption (litres)", icon = icon("beer"))
  })
  
  output$average_consumption <- renderValueBox({
    avg_consumption <- mean(filtered_data()$alcconsumption, na.rm = TRUE)
    valueBox(value = round(avg_consumption, 2), subtitle = "Average Consumption (litres)", icon = icon("glass-cheers"))
  })
  
  output$average_income <- renderValueBox({
    avg_income <- mean(filtered_data()$incomeperperson, na.rm = TRUE)
    valueBox(value = round(avg_income, 2), subtitle = "Average Income Per Person (USD)", icon = icon("dollar-sign"))
  })
  
  output$average_suicide_rate <- renderValueBox({
    avg_suicide_rate <- mean(filtered_data()$suicideper100th, na.rm = TRUE)
    valueBox(value = round(avg_suicide_rate, 2), subtitle = "Average Suicide Rate (per 100k)", icon = icon("skull-crossbones"))
  })
  
  # Horizontal bar graph for Alcohol Consumption based on selected countries or all in income category
  output$barPlotAlcohol <- renderPlot({
    data_to_plot <- filtered_data()
    
    if (!is.null(input$country_select) && length(input$country_select) > 0) {
      data_to_plot <- data_to_plot %>% filter(country %in% input$country_select)
    }
    
    ggplot(data_to_plot, aes(x=reorder(country, -alcconsumption), y=alcconsumption)) +
      geom_bar(stat='identity', fill='blue') +
      coord_flip() + 
      labs(title='Alcohol Consumption by Selected Countries', x='Country', y='Alcohol Consumption (litres)') +
      theme_minimal()
  })
  
  # Bar plot for Income Category by Urban Rate
  output$urbanRatePlot <- renderPlot({
    ggplot(gapminder_cleaned, aes(x=IncomeCategory, y=urbanrate)) +
      geom_bar(stat="identity", fill="green") +
      labs(title="Urbanization Rate by Income Category", x="Income Category", y="Urbanization Rate (%)") +
      theme_minimal()
  })
  
  # Grouped Bar Chart for Comparison based on selected countries or all in income category
  output$barPlotComparison <- renderPlot({
    df_comparison <- filtered_data()
    
    if (!is.null(input$country_select) && length(input$country_select) > 0) {
      df_comparison <- df_comparison %>%
        filter(country %in% input$country_select)
    }
    
    df_comparison <- df_comparison %>%
      select(country, alcconsumption, suicideper100th) %>%
      pivot_longer(cols=c(alcconsumption, suicideper100th), 
                   names_to="metric", 
                   values_to="value")
    
    ggplot(df_comparison, aes(x=reorder(country, -value), y=value, fill=metric)) +
      geom_bar(stat="identity", position="dodge") +
      labs(title='Comparison of Alcohol Consumption and Suicide Rates by Selected Countries',
           x='Country', y='Value') +
      scale_fill_manual(values=c("blue", "orange"), 
                        labels=c("Alcohol Consumption (litres)", "Suicide Rate (per 100k)")) +
      theme_minimal() +
      theme(axis.text.x=element_text(angle=45, hjust=1))
  })
  
  # Plotting moving averages for alcohol consumption
  output$moving_average_plot <- renderPlot({
    data_to_plot <- filtered_data()
    
    if (nrow(data_to_plot) > 0) {
      # Calculate moving average without needing a time component
      moving_avg <- rollmean(data_to_plot$alcconsumption, k=3, fill=NA, align="right") 
      
      ggplot(data_to_plot) +
        geom_line(aes(x=row_number(), y=alcconsumption), color="blue", size=1.2) + 
        geom_line(aes(x=row_number(), y=moving_avg), color="red", linetype="dashed", size=1.2) + 
        labs(title='Alcohol Consumption with Moving Average',
             x='Observation Number', y='Alcohol Consumption (litres)', color='Country') +
        theme_minimal() +
        scale_y_continuous(labels=scales::comma)
    } else {
      ggplot() + 
        labs(title='No Data Available', x='', y='')
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)