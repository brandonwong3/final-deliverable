# INFO 201
# Final Group Project
# Date: December 9, 2020

library("dplyr")
library("tidyr")

# Load the `ggplot2` library for data manipulation 
library("ggplot2")

library("plotly")
library("shiny")


# =====================================================================
# Read the U.S.Census "Historical Income Tables: Race" data
# =====================================================================
income_data <- read.csv("https://raw.githubusercontent.com/brandonwong3/final-deliverable/master/Census_Summary.csv", stringsAsFactors = FALSE, header=TRUE)

# Function to remove ',' in CSV data for # in the thousands
replaceCommas<-function(x){
  x<-as.numeric(gsub(",", "", x))
}

# =====================================================================
# Wrangle Data from US Census
# =====================================================================

# Clean Data, remove commas from columns with strings of numbers
# Note: in apply() use MARGIN=2 for columns
data_apply <- apply(income_data[ ,c(2:7)], 2, replaceCommas)
data_new <- income_data                                                         # Replicate original data
data_new[ , colnames(data_new) %in% colnames(data_apply)] <- data_apply  # Replace specific columns

# Data for all four Race categories start at 2002; filter from 2002
data_new <- data_new %>% 
  filter(Year >= 2002)


# =====================================================================
# FUNCTIONS
# =====================================================================


# ========================
# Reactive Chart
# Function: Household income by Fifth (Line Chart)
# ggplot geom_ function
# Parameter chooses which fifth of the household income data to graph

plot_chart <- function(by_column) {
  by_column <- ensym(by_column)   # needed to read column name
  
  df <- ggplot(data = data_new) +
    geom_line(mapping = aes(x = Year, y = !!by_column, color = Race)) +
    
    labs(     # Add title and axis labels
      title = paste("Household Income for Each Fifth"),
      x = "Year", # x-axis label
      y = "Income ($)", # y-axis label
      color = "" # legend label for the "color" property
    ) 
   
  df
  ggplotly(df) %>% 
    # Legend overlapped the plotly menus, used manual placement for legend
    add_annotations( text="Race", xref="paper", yref="paper",
                            x=1.02, xanchor="left",
                            y=0.8, yanchor="bottom",  # Same y as legend below
                            legendtitle=TRUE, showarrow=FALSE ) %>%
    layout( legend=list(y=0.8, yanchor="top" ) )
}


# ========================
# Reactive Chart
# Function: Percentage Comparison of Non-White to White Income (Line Chart)
# ggplot geom_ function
# Parameter chooses which fifth of the household income data to graph

plot_income_percent <- function(by_column) {
  by_column <- ensym(by_column) # fixes column name passed as argument
  
  income_long <- data_new %>%   # change data frame to long format
    select(Year, by_column, Race) # select fifth vector
  
  income_dev <- spread(
    income_long, # data frame to spread from
    key = Race, 
    value = by_column  
  )
  income_dev <- mutate(income_dev, # calculate % of non-white to white
                       Asians = Asian / White,
                       Hispanics = Hispanic / White,
                       Whites = 1.0,
                       Blacks = Black / White) 
  
  income_dev <- income_dev %>% # remove orig data, keep only %
    select(Year, Asians, Hispanics, Whites, Blacks)

  income_long <- gather(   # reassemble from long to wide to plot
    income_dev, # data frame to gather from
    key = Race,  
    value = Income, 
    -Year # columns to gather data from, as in dplyr's `select 
  )
  df <- ggplot(data = income_long) +
    geom_line(mapping = aes(x = Year, y = Income, color = Race)) +
    
    labs(
      title = "Income by Race compared to Whites", # plot title 
      x = "Year", # x-axis label
      y = "Income (%)", # y-axis label
      color = "" # legend label deleted
      ) +
    scale_y_continuous(breaks=seq(0.5, 1.5, by = 0.1))
  
  df
  ggplotly(df) %>% 
    # Legend overlapped the plotly menus, used manual placement for legend
    add_annotations( text="Race", xref="paper", yref="paper",
                   x=1.02, xanchor="left",
                   y=0.8, yanchor="bottom",  # Same y as legend below
                   legendtitle=TRUE, showarrow=FALSE ) %>%
    layout( legend=list(y=0.8, yanchor="top" ) )
}

# =====================================================================
# Use data from the NCES (National Center for Educational Statistics)
# 
# =====================================================================
nces_grad <- read.csv("https://raw.githubusercontent.com/brandonwong3/final-deliverable-ba-group-5/master/NCES_grad_rate.csv", 
                      stringsAsFactors = FALSE, header=TRUE)

nces_enroll <- read.csv("https://raw.githubusercontent.com/brandonwong3/final-deliverable-ba-group-5/master/NCES_enrollment_rate.csv", 
                        stringsAsFactors = FALSE, header=TRUE)


# =======================
# Wrangle NCES data
# =======================

# Clean Year data by removing " starting chort ....." in String
# Rename value column 'His-panic' to 'Hispanic'
nces_grad <- mutate(nces_grad, Year_clean = substr(nces_grad$Year, 1, 4))
nces_grad <- nces_grad %>% 
  select(Year_clean, White, Black, His..panic, Asian) %>% 
  rename(Year=Year_clean, Hispanic=His..panic)

nces_enroll <- mutate(nces_enroll, Year_clean = substr(nces_enroll$Year, 1, 4))
nces_enroll <- nces_enroll %>% 
  select(Year_clean, White, Black, Hispanic, Asian) %>% 
  rename(Year=Year_clean)



# ==========================
# Format for plotting as long
# ============================

nces_grad_long <- gather(   # reassemble from long to wide to plot
  nces_grad, # data frame to gather from
  key = Race,  
  value = Grad_Rate, 
  -Year # columns to gather data from, as in dplyr's `select 
)

nces_enroll_long <- gather(   # reassemble from long to wide to plot
  nces_enroll, # data frame to gather from
  key = Race,  
  value = Enroll_Rate, 
  -Year # columns to gather data from, as in dplyr's `select 
)

# Join data sets
combined_nces <- left_join(nces_enroll_long, nces_grad_long, 
                           by=c('Year'='Year', 'Race'='Race'))
combined_nces <- combined_nces %>% 
  rename("Graduation"="Grad_Rate", "Enrollment"="Enroll_Rate") %>% 
  filter(Year >= 2000)


# ===================================
# Plot NCES
# ===================================

plot_nces <- function(by_column) {
  by_column <- ensym(by_column)
  
  df <- ggplot(data = combined_nces) +
    geom_line(mapping = aes(x=Year, y=!!by_column, group=Race, color=Race)) +
    
    scale_x_discrete(breaks=seq(2000, 2020, 2)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    
    # Add labels 
    labs(
      title = paste("College", by_column,"Rates"), # plot title 
      x = "Year", # x-axis label
      y = "Percent (%)", # y-axis label
      color = "Race" # legend label for the "color" property
    ) 
  
  df
}


# ===================================
# UI code
# ===================================

# Define the first page content; uses `tabPanel()` and `sidebarLayout()` 
# layout functions together (as an example)
page_one <- tabPanel(
  "Intro tab", # label for the tab in the navbar 
  titlePanel("Exploring the U.S. Income Gap by Race"), # show with a displayed title
  
  "It should be no surprise that the yearly income of households in the",
  "United States varies by race. Despite the many decades that have passed",
  "since the Civil Rights movement, the income gap between races remains.",
  "This project uses data from the ",
  a("U.S. Census Bureau", href="https://www.census.gov/data/tables/time-series/demo/income-poverty/historical-income-inequality.html"),
  " to explore this gap.",
  "",
  "",
  "",
  
  br(), br(),
  

  p("Income is just one measure of the economical financial racial gap ",
    "in the United Stats. Ultimately, the historical roots of structural ",
    "income inequality has manifested in a racial wealth gap that is ",
    "difficult to overcome. The White middle class prospered and built ",
    "wealth under the Jim Crow laws, the G.I. Bill, taxcuts, segregation, ",
    "and discrimination against Blacks owning wealth. Future generations ",
    "benefited from this accumulated wealth. According to ",
    a("Racial Wealth Gap in the United States", href="https://www.thebalance.com/racial-wealth-gap-in-united-states-4169678"),
    " the racial wealth gap is widening."),
  br(),
  img(src="https://cdn.vox-cdn.com/thumbor/pW69mUpDW5mxRD7Oo2n0cL-cfi8=/0x0:2400x1600/2820x1586/filters:focal(1280x496:1664x880)/cdn.vox-cdn.com/uploads/chorus_image/image/66634984/031920_healthfood_rg_21.0.jpg", width=600),
  p("")
  
)


# Define content for the second page 
page_two <- tabPanel(
  "Income tab", # label for the tab in the navbar
  # ...more content would go here... 
  # This content uses a sidebar layout
  sidebarLayout( sidebarPanel(
    
    radioButtons(inputId = "rb_chosen", # key assigned
                 label = "Fifth (quintile) Selection",
                 choices = list("Lowest" = "Lowest", 
                                "Second" = "Second",
                                "Third" = "Third",
                                "Fourth" = "Fourth",
                                "Top 5 Percent" = "Top5")
                 ), # close radio button
  ), # close sidebarLayout
  
  mainPanel(
    h3("Historical Income Inequality"),
    p("Source: U.S. Census Bureau (CPS ASEC)"),
    plotlyOutput(outputId = "income_plot"),    # panel output
    br(),
    
    br("The income gap for Blacks has not improved despite what people ",
       "may think. The income gap for Hipanics has narrowed somewhat. ",
       "What is most interesting is that the gaps have remained the same ",
       "at every income level. Even within the top five percent of incomes ",
       "for each race, Blacks and Hispanics experience the same amount ",
       "of reduced income as the lower levels of income. "),
    br("In order to close the gap, Blacks and Hispanics must ",
      "move more of their population in to the middle class. ",
      "This means more preparing them for employment in middle class ",
      "jobs and ensuring they receive compensation equal to Whites for ",
      "the same work.")
  ) )
)


# Define content for the third page 
page_three <- tabPanel(
  "Comparison tab", # label for the tab in the navbar

  sidebarLayout( sidebarPanel(
    radioButtons(inputId = "rb_chosen_c", # key assigned
                 label = "Fifth (quintile) Selection",
                 choices = list("Lowest" = "Lowest", 
                                "Second" = "Second",
                                "Third" = "Third",
                                "Fourth" = "Fourth",
                                "Top 5 Percent" = "Top5")
    ), # close radio button
  ), # close sidebarLayout
  
  mainPanel(
    h3("Historical Income Inequality"),
    p("Source: U.S. Census Bureau (CPS ASEC)"),
    plotlyOutput(outputId = "compare_plot"),    # panel output
    br(),
    
    br("This visual illustrates a comparison of income between Non-Whites ",
       "and Whites. This clearly shows that Blacks and Hipanics have not  ",
       "made significant strides in closing the income gap with Whites. "),
    p(),
    p("Black income has remained flat (no growth) at all income levels."),
    p("Hispanics have made slight improvements in income over time."),
    p("Asians have trended higher and even widened the gap with Whites.")
  ) )
)

# Define content for the fourth page 
page_four <- tabPanel(
  "Higher Education tab", # label for the tab in the navbar
  
  sidebarLayout( sidebarPanel(
    radioButtons(inputId = "rb_chosen_nces", # key assigned
                 label = "College Education",
                 choices = list("Enrollment Rate" = "Enroll", 
                                "Graduation Rate" = "Grads")
    ), # close radio button
  ), # close sidebarLayout
  
  mainPanel(
    h3("College Statistics"),
    p("Source: National Center for Education Statistics"),
    plotlyOutput(outputId = "nces_plot"),    # panel output
    br(),
    
    br("The college enrollment rates, in general, matched the order of ",
       "incomes by race, where Asians were at the top, followed by Whites, ",
       "Hispanics then Blacks. The most encouraging news was that for all ",
       "races, the percentage of students that enrolled in college after ",
       "high school was above 50%."),
    p(),
    p("The graduation ratea of all races has trended up. Blacks have not increased ",
      "their rates as much as the other races. When comparing the income ",
      "data with college graduation data, the graduation data shows a much ",
      "wider gap between Whites versus Blacks and Hispanics. An improvement ",
      "in college graduation rates of Blacks could be a stimulus to potentially ",
      "close the income disparity between races.")
  ) )
)


# =====================================================================
# Conclusion page
# =====================================================================

# Define content for the fifth page
page_five <- tabPanel(
  "Conclusion tab", # label for the tab in the navbar 
  titlePanel("Final Thoughts on the Income Gap and Race"), # show with a displayed title
  
 p("After our group conducted our research, it becomes clear that racial ",
  "income gaps have only been sustained throughout recent history. Although ",
  "Asians have been able to break out of the systemic income inequality present ",
  "in the United States, one major takeaway from our analysis is that race still plays a  ",
  "major role in the issue of the income gap in the United States; underprivileged communities, ",
  "specifically black and hispanic communities, are still experiencing systemic oppression ",
  "that has failed to lessen over the past decade. ",
  "",
  "",
  "",
  br(), br(),
  img(src="https://borgenproject.org/wp-content/uploads/32855286416_066fc5371d_k-930x621.jpg")),

p("Another takeaway from the data was how non-white racial groups, specifically ",
  "Asians, have been successful in overtaking whites in income percentage, ",
  "while other non-white racial groups (specifically Hispanics and blacks)  ",
  "have had little to no success in matching whites in their income percentage. ",
  "This gap is mostly due to systemic racism that is present in all forms of life: ",
  "mass incarceration, over policing, and other forms of systemic racism lead to less ",
  "opportunities for minorities, overall leading to less average income. ",
  br(), br(),
  img(src="https://www.pambazuka.org/sites/default/files/styles/flexslider_full/public/field/image/HT-natalie-keyssar-blm-protest-01-as-170106_3x2_1600.jpg?itok=L3QqU18j")),

p("The final takeaway that we concluded from the data ",
  "was that there is still much work to be done regarding ",
  "the issue of the income gap and race. Although this project ",
  "did allow us to gain a much better understanding of how ",
  "race and economic issues are related, as a collective group ",
  "we still possess only a small amount of the knowledge ",
  "necessary to fully comprehend this issue. Therefore, as a ",
  "future goal, we have all agreed to continue to pursue ",
  "this topic in an attempt to solve the unjust inequalities ",
  "present in the United States. "),
br(), br(),
img(src="https://www.mindingthecampus.org/wp-content/uploads/2019/03/hands-2082x1171.jpg"),
)

# Pass each page to a multi-page layout (`navbarPage`) 
my_ui <- navbarPage(
  "My Application", # application title
  page_one,   # include the first page content
  page_two,   # include the second page content
  page_three, # include the third page content
  page_four,  # include the fourth page content
  page_five   # include the fifth page content
)


# =====================================================================
# Server code
# =====================================================================

my_server <- function(input, output) {
  # Create and return line chart of Page two
  output$income_plot <- renderPlotly({
    plot_chart(!!input$rb_chosen)
  })

  # Create and return line chart of Page Three
  output$compare_plot <- renderPlotly({
    plot_income_percent(!!input$rb_chosen_c)
  })
  
  # Create and return line chart of Page Four
  output$nces_plot <- renderPlotly({
    if (!!input$rb_chosen_nces == "Enroll") {
      plot_nces(Enrollment)
    } else {
      plot_nces(Graduation)
    }
  })
}




# =====================================================================
# Start running the application
# =====================================================================

shinyApp(ui = my_ui, server = my_server)
