# This project was done individually by Samarth Majmudar (net id: smajm)

# Project Introduction:
# This project takes two of my favorite topics and combines them: Football and Politics!
# This project aims to examine the relationship between NFL team fandoms and political leaning.
# The data is from the FiveThirtyEight library and comes from a survey conducted by survey monkey.
# Note: the data was a survey and represents a very small sample size.
# The data is organized by fandom, race, and political leaning.
# The rows are an NFL team.
# The columns are a race-political leaning combination.
# Example columns include Black Democrat or Asian Independent or White Republican
# There are also columns with the overall percentage of a party's support.
# There is also a row called "Grand Total" which has the combined statistics for everyone in the survey.
# The first part of this project aims to predict someone's political leaning based on their favorite NFL team and race. 
# A user will input their favorite NFL team and race and it will try to predict the odds of that person being a Democrat using logistic regression.
# Note: Since there is an independent category, the percentage of Democrat (and Republican) may seem low. That is because of most identify as an Independent.
# Since we are using logistic regression, I just made it try to predict if they are a Democrat or not instead of trying to predict a political party.
# The second part of the project is trying to see if the proportion of Democrats is statistically significant between two fan bases.
# I used a 2 proportion z test to do this.
# A user will input two NFL teams and we will perform a 2 proportion z test to see if there is a statistically significant difference between the proportion of Democrats between two teams.
# You can also use the Grand Total option to compare the average NFL fan base with another team.
# Finally, there is a big visualization of a bar chart that orders teams from most Democratic to least Democratic.
# To make the graph look visually pleasing, I have added a color gradient that goes from blue to red depending on how Democratic a team is.
# I've also used ploty to make it more interactive.


# What can it be used for?:
# The first part (where we predict someone's political leaning) is a fun tool that tries to predict is one is a Democrat or not.
# It can be used to examine how someone's political leaning is affected by their race and NFL fandom.
# It's also a fun way for people to test out if the model predicts their political leanings correctly.
# The second portion (testing if fan bases politically differ) can be used for many different purposes.
# This can be used by political scientists to see if there are any underlying differences in the politics of two teams are different and then examine the reasons why they might be different.
# For example a political scientist can ask "are the Buffalo Bills fans are the least Democratic because Buffalo is a relatively small city" and then look at other small cities to determine if it is due to city size.
# If political parties want to run ad campaigns during an NFL game, they can use this tool to see if there is a statistically significant difference between running an ad campaign at one game or another.
# It can better help them identify what games to run ads on.
# The same can be said about the visualization. 
# It shows which teams are the most to least Democratic leaning.
# Political parties and candidates can use it to find what NFL games to target their ad campaigns.


library(shiny)
library(tidyverse)
library(fivethirtyeight)
library(plotly)

# Loading the data set
data <- fivethirtyeight::nfl_fandom_surveymonkey

# This gets us the total number of fans for each race for each team.
data <- data %>%
  mutate(
    white_total = white_dem + white_ind + white_gop,
    black_total = black_dem + black_ind + black_gop,
    asian_total = asian_dem + asian_ind + asian_gop,
    hispanic_total = hispanic_dem + hispanic_ind + hispanic_gop,
    other_total = other_dem + other_ind + other_gop
  )

# Here we are selecting the data we need.
# Since we only need total respondents, democrats per team, 
# and white vs. non-white population we only keep those pieces of data. 
# Then just to make it easier, we change "white_total" and "black_total" to just "white" and "black to make the code more readable. We do this for all races.
small_data <- data %>%
  pivot_longer(
    cols = c(white_dem, black_dem, asian_dem, hispanic_dem, other_dem),
    names_to = "race",
    values_to = "dem_count"
  ) %>%
  mutate(
    race = str_replace(race, "_dem", ""),
    race_total = case_when(
      race == "white" ~ white_total,
      race == "black" ~ black_total,
      race == "asian" ~ asian_total,
      race == "hispanic" ~ hispanic_total,
      race == "other" ~ other_total
    ),
    non_dem = race_total - dem_count
  )

# Let's fit our logistic regression model!
# This model should predict if a person is a Democrat or not based on their favorite NFL team and race.
model <- glm(
  cbind(dem_count, non_dem) ~ team + race,
  data = small_data,
  family = binomial
)


# Here we create our UI.
# For our side panel, we are interactively making our logistic regression predictor.
# Using a selection input (drop down) click on your favorite NFL team and using a radio button click on your race.
# Then click the "Predict Probability" button to see what are the chances that the model predicts you to be a Democrat.
# Also in our side panel, we have another interactive feature.
# Using another drop down feature, compare two NFL teams and see if their fan bases are politically statistically different.


ui <- fluidPage(
  titlePanel("NFL Fandom & Political Leaning"),
  
  sidebarLayout(
    sidebarPanel(
      h3("Logistic Regression Prediction"),
      selectInput("team_input", "Favorite Team:",
                  choices = unique(small_data$team)),
      radioButtons("race_input", "Race:",
                   choices = unique(small_data$race)),
      actionButton("predict_btn", "Predict Probability"),
      
      hr(),
      
      h3("Team Comparison Test"),
      selectInput("team1", "Team 1:",
                  choices = unique(data$team)),
      selectInput("team2", "Team 2:",
                  choices = unique(data$team)),
      actionButton("test_btn", "Run Test")
    ),
    
    mainPanel(
      h3("Percent Of Being A Democrat"),
      verbatimTextOutput("prediction"),
      
      h3("Statistical Test Result"),
      verbatimTextOutput("test_result"),
      
      h3("Teams Proportion of Democrats"),
      plotlyOutput("barplot")
    )
  )
)


server <- function(input, output) {
  
  # Here is where we will attempt to predict the likelihood of someone leaning Democratic.
  # We collect the user's input in new_data. 
  # Then we predict the probability of someone being a Democrat and paste the output.
  observeEvent(input$predict_btn, {
    
    new_data <- data.frame(
      team = input$team_input,
      race = input$race_input
    )
    
    prob <- predict(model, newdata = new_data, type = "response")
    
    output$prediction <- renderText({
      paste0("Estimated probability of leaning Democrat: ",
             sprintf("%.1f", prob * 100), "%")
    })
  })
  
  # For this next section, we will create a 2 proportion z test.
  # First we gather the two teams that were selected.
  # Next we gather the number of Democrats and non-Democrats and calculate the Dem rate.
  # Then we perform a 2 prop z test. 
  # Finally we check if the outcome was statistically significant with a alpha value of 0.05 and then paste the result.
  
  # Statistical Analysis: 
  # If the p-value is 0.05 or higher, we say that there is not a statistically significance difference in the proportion of Democrats.
  # If the p-value is 0.05 or lower, we say that there is a statistically significant difference in the proportion of Democrats.
  observeEvent(input$test_btn, {
    
    team1 <- data %>% filter(team == input$team1)
    team2 <- data %>% filter(team == input$team2)
    
    Dems <- c(team1$total_dem, team2$total_dem)
    total <- c(team1$total_respondents, team2$total_respondents)
    
    test <- prop.test(Dems, total)
    
    output$test_result <- renderText({
      if (test$p.value < 0.05) {
        paste("Statistically significant difference (p-value =",
              round(test$p.value, 4), ")")
      } else {
        paste("Not statistically significant (p-value =",
              round(test$p.value, 4), ")")
      }
    })
  })
  
  # Here is where we make a bar chart that has the least Democratic teams on the bottom and most Democratic teams on the top.
  # The color of the bar chart also changes with the least Democratic fan bases in red and most Democratic fan bases in blue, with teams in the middle being in purple.
  # I've also used Ploty to make the graph more interactive.
  
  # Explanation of the plot:
  # The most Democratic leaning teams are on the top and are in blue or dark purple.
  # The least Democratic leaning teams are on the bottom and they are in red or a reddish-purple.
  # What's super interesting is that this list feels somewhat random.
  # It seems like cities that lean really Democratic the most Democratic like San Francisco, New York, and Philadelphia.
  # However, there are many cities that don't conform to that idea like Houston which is super Democratic, but is one of the least Democratic in this survey.
  # Perhaps it has more to do with the surrounding area as New York City is super urban meaning that most people who live near NYC support the Democrats.
  # However, many people who live in upstate New York and are more Republican root for the Buffalo Bills.
  # I suppose that the same might be true for Houston as it probably has a lot of fans in rural Texas.
  # What makes this data super interesting is the randomness of it and additional tests need be done to better understand this data.
  
  output$barplot <- renderPlotly({
    
    team_summary <- data %>%
      mutate(dem_rate = total_dem / total_respondents) %>%
      arrange(dem_rate)
    
    p <- ggplot(team_summary,
                aes(x = reorder(team, dem_rate),
                    y = dem_rate,
                    text = paste0(
                      "Team: ", team,
                      "<br>Percent Democrat: ", round(dem_rate * 100, 1), "%"
                    ))) +
      geom_bar(aes(fill = dem_rate), stat = "identity") +
      scale_fill_gradient(low = "red", high = "blue") +
      coord_flip() +
      labs(
        x = "Team",
        y = "Proportion Democrat",
        title = "NFL Team Political Leaning"
      ) +
      theme_minimal()
    
    ggplotly(p, tooltip = "text")
  })
}

shinyApp(ui = ui, server = server)