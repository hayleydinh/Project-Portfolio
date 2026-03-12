library(shiny)
library(shinydashboard)
library(plotly)
library(dplyr)
library(ggplot2)
library(shinyfullscreen)

##
health = read.csv("health.csv")
exercise_age = dplyr::filter(health, Topic == "Physical Activity Index",
                             Break_Out_Category == "Age Group")
exercise_age = exercise_age[,1:11]
exercise_age$Data_value = as.numeric(exercise_age$Data_value)

exercise_age =  exercise_age %>% group_by(Year, Response, Break_Out) %>%
  summarise(percentage = mean(Data_value, na.rm = TRUE))

exercise_age = exercise_age %>% filter(Response == "Yes")

exercise_mean <- exercise_age %>%
  group_by(Year) %>%
  summarise(mean_percentage = mean(percentage))

exercise_mean_age <- exercise_age %>%
  group_by(Break_Out) %>%
  summarise(mean_percentage = mean(percentage), .groups = "drop")

##
exercise_inc = dplyr::filter(health, Topic == "Physical Activity Index",
                             Break_Out_Category == "Household Income")
exercise_inc = exercise_inc[,1:11]
exercise_inc$Data_value = as.numeric(exercise_inc$Data_value)

exercise_inc =  exercise_inc %>% group_by(Year, Response, Break_Out) %>%
  summarise(percentage = mean(Data_value, na.rm = TRUE))

exercise_inc = exercise_inc %>% filter(Response == "Yes")

exercise_inc = exercise_inc %>%
  mutate(
    Break_Out = case_when(
      Break_Out %in% c(
        "$50,000-$99,999",
        "$100,000-$199,999",
        "$200,000+",
        "$50,000+"
      ) ~ "$50,000+",
      TRUE ~ Break_Out
    )
  ) %>%
  group_by(Year, Response, Break_Out) %>%
  summarise(percentage = mean(percentage, na.rm = TRUE), .groups = "drop")

exercise_inc$Break_Out = factor(exercise_inc$Break_Out,
                                levels = c("Less than $15,000", "$15,000-$24,999",
                                           "$25,000-$34,999", "$35,000-$49,999", "$50,000+"))

exercise_year_inc = exercise_inc %>%
  group_by(Year) %>%
  summarise(mean_percentage = mean(percentage))

exercise_mean_inc = exercise_inc %>%
  group_by(Break_Out) %>%
  summarise(mean_percentage = mean(percentage), .groups = "drop")

##
exercise_gender = dplyr::filter(health, Topic == "Physical Activity Index",
                             Break_Out_Category == "Sex")
exercise_gender = exercise_gender[,1:11]
exercise_gender$Data_value = as.numeric(exercise_gender$Data_value)

exercise_gender =  exercise_gender %>% group_by(Year, Response, Break_Out) %>%
  summarise(percentage = mean(Data_value, na.rm = TRUE))

exercise_gender = exercise_gender %>% filter(Response == "Yes")

exercise_gender_year = exercise_gender %>%
  group_by(Year) %>%
  summarise(mean_percentage = mean(percentage))

exercise_mean_gender = exercise_gender %>%
  group_by(Break_Out) %>%
  summarise(mean_percentage = mean(percentage), .groups = "drop")

##
plot1 = ggplot(exercise_age, aes(x = factor(Year), y = percentage, fill = Break_Out)) +
  geom_bar(
    stat = "identity",
    linewidth = 0.2,
    position = position_dodge(width = 0.9)
  ) +
  geom_line(
    data = exercise_mean,
    aes(x = factor(Year), y = mean_percentage, group = 1),
    color = "#D55E00",
    linewidth = 0.9,
    inherit.aes = FALSE
  ) +
  geom_point(
    data = exercise_mean,
    aes(x = factor(Year), y = mean_percentage),
    color = "#D55E00",
    size = 2.5,
    inherit.aes = FALSE
  ) +
  scale_fill_brewer(palette = "YlGnBu", direction = -1) +
  labs(
    title = "Physical Activity Index by Age Group",
    x = "Year",
    y = "Percentage",
    fill = "Age Group"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    panel.grid.major.x = element_blank()
  )

##
plot2 = ggplot(exercise_mean_age, aes(x = Break_Out, y = mean_percentage, fill = Break_Out)) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "YlGnBu", direction = -1) +
  labs(
    title = "Average Physical Activity Index by Age Group",
    x = "Age Group",
    y = "Mean Percentage",
    fill = "Age Group"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    panel.grid.major.x = element_blank()
  )

##
vegetable_age = dplyr::filter(health, Topic == "Vegetable Consumption",
                              Break_Out_Category == "Age Group")
vegetable_age = vegetable_age[,1:11]
vegetable_age$Data_value = as.numeric(vegetable_age$Data_value)

vegetable_age =  vegetable_age %>% group_by(Year, Response, Break_Out) %>%
  summarise(percentage = mean(Data_value, na.rm = TRUE))

vegetable_age_summary = vegetable_age %>%
  group_by(Year, Response) %>%
  summarise(mean_percentage = mean(percentage), .groups = "drop")

##
vegetable_inc = dplyr::filter(health, Topic == "Vegetable Consumption",
                              Break_Out_Category == "Household Income")
vegetable_inc = vegetable_inc[,1:11]
vegetable_inc$Data_value = as.numeric(vegetable_inc$Data_value)

vegetable_inc =  vegetable_inc %>% group_by(Year, Response, Break_Out) %>%
  summarise(percentage = mean(Data_value, na.rm = TRUE))

vegetable_inc = vegetable_inc %>%
  mutate(
    Break_Out = case_when(
      Break_Out %in% c(
        "$50,000-$99,999",
        "$100,000-$199,999",
        "$200,000+",
        "$50,000+"
      ) ~ "$50,000+",
      TRUE ~ Break_Out
    )
  ) %>%
  group_by(Year, Response, Break_Out) %>%
  summarise(percentage = mean(percentage, na.rm = TRUE), .groups = "drop")

vegetable_inc_summary = vegetable_inc %>%
  group_by(Year, Response) %>%
  summarise(mean_percentage = mean(percentage), .groups = "drop")
##
vegetable_gender = dplyr::filter(health, Topic == "Vegetable Consumption",
                              Break_Out_Category == "Sex")
vegetable_gender = vegetable_gender[,1:11]
vegetable_gender$Data_value = as.numeric(vegetable_gender$Data_value)

vegetable_gender =  vegetable_gender %>% group_by(Year, Response, Break_Out) %>%
  summarise(percentage = mean(Data_value, na.rm = TRUE))

vegetable_gender_summary = vegetable_gender %>%
  group_by(Year, Response) %>%
  summarise(mean_percentage = mean(percentage), .groups = "drop")

##
plot3 = ggplot() +
  geom_area(
    data = vegetable_age_summary,
    aes(x = Year, y = mean_percentage, fill = Response),
    alpha = 0.5
  ) +
  geom_line(
    data = filter(vegetable_age, Response == "One or more times per day"),
    aes(x = Year, y = percentage, color = Break_Out, group = Break_Out),
    size = 1.1,
    alpha = 0.8
  ) +
  geom_point(
    data = filter(vegetable_age, Response == "One or more times per day"),
    aes(x = Year, y = percentage, color = Break_Out, group = Break_Out)
  )+
  scale_fill_manual(
    values = c(
      "One or more times per day" = "#009E73",      # Bluish green
      "Less than one time per day" = "#56B4E9"     # Sky blue
    )
  ) +
  labs(
    title = "Daily Vegetable Consumption by Age Group",
    x = "Year",
    y = "Percentage",
    fill = "Response",
    color = "Age Group"
  ) +
  scale_x_continuous(breaks = unique(vegetable_age$Year)) +
  theme(
    legend.position = "right",
  ) +
  scale_color_brewer(palette = "Dark2") 

##
plot4 = ggplot(exercise_inc, aes(x = factor(Year), y = percentage, fill = Break_Out)) +
  geom_bar(
    stat = "identity",
    linewidth = 0.2,
    position = position_dodge(width = 0.9)
  ) +
  geom_line(
    data = exercise_year_inc,
    aes(x = factor(Year), y = mean_percentage, group = 1),
    color = "#D55E00",
    linewidth = 0.9,
    inherit.aes = FALSE
  ) +
  geom_point(
    data = exercise_year_inc,
    aes(x = factor(Year), y = mean_percentage),
    color = "#D55E00",
    size = 2.5,
    inherit.aes = FALSE
  ) +
  scale_fill_brewer(palette = "YlGnBu", direction = -1) +
  labs(
    title = "Physical Activity Index by Household Income",
    x = "Year",
    y = "Percentage",
    fill = "Household Income"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    panel.grid.major.x = element_blank()
  )

##
plot5 = ggplot(exercise_mean_inc, aes(x = Break_Out, y = mean_percentage, fill = Break_Out)) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "YlGnBu", direction = -1) +
  labs(
    title = "Average Physical Activity Index by Household Income",
    x = "Income",
    y = "Mean Percentage",
    fill = "Income"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(),
    panel.grid.major.x = element_blank()
  ) +
  coord_flip()

#
plot6 = ggplot() +
  geom_area(
    data = vegetable_inc_summary,
    aes(x = Year, y = mean_percentage, fill = Response),
    alpha = 0.5
  ) +
  geom_line(
    data = filter(vegetable_inc, Response == "One or more times per day"),
    aes(x = Year, y = percentage, color = Break_Out, group = Break_Out),
    size = 1.1,
    alpha = 0.8
  ) +
  geom_point(
    data = filter(vegetable_inc, Response == "One or more times per day"),
    aes(x = Year, y = percentage, color = Break_Out, group = Break_Out)
  )+
  scale_fill_manual(
    values = c(
      "One or more times per day" = "#009E73",      # Bluish green
      "Less than one time per day" = "#56B4E9"     # Sky blue
    )
  ) +
  labs(
    title = "Daily Vegetable Consumption by Household Income",
    x = "Year",
    y = "Percentage",
    fill = "Response",
    color = "Household Income"
  ) +
  scale_x_continuous(breaks = unique(vegetable_inc$Year)) +
  theme(
    legend.position = "right",
  ) +
  scale_color_brewer(palette = "Dark2")

##
plot7 = ggplot(exercise_gender, aes(x = factor(Year), y = percentage, fill = Break_Out)) +
  geom_bar(
    stat = "identity",
    linewidth = 0.2,
    position = position_dodge(width = 0.9)
  ) +
  geom_line(
    data = exercise_gender_year,
    aes(x = factor(Year), y = mean_percentage, group = 1),
    color = "#D55E00",
    linewidth = 0.9,
    inherit.aes = FALSE
  ) +
  geom_point(
    data = exercise_gender_year,
    aes(x = factor(Year), y = mean_percentage),
    color = "#D55E00",
    size = 2.5,
    inherit.aes = FALSE
  ) +
  scale_fill_brewer(palette = "YlGnBu", direction = -1) +
  labs(
    title = "Physical Activity Index by Gender",
    x = "Year",
    y = "Percentage",
    fill = "Gender"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    panel.grid.major.x = element_blank()
  )

##
plot8 = ggplot(exercise_mean_gender, aes(x = Break_Out, y = mean_percentage, fill = Break_Out)) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "YlGnBu", direction = -1) +
  labs(
    title = "Average Physical Activity Index by Gender",
    x = "Gender",
    y = "Mean Percentage",
    fill = "Gender"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    panel.grid.major.x = element_blank()
  )

##
plot9 = ggplot() +
  geom_area(
    data = vegetable_gender_summary,
    aes(x = Year, y = mean_percentage, fill = Response),
    alpha = 0.5
  ) +
  geom_line(
    data = filter(vegetable_gender, Response == "One or more times per day"),
    aes(x = Year, y = percentage, color = Break_Out, group = Break_Out),
    size = 1.1,
    alpha = 0.8
  ) +
  geom_point(
    data = filter(vegetable_gender, Response == "One or more times per day"),
    aes(x = Year, y = percentage, color = Break_Out, group = Break_Out)
  )+
  scale_fill_manual(
    values = c(
      "One or more times per day" = "#009E73",      # Bluish green
      "Less than one time per day" = "#56B4E9"     # Sky blue
    )
  ) +
  labs(
    title = "Daily Vegetable Consumption by Gender",
    x = "Year",
    y = "Percentage",
    fill = "Response",
    color = "Gender"
  ) +
  scale_x_continuous(breaks = unique(vegetable_age$Year)) +
  theme(
    legend.position = "right",
  ) +
  scale_color_brewer(palette = "Dark2") 

##
ui = dashboardPage(
  dashboardHeader(title = "How do different factors impact healthy habits?",
                  titleWidth = 420),
  dashboardSidebar(width = 420,
                   sidebarMenu(
                     id = "pages",
                     menuItem("Age", tabName = "age"),
                     menuItem("Household Income", tabName = "income"),
                     menuItem("Gender", tabName = "gender")
                   )),
  dashboardBody(
    tags$head(
      tags$style(HTML("
      html, body {
        overflow: hidden;
        height: 100%;
        margin: 0;
        padding: 0;
      }

      .content-wrapper, .right-side {
        width: 1920px !important;
        height: 1080px !important;
        overflow: hidden;
        margin: 0 auto;
        transform-origin: top center;
      }

      .content {
        padding: 0 !important;
        margin: 0 !important;
      }

      .box {
        margin: 5px !important;
      }
    "))
    ),
    fluidRow(
      column(
        width = 12,
        tags$p(
          "Data Source: (Centers for Disease Control and Prevention [CDC], 2023)",
          style = "text-align: center; 
                 font-size: 13px; 
                 color: #666; 
                 margin-bottom: 10px; 
                 margin-top: -10px;"
        )
      )
    ),
    tabItems(
      tabItem(
        "age",
        fluidRow(
          box(plotlyOutput('plot2', height = "330px")),
          box(plotlyOutput('plot3', height = "330px"))
        ),
        fluidRow(
          box(width = 8, plotlyOutput('plot1', height = "330px")),
          box(height = "355px",
            width = 4,
            title = "How is your group doing?",
            selectInput(
              "select_age",
              "Select your age group:",
              choices = c("18-24", "25-34", "35-44", "45-54", "55-64", "65+"),
              selected = "25-34"
            ),
            valueBoxOutput("activity_box", width = 18),
            valueBoxOutput("veg_box", width = 18)
          )
        )
      ),
      tabItem(
        "income",
        fluidRow(
          box(plotlyOutput('plot4', height = "340px")),
          box(plotlyOutput('plot5', height = "340px"))
        ),
        fluidRow(
          box(width = 8, plotlyOutput('plot6', height = "340px")),
          box(height = "340px",
          width = 4,
          title = "How is your group doing?",
          selectInput(
            "select_inc",
            "Select your income:",
            choices = c("Less than $15,000", "$15,000-$24,999","$25,000-$34,999", "$35,000-$49,999", "$50,000+"),
            selected = "Less than $15,000"
          ),
          valueBoxOutput("activity_box2", width = 18),
          valueBoxOutput("veg_box2", width = 18)
        )
        )
      ),
      tabItem(
        "gender",
        fluidRow(
          box(plotlyOutput('plot7', height = "340px")),
          box(plotlyOutput('plot8', height = "340px"))
        ),
        fluidRow(
          box(width = 8, plotlyOutput('plot9', height = "340px")),
          box(height = "340px",
            width = 4,
            title = "How is your group doing?",
            selectInput(
              "select_gender",
              "Select your gender:",
              choices = c("Female", "Male"),
              selected = "Female"
            ),
            valueBoxOutput("activity_box3", width = 18),
            valueBoxOutput("veg_box3", width = 18)
          )
      )
    )
  )))

server = function(input, output) {
  output$plot1 = renderPlotly({
    ggplotly(plot1, tooltip = c("x", "y", "fill"))
  })
  
  output$plot2 = renderPlotly({
    ggplotly(plot2, tooltip = c("x", "y", "fill"))
  })
  
  output$plot3 = renderPlotly({
    ggplotly(plot3, tooltip = c("x", "y", "color")) %>%
      layout(legend = list(title = list(text = "Legend")))
  })
  
  output$plot4 = renderPlotly({
    ggplotly(plot4, tooltip = c("x", "y", "fill"))
  })
  
  output$plot5 = renderPlotly({
    ggplotly(plot5, tooltip = c("x", "y", "fill"))
  })
  
  output$plot6 = renderPlotly({
    ggplotly(plot6, tooltip = c("x", "y")) %>%
      layout(legend = list(title = list(text = "Legend")))
  })
  
  output$plot7 = renderPlotly({
    ggplotly(plot7, tooltip = c("x", "y", "fill"))
  })
  
  output$plot8 = renderPlotly({
    ggplotly(plot8, tooltip = c("x", "y", "fill"))
  })
  
  output$plot9 = renderPlotly({
    ggplotly(plot9, tooltip = c("x", "y")) %>%
      layout(legend = list(title = list(text = "Legend")))
  })
  
  output$activity_box = renderValueBox({
    
    # Filter your dataset based on selection
    selected_physical = exercise_age %>%
      filter(Break_Out == input$select_age)
    
    avg_physical = mean(selected_physical$percentage, na.rm = TRUE)
    
    valueBox(
      value = paste0(round(avg_physical, 1), "%"),
      subtitle = paste("Of people in your group have enough exercise"),
      icon = icon("heartbeat"),
      color = if (avg_physical >= 70) "green" else if (avg_physical >= 50) "yellow" else "blue"
    )
  })
  
  output$activity_box2 = renderValueBox({
    
    selected_physical2 = exercise_inc %>%
      filter(Break_Out == input$select_inc)
    
    avg_physical2 = mean(selected_physical2$percentage, na.rm = TRUE)
    
    valueBox(
      value = paste0(round(avg_physical2, 1), "%"),
      subtitle = paste("Of people in your group have enough exercise"),
      icon = icon("heartbeat"),
      color = if (avg_physical2 >= 70) "green" else if (avg_physical2 >= 50) "yellow" else "blue"
    )
  })
  
  output$activity_box3 = renderValueBox({
    
    selected_physical3 = exercise_gender %>%
      filter(Break_Out == input$select_gender)
    
    avg_physical3 = mean(selected_physical3$percentage, na.rm = TRUE)
    
    valueBox(
      value = paste0(round(avg_physical3, 1), "%"),
      subtitle = paste("Of people in your group have enough exercise"),
      icon = icon("heartbeat"),
      color = if (avg_physical3 >= 70) "green" else if (avg_physical3 >= 50) "yellow" else "blue"
    )
  })
  
  output$veg_box = renderValueBox({
  selected_veg = vegetable_age %>%
    filter(Break_Out == input$select_age,
           Response == "One or more times per day")
  
  avg_veg = mean(selected_veg$percentage, na.rm = TRUE)
  
  valueBox(
    value = paste0(round(avg_veg, 1), "%"),
    subtitle = paste("Of people in your group eat enough vegetables"),
    icon = icon("carrot"),
    color = if (avg_veg >= 70) "green" else if (avg_veg >= 50) "yellow" else "blue"
   )
})
  
  output$veg_box2 = renderValueBox({
    selected_veg2 = vegetable_inc %>%
      filter(Break_Out == input$select_inc,
             Response == "One or more times per day")
    
    avg_veg2 = mean(selected_veg2$percentage, na.rm = TRUE)
    
    valueBox(
      value = paste0(round(avg_veg2, 1), "%"),
      subtitle = paste("Of people in your group eat enough vegetables"),
      icon = icon("carrot"),
      color = if (avg_veg2 >= 70) "green" else if (avg_veg2 >= 50) "yellow" else "blue"
    )
  })
  
  output$veg_box3 = renderValueBox({
    selected_veg3 = vegetable_gender %>%
      filter(Break_Out == input$select_gender,
             Response == "One or more times per day")
    
    avg_veg3 = mean(selected_veg3$percentage, na.rm = TRUE)
    
    valueBox(
      value = paste0(round(avg_veg3, 1), "%"),
      subtitle = paste("Of people in your group eat enough vegetables"),
      icon = icon("carrot"),
      color = if (avg_veg3 >= 70) "green" else if (avg_veg3 >= 50) "yellow" else "blue"
    )
  })
}

shinyApp(ui, server)