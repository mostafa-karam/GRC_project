# import library i will use
# --------------------------
  library(dplyr)
  library(ggplot2)
  library(arules)
  library(shiny)
# -------------------------------------------------------------------
# import and clean the data set
# -----------------------------
  grc_data_frame <- data.frame(read.csv("grc.csv"))
  sum(duplicated(grc_data_frame))
  grc_data_frame = distinct(grc_data_frame)
  sum(duplicated(grc_data_frame))
  sum(is.na(grc_data_frame))
# -------------------------------------------------------------------
# change the structure of my data
# ---------------------------------
  grc_data_frame$count = as.numeric(grc_data_frame$count)
  
  grc_data_frame$total = as.numeric(grc_data_frame$total)
  
  grc_data_frame$age = as.numeric(grc_data_frame$age)
  
  grc_data_frame$rnd = as.numeric(grc_data_frame$rnd)
# -------------------------------------------------------------------
# kmeans
# ------
  x <- TRUE
  while(x){
    number_group = readline("pls enter the number of groups(between 2,4): ")
    number_group = as.numeric(number_group)
    if (number_group >=2 && number_group <= 4) {
      print(paste("the number of group you want is",number_group))
      # Create a new data frame for clustering
      cluster_df <- data.frame(grc_data_frame$age, grc_data_frame$total)
      
      # Perform k-means clustering
      kmeans_result <- kmeans(cluster_df, centers=number_group)
      new_df <- data.frame(grc_data_frame$customer,grc_data_frame$age,
                           grc_data_frame$total,kmeans_result$cluster)
      x <- FALSE
    }else{
      print("PLS try agin")
    }
  }
# -------------------------------------------------------------------
# Association rules 
# -----------------
  df <-data.frame(grc_data_frame$items)
  transactions <- as(df, "transactions")
  x <- TRUE
  while(x){
    support_no = readline("pls enter the support No: ")
    support_no = as.numeric(support_no)
    if (support_no >=0.001 && support_no <= 1) {
      print(paste("the suport number is",support_no))
      x <- FALSE
    }else{
      print("PLS try agin")
    }
  }
  y <- TRUE
  while(y){
    confidence_no = readline("pls enter the confidence No: ")
    confidence_no = as.numeric(confidence_no)
    if (confidence_no >=0.001 && confidence_no <= 1) {
      print(paste("the confidence number is",confidence_no))
      y <- FALSE
    }else{
      print("PLS try agin")
    }
  }
  apriori_rules <- apriori(transactions,
                           parameter = list(supp = support_no,
                                            conf = confidence_no))
# -------------------------------------------------------------------
# The user interfaces to enter the required data. 
# -----------------------------------------------
  # Define UI
  # ----------
    ui <- fluidPage(
      titlePanel("Analytic Dashboard"),
      sidebarLayout(
        sidebarPanel(
            # Drop down selection for Data Visualization:
            # -------------------------------------------
            selectInput(inputId = "UserInput", label = "Choose Plot:",
                        choices = c("Null" ="N",
                                    "Cash and credit totals." = "C", 
                                    "Age and sum of total spending." = "A", 
                                    "City total spending ." = "B",
                                    "Distribution of total spending."="D")),
            numericInput("num", "number of Groups",
                         value = 2,min = 2,max = 4)

        ),
        mainPanel(plotOutput("histogram"))
      )
     )
  # Define server logic
  # -------------------
    server <- function(input, output) {
      output$histogram <- renderPlot({
        
        if(input$UserInput == "C"){
          # Compare cash and credit totals.
          # -------------------------------
            x= table(grc_data_frame$paymentType)
            percentage=paste0(round(100*x/sum(x)),"%")
            pie(x,labels = percentage,
                main =  "Compare cash and credit totals. ",
                col=c("pink","lightblue"))
            legend("bottomleft",
                   legend = c("cash", "credit"), 
                   fill = c("pink", "lightblue"))
        } else if(input$UserInput == "A"){
          # Age and sum of total spending.
          # ------------------------------ 
            ggplot(grc_data_frame, aes(x = age, y = total)) +
              geom_bar(stat = "summary", fun = "sum") +
              labs(title = "Total Spending by Age"
                   ,x = "Age", y = "Total Spending")
        }else if (input$UserInput == "B"){
          # Show each city total spending and arrange it by total descending
          # ----------------------------------------------------------------
            ggplot(grc_data_frame, aes(x = reorder(city,-total),
                                       y = total,fill = city)) +
              geom_bar(stat="identity") +
              scale_y_continuous(labels = scales::comma)+
              theme_bw()+
              labs(title = "Total Spending by city",
                   x = "city", y = "Total Spending")
        }else if(input$UserInput == "D"){
          # Display the distribution of total spending. 
          # -------------------------------------------
          boxplot( x = grc_data_frame$total, 
                   main = "Distribution of total spending.", 
                   xlab = "total spending.")
        }
      })
    }
  # Create Shiny app
  # -----------------
    shinyApp(ui = ui, server = server)
  
