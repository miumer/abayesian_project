library(tidyverse)
library(shiny)
library(kableExtra)
library(ggplot2)
library(brms)
ui <- fluidPage(sidebarPanel(
                  fileInput("mrr_file", "Choose a .csv file", accept = ".csv"),
                  checkboxInput("header", "Header true", TRUE),
                  textInput("varA", label = "Variation A label", value = ""),
                  textInput("varB", label = "Variation B label", value = ""),
                  checkboxInput("log", "Log Transform", TRUE),
                  actionButton("assign_vars", label = "Raw Summary!"),
                  actionButton("mrr_model", label = "Model avg MRR!")),
                mainPanel(
                  tableOutput("raw_tabl"),
                  fluidRow(splitLayout(cellWidths = c("50%", "50%"), plotOutput("raw_plt"), plotOutput("compare")))
                )
              )

server <- function(input, output) {
    npc_dat <- eventReactive(input$assign_vars, {
      req(input$mrr_file)
      npc_dat1 <- read.csv(input$mrr_file$datapath, header = T) %>% 
        select(1,2) %>% 
        mutate(variation = recode(variation, !!sym(input$varA) := "A", !!(input$varB) := "B")) %>% 
        rename(firstmrr = 2)
      npc_dat2 <- npc_dat1 %>%  
        mutate(A = as.numeric(as.character(factor(variation, labels=c(1,0))))) %>% 
        mutate(B = 1-A)
      list(npc_dat1, npc_dat2)
    })
    
    sample <- eventReactive(input$mrr_model,{
      withProgress(message = "Modeling in progress. Please wait...",{
        model2 <- brm(data = npc_dat()[[2]], family = shifted_lognormal(link_sigma = "identity"),
                 bf(firstmrr ~ 0 + A + B, sigma ~ 0 + A + B), #sigma is modeled as also depending on variation
                 #prior = c(prior(student_t(3,0,1), class = b)), #Look into  more informative priors for accuracy managment
                 control = list(adapt_delta = 0.95),
                 iter = 3000, warmup = 600, chains = 4, cores = 4, #Look into this for managing computing time 
                 seed = 4)
        post_pred <- as.data.frame(t(posterior_predict(object = model2, 
                                                       newdata = npc_dat()[[2]][,3:4], nsamples = 1)))
        npc_dat2 <- cbind(npc_dat()[[1]], post_pred)
        npc_dat2
      })
   })
      
    output$raw_tabl = function(){npc_dat()[[1]] %>%
      #filter(.[[2]] < 300) %>% #Possible to put in filter for removing outliers. Look into making optional? 
      group_by(variation) %>% 
      summarise(mean = mean(firstmrr),
                SD = sd(firstmrr),
                max = max(firstmrr),
                min= min(firstmrr),
                Q25 = quantile(firstmrr, prob = c(.25)),
                median = median(firstmrr),
                Q75 = quantile(firstmrr, prob = c(.75))) %>% 
      kbl(caption="Overview of raw MRR data") %>% 
      kable_styling(
    )}
  
    output$raw_plt = renderPlot({
      raw_plt <- npc_dat()[[1]] %>% 
      ggplot(aes(x = as.factor(variation), y = firstmrr, fill = as.factor(variation))) +
      ggtitle("Distribution of log-transformed raw MRR data")+
      geom_violin(trim=FALSE, alpha =0.5)+
      geom_jitter(shape=10, position=position_jitter(0.05), color = "#7570B3")+
      geom_boxplot(width=0.15, fill = "white", alpha = 0.5, color = "black" )+
      scale_fill_brewer(palette="Dark2")+
      scale_x_discrete(name = "Variation")+
      scale_y_continuous(name = "MRR", n.breaks = 20)+
      guides(fill=guide_legend(title = "Variations"))
      
    if(input$log)
      raw_plt <- npc_dat()[[1]] %>% 
      ggplot(aes(x = as.factor(variation), y = log(firstmrr), fill = as.factor(variation))) +
      ggtitle("Distribution of log-transformed raw MRR data")+
      geom_violin(trim=FALSE, alpha =0.5)+
      geom_jitter(shape=10, position=position_jitter(0.05), color = "#7570B3")+
      geom_boxplot(width=0.15, fill = "white", alpha = 0.5, color = "black" )+
      scale_fill_brewer(palette="Dark2")+
      scale_x_discrete(name = "Variation")+
      scale_y_continuous(name = "MRR", n.breaks = 20)+
      guides(fill=guide_legend(title = "Variations"))
    return(raw_plt)
  })
    
      output$compare <- renderPlot({sample() %>% 
          ggplot(aes(x = as.factor(variation), y = log(.[[3]]), fill = as.factor(variation))) +
          ggtitle("Distribution of model predicted data \n (log-transformed)")+
          geom_violin(trim=FALSE, alpha =0.5)+
          geom_jitter(shape=10, position=position_jitter(0.05), color = "#7570B3")+
          geom_boxplot(width=0.15, fill = "white", alpha = 0.5, color = "black" )+
          scale_fill_brewer(palette="Dark2")+
          scale_x_discrete(name = "Variation")+
          scale_y_continuous(name = "MRR", n.breaks = 20, limits = ggplot_build(raw_dist)$layout$panel_scales_y[[1]]$range$range)+
          guides(fill=guide_legend(title = "Variations"))
   })
    
}

shinyApp(ui = ui, server = server)