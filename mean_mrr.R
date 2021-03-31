library(tidyverse)
library(brms)
library(magrittr)
library(forcats)
library(tidyr)
library(modelr)
library(ggdist)
library(tidybayes)
library(cowplot)
library(ggrepel)
library(RColorBrewer)
library(bayestestR)
library(scales)
library(kableExtra)
library(plotly)
ui <- fluidPage(sidebarPanel(
                  fileInput("mrr_file", "Choose a .csv file", accept = ".csv"),
                  uiOutput('ui_varA'),
                  uiOutput('ui_varB'),
                  checkboxInput("log", "Log Transform", TRUE),
                  actionButton("assign_vars", label = "Raw Summary!"),
                  actionButton("model_mrr", label = "Model avg MRR!")),
                mainPanel(
                  tableOutput("raw_tabl"),
                  fluidRow(splitLayout(cellWidths = c("50%", "50%"), plotOutput("raw_plt"), plotOutput("compare"))),
                  tableOutput("model2_tabl"),
                  plotlyOutput("model2_plt_both"),
                  plotlyOutput("model2_plt_dif")
                )
              )

server <- function(input, output) {
    variations <- eventReactive(input$mrr_file, {
      req(input$mrr_file)
      var_names <- read.csv(input$mrr_file$datapath, header = T) %>% 
        distinct(variation)
      var_names <- c(var_names)
      var_names
    })
    output$ui_varA <- renderUI({
      selectInput('pick_varA',
                  label ='Pick variation A',
                  choices=variations(),
                  selected = NULL, multiple = FALSE,width="450px", selectize = TRUE)
    })
    output$ui_varB <- renderUI({
      selectInput('pick_varB',
                  label ='Pick variation B',
                  choices=variations(),
                  selected = NULL, multiple = FALSE,width="450px", selectize = TRUE)
    })
    npc_dat <- eventReactive(input$assign_vars, {
      req(input$mrr_file)
      npc_dat <- read.csv(input$mrr_file$datapath, header = T) %>% 
        select(1,2) %>% 
        mutate(variation = recode(variation, !!sym(input$pick_varA) := "A", !!sym(input$pick_varB) := "B")) %>% 
        rename(firstmrr = 2)
      npc_dat
    })
    
    model2 <- eventReactive(input$model_mrr,{
      req(input$mrr_file)
      npc_wide <- read.csv(input$mrr_file$datapath, header = T) %>% 
        select(1,2) %>% 
        mutate(variation = recode(variation, !!sym(input$pick_varA) := "A", !!(input$pick_varB) := "B")) %>% 
        rename(firstmrr = 2) %>% 
        mutate(A = as.numeric(as.character(factor(variation, labels=c(1,0))))) %>% 
        mutate(B = 1-A)
      withProgress(message = "Modeling in progress. Please wait...",{
        b4.3 <- brm(data = npc_wide, family = shifted_lognormal(link_sigma = "identity"),
                 bf(firstmrr ~ 0 + A + B, sigma ~ 0 + A + B), #sigma is modeled as also depending on variation
                 #prior = c(prior(student_t(3,0,1), class = b)), #Look into  more informative priors for accuracy managment
                 control = list(adapt_delta = 0.95),
                 iter = 3000, warmup = 600, chains = 4, cores = 4, #Look into this for managing computing time 
                 seed = 4)
        post_pred <- as.data.frame(t(posterior_predict(object = b4.3, 
                                                       newdata = npc_wide[,3:4], nsamples = 1)))
        npc_dat2 <- cbind(npc_wide[,1:2], post_pred)
        
        post_dist <- as.data.frame(t(posterior_epred(b4.3, nsamples = 6000)))
        mean_samp <- cbind(npc_wide$variation, post_dist) %>%
          distinct() %>%
          pivot_longer(-`npc_wide$variation`) %>%    
          pivot_wider(names_from=`npc_wide$variation`, values_from=value) %>% 
          select(A,B) 
        
        #Calculations with model distribution of means
        mrr_mean <- mean_samp %>%
          pivot_longer(cols = c(A, B), values_to = "means", names_to = "Variation")  %>% 
          group_by(Variation) %>% 
          summarise(`Estimate of mean` = round(mean(means),2),
                    `Est. error of mean` = round(sd(means),2),
                    Q2.5 = round(quantile(means, prob = c(.025)),2),
                    Q97.5 = round(quantile(means, prob = c(.975)),2))
        
        #Calculating probabikity of each variation having higher true mean MRR
        mrr_better <- mean_samp %>%  
          summarise(a_better = paste0(round((sum(.[,1]>.[,2])/n())*100,2),"%"),
                    b_better = paste0(round((sum(.[,2]>.[,1])/n())*100,2),"%")) %>% 
          pivot_longer(cols = c(a_better, b_better), values_to = "Prob. of outperforming (on mean)") %>% 
          select("Prob. of outperforming (on mean)")
        
        model2_tabl <- cbind(mrr_mean, mrr_better)
        
        list(b4.3, npc_dat2, mean_samp, model2_tabl)
      })
   })
    output$raw_tabl = function(){npc_dat() %>%
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
      raw_plt <- npc_dat() %>% 
      ggplot(aes(x = as.factor(variation), y = firstmrr, fill = as.factor(variation))) +
      ggtitle("Distribution of log-transformed \n raw MRR data")+
      geom_violin(trim=FALSE, alpha =1)+ #alpha changed from backend
      geom_jitter(shape=10, position=position_jitter(0.05), color = "darkgoldenrod2")+ #colours changed from backend
      geom_boxplot(width=0.15, fill = "white", alpha = 0.5, color = "black" )+
      scale_fill_brewer(palette="Set1")+
      scale_x_discrete(name = "Variation")+
      scale_y_continuous(name = "MRR", n.breaks = 20)+
      guides(fill=guide_legend(title = "Variations"))+
      theme_bw()
      
    if(input$log)
      raw_plt <- npc_dat() %>% 
      ggplot(aes(x = as.factor(variation), y = log(firstmrr), fill = as.factor(variation))) +
      ggtitle("Distribution of log-transformed \n raw MRR data")+
      geom_violin(trim=FALSE, alpha =1)+ #alpha changed from backend
      geom_jitter(shape=10, position=position_jitter(0.05), color = "darkgoldenrod1")+ #colours changed from backend
      geom_boxplot(width=0.15, fill = "white", alpha = 0.5, color = "black" )+
      scale_fill_brewer(palette="Set1")+
      scale_x_discrete(name = "Variation")+
      scale_y_continuous(name = "MRR", n.breaks = 20)+
      guides(fill=guide_legend(title = "Variations"))+
      theme_bw()
    return(raw_plt)
  })
    
    output$compare <- renderPlot({model2()[[2]] %>% 
        ggplot(aes(x = as.factor(variation), y = log(model2()[[2]][[3]]), fill = as.factor(variation))) +
        ggtitle("Diagnostic sample dist. of model predicted data")+
        geom_violin(trim=FALSE, alpha =1)+ #alpha changed from backend
        geom_jitter(shape=10, position=position_jitter(0.05), color = "darkgoldenrod1")+ #colours changed from backend
        geom_boxplot(width=0.15, fill = "white", alpha = 0.5, color = "black" )+
        scale_fill_brewer(palette="Set1")+
        scale_x_discrete(name = "Variation")+
        scale_y_continuous(name = "MRR", n.breaks = 20, limits = ggplot_build(raw_dist)$layout$panel_scales_y[[1]]$range$range)+
        guides(fill=guide_legend(title = "Variations"))+
        theme_bw()
   })
    output$model2_tabl <- function(){model2()[[4]] %>% 
        kbl(caption = "Table of model implied estimates of mean MRR per PC per variation") %>% 
        kable_styling()
    }
    output$model2_plt_both <- renderPlotly({ #plotly used so stat_slab, stat_pointinterval not used here
     g_mod2_both <- model2()[[3]] %>% 
        gather(key = "variation", value = "mean_mrr", A, B) %>% 
        ggplot(aes(x = mean_mrr, fill = variation , color = variation))+
        ggtitle("Distributions of model implied mean MRR per PC per variation")+
        geom_density(alpha = 0.7)+ #alpha changed from back end
        scale_fill_brewer(palette="Set1")+ #colours changed from back-end code
        scale_x_continuous(name  = "Mean MRR per PC", labels = dollar_format())+
        scale_y_continuous(NULL, breaks = NULL) +
        guides(color = FALSE, fill = guide_legend(title=NULL))+
        theme_bw() 
      
     ax <- list(
       title = "",
       zeroline = FALSE,
       showline = FALSE,
       showticklabels = FALSE,
       ticks = "",
       showgrid = FALSE)
     
      p_mod2_both <- ggplotly(g_mod2_both, tooltip= c("x", "variation")) %>% 
        layout(yaxis = ax)
      p_mod2_both
      
    })
    output$model2_plt_dif <- renderPlotly({ #plotly used so stat_slab, stat_pointinterval not used here
      g_mod2_dif <- model2()[[3]] %>% 
        transmute(dif = B - A) %>% 
        ggplot(aes(x = dif)) +
        ggtitle("Distribution of model implied difference of mean MRR per NPC between variations")+
        geom_density(alpha = 0.7, color = "#4DAF4A", fill = "#4DAF4A")+ #colours changed from back-end code
        scale_y_continuous(NULL, breaks = NULL) +
        scale_x_continuous(name  = "mean MRR per PC of B - mean MRR per PC of A", labels = dollar_format())+
        theme_bw()
      
      ax2 <- list(
        title = "",
        zeroline = FALSE,
        showline = FALSE,
        showticklabels = FALSE,
        ticks = "",
        showgrid = FALSE)
      
      p_mod2_dif <- ggplotly(g_mod2_dif, tooltip= c("x")) %>% 
        layout(yaxis = ax2)
      p_mod2_dif
      })
}

shinyApp(ui = ui, server = server)