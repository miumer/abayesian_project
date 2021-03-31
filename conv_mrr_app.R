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
library(shiny)

logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}

uplift <- function(x,y){
  b_rate = logit2prob(x+y)
  dif = b_rate - logit2prob(x)
  uplift = dif/logit2prob(x)
  return(uplift)
}

ui <- fluidPage(sidebarPanel(
  numericInput(
    inputId="Avisitor",
    label= "Variation A New visitors", value=0),
  numericInput(
    inputId="Bvisitor",
    label= "Variation B New visitors", value=0),
  numericInput(
    inputId="Aconversion",
    label= "Variation A Conversions", value=0),
  numericInput(
    inputId="Bconversion",
    label= "Variation B conversions", value=0),
  actionButton(inputId="go", label= "Analyze Conversion!"),
  fileInput("mrr_file", "Choose a .csv file", accept = ".csv"),
  uiOutput('ui_varA'),
  uiOutput('ui_varB'),
  checkboxInput("log", "Log Transform", TRUE),
  actionButton("assign_vars", label = "Raw Summary!"),
  actionButton("model_mrr", label = "Model avg MRR!")),
  mainPanel(
    htmlOutput("convtab"),
    plotlyOutput("convrdist"),
    plotlyOutput("convdifdist"),
    sliderInput("creds1",
                label = "Choose a credible interval",
                value = 0.68, min = 0.01, max = 0.999),
    tableOutput("raw_tabl"),
    fluidRow(splitLayout(cellWidths = c("50%", "50%"), plotOutput("raw_plt"), plotOutput("compare"))),
    tableOutput("model2_tabl"),
    plotlyOutput("model2_plt_both"),
    plotlyOutput("model2_plt_dif"),
    sliderInput("creds2",
                label = "Choose a credible interval",
                value = 0.68, min = 0.01, max = 0.999)
  )
)

server <- function(input, output) {
  conv_model <- eventReactive(input$go, {
    
    conv_dat <- tibble(variation = c("A", "B"),
           n = c(input$Avisitor, input$Bvisitor),
           conversions =c(input$Aconversion, input$Bconversion))
    withProgress(message = "Modeling in progress. Please wait Mr. Strang ...",{
    fit <- brm(family = binomial,
                        conversions | trials(n) ~ variation,
                        data = conv_dat,
                        iter = 2000,
                        warmup = 500,
                        refresh = 0)})
    
    outperforming <- posterior_samples(fit) %>%
      select(1:2) %>%
      mutate(varA_per = logit2prob(.[,1]) , VarB_per = logit2prob(.[,1]+.[,2])) %>%
      select(varA_per, VarB_per) %>% 
      summarise(a_better = paste0(round((sum(.[,1]>.[,2])/n())*100,2),"%"),
                b_better = paste0(round((sum(.[,2]>.[,1])/n())*100,2),"%")) %>% 
      gather(key = variation, value = `Prob. of outperforming`, a_better, b_better) %>% 
      select(`Prob. of outperforming`) 
    
    conv_tab <- conv_dat %>% 
      cbind(., outperforming) %>%
      mutate(CR = paste0(round((conversions/n)*100,2),"%")) %>%
      mutate(uplift = c("", paste0(round(uplift(posterior_summary(fit)[1,1],posterior_summary(fit)[2,1])*100,2
      ),"%"))) 
    
    conv_dist_tab <- posterior_samples(fit) %>% 
      select(1:2) %>%
      mutate(`Variation A` = logit2prob(.[,1]), `Variation B` = logit2prob(.[,1]+.[,2])) %>%
      select(`Variation A`, `Variation B`) %>% 
      gather(key = "variation", value = "CR", `Variation A`, `Variation B`) 
    
    conv_dif_tab <- posterior_samples(fit) %>%
      select(1:2) %>%
      mutate(varA_per = logit2prob(.[,1]), VarB_per = logit2prob(.[,1]+.[,2])) %>%
      select(varA_per, VarB_per) %>% 
      transmute(dif = (VarB_per-varA_per)/mean(varA_per)) 
    
    conv_dif_dist <- posterior_samples(fit) %>%
      select(1:2) %>%
      mutate(varA_per = logit2prob(.[,1]), VarB_per = logit2prob(.[,1]+.[,2])) %>%
      select(varA_per, VarB_per) %>% 
      transmute(dif = (VarB_per-varA_per)*100)
    
    list(conv_dat, fit, outperforming, conv_tab, conv_dist_tab, conv_dif_tab, conv_dif_dist)
        })
  
    output$convtab <- renderText({conv_model()[[4]] %>% 
        kbl(caption = "Conversion rate statistics") %>% 
        kable_styling()
    })
      
    output$convrdist <- renderPlotly({
      g_conv_both <- conv_model()[[5]] %>% 
        ggplot(aes(x = CR, fill = variation, color = variation)) +
        ggtitle("Conversion (visitor to npc or pc) rates for the variations")+
        geom_density(alpha = 0.7)+
        scale_fill_brewer(palette="Set1")+
        scale_x_continuous(name  = "Conversion Rate", labels = percent)+
        scale_y_continuous(NULL, breaks = NULL) +
        guides(color = FALSE, fill = guide_legend(title=NULL))+
        theme_bw()
      
      ax2 <- list(
        title = "",
        zeroline = FALSE,
        showline = FALSE,
        showticklabels = FALSE,
        ticks = "",
        showgrid = FALSE)
      
      p_conv_both <- ggplotly(g_conv_both, tooltip= c("x", "variation")) %>% 
        layout(yaxis = ax2)
      p_conv_both
      
      })
    
    output$convdifdist <- renderPlotly({
      
      g_conv_dif<- conv_model()[[6]] %>% 
        ggplot(aes(x = dif)) +
        ggtitle("Distribution of difference in conversion rate (visitor to npc or pc)")+
        geom_density(alpha = 0.7, color = "#4DAF4A", fill = "#4DAF4A")+
        scale_x_continuous(name  = "Difference in conversion Rate", labels = percent)+
        scale_y_continuous(NULL, breaks = NULL) +
        guides(color = FALSE, fill = guide_legend(title=NULL))+
        theme_bw()
      
      ax2 <- list(
        title = "",
        zeroline = FALSE,
        showline = FALSE,
        showticklabels = FALSE,
        ticks = "",
        showgrid = FALSE)
      
      p_mod2_dif <- ggplotly(g_conv_dif, tooltip= c("x")) %>% 
        layout(yaxis = ax2) %>% 
        add_segments(x=c(quantile(conv_model()[[7]]$dif, probs = 1-((1-input$creds1)*0.5), names=FALSE),
                         quantile(conv_model()[[7]]$dif, probs = (1-input$creds1)*0.5, names = FALSE)), 
                     xend = c(quantile(conv_model()[[7]]$dif, probs = 1-((1-input$creds1)*0.5), names = FALSE),
                              quantile(conv_model()[[7]]$dif, probs = (1-input$creds1)*0.5, names = FALSE)),
                     y=c(0,0), yend= c(12,12), line=list(color=c("darkgreen", "darkgreen"), width = c(4,4))) 
      p_mod2_dif
      }) 
  
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
    withProgress(message = "Modeling in progress. Please wait Mr. Strang ...",{
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
      
      mean_dif <- mean_samp %>% 
        transmute(dif = B - A)
      
      list(b4.3, npc_dat2, mean_samp, model2_tabl, mean_dif)
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
    g_mod2_dif <- model2()[[5]] %>% 
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
      layout(yaxis = ax2) %>%  
      #shapes = list(
      #list(type = "rect",
      #fillcolor = "green", line = list(color = "red"), opacity = 0.9,
      #x0 = 0, 
      #x1 = 7, xref = "x",
      #y0 = 0, y1 = 12, yref = "y"),
      #list(type = "rect",
      #fillcolor = "green", line = list(color = "green"), opacity = 0.9,
      #x0 = -5, 
      #x1 = -1, xref = "x",
      #y0 = 0, y1 = 12, yref = "y")))
    
    add_segments(type = "rect", x=c(quantile(model2()[[5]]$dif, probs = 1-((1-input$creds2)*0.5), names=FALSE),
                                    quantile(model2()[[5]]$dif, probs = (1-input$creds2)*0.5, names = FALSE)), 
                 xend = c(quantile(model2()[[5]]$dif, probs = 1-((1-input$creds2)*0.5), names = FALSE),
                          quantile(model2()[[5]]$dif, probs = (1-input$creds2)*0.5, names = FALSE)),
                 y=c(0,0), yend= c(12,12), line=list(color=c("darkgreen", "darkgreen"), width = c(4,4))) 
    p_mod2_dif
  })
}

shinyApp(ui = ui, server = server)