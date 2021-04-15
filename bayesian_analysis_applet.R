library(DT)
library(htmlwidgets)
###
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
library(HDInterval)
library(shinyBS)
####
library(shinydashboard)

#Function for transforming logits to probability
logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}

#Uplift calculation function using the back-transformed (with logit2prob) model coefficinets
uplift <- function(x,y){
  b_rate = logit2prob(x+y)
  dif = b_rate - logit2prob(x)
  uplift = dif/logit2prob(x)
  return(uplift)
}

ui <- dashboardPage(
  dashboardHeader(title="A random bayesian analysis tool", titleWidth = 350),
  dashboardSidebar(width = 350,
    menuItem(startExpanded = TRUE,
      text="CR analysis",
      icon = icon("dashboard", lib = "glyphicon"),
  numericInput(
    inputId="Avisitor",
    label= "Variation A New visitors", value = NULL, width = "50%"),
  numericInput(
    inputId="Bvisitor",
    label= "Variation B New visitors", value = NULL, width = "50%"),
  numericInput(
    inputId="Aconversion",
    label= "Variation A Conversions",  value = NULL, width = "50%"),
  numericInput(
    inputId="Bconversion",
    label= "Variation B conversions",  value = NULL, width = "50%"),
  actionButton(inputId="go", label= "Analyze Conversion!")
  ),
  menuItem(startExpanded = TRUE,
           text = "Data entry and raw descriptives",
           icon = icon("stats", lib = "glyphicon"),
  fileInput("mrr_file", "Choose a .csv file", accept = ".csv"),
  uiOutput('ui_varA'),
  uiOutput('ui_varB'),
  #checkboxInput("diag", "Check performance", FALSE),
  actionButton("assign_vars", label = "Give it to me raw!"),
  numericInput(
    inputId="outliers",
    label= "Remove datapoints with $$ over", value = 99999999, width = "50%"),
  checkboxInput("log", "Log transformed", TRUE)
  ),
  actionButton("model_mrr", label = "Model mean customer MRR!"),
  actionButton("model_mrr_and_conv", label = "Model mean visitor MRR!")
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML(".main-sidebar a{ font-size: 20px; }")),
    ),
    tabsetPanel(
      tabPanel(
        "Conversion model",
        p(""),
        box(
        DT::dataTableOutput("convtab"),
        width = "100%",
        collapsible = TRUE),
        box(
        plotlyOutput("convrdist"),
        width = "100%",
        collapsible = TRUE),
        box(
        uiOutput("mean_btn1"),
        plotlyOutput("convdifdist"),
        uiOutput("conv_creds"),
        width = '100%',
        collapsible = TRUE)
      ),
      tabPanel(
      "Raw data",
      p(""),
      box(
      DT::dataTableOutput("raw_tabl"),
      width = '100%',
      collapsible = TRUE),
      box(
      #fluidRow(splitLayout(cellWidths = c("50%", "50%"), plotOutput("raw_plt"), plotOutput("compare"))) Possible diagnostics here
      plotOutput("raw_plt",
                 height = "800px"),
      width = '100%',
      collapsible = TRUE
      )
    ),
      tabPanel(
      "Customer mean MRR model",
      p(""),
      box(
      DT::dataTableOutput("model2_tabl"),
      width = '100%',
      collapsible = TRUE),
      box(
      plotlyOutput("model2_plt_both"),
      width = '100%',
      collapsible = TRUE),
      box(
      uiOutput("mean_btn2"),
      plotlyOutput("model2_plt_dif"),
      uiOutput("mrr_creds"),
      width = '100%',
      collapsible = TRUE
      )
    ),
      tabPanel(
      "Visitor mean MRR model",
      p(""),
      box(
      DT::dataTableOutput("mrr_per_visitor_tab"),
      width = '100%',
      collapsible = TRUE),
      box(
      plotlyOutput("overall_dist"),
      width = '100%',
      collapsible = TRUE),
      box(
      uiOutput("mean_btn"),
      plotlyOutput("overall_dist_dif"),
      uiOutput("mrr_v_creds"),
      width = '100%',
      collapsible = TRUE
      )
    )
    )
  )
  )

server <- function(input, output) {
  conv_model <- eventReactive(input$go, { 
    
    output$conv_creds <- renderUI({ 
      sliderInput("creds1",
                  label = "Choose a credible interval",
                  value = 0.68, min = 0.001, max = 0.999)})
    
    #Data entry for binomial test
    conv_dat <- tibble(variation = c("A", "B"), 
           n = c(input$Avisitor, input$Bvisitor),
           conversions =c(input$Aconversion, input$Bconversion))
    
    #Binomial model
    withProgress(message = "Modeling in progress. Please wait Mr. Strang ...",{
    fit <- brm(family = binomial, #
                        conversions | trials(n) ~ variation,
                        data = conv_dat,
                        iter = 2000,
                        warmup = 500,
                        refresh = 0)})
    
    #Calculating the probability of out performing for each variation
    outperforming <- posterior_samples(fit) %>%
      select(1:2) %>%
      mutate(varA_per = logit2prob(.[,1]) , VarB_per = logit2prob(.[,1]+.[,2])) %>%
      select(varA_per, VarB_per) %>% 
      summarise(a_better = paste0(round((sum(.[,1]>.[,2])/n())*100,2),"%"),
                b_better = paste0(round((sum(.[,2]>.[,1])/n())*100,2),"%")) %>% 
      gather(key = variation, value = `Prob. of outperforming`, a_better, b_better) %>% 
      select(`Prob. of outperforming`) 
    
    outperforming_num <- posterior_samples(fit) %>%
      select(1:2) %>%
      mutate(varA_per = logit2prob(.[,1]) , VarB_per = logit2prob(.[,1]+.[,2])) %>%
      select(varA_per, VarB_per) %>% 
      summarise(a_better_num = round((sum(.[,1]>.[,2])/n())*100,2),
                b_better_num = round((sum(.[,2]>.[,1])/n())*100,2)) %>% 
      gather(key = variation, value = `prob_oop_num`, a_better_num, b_better_num) %>% 
      select(prob_oop_num) 
    
    #Creating the table ready numbers from the entry and outperforming numbers
    conv_tab <- conv_dat %>% 
      cbind(., outperforming) %>%
      mutate(CR = paste0(round((conversions/n)*100,2),"%")) %>%
      mutate(uplift = c("", paste0(round(uplift(posterior_summary(fit)[1,1],posterior_summary(fit)[2,1])*100,2
      ),"%"))) %>% 
      cbind(., outperforming_num)
    
    #Simulated conversion rate distribution for both variations 
    conv_dist <- posterior_samples(fit) %>% 
      select(1:2) %>%
      mutate(`Variation A` = logit2prob(.[,1]), `Variation B` = logit2prob(.[,1]+.[,2])) %>%
      select(`Variation A`, `Variation B`) %>% 
      gather(key = "variation", value = "CR", `Variation A`, `Variation B`) 
    
    #Distribution of uplifts
    uplift_dist <- posterior_samples(fit) %>%
      select(1:2) %>%
      mutate(varA_per = logit2prob(.[,1]), VarB_per = logit2prob(.[,1]+.[,2])) %>%
      select(varA_per, VarB_per) %>% 
      transmute(dif = (VarB_per-varA_per)/mean(varA_per))
    
    #Expected uplift
    expected_conv_dif <- uplift_dist %>% 
      summarise(mean = mean(dif))
    
    #Expect uplift button
    output$mean_btn1 <- renderUI({
      bsButton("mean_btn1", label = paste0("Expected uplift is: ",round(conv_model()[[8]]$mean*100,0), "%"),
               type = "toggle", value = TRUE, size = "large", style="warning", icon = icon("arrow-alt-circle-up"))
    })
    
    list(conv_dat, fit, outperforming, conv_tab, conv_dist, uplift_dist, uplift_dist, expected_conv_dif)
        })
  

    ########Converstion rate table

  
  
    #Conversion rate table
    output$convtab <- DT::renderDataTable({
      datatable(conv_model()[[4]],
                class = "compact hover",
                options = list(paging = F, searching = F, ordering = F, 
                               columnDefs = list(
                  list(targets = -1, visible=FALSE), list(className = "dt-right", targets = 4:6))),
                colnames = c("Variation", "Sample size", "Conversions", "Prob. of outperforming", "CR", "Uplift", "temp")) %>% 
        formatStyle(
          "Prob. of outperforming", valueColumn = ncol(conv_model()[[4]]),
          background = styleColorBar(c(0,100), "deepskyblue"))%>% 
        formatStyle(
          "variation",
          backgroundColor = styleEqual(c("A", "B"), c("#E41A1C", "#377EB8"))) %>% 
        formatStyle(
          "uplift", valueColumn = "variation",
          backgroundColor = styleEqual(c("B"), c("#EC9706")))

    })
    
    ########Two distributions plot
    
    output$convrdist <- renderPlotly({
      
      #Both CR distributions
      g_conv_both <- conv_model()[[5]] %>% 
        ggplot(aes(x = CR, fill = variation, color = variation)) +
        ggtitle("Conversion rates for the variations")+
        geom_density(alpha = 0.7)+
        scale_fill_brewer(palette="Set1")+
        scale_x_continuous(name  = "Conversion rate", labels = percent)+
        scale_y_continuous(NULL, breaks = NULL) +
        guides(color = FALSE, fill = guide_legend(title=NULL))+
        theme_bw()
      
      #list to empty the y axis in plotly
      ax2 <- list(
        title = "",
        zeroline = FALSE,
        showline = FALSE,
        showticklabels = FALSE,
        ticks = "",
        showgrid = FALSE)
      
      #plotly plot
      p_conv_both <- ggplotly(g_conv_both, tooltip= c("x", "variation")) %>% 
        layout(yaxis = ax2)
      p_conv_both
      
      })
    
    ######### Uplift plot
    
    output$convdifdist <- renderPlotly({
      req(input$creds1)
      
      #Preliminary plot to extract density values on y axis to use polygons 
      pre_g_conv_dif <- conv_model()[[6]] %>% 
        ggplot(aes(x = dif)) +
        geom_density()
      
      #extract densities
      dens1 <- ggplot_build(pre_g_conv_dif)$data[[1]]
      
      #create plot with interactive geom_are polygons to cover area outside designated range
      g_conv_dif <- pre_g_conv_dif+
        ggtitle("Distribution of uplift (B-A)/A")+
        geom_density(color = "#4DAF4A", fill = "#4DAF4A")+
        #geom_area(data = dens1 %>% filter(x > 0), #Random idea to paint areas on both sides of zero different colours
                  #aes(x=x,y=y),
                  #fill = "#377EB8",
                  #color = "#377EB8",
                  #alpha = 0.8)+
        #geom_area(data = dens1 %>% filter(x < 0),
                  #aes(x=x,y=y),
                  #fill = "#E41A1C",
                  #color = "#E41A1C",
                  #alpha = 0.8)+
        geom_area(data = dens1 %>% filter(x < quantile(conv_model()[[6]]$dif, probs = (1-input$creds1)*0.5, names = FALSE)),
          aes(x=x,y=y),
          fill = "black",
          color = "black",
          alpha = 0.5)+
        geom_area(data = dens1 %>% filter(x > quantile(conv_model()[[6]]$dif, probs = 1-((1-input$creds1)*0.5), names=FALSE)),
          aes(x=x,y=y),
          fill = "black",
          color = "black",
          alpha = 0.5)+
        scale_x_continuous(name  = "Uplift", labels = percent)+
        scale_y_continuous(NULL, breaks = NULL) +
        guides(color = FALSE, fill = guide_legend(title=NULL))+
        theme_bw()
      
      #empty axes on plotly
      ax2 <- list(
        title = "",
        zeroline = FALSE,
        showline = FALSE,
        showticklabels = FALSE,
        ticks = "",
        showgrid = FALSE)
      
      #Plotly with black dashed interactive lines for credible intervals
      p_mod2_dif <- ggplotly(g_conv_dif, tooltip= c("x")) %>% 
        layout(yaxis = ax2)  %>% 
        add_segments(x=c(quantile(conv_model()[[6]]$dif, probs = 1-((1-input$creds1)*0.5), names=FALSE),
                         quantile(conv_model()[[6]]$dif, probs = (1-input$creds1)*0.5, names = FALSE)), 
                     xend = c(quantile(conv_model()[[6]]$dif, probs = 1-((1-input$creds1)*0.5), names = FALSE),
                              quantile(conv_model()[[6]]$dif, probs = (1-input$creds1)*0.5, names = FALSE)),
                     y=c(0,0), yend= c(999,999), line=list(color=c("black", "black"), width = c(2,2)))
      
      #Wether to show mean with button action
      if(input$mean_btn1 == TRUE)
        p_mod2_dif <- p_mod2_dif %>%  
        add_segments(type="rect", x = conv_model()[[8]]$mean, xend = conv_model()[[8]]$mean, 
                     y = 0, yend=999, line=list(color = "darkgoldenrod1", width = 4, dash = "dash"))
      
      p_mod2_dif
      }) 
    
    ############
    ###### MODEL 2
    ###########
    
  variations <- eventReactive(input$mrr_file, {
    req(input$mrr_file)
    
    #
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
      filter(variation %in% c("A", "B")) %>%
      rename(firstmrr = 2)
    
      npc_dat
  })
  
  model2 <- eventReactive(input$model_mrr,{
    req(input$mrr_file)
    
    output$mrr_creds <- renderUI({
      sliderInput("creds2",
                  label = "Choose a credible interval",
                  value = 0.68, min = 0.001, max = 0.999)})
    
    npc_wide <- read.csv(input$mrr_file$datapath, header = T) %>% 
      select(1,2) %>% 
      mutate(variation = recode(variation, !!sym(input$pick_varA) := "A", !!(input$pick_varB) := "B")) %>% 
      filter(variation %in% c("A", "B")) %>% 
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
      
      mrr_better_num <- mean_samp %>%  
        summarise(a_better_num = round((sum(.[,1]>.[,2])/n())*100,2),
                  b_better_num = round((sum(.[,2]>.[,1])/n())*100,2)) %>% 
        pivot_longer(cols = c(a_better_num, b_better_num), values_to = "prob_oop_num") %>% 
        select("prob_oop_num")
      
      #Calculating probabikity of each variation having higher true mean MRR
      mrr_better <- mean_samp %>%  
        summarise(a_better = paste0(round((sum(.[,1]>.[,2])/n())*100,2),"%"),
                  b_better = paste0(round((sum(.[,2]>.[,1])/n())*100,2),"%")) %>% 
        pivot_longer(cols = c(a_better, b_better), values_to = "Prob. of outperforming (on mean)") %>% 
        select("Prob. of outperforming (on mean)")
      
      model2_tabl <- cbind(mrr_mean, mrr_better, mrr_better_num)
      
      mean_dif <- mean_samp %>% 
        transmute(dif = B - A)
      
      expected_mean_dif <- mean_dif %>% 
        summarise(mean = mean(dif))
      
      output$mean_btn2 <- renderUI({
        bsButton("mean_btn2", label = paste0("Expected difference is: ", "$",round(model2()[[6]],2)),
                 type = "toggle", value = TRUE, size = "large", style="warning", icon = icon("balance-scale-right"))
      })
      
      list(b4.3, npc_dat2, mean_samp, model2_tabl, mean_dif, expected_mean_dif)
      
    })
  })
  
  output$raw_tabl <- DT::renderDataTable({
    datatable(npc_dat() %>%
                filter(.[[2]] < input$outliers) %>% 
                group_by(variation) %>% 
                summarise(mean = round(mean(firstmrr),2),
                          SD = round(sd(firstmrr),2),
                          max = round(max(firstmrr),2),
                          min= round(min(firstmrr),2),
                          Q25 = round(quantile(firstmrr, prob = c(.25)),2),
                          median = round(median(firstmrr),2),
                          Q75 = round(quantile(firstmrr, prob = c(.75)),2)),
              class = "compact hover",
              options = list(paging = F, searching = F, ordering = F),
              colnames = c("Variation", "Mean", "SD", "Max", "Min", "Q25", "Median", "Q75")) %>% 
              formatStyle(
                "mean",
                background = styleColorBar(c(0,mean(npc_dat()[,2])*2), "deepskyblue"))%>% 
              formatStyle(
                "variation",
                backgroundColor = styleEqual(c("A", "B"), c("#E41A1C", "#377EB8")))
        
  })
  
  output$raw_plt = renderPlot({
    raw_plt <- npc_dat() %>% 
      filter(.[[2]] < input$outliers) %>%
      ggplot(aes(x = as.factor(variation), y = firstmrr, fill = as.factor(variation), color = as.factor(variation))) +
      ggtitle("Distribution of raw MRR data")+
      geom_violin(trim=FALSE, alpha =1, color = FALSE)+ #alpha changed from backend
      geom_jitter(shape=18, position=position_jitter(0.05), size = 5)+ #colours changed from backend
      geom_boxplot(width=0.15, fill = "white", alpha = 0.5, color = "black" )+
      scale_colour_manual(values = c("#4DAF4A", "orange"), guide = FALSE)+
      scale_fill_brewer(palette="Set1")+
      scale_x_discrete(name = "Variation")+
      scale_y_continuous(name = "MRR", n.breaks = 20)+
      guides(fill=guide_legend(title = "Variations"))+
      theme_bw()+
      theme(plot.title = element_text(size = 17))+
      theme(axis.text.x = element_text(face = "bold", color = "black", 
                                       size = 13),
            axis.text.y = element_text(face = "bold", color = "black", 
                                       size = 13))+
      theme(axis.title.x = element_text(face = "bold", color = "black", 
                                        size = 15),
            axis.title.y = element_text(face = "bold", color = "black", 
                                        size = 15))+
      theme(legend.key=element_rect(fill = "gray30"),
            legend.key.size = unit(2,"line"), 
            legend.title=element_text(size=15),
            legend.text=element_text(size=13))+
      theme(legend.key.size = unit(2,"line"))
    
    if(input$log)
      raw_plt <- npc_dat() %>% 
        filter(.[[2]] < input$outliers) %>%
        ggplot(aes(x = as.factor(variation), y = log(firstmrr), fill = as.factor(variation), color = as.factor(variation))) +
        ggtitle("Distribution of raw MRR data")+
        geom_violin(trim=FALSE, alpha =1,color=FALSE)+ #alpha changed from backend
        geom_jitter(shape=18, position=position_jitter(0.05), size = 5)+ #colours changed from backend
        geom_boxplot(width=0.15, fill = "white", alpha = 0.5, color = "black" )+
        scale_colour_manual(values = c("#4DAF4A", "orange"), guide = FALSE)+
        scale_fill_brewer(palette="Set1")+
        scale_x_discrete(name = "Variation")+
        scale_y_continuous(name = "MRR", n.breaks = 20)+
        guides(fill=guide_legend(title = "Variations"))+
        theme_bw()+
        theme(plot.title = element_text(size = 17))+
        theme(axis.text.x = element_text(face = "bold", color = "black", 
                                         size = 13),
              axis.text.y = element_text(face = "bold", color = "black", 
                                         size = 13))+
        theme(axis.title.x = element_text(face = "bold", color = "black", 
                                          size = 15),
              axis.title.y = element_text(face = "bold", color = "black", 
                                          size = 15))+
        theme(legend.key=element_rect(fill = "gray30"),
              legend.key.size = unit(2,"line"), 
              legend.title=element_text(size=15),
              legend.text=element_text(size=13))+
        theme(legend.key.size = unit(2,"line"))
      
      raw_plt
  })
  
  #output$compare <- renderPlot({model2()[[2]] %>% 
      #ggplot(aes(x = as.factor(variation), y = log(model2()[[2]][[3]]), fill = as.factor(variation), color = as.factor(variation))) +
      #ggtitle("Diagnostic sample dist. of model predicted data")+
      #geom_violin(trim=FALSE, alpha =1, color=FALSE)+ #alpha changed from backend
      #geom_jitter(shape=10, position=position_jitter(0.05))+ #colours changed from backend
      #geom_boxplot(width=0.15, fill = "white", alpha = 0.5, color = "black" )+
      #scale_colour_manual(values = c("#4DAF4A", "gold"), guide = FALSE)+
      #scale_fill_brewer(palette="Set1")+
      #scale_x_discrete(name = "Variation")+
      #scale_y_continuous(name = "MRR", n.breaks = 20, limits = ggplot_build(raw_dist)$layout$panel_scales_y[[1]]$range$range)+
      #guides(fill=guide_legend(title = "Variations"))+
      #theme_bw()
  #})
  
  
  output$model2_tabl <- DT::renderDataTable({
    datatable(model2()[[4]],
              class = "compact hover",
              options = list(paging = F, searching = F, ordering = F, columnDefs = list(
                list(targets = -1, visible=FALSE), list(className = "dt-right", targets = 6)))) %>% 
      formatStyle(
        "Prob. of outperforming (on mean)", valueColumn = ncol(model2()[[4]]),
        background = styleColorBar(c(0,100), "deepskyblue"))%>% 
      formatStyle(
        "Variation",
        backgroundColor = styleEqual(c("A", "B"), c("#E41A1C", "#377EB8")))
  })

  output$model2_plt_both <- renderPlotly({ #plotly used so stat_slab, stat_pointinterval not used here
    g_mod2_both <- model2()[[3]] %>% 
      gather(key = "variation", value = "mean_mrr", A, B) %>% 
      ggplot(aes(x = mean_mrr, fill = variation , color = variation))+
      ggtitle("Distributions of model implied mean MRR per customer")+
      geom_density(alpha = 0.7)+ #alpha changed from back end
      scale_fill_brewer(palette="Set1")+ #colours changed from back-end code
      scale_x_continuous(name  = "Mean MRR per customer", labels = dollar_format())+
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
    req(input$creds2)
    
    pre_g_mod2_dif <- model2()[[5]] %>% 
      ggplot(aes(x = dif)) +
      geom_density()
    
    dens2 <- ggplot_build(pre_g_mod2_dif)$data[[1]]
    
    g_mod2_dif <- ggplot(data = model2()[[5]], aes(x=dif))+
      geom_density(color = "#4DAF4A", fill = "#4DAF4A")+
      ggtitle("Distribution of model implied difference of mean MRR per customer between variations")+
      #geom_area(data = dens2 %>% filter(x > 0),
                #aes(x=x,y=y),
                #fill = "#377EB8",
                #color = "#377EB8",
                #alpha = 0.8)+
      #geom_area(data = dens2 %>% filter(x < 0),
                #aes(x=x,y=y),
                #fill = "#E41A1C",
                #color = "#E41A1C",
                #alpha = 0.8) +
      #geom_area(data = dens2 %>% filter(x < hdi(model2()[[5]]$dif, credMass = input$creds2)[1]),
                #aes(x=x,y=y),
                #fill = "black",
                #color = "black",
                #alpha = 0.5)+
      #geom_area(data = dens2 %>% filter(x > hdi(model2()[[5]]$dif, credMass = input$creds2)[2]),
                #aes(x=x,y=y),
                #fill = "black",
                #color = "black",
                #alpha = 0.5)+
      geom_area(data = dens2 %>% filter(x < quantile(model2()[[5]]$dif, probs = (1-input$creds2)*0.5, names = FALSE)),
                aes(x=x,y=y),
                fill = "black",
                color = "black",
                alpha = 0.5)+
      geom_area(data = dens2 %>% filter(x > quantile(model2()[[5]]$dif, probs = 1-((1-input$creds2)*0.5), names=FALSE)),
                aes(x=x,y=y),
                fill = "black",
                color = "black",
                alpha = 0.5)+
      scale_y_continuous(NULL, breaks = NULL) +
      scale_x_continuous(name  = "Mean MRR per customer of B - Mean MRR per customer of A", labels = dollar_format())+
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
      add_segments(type = "rect", x=c(quantile(model2()[[5]]$dif, probs = 1-((1-input$creds2)*0.5), names=FALSE),
                                    quantile(model2()[[5]]$dif, probs = (1-input$creds2)*0.5, names = FALSE)), 
                xend = c(quantile(model2()[[5]]$dif, probs = 1-((1-input$creds2)*0.5), names = FALSE),
                          quantile(model2()[[5]]$dif, probs = (1-input$creds2)*0.5, names = FALSE)),
                 y=c(0,0), yend= c(999,999), line=list(color=c("black", "black"), width = c(2,2))) 
    
    if(input$mean_btn2 == TRUE)
     p_mod2_dif <- p_mod2_dif %>%  
      add_segments(type="rect", x = model2()[[6]]$mean, xend = model2()[[6]]$mean, 
                   y = 0, yend=999, line=list(color = "darkgoldenrod1", width = 4, dash = "dash"))
    
    p_mod2_dif
  })
  
  ##Model 3
  
  conv_mrr_model <- eventReactive(input$model_mrr_and_conv, {
    
    output$mean_btn <- renderUI({
      bsButton("mean_btn", label = paste("Expected difference is: ", "$",round(conv_mrr_model()[[5]],2)),
               type = "toggle", value = TRUE, size = "large", style="warning", icon = icon("balance-scale-left"))
    })
    
    
    output$mrr_v_creds <- renderUI({
      sliderInput("creds3",
                  label = "Choose a credible interval",
                  value = 0.68, min = 0.001, max = 0.999)})
    
    post_samp_transformed <- posterior_samples(conv_model()[[2]]) %>%
      select(1:2) %>%
      mutate(varA_per = logit2prob(.[,1]), VarB_per = logit2prob(.[,1]+.[,2])) %>%
      select(varA_per, VarB_per) 
    
    overall_results <- model2()[[3]]%>%
      select(A, B) %>% 
      mutate(A_overall = A*post_samp_transformed$varA_per,
             B_overall = B*post_samp_transformed$VarB_per) %>% 
      select(A_overall, B_overall)
    
    overall_better_num <- overall_results%>% 
      summarise(a_better_num = round((sum(.[,1]>.[,2])/n())*100,2),
                b_better_num = round((sum(.[,2]>.[,1])/n())*100,2)) %>% 
      gather(key = variation, value = `prob_oop_num`, a_better_num, b_better_num) %>% 
      select(prob_oop_num)
    
    overall_better <- overall_results%>% 
      summarise(a_better = paste0(round((sum(.[,1]>.[,2])/n())*100,2),"%"),
                b_better = paste0(round((sum(.[,2]>.[,1])/n())*100,2),"%")) %>% 
      gather(key = variation, value = `Prob. of outperforming`, a_better, b_better) %>% 
      select(`Prob. of outperforming`)
    
    tab_overall <- overall_results %>%
      rename(A = "A_overall", B = "B_overall") %>% 
      gather(key="Variation", value = "Values", A, B)%>%
      group_by(Variation) %>% 
      summarise(Mean = round(mean(Values),2),
                SD = round(sd(Values),2),
                Q2.5 = round(quantile(Values, prob = c(.025)),2),
                Q97.5 = round(quantile(Values, prob = c(.975)),2)) %>% 
      cbind(overall_better) %>%
      mutate(`MRR per 1000 visitors` = 1000*Mean) %>% 
      cbind(., overall_better_num)
    
    mean_difference <- overall_results %>% 
      summarise(dif=mean(B_overall-A_overall))
    
    dist_overall <- overall_results %>%
      gather(key = "variation", value = "overall", A_overall, B_overall)
    
    dist_dif_overall <- overall_results %>% 
      transmute(dif = B_overall-A_overall)
    
    list(post_samp_transformed, overall_results, overall_better, tab_overall, mean_difference, dist_overall, dist_dif_overall)
    
  })
  
  
  output$mrr_per_visitor_tab <- DT::renderDataTable({
    datatable(conv_mrr_model()[[4]],
              class = "compact hover",
              options = list(paging = F, searching = F, ordering = F, columnDefs = list(
                list(targets = -1, visible=FALSE),list(className = "dt-right", targets = 6)))) %>% 
      formatStyle(
        "Prob. of outperforming", valueColumn = ncol(conv_mrr_model()[[4]]),
        background = styleColorBar(c(0,100), "deepskyblue"))%>% 
      formatStyle(
        "Variation",
        backgroundColor = styleEqual(c("A", "B"), c("#E41A1C", "#377EB8")))
  })
  
  output$overall_dist <- renderPlotly({
    
    g_overall_dist <- conv_mrr_model()[[6]] %>% 
      ggplot(aes(x = overall, fill = variation , color = variation))+
      ggtitle("Model implied distributions of MRR per visitor")+
      geom_density(alpha = 0.7)+
      scale_fill_brewer(palette="Set1")+
      scale_x_continuous(name  = "MRR per visitor", labels = dollar_format())+
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
    
    p_overall_dist <- ggplotly(g_overall_dist, tooltip= c("x", "variation")) %>% 
      layout(yaxis = ax2)
    p_overall_dist
    
  })
  
  output$overall_dist_dif <- renderPlotly({
    req(input$creds3)
    
    pre_g_overall_dist_dif <- conv_mrr_model()[[7]] %>%
      ggplot(aes(x = dif))+
      geom_density()
    
    dens3 <- ggplot_build(pre_g_overall_dist_dif)$data[[1]]
    
    g_overall_dist_dif <- ggplot(data = conv_mrr_model()[[7]], aes(x=dif))+
      geom_density(color = "#4DAF4A", fill = "#4DAF4A")+
      ggtitle("Model implied distribution of difference of MRR per visitor")+
      #geom_area(data = dens3 %>% filter(x > 0),
              #aes(x=x,y=y),
              #fill = "#377EB8",
              #color = "#377EB8",
              #alpha = 0.8)+
      #geom_area(data = dens3 %>% filter(x < 0),
                #aes(x=x,y=y),
                #fill = "#E41A1C",
                #color = "#E41A1C",
                #alpha = 0.8) +
      #geom_area(data = dens2 %>% filter(x < hdi(conv_mrr_model()[[7]]$dif, credMass = input$creds3)[1]),
                #aes(x=x,y=y),
                #fill = "black",
                #color = "black",
                #alpha = 0.5)+
      #geom_area(data = dens2 %>% filter(x > hdi(conv_mrr_model()[[7]]$dif, credMass = input$creds3)[2]),
                #aes(x=x,y=y),
                #fill = "black",
                #color = "black",
                #alpha = 0.5)+
      geom_area(data = dens3 %>% filter(x < quantile(conv_mrr_model()[[7]]$dif, probs = (1-input$creds3)*0.5, names = FALSE)),
                aes(x=x,y=y),
                fill = "black",
                color = "black",
                alpha = 0.5)+
      geom_area(data = dens3 %>% filter(x > quantile(conv_mrr_model()[[7]]$dif, probs = 1-((1-input$creds3)*0.5), names=FALSE)),
                aes(x=x,y=y),
                fill = "black",
                color = "black",
                alpha = 0.5)+
      scale_y_continuous(NULL, breaks = NULL) +
      scale_x_continuous(name  = "Mean MRR per visitor of B - Mean MRR per visitor of A", labels = dollar_format())+
      theme_bw()
    
    ax2 <- list(
      title = "",
      zeroline = FALSE,
      showline = FALSE,
      showticklabels = FALSE,
      ticks = "",
      showgrid = FALSE)
    
    p_overall_dist_dif <- ggplotly(g_overall_dist_dif, tooltip= c("x", "variation")) %>% 
      layout(yaxis = ax2) %>% 
      add_segments(type = "rect", x=c(quantile(conv_mrr_model()[[7]]$dif, probs = 1-((1-input$creds3)*0.5), names=FALSE),
                                      quantile(conv_mrr_model()[[7]]$dif, probs = (1-input$creds3)*0.5, names = FALSE)), 
                                      xend = c(quantile(conv_mrr_model()[[7]]$dif, probs = 1-((1-input$creds3)*0.5), names = FALSE),
                                      quantile(conv_mrr_model()[[7]]$dif, probs = (1-input$creds3)*0.5, names = FALSE)),
                                      y=c(0,0), yend= c(999,999), line=list(color=c("black", "black"), width = c(2,2)))
    
    if(input$mean_btn == TRUE)
      p_overall_dist_dif <- p_overall_dist_dif %>%  
      add_segments(type="rect", x = conv_mrr_model()[[5]]$dif, xend = conv_mrr_model()[[5]]$dif, 
                   y = 0, yend=999, line=list(color = "darkgoldenrod1", width = 4, dash = "dash"))
    
    p_overall_dist_dif
  })
  
}

shinyApp(ui = ui, server = server)