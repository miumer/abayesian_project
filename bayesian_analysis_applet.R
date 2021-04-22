library(DT)
library(tidyverse)
library(brms)
library(scales)
library(plotly)
library(shiny)
library(shinyBS)
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

ui <- dashboardPage( #using shinydashboard package as design basis
  dashboardHeader(tags$li(class="dropdown"), #dropdown class has to be used to put the pipedrive logo on on the title bar
  title = tags$a(href="pd_logo.png", "A random bayesian analysis tool", 
                img(src = 'pd_logo.png',
                          height = "30px",
                    style="float:left; margin-top:10px"), style = "color: #ffffff; font-weight: 325"), titleWidth = 350),
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
  uiOutput('ui_varA'), #choose variations to compare
  uiOutput('ui_varB'),
  #checkboxInput("diag", "Check performance", FALSE), possible diagnostic button (concider adding a separate tab for model diagnostics)
  actionButton("assign_vars", label = "Give it to me raw!"), 
  numericInput(
    inputId="outliers", #outlier removal
    label= "Remove datapoints with $$ over", value = 99999999, width = "50%"),
  checkboxInput("log", "Log transformed", TRUE)
  ),
  actionButton("model_mrr", label = "Model mean customer MRR!"),
  actionButton("model_mrr_and_conv", label = "Model mean visitor MRR!")
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
      
        /*main header navbar*/
        .skin-blue .main-header .navbar {
                              background-color: #08A742;
        }
  
        /* toggle button when hovered */                    
         .skin-blue .main-header .navbar .sidebar-toggle:hover{
                              background-color: #0D923E;
         }
        
        /*Table title colour*/
        .tabbable > .nav > li > a   {color:#0D923E}
  
        /*sidebar logo*/
        .skin-blue .main-header .logo {
                              background-color: #0D923E;
                              color: #26292C
        }
        
        /* sidebar logo when hovered */
        .skin-blue .main-header .logo:hover {
                              background-color: #0D923E;
        }
        
         /* main sidebar */
        .main-sidebar a{ font-size: 20px; }
        .skin-blue .main-sidebar {
                              background-color: #26292C;
                              }")),
    ),
    tabsetPanel(
      tabPanel(
        "Conversion model",
        p(""),
        box(
        DT::dataTableOutput("convtab"), #use datatable package for table customization 
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
      #fluidRow(splitLayout(cellWidths = c("50%", "50%"), plotOutput("raw_plt"), plotOutput("compare"))) Possible diagnostics plot
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
    
    #######
    #####MODEL 1 CONVERSION RATE 
    #######
    
    #credible interval slider
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
    
    #calculating probability of outperforming
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
  

    ########Converstion rate table output

    #Conversion rate table
    output$convtab <- DT::renderDataTable({
      datatable(conv_model()[[4]], selection = "none",
                class = "cell-border stripe", rownames = FALSE,
                options = list(paging = F, searching = F, ordering = F, info = FALSE,
                               columnDefs = list(
                  list(targets = -1, visible=FALSE), list(className = "dt-right", targets = 3:6))),
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
    
    ########Two distributions plot output
    
    output$convrdist <- renderPlotly({
      
      #Both CR distributions
      g_conv_both <- conv_model()[[5]] %>% 
        ggplot(aes(x = CR, fill = variation, color = variation)) +
        ggtitle("Conversion rates for the variations")+
        geom_density(alpha = 0.7)+
        scale_fill_brewer(palette="Set1")+
        scale_x_continuous(name  = "Conversion rate", breaks = scales::pretty_breaks(n = 10), labels = label_percent(accuracy = 0.01))+
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
    
    ######### Uplift plot output
    
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
        scale_x_continuous(name  = "Uplift", breaks = scales::pretty_breaks(n = 10), labels = label_percent(accuracy = 0.01))+
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
    ###### Data entry preparation
    ###########
    
    #extracts filenames from entered file to variations
  variations <- eventReactive(input$mrr_file, { #reacts to file being entered
    req(input$mrr_file) #requires that file be entered
    
    #file entry
    var_names <- read.csv(input$mrr_file$datapath, header = T) %>% 
      distinct(variation)
    var_names <- c(var_names)
    var_names
  })
    
    #variation picker for A (gives the exctracted variable names (above) as choices)
  output$ui_varA <- renderUI({
    selectInput('pick_varA',
                label ='Pick variation A',
                choices=variations(),
                selected = NULL, multiple = FALSE,width="450px", selectize = TRUE)
  })
  
  #variation picker for B (gives the exctracted variable names (above) as choices)
  output$ui_varB <- renderUI({
    selectInput('pick_varB',
                label ='Pick variation B',
                choices=variations(),
                selected = NULL, multiple = FALSE,width="450px", selectize = TRUE)
  })
  #renames variations as A and B for visualisations and tables of raw data
  npc_dat <- eventReactive(input$assign_vars, { #reacts to assign_vars (the "give it to me raw button")
    req(input$mrr_file) #requires input file to be present 
    
    npc_dat <- read.csv(input$mrr_file$datapath, header = T) %>% 
      select(1,2) %>% 
      mutate(variation = recode(variation, !!sym(input$pick_varA) := "A", !!sym(input$pick_varB) := "B")) %>% #inputs need to be converted to symbols so dplyr could handle them
      filter(variation %in% c("A", "B")) %>%
      rename(firstmrr = 2)
    
      npc_dat
  })
  
  model2 <- eventReactive(input$model_mrr,{ #reacts to  "Model mean customer MRR!" button
    req(input$mrr_file)
    
    #creates credible interval entry user interface
    output$mrr_creds <- renderUI({
      sliderInput("creds2",
                  label = "Choose a credible interval",
                  value = 0.68, min = 0.001, max = 0.999)})
    
    #prepares data for modelling based on variable selection 
    npc_wide <- read.csv(input$mrr_file$datapath, header = T) %>% 
      select(1,2) %>% 
      mutate(variation = recode(variation, !!sym(input$pick_varA) := "A", !!(input$pick_varB) := "B")) %>% 
      filter(variation %in% c("A", "B")) %>% 
      rename(firstmrr = 2) %>% 
      mutate(A = as.numeric(as.character(factor(variation, labels=c(1,0))))) %>% 
      mutate(B = 1-A) #creates a dummy variable to reparameterize the bayesian regression model
    
    #The model itself
    withProgress(message = "Modeling in progress. Please wait Mr. Strang ...",{
      b4.3 <- brm(data = npc_wide, family = shifted_lognormal(link_sigma = "identity"),
                  bf(firstmrr ~ 0 + A + B, sigma ~ 0 + A + B), #alternative parametrisation and sigma is modeled as also depending on variation
                  #prior = c(prior(student_t(3,0,1), class = b)), #Possibility for informative priors or prior entry for accuracy managment
                  control = list(adapt_delta = 0.95),
                  iter = 3000, warmup = 600, chains = 4, cores = 4, #Look into this for managing computation time (might make it less robust and require more diagnostics). With a better computer, cores should be changed
                  seed = 4)
      
      #extracts posterior predictive distribution. For possible diagnostics not covered in the current version.
      post_pred <- as.data.frame(t(posterior_predict(object = b4.3, 
                                                     newdata = npc_wide[,3:4], nsamples = 1))) #only one sample!!!!
      
      #binds posterior predictive to variations  to raw data 
      npc_dat2 <- cbind(npc_wide[,1:2], post_pred)
      
      #extracting distribution of means with the posterior_epred function. 
      post_dist <- as.data.frame(t(posterior_epred(b4.3, nsamples = 6000)))
      
      #dataframe with variations püaired with sampled means to create a distribution for both variations 
      mean_samp <- cbind(npc_wide$variation, post_dist) %>%
        distinct() %>%
        pivot_longer(-`npc_wide$variation`) %>%    
        pivot_wider(names_from=`npc_wide$variation`, values_from=value) %>% 
        select(A,B) 
      
      #Calculations with distribution of means
      mrr_mean <- mean_samp %>%
        pivot_longer(cols = c(A, B), values_to = "means", names_to = "Variation")  %>% 
        group_by(Variation) %>% 
        summarise(`Estimate of mean` = round(mean(means),2), 
                  `Est. error of mean` = round(sd(means),2),
                  Q2.5 = round(quantile(means, prob = c(.025)),2),
                  Q97.5 = round(quantile(means, prob = c(.975)),2))
      
      #numeric probability of out performing for sparkline style barchart inside table (light blue bars)
      mrr_better_num <- mean_samp %>%  
        summarise(a_better_num = round((sum(.[,1]>.[,2])/n())*100,2),
                  b_better_num = round((sum(.[,2]>.[,1])/n())*100,2)) %>% 
        pivot_longer(cols = c(a_better_num, b_better_num), values_to = "prob_oop_num") %>% 
        select("prob_oop_num")
      
      #Calculating probabikity of each variation having higher mean MRR as character
      mrr_better <- mean_samp %>%  
        summarise(a_better = paste0(round((sum(.[,1]>.[,2])/n())*100,2),"%"),
                  b_better = paste0(round((sum(.[,2]>.[,1])/n())*100,2),"%")) %>% 
        pivot_longer(cols = c(a_better, b_better), values_to = "Prob. of outperforming (on mean)") %>% 
        select("Prob. of outperforming (on mean)")
      
      #binding together for a table
      model2_tabl <- cbind(mrr_mean, mrr_better, mrr_better_num)
      
      #sample of difference of means (from distribution of A mean and B mean) 
      mean_dif <- mean_samp %>% 
        transmute(dif = B - A)
      
      expected_mean_dif <- mean_dif %>% 
        summarise(mean = mean(dif))
      
      #orange button to show expected difference
      output$mean_btn2 <- renderUI({
        bsButton("mean_btn2", label = paste0("Expected difference is: ", "$",round(model2()[[6]],2)),
                 type = "toggle", value = TRUE, size = "large", style="warning", icon = icon("balance-scale-right"))
      })
      
      list(b4.3, npc_dat2, mean_samp, model2_tabl, mean_dif, expected_mean_dif)
      
    })
  })
  
  #creating the data table
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
              class = "cell-border stripe", rownames = FALSE, selection = "none",
              options = list(paging = F, searching = F, ordering = F, info = FALSE),
              colnames = c("Variation", "Mean", "SD", "Max", "Min", "Q25", "Median", "Q75")) %>% 
              formatStyle(
                "mean",
                background = styleColorBar(c(0,mean(npc_dat()[,2])*2), "deepskyblue"))%>% #miniature barchart based on raw numbers
              formatStyle(
                "variation",
                backgroundColor = styleEqual(c("A", "B"), c("#E41A1C", "#377EB8"))) #variation colouring
        
  })
  
  #Raw data plot with log-transformed unticked
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
    
    #If log-tranformation is ticked
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
  
  #######
  ###MODEL 2 - CUSTOMER MEAN MRR DATA
  ######
  
  #Table for customer mean mrr modeled data
  output$model2_tabl <- DT::renderDataTable({
    datatable(model2()[[4]],
              class = "cell-border stripe", rownames = FALSE, selection = "none",
              options = list(paging = F, searching = F, ordering = F, info = FALSE, columnDefs = list(
                list(targets = -1, visible=FALSE), list(className = "dt-right", targets = 2:6)))) %>% #numeric prob of outperforming used only for calculation, not displayed, and eveyrthing aligned to right
      formatStyle(
        "Prob. of outperforming (on mean)", valueColumn = ncol(model2()[[4]]),
        background = styleColorBar(c(0,100), "deepskyblue"))%>% #intable barcharts
      formatStyle(
        "Variation",
        backgroundColor = styleEqual(c("A", "B"), c("#E41A1C", "#377EB8")))
  })
  
  #Plot for both variations distributions of means
  output$model2_plt_both <- renderPlotly({ #plotly used so stat_slab, stat_pointinterval used in backend not used here
    g_mod2_both <- model2()[[3]] %>% 
      gather(key = "variation", value = "mean_mrr", A, B) %>% 
      ggplot(aes(x = mean_mrr, fill = variation , color = variation))+
      ggtitle("Distributions of model implied mean MRR per customer")+
      geom_density(alpha = 0.7)+ #alpha changed from back end
      scale_fill_brewer(palette="Set1")+ #colours changed from back-end code
      scale_x_continuous(name  = "Mean MRR per customer", labels = dollar_format(), breaks = scales::pretty_breaks(n = 13))+
      scale_y_continuous(NULL, breaks = NULL) +
      guides(color = FALSE, fill = guide_legend(title=NULL))+
      theme_bw() 
    
    ax <- list( #empty y axis
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
    
    #bare bones distribution in ggplot to extract polygon coordinates for shading in the next step
    pre_g_mod2_dif <- model2()[[5]] %>% 
      ggplot(aes(x = dif)) +
      geom_density()
    
    #extracting polygon coordinates
    dens2 <- ggplot_build(pre_g_mod2_dif)$data[[1]]
    
    #Creating the plot with extracted polygons shaded black with opacity 0.5
    g_mod2_dif <- ggplot(data = model2()[[5]], aes(x=dif))+
      geom_density(color = "#4DAF4A", fill = "#4DAF4A")+
      ggtitle("Distribution of model implied difference of mean MRR per customer between variations")+
      #Possiblity of using HDI or highest density intervals hear but they behave erratically and since the distro should be normal, it doesnt matter here
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
    #using equal tailed intervals here instead
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
      scale_x_continuous(name  = "Mean MRR per customer of B - Mean MRR per customer of A", labels = dollar_format(), breaks = scales::pretty_breaks(n = 13))+
      theme_bw()
    
    ax2 <- list(
      title = "",
      zeroline = FALSE,
      showline = FALSE,
      showticklabels = FALSE,
      ticks = "",
      showgrid = FALSE)
    
    #creating an equal tailed credibility (confidence for bayesians) interval shader for the probability
    p_mod2_dif <- ggplotly(g_mod2_dif, tooltip= c("x")) %>% 
      layout(yaxis = ax2) %>% 
      add_segments(type = "rect", x=c(quantile(model2()[[5]]$dif, probs = 1-((1-input$creds2)*0.5), names=FALSE),
                                    quantile(model2()[[5]]$dif, probs = (1-input$creds2)*0.5, names = FALSE)), 
                xend = c(quantile(model2()[[5]]$dif, probs = 1-((1-input$creds2)*0.5), names = FALSE),
                          quantile(model2()[[5]]$dif, probs = (1-input$creds2)*0.5, names = FALSE)),
                 y=c(0,0), yend= c(999,999), line=list(color=c("black", "black"), width = c(2,2))) 
    
    #clickable button to remove expected value line
    if(input$mean_btn2 == TRUE)
     p_mod2_dif <- p_mod2_dif %>%  
      add_segments(type="rect", x = model2()[[6]]$mean, xend = model2()[[6]]$mean, 
                   y = 0, yend=999, line=list(color = "darkgoldenrod1", width = 4, dash = "dash"))
    
    p_mod2_dif
  })
  
  ########
  ###Model 3 - Mean visitor MRR
  ########
  
  conv_mrr_model <- eventReactive(input$model_mrr_and_conv, { #reacts to "Analyze visitor MRR!" Button
    
    #Orange button to show mean of distribution
    output$mean_btn <- renderUI({
      bsButton("mean_btn", label = paste("Expected difference is: ", "$",round(conv_mrr_model()[[5]],2)),
               type = "toggle", value = TRUE, size = "large", style="warning", icon = icon("balance-scale-left"))
    })
    
    #slider for credible intervals
    output$mrr_v_creds <- renderUI({
      sliderInput("creds3",
                  label = "Choose a credible interval",
                  value = 0.68, min = 0.001, max = 0.999)})
    
    #extracts posterior sample of conversion rates as probabilities
    post_samp_transformed <- posterior_samples(conv_model()[[2]]) %>%
      select(1:2) %>%
      mutate(varA_per = logit2prob(.[,1]), VarB_per = logit2prob(.[,1]+.[,2])) %>%
      select(varA_per, VarB_per) 
    
    #multiplies posterior of mean MRR per customer to posterior of conversion from visitor to customer for both variations
    overall_results <- model2()[[3]]%>%
      select(A, B) %>% 
      mutate(A_overall = A*post_samp_transformed$varA_per,
             B_overall = B*post_samp_transformed$VarB_per) %>% 
      select(A_overall, B_overall)
    
    #Probability of outperforming as numeric for mini barchart on table (not visible in the table)
    overall_better_num <- overall_results%>% 
      summarise(a_better_num = round((sum(.[,1]>.[,2])/n())*100,2),
                b_better_num = round((sum(.[,2]>.[,1])/n())*100,2)) %>% 
      gather(key = variation, value = `prob_oop_num`, a_better_num, b_better_num) %>% 
      select(prob_oop_num)
    
    #Probability of outperforming as character for table display
    overall_better <- overall_results%>% 
      summarise(a_better = paste0(round((sum(.[,1]>.[,2])/n())*100,2),"%"),
                b_better = paste0(round((sum(.[,2]>.[,1])/n())*100,2),"%")) %>% 
      gather(key = variation, value = `Prob. of outperforming`, a_better, b_better) %>% 
      select(`Prob. of outperforming`)
    
    #create table as dataframe
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
    
    #expected difference
    mean_difference <- overall_results %>% 
      summarise(dif=mean(B_overall-A_overall))
    
    #Df ready for ggplot
    dist_overall <- overall_results %>%
      gather(key = "variation", value = "overall", A_overall, B_overall)
    
    #Distiribution of differences
    dist_dif_overall <- overall_results %>% 
      transmute(dif = B_overall-A_overall)
    
    list(post_samp_transformed, overall_results, overall_better, tab_overall, mean_difference, dist_overall, dist_dif_overall)
    
  })
  
  #Table for overview of analysis
  output$mrr_per_visitor_tab <- DT::renderDataTable({
    datatable(conv_mrr_model()[[4]],
              class = "cell-border stripe", rownames = FALSE, selection = "none",
              options = list(paging = F, searching = F, ordering = F, info = FALSE, columnDefs = list(
                list(targets = -1, visible=FALSE),list(className = "dt-right", targets = 5)))) %>% 
      formatStyle(
        "Prob. of outperforming", valueColumn = ncol(conv_mrr_model()[[4]]),
        background = styleColorBar(c(0,100), "deepskyblue"))%>% 
      formatStyle(
        "Variation",
        backgroundColor = styleEqual(c("A", "B"), c("#E41A1C", "#377EB8")))
  })
  
  #Plot for both distribution sepparately
  output$overall_dist <- renderPlotly({
    
    g_overall_dist <- conv_mrr_model()[[6]] %>% 
      ggplot(aes(x = overall, fill = variation , color = variation))+
      ggtitle("Model implied distributions of MRR per visitor")+
      geom_density(alpha = 0.7)+
      scale_fill_brewer(palette="Set1")+
      scale_x_continuous(name  = "MRR per visitor", labels = dollar_format(), breaks = scales::pretty_breaks(n = 10))+
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
  
  #Plot for distribution of difference (Everything is nearly the same as previous model plots)
  output$overall_dist_dif <- renderPlotly({
    req(input$creds3)
    
    pre_g_overall_dist_dif <- conv_mrr_model()[[7]] %>%
      ggplot(aes(x = dif))+
      geom_density()
    
    dens3 <- ggplot_build(pre_g_overall_dist_dif)$data[[1]]
    
    g_overall_dist_dif <- ggplot(data = conv_mrr_model()[[7]], aes(x=dif))+
      geom_density(color = "#4DAF4A", fill = "#4DAF4A")+
      ggtitle("Model implied distribution of difference of MRR per visitor")+
      #Again possibility for HDI here.
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
      scale_x_continuous(name  = "Mean MRR per visitor of B - Mean MRR per visitor of A", labels = dollar_format(), breaks = scales::pretty_breaks(n = 10))+
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