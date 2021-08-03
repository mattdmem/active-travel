library(shiny)
library(shinycssloaders)
library(ggplot2)
library(plyr)
library(hrbrthemes)
library(dplyr)
library(waffle)
library(ftplottools)
library(stringr)
navy = rgb(25, 62, 114, maxColorValue = 255)
coral = rgb(234, 93, 78, maxColorValue = 255)
orange = rgb(242, 147, 48, maxColorValue = 255)
yellow = rgb(254, 212, 122, maxColorValue = 255)
purple = rgb(102, 103, 173, maxColorValue = 255)
aqua = rgb(46, 169, 176, maxColorValue = 255)
green = rgb(70, 168, 108, maxColorValue = 255)
grey = rgb(172, 188, 195, maxColorValue = 255)

grey60 = rgb(206, 214, 219, maxColorValue = 255)
green60 = rgb(121, 185, 137, maxColorValue = 255)
navy60 = rgb(116, 124, 163, maxColorValue = 255)
coral60 = rgb(242, 188, 149, maxColorValue = 255)
orange60 = rgb(249, 193, 135, maxColorValue = 255)

orange20 = rgb(253,235,216, maxColorValue = 255)
green20 = rgb(226, 236, 227, maxColorValue = 255)
grey20 = rgb(239,241,243, maxColorValue = 255)
navy20 = rgb(208,208,224, maxColorValue = 255)
coral20 = rgb(248,221,219, maxColorValue = 255)

schemes = c("sheaf_valley_cycle_route",
          "crookes_active_neighbourhood",
          "nether_edge_active_neighbourhood",
          "neepsend_kelham_city_centre",
          "nether_edge_city_centre",
          "darnall_attercliffe_city_centre",
          "magna_tinsley"
          )

titles = c("Sheaf Valley\nCycle Route",
           "Crookes Active\nNeighbourhood",
           "Nether Edge\nActive\nNeighbourhood",
           "Neepsend Kelham\nCity",
           "Nether Edge\nCity",
           "Darnall\nAttercliffe\nCity",
           "Magna\nTinsley")

names(titles)=schemes

feeling = c(0,25,50,75,100)
meaning = c("oppose","somewhat-oppose","neutral","somewhat-support","support")

get_data = function(scheme){
  scheme_new = str_replace_all(scheme,"_","-")
  print(scheme_new)
  url=paste0("https://connectingsheffield.commonplace.is/schemes/proposals/",scheme_new,"/comments.json?pageSize=10000")
  data=jsonlite::fromJSON(url)
  return(data)
}



# Define UI ----
ui <- navbarPage("Connecting Sheffield: Live Results",id = "navbarID", inverse = T,
                 tabPanel("All",
                          h2("Connecting Sheffield: Better Travel Choices"),
                          p("This is an UNOFFICIAL live summary of the comments on Sheffield City Councils active travel proposals. For more information see the official site: https://connectingsheffield.commonplace.is"),
                          h4("All Proposal Comments by Feeling"),
                          p("This takes a a few seconds to load while the app fetches all the comments."),
                          withSpinner(plotOutput("all_plot",height=700),color = grey)),
                 do.call(navbarMenu, c("Individual Schemes", lapply(schemes, function(i) {
                   
                   tabPanel(
                               i,
                               h2(titles[[i]]),
                               withSpinner(plotOutput(paste0("summary_plot_",i),width=700),color=grey),
                               h4("Comments Over Time"),
                               withSpinner(plotOutput(paste0("time_plot_",i),height=700),color=grey),
                               h4("Comments Over Time - Proportion"),
                               withSpinner(plotOutput(paste0("time_plot_prop_",i),height=700),color=grey),
                              )
                            }))),
                tabPanel("About",
                         h2("About"),
                         p("This app fectches comments from the \"Connecting Sheffield: Better travel choices\" website and plots them in various ways."),
                         p("Written by Matthew Paker (twitter: @bioinfomatt)")
                         ),
                 mainPanel(
                # actionButton("browser", "browser"),
                # tags$script("$('#browser').hide();")
                # Add to your server 
                # And to show the button in your app, go 
                # to your web browser, open the JS console, 
                # And type:
                # $('#browser').show();
                 #   h2("Connecting Sheffield: Better Travel Choices"),
                 #   p("This is an UNOFFICIAL live summary of the comments on Sheffield City Councils active travel proposals. For more information see the official site: https://connectingsheffield.commonplace.is"),
                 #   h4("All Proposal Comments by Feeling"),
                 #   withSpinner(plotOutput("all_plot",height=700),color = grey)
                )
)

# Define server logic ----
server <- function(input, output) {
  
  # observeEvent(input$browser,{
  #   browser()
  # })
  datas=list()
  
  for(i in schemes){ 
    print(i)
    assign(paste0("data_",i),get_data(i)$data) 
    datas = c(datas,list(eval(as.name(paste0("data_",i)))))
  }
  names(datas) = schemes
  
  summary_list=list()
  for (i in schemes){
    dat=data.frame(feeling=datas[[i]]$feeling) %>% group_by(feeling) %>% dplyr::summarise(n=n()) %>% mutate(scheme=i)
    summary_list[[i]] = dat
  }
  summary = do.call(rbind, summary_list) 
  
  
  output$all_plot <- renderPlot({
    
   
    
    ggplot(summary, aes(fill = as.factor(feeling), values = n)) +
      geom_waffle(color = "white", size = .25, n_rows = 10, flip = TRUE) +
      facet_wrap(~scheme, nrow = 1, strip.position = "bottom",labeller=as_labeller(titles)) +
      scale_x_discrete() +
      scale_y_continuous(labels = function(x) x * 10, # make this multiplyer the same as n_rows
                         expand = c(0,0)) +
      ggthemes::scale_fill_tableau(name=NULL) +
      labs(
        caption = "*All active proposals, data from all live displayable comments. Color based on 'Feeling' field.",
        x = "Proposal",
        y = "Comments"
      ) +
      ft_theme() +
      theme(panel.grid = element_blank(), axis.ticks.y = element_line()) +
      # guides(fill = guide_legend(reverse = TRUE))+
      # scale_fill_manual(values=c(coral,coral60,grey,green60,green))+
      scale_fill_manual("Feeling",values=c(coral,coral60,grey,navy60,navy),labels=meaning,breaks=feeling)
  })
  # observeEvent(input$navbarID, {dat=reactive(eval(as.name(paste0("data_",input$navbarID))))})
  dat = reactive(eval(as.name(paste0("data_",input$navbarID))))
  selected =  reactive(input$navbarID)
  for (s in schemes){
    print(paste0("plotting over time for ",s))
    output[[paste0("time_plot_",s)]] = renderPlot({
    ggplot(dat(),aes(as.Date(date),color=as.factor(feeling),fill=as.factor(feeling)))+
      geom_bar(position="stack")+
      ft_theme()+
      scale_fill_manual("Feeling",values=c(coral,coral60,grey,green60,green),labels=meaning,breaks=feeling)+
      scale_color_manual("Feeling",values=c(coral,coral60,grey,green60,green),labels=meaning,breaks=feeling)+
      scale_x_date("Date of Comment")+
      scale_y_continuous("Number of Comments")
    })
      
    print(paste0("plotting over time (prop) for ",s))
    output[[paste0("time_plot_prop_",s)]] = renderPlot({
      ggplot(dat(),aes(as.Date(date),color=as.factor(feeling),fill=as.factor(feeling)))+
        geom_bar(position="fill")+
        ft_theme()+
        scale_fill_manual("Feeling",values=c(coral,coral60,grey,green60,green),labels=meaning,breaks=feeling)+
        scale_color_manual("Feeling",values=c(coral,coral60,grey,green60,green),labels=meaning,breaks=feeling)+
        scale_x_date("Date of Comment")+
        scale_y_continuous("Number of Comments")
        
      })
    
    print(paste0("plotting summary for ",s))
    output[[paste0("summary_plot_",s)]] = renderPlot({
      
      ggplot(summary %>% filter(scheme==selected()) %>% mutate(percentage=n/sum(n)*100), aes(x = "", y = n, fill = as.factor(feeling))) +
        geom_bar(position = position_stack(), stat = "identity", width = .7) +
        geom_text(aes(label = round(percentage,1)), position = position_stack(vjust = 0.5), size = 4,color="white") +
        coord_flip()+
        labs(fill = NULL, colour = NULL) +
        theme_ipsum_rc(grid="") +
        theme_enhance_waffle()+
        scale_fill_manual("Feeling",values=c(coral,coral60,grey,green60,green),labels=meaning,breaks=feeling)
    
      })
  }
    
  }
  

# Run the app ----
shinyApp(ui = ui, server = server)
