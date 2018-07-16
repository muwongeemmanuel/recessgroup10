

shinyUI(fluidPage(
  
  tags$style(type = 'text/css', 
             '.navbar { background-color: black;color: blue;}',
             '.navbar-default .navbar-brand{color: blue;}',
             
             'body{ color: blue; background-color:green}',
             '#sidebar {
             background-color: brown;
             }',
            ' #choice3{ background-color:brown}'
  ),
  #useShinyjs(),
  
 
  
  uiOutput("sidepage")
))






