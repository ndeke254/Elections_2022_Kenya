# application user interface
ui <- navbarPage(
  tags$style(HTML("

.selectize-input.items.not-full.has-options:before {
content:'';
background: url('kenya.png') ; /*url of image*/
height: 20px; /*height of image*/
width: 20px;  /*width of image*/
position: absolute;
 display: block;
 position: absolute;
 left: 0;
 background-size: 20px 20px;
 background-repeat: no-repeat;
 margin-left: 3px;
}

.selectize-input.dropdown-active:before {
    top: 0;
    margin-top: 6px;
 }
  
  .selectize-input.items.not-full.has-options {
    padding-left: 24px;
  }
 
 .selectize-input.items.not-full.has-options.has-items {
    padding-left: 0px;
 }
 
  .selectize-input.items.not-full.has-options .item:first-child {
      margin-left: 20px;
 }

")),
  header = tagList(
    useShinydashboard()
    ),
  title = tags$div(
    class ='logo',
    tags$a(
      tags$img(
        src='shape.png',
        width='50px'
      ),
      "KENYA'S CHOICE 2022"
    )
  ),
 tabPanel(
   title = 'Overall',
   value = 'overall',
   column(6,
          wellPanel(
     
   )
   ),
   column(6,
          wellPanel(
            
          )
   )
 ),
 tabPanel(
   #create a select input
   selectizeInput(
     inputId = 'search_name',
                     label = 'County:',
                     width = '200px',
                     choices = NULL,
                     options = list(placeholder ='Search County',
                                    create = FALSE,
                                    maxOptions = 5,
                                    maxItems = '1',
                                    onDropdownOpen = I("function($dropdown) {if (!this.lastQuery.length) {this.close(); this.settings.openOnFocus = false;}}"),
                                    onType = I("function (str) {if (str === \"\") {this.close();}}"))
                     ),
   fluidRow(
     infoBoxOutput('county_name',
                   width = 2),
     valueBoxOutput('registered',
                    width = 2)
   ),
   wellPanel(
     
   )
                   ),
 tabPanel(
   checkboxInput(
     inputId = 'check',
     label = '',
     width = '12px'
   )
 ),
 tabPanel(
   title ='Diaspora',
   value = 'diaspora'
 ),
 
 includeCSS(path = "www/styles.css")
)

