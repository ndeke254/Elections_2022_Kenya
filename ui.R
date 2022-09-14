# application user interface
ui <- navbarPage(
 title = 'Home',
 header = column(12,
                 tags$div(
                   class = 'header',
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
                     )
                   )
 )
)


