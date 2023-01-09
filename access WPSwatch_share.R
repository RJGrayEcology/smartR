############################################
### Scrape WPS Watch Data
############################################

### Step 1.
# download and install Docker
# https://www.docker.com/products/docker-desktop/

### Step 2
# Open Docker Desktop

### Step 3. 
# Open your command prompt and input: docker run -d -p 4445:4444 selenium/standalone-chrome
# then write: docker ps

# continue with the code afterward

# set working directory, this is the folder your data will be saved
# change all \ to / 
setwd("C:/Your/File/Folder/Path/Goes/Here")

# open libraries (install first if necessary)
require(RSelenium)
require(rvest)
require(xml2)
require(tidyverse)
require(RCurl)
require(rjson)

# force the docker instance from shell 
# if you get an error message, start from here again!
# if it still doesn't work, restart your R session
shell('docker run -d -p 4444:4444 -v /dev/shm:/dev/shm selenium/standalone-chrome:3.141.59-20200326')

# open a docker container image with R
remDr <- RSelenium::remoteDriver(remoteServerAddr = "localhost",
                                 port = 4444L,
                                 browserName = "chrome")
# open the remote server
remDr$open()

# access the login page of WPS watch
remDr$navigate("https://wpswatch.org/Spa/Account/Login") #Entering our URL gets the browser to navigate to the page

#screenshot the page to see if it worked
remDr$screenshot(display = TRUE)

# Find the username and password fields
username_field <- remDr$findElement(using = "xpath", "//*[@id='Email']")
password_field <- remDr$findElement(using = "xpath", "//*[@id='Password']")

# write your username and password here
#username <- "example_username"
#password <- "example_password"

# Enter the login credentials
username_field$sendKeysToElement(list("example_email@email.com")) # change this to your email
password_field$sendKeysToElement(list("Examplepassword123")) # change this to your login PW

# Find the submit button and click it
submit_button <- remDr$findElement(using = "xpath", "//*[@id='page-wrapper']/div/div/form/button")
submit_button$clickElement()

# Now navigate to the data page
remDr$navigate("https://wpswatch.org/Spa/Incidents") #Entering our URL gets the browser to navigate to the page
# Wait for the page to load
Sys.sleep(5)
# show the page to make sure it worked
remDr$screenshot(display = TRUE) #This will take a screenshot and display it in the RStudio viewer

# scrape the data table
table_element <- remDr$findElement(using="xpath", value="//*[@id='page-wrapper']/div[2]/div[3]/div/div/div/div[2]/div[1]/table")

# Get the HTML source of the page
html_source <- remDr$getPageSource()[[1]]

# Parse the HTML source
doc <- read_html(html_source)

# extract the html table
data <- html_table(doc)[[1]]

# remove the "photo" column, its unnecessary 
data <- data[,-6]

#--------------------------------------------------------------------------------

# Only 25 records show up, but there are hundreds. we need to loop and scrape
# Initialize a variable to store the "Next" button element
next_button <- NULL

# make an empty dataframe to contain the data
df <- data.frame()

# Loop until the "Next" button is not found
while(is.null(next_button)) {
  
  # Find the "Next" button element
  next_button <- remDr$findElement(using = "xpath", "//*[@id='page-wrapper']/div[2]/div[3]/div/div/div/div[2]/div[2]/div/ul/li[7]/a")
  
  # Check if the "Next" button is found
  if(!is.null(next_button)) {
    
    # Click on the "Next" button
    next_button$clickElement()
    
    # Wait for the page to load
    Sys.sleep(5)
    
    # scrape the data table
    table_element <- remDr$findElement(using="xpath", value="//*[@id='page-wrapper']/div[2]/div[3]/div/div/div/div[2]/div[1]/table")
    
    # Get the HTML source of the page
    html_source <- remDr$getPageSource()[[1]]
    
    # Parse the HTML source
    doc <- read_html(html_source)
    
    # extract the html table
    data <- html_table(doc)[[1]]
    
    # remove the "photo" column, its unnecessary 
    data <- data[,-6]
    
    # Append the data from the current page to the data from previous pages
    df <- rbind(df, data)
    
    # Reset the "Next" button element
    next_button <- NULL
  }
}

# save the resulting data frame as a .csv
write.csv(df, "PoacherCam_data.csv")

#############################################
### Basic analysis
#############################################

# remove scientific notation
options(scipen = 999)

# how many records in total
length(df$Status)

# check the status numbers
table(df$Status)

# check the status percentages
table(df$Status)/sum(table(df$Status)*100)

# how many records per site
df%>%
  group_by(Deployment)%>%
  count(Status)


