##########################################################################
#                                                                        #
#                        WEB SCRAPING WITH R                             #
#                                                                        #
##########################################################################

########################## PACKAGES ######################################

install.packages("tidyverse")
library(tidyverse)

install.packages("rvest")
library(rvest)

install.packages("xml2")
library(xml2)

install.packages("httr")
library(httr)


####################### READING HTML #################################

sample_html <- '
   <!DOCTYPE html>
   <html lang="en">
   <head>
       <meta charset="UTF-8">
       <meta name="viewport" content="width=device-width, initial-scale=1.0">
       <title>SICSS</title>
   </head>
   <body>
       <div>
       <h1>Summer Institute of Computational Social Sciences (SICSS)</h1>
       <p>A great organization for advancing knowledge in computational
          social sciences.</p>
        </div>
        <div>
       <h2>Web Scraping with R</h2>
       <p>We will learn how to extract data from website 
          for analytics and research purposes.</p>
       <img src="about-sicss.png" alt="About SICSS">
       <a href="sicss@gmail.com">Contact Us</a>
       I admit SICSS is a great community to be a part of.
       </div>
   </body>
   </html>
'

html <- read_html(x = sample_html)

## Check the structure of the HTML document
xml_structure(x = html)
xml_structure(x = html, indent = 3)

####################### NAVIGATING HTML #################################

#........... Navigating Nodes with Selectors

html_node(html, "p")
html_node(html, "h1")
html_node(html, "body")

html %>% html_node("p") %>% html_text()
html %>% html_nodes("p") %>% html_text()

html %>% html_element("p")
html %>% html_elements("p")

html %>% html_element("p") %>% html_text()
html %>% html_elements("p") %>% html_text()

#.......... Navigating Attributes

html %>% html_element("a") %>% html_attr("href")

html %>% html_element("img") %>% html_attr("alt")
html %>% html_element("img") %>% html_attr("src")
html %>% html_element("img") %>% html_attrs()


####################### SCRAPING TABLES #################################

table <- '
  <table border="1">
        <tbody>
            <tr>
                <th>Category</th><th>Definition</th><th>Skills</th><th>Applications</th>
            </tr>
            <tr>
                <td>Data</td><td>Analysis</td><td>Statistics</td><td>Research</td>
            </tr>
            <tr>
                <td>Machine</td><td>Learning</td><td>Algorithms</td><td>Recognition</td>
            </tr>
            <tr>
                <td>Big</td><td>Data</td><td>Hadoop</td><td>Analytics</td>
            </tr>
        </tbody>
    </table>
'





# Table of Nobel Laureates on a Wikipedia page
url = "https://en.wikipedia.org/wiki/List_of_Nobel_laureates"



###################### CASCADING STYLE SHEETS (CSS) ########################



#......... Type Selectors



#......... Classes and IDs



#......... Pseudo Classes



#......... CSS Combinators 
# (Descendant, Child, Adjacent sibling, General sibling)




###################### HTTP REQUESTS ########################



#......... GET a request



#......... Modify Headers





### SICSS Calabar

html <- read_html(x = "https://sicss.io/2023/calabar/people")

html <- httr::GET("https://sicss.io/2023/calabar/people")

html <- content(html)

html %>% html_elements("img") %>% html_attr("alt")

