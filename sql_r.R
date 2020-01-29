# open xampp and view database in phpmyadmin in information_schema database in COLUMNS table
library(RMySQL)
con = dbConnect(MySQL(),user="root",password="",dbname="information_schema",host="127.0.0.1")
dataFromDB <- dbGetQuery(con,"SELECT * FROM COLUMNS")
dbDisconnect(con)


ibrary(dplyr)
library(ggplot2)
library(RMySQL)
con = dbConnect(MySQL(),user="root",password="",dbname="information_schema",host="127.0.0.1")
query <- dbGetQuery(con,"SELECT 
  	                      NUMBER AS number, 
                          COUNT(NUMBER) AS scale
                          FROM (
                                  SELECT 
		                                CASE 
    		                              WHEN COUNT(TABLE_SCHEMA) >10 THEN 'more than 10'
        		                        ELSE 'less than 10' END
       	                          AS NUMBER
                                  FROM COLUMNS
                                  GROUP BY TABLE_NAME
                              ) AS TB_NUMBER
                          GROUP BY NUMBER")
dbDisconnect(con)

## BAR CHART fix x, y ##

#Marriages by officiate
ggplot(query, aes(x=number, y=scale)) + 
  geom_bar(stat="identity") 



## BAR CHART x ##
ibrary(dplyr)
library(ggplot2)
library(RMySQL)
con = dbConnect(MySQL(),user="root",password="",dbname="information_schema",host="127.0.0.1")
query <- dbGetQuery(con,"SELECT * FROM COLUMNS")
dbDisconnect(con)

ggplot(query, aes(x = DATA_TYPE)) +
  geom_bar(fill = "cornflowerblue",
           color="black") +
  labs(x = "DATA_TYPE",
       y = "Scale",
       title = "Participants by DATA_TYPE")

## BAR CHART x with pecent ##
ibrary(dplyr)
library(ggplot2)
library(RMySQL)
con = dbConnect(MySQL(),user="root",password="",dbname="information_schema",host="127.0.0.1")
query <- dbGetQuery(con,"SELECT 
	                          a.DATA_TYPE , (	
        			                              SELECT 
        				                            COUNT(b.DATA_TYPE) 
        			                              FROM COLUMNS AS b
        			                              WHERE b.DATA_TYPE = a.DATA_TYPE 	
    			                                )AS n
                          FROM COLUMNS AS a")
dbDisconnect(con)

ggplot(query, aes(x = DATA_TYPE)) +
  geom_bar() + 
  labs(x = "DATA_TYPE",
       y = "Scale",
       title = "Participants by DATA_TYPE")




# create a basic ggplot2 pie chart
ibrary(dplyr)
library(ggplot2)
library(RMySQL)
con = dbConnect(MySQL(),user="root",password="",dbname="information_schema",host="127.0.0.1")
query <- dbGetQuery(con,"SELECT 
	                          a.DATA_TYPE , (	
        			                              SELECT 
        				                            COUNT(b.DATA_TYPE) 
        			                              FROM COLUMNS AS b
        			                              WHERE b.DATA_TYPE = a.DATA_TYPE 	
    			                                )AS n
                          FROM COLUMNS AS a")
dbDisconnect(con)
plotdata <- query %>%
  count(DATA_TYPE) %>%
  arrange(desc(DATA_TYPE)) %>%
  mutate(prop = round(n * 100 / sum(n), 1), lab.ypos = cumsum(prop) - 0.5 *prop)
plotdata$label <- paste0(plotdata$race, "\n",round(plotdata$prop), "%")
ggplot(plotdata, aes(x = "", y = prop, fill = DATA_TYPE)) +
  geom_bar(width = 1, stat = "identity", color = "black") +
  geom_text(aes(y = lab.ypos, label = label), color = "black") +
  coord_polar("y", start = 0, direction = -1) +
  theme(legend.position = "FALSE") +
  labs(title = "Participants by DATA_TYPE")
  theme_void()
  
  
  # create a treemap of marriage officials
  ibrary(dplyr)
  library(ggplot2)
  library(RMySQL)
  con = dbConnect(MySQL(),user="root",password="",dbname="information_schema",host="127.0.0.1")
  query <- dbGetQuery(con,"SELECT 
	                          a.DATA_TYPE , (	
        			                              SELECT 
        				                            COUNT(b.DATA_TYPE) 
        			                              FROM COLUMNS AS b
        			                              WHERE b.DATA_TYPE = a.DATA_TYPE 	
    			                                )AS n
                          FROM COLUMNS AS a")
  dbDisconnect(con)
  library(treemapify)
  plotdata <- query %>%
    count(DATA_TYPE)
  ggplot(plotdata, aes(fill = DATA_TYPE, area = n)) +
    geom_treemap() +
    labs(title = "Marriages by DATA_TYPE")
  
  
  # create a treemap with tile labels
  ibrary(dplyr)
  library(ggplot2)
  library(RMySQL)
  con = dbConnect(MySQL(),user="root",password="",dbname="information_schema",host="127.0.0.1")
  query <- dbGetQuery(con,"SELECT 
	                          a.DATA_TYPE , (	
        			                              SELECT 
        				                            COUNT(b.DATA_TYPE) 
        			                              FROM COLUMNS AS b
        			                              WHERE b.DATA_TYPE = a.DATA_TYPE 	
    			                                )AS n
                          FROM COLUMNS AS a")
  dbDisconnect(con)
  library(treemapify)
  plotdata <- query %>%
    count(DATA_TYPE)
  ggplot(plotdata, aes(fill = DATA_TYPE, area = n , label = DATA_TYPE)) +
    geom_treemap() +
    geom_treemap_text(colour = "white", place = "centre") +
    labs(title = "Marriages by DATA_TYPE")+
    theme(legend.position = "none")
  
  ####################################################################################################
  # Create a kernel density plot of DATA_TYPE
  library(dplyr)
  library(ggplot2)
  library(RMySQL)
  con = dbConnect(MySQL(),user="root",password="",dbname="information_schema",host="127.0.0.1")
  query <- dbGetQuery(con,"SELECT 
	                          a.DATA_TYPE , (	
        			                              SELECT 
        				                            COUNT(b.DATA_TYPE) 
        			                              FROM COLUMNS AS b
        			                              WHERE b.DATA_TYPE = a.DATA_TYPE 	
    			                                )AS n
                          FROM COLUMNS AS a")
  dbDisconnect(con)
  # Create a kernel density plot of DATA_TYPE
  ggplot(query, aes(x = n)) +
    geom_density(fill = "indianred3") +
    labs(title = "Participants by n")
  
  
  # Plot ages as a dot plot using
  library(dplyr)
  library(ggplot2)
  library(RMySQL)
  con = dbConnect(MySQL(),user="root",password="",dbname="information_schema",host="127.0.0.1")
  query <- dbGetQuery(con,"SELECT 
	                          a.DATA_TYPE , (	
        			                              SELECT 
        				                            COUNT(b.DATA_TYPE) 
        			                              FROM COLUMNS AS b
        			                              WHERE b.DATA_TYPE = a.DATA_TYPE 	
    			                                )AS n
                          FROM COLUMNS AS a")
  dbDisconnect(con)
  # gold dots with black borders
  ggplot(query, aes(x = DATA_TYPE)) +
    geom_dotplot(fill = "gold", color = "black") +
    labs(title = "Participants by age", y = "Proportion", x = "DATA_TYPE")
  
  
  ###################################################################################################################
  
  # stacked bar chart
  library(dplyr)
  library(ggplot2)
  library(RMySQL)
  con = dbConnect(MySQL(),user="root",password="",dbname="information_schema",host="127.0.0.1")
  query <- dbGetQuery(con,"SELECT 
	                          a.DATA_TYPE , (	
        			                              SELECT 
        				                            COUNT(b.DATA_TYPE) 
        			                              FROM COLUMNS AS b
        			                              WHERE b.DATA_TYPE = a.DATA_TYPE 	
    			                                )AS n
                          FROM COLUMNS AS a")
  dbDisconnect(con)
  ggplot(query,
         aes(x = DATA_TYPE,
             fill = n)) +
    geom_bar(position = "stack")