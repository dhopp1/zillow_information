library(shinyapps)
library(shiny)
library(RCurl)
library(XML)
county <- read.csv("FIPS.csv")
shinyServer(function(input, output, session) {
  output$table <- renderTable({
    inFile <- input$file1
    if(is.null(inFile)){
      output$downloadReport <- downloadHandler(
        filename = "Zillowed Output.csv",
        content = function(file){write.csv(output, file, row.names = F)}
      )
      template <- data.frame(unique = "", address = "", zip ="")
      
      output$downloadTemplate <- downloadHandler(
        filename = "Addresses to Zillow.csv",
        content = function(file){write.csv(template, file, row.names = F)}
      )
      if(input$type==T){
        return(NULL)
      }else{
       addresses <- data.frame(unique = rep(NA, nrow(data.frame((strsplit(as.character(input$uniques), "\n"))))), address = rep(NA, nrow(data.frame((strsplit(as.character(input$uniques), "\n"))))), zip = rep(NA, nrow(data.frame((strsplit(as.character(input$uniques), "\n"))))))
        #addresses <- data.frame(unique =numeric(0) , address = character(0), zip = character(0))
        if(nrow(data.frame((strsplit(as.character(input$uniques), "\n")))) == 1){
          addresses[1,"unique"] <- as.character(input$uniques)
          addresses[,"address"] <- as.character(input$addresses)
          addresses[,"zip"] <- as.character(input$zips)
        }else{
          for(i in 1:nrow(data.frame((strsplit(as.character(input$uniques), "\n"))))){
            addresses[i,"unique"] <- unlist(strsplit(as.character(input$uniques), "\n"))[i] 
          }
          for(i in 1:nrow(data.frame((strsplit(as.character(input$uniques), "\n"))))){
            addresses[i,"address"] <- unlist(strsplit(as.character(input$addresses), "\n"))[i] 
          }
          for(i in 1:nrow(data.frame((strsplit(as.character(input$uniques), "\n"))))){
            addresses[i,"zip"] <- unlist(strsplit(as.character(input$zips), "\n"))[i] 
          }
        }
        if(!is.na(addresses[1,"unique"]) & !is.na(addresses[1, "address"]) & !is.na(addresses[1, "zip"])){
          addresses$address <- as.character(addresses$address)
          addresses$zip <- as.numeric(addresses$zip)
          
          output <- data.frame(unique = numeric(0),address = character(0), city = character(0),state = character(0),zip = numeric(0),county = character(0), zestimate = numeric(0), rentzestimate = numeric(0), yb = numeric(0), beds = numeric(0), baths = numeric(0),   sqft = numeric(0), lastsoldprice = numeric(0), lastsolddate = numeric(0), zillowid = numeric(0), zillowaddress = character(0))
          for(q in 1:nrow(addresses)){
            if(input$apikey==T){
              apikey <- input$ownapi
            }else{
              apikey <- "X1-ZWz1dvy6tisft7_60bkd"
            }
            txt <- getForm("http://www.zillow.com/webservice/GetDeepSearchResults.htm",
                           address = addresses[q,"address"], citystatezip = addresses[q, "zip"], 'zws-id' = apikey, rentzestimate = T)
            
            if(nchar(txt) > 850){
              doc = xmlParse(txt, asText = TRUE)
              principal = doc[["//response/results/result"]]
              output <- rbind(output,
                              data.frame(
                                unique = addresses[q, "unique"],
                                address = addresses[q,"address"],
                                city = xmlValue(principal[["address"]][["city"]]),
                                state = xmlValue(principal[["address"]][["state"]]),
                                zip = addresses[q, "zip"],
                                county = if(length(county[county$fips == as.numeric(xmlValue(principal[["FIPScounty"]])),"county"])==1){as.character(county[county$fips == as.numeric(xmlValue(principal[["FIPScounty"]])),"county"])}else{NA},
                                zestimate = as.numeric(xmlValue(principal[["zestimate"]][["amount"]])),
                                rentzestimate = as.numeric(xmlValue(principal[["rentzestimate"]][["amount"]])),
                                yb = as.numeric(xmlValue(principal[["yearBuilt"]])),
                                beds = as.numeric(xmlValue(principal[["bedrooms"]])),
                                baths = as.numeric(xmlValue(principal[["bathrooms"]])),
                                sqft = as.numeric(xmlValue(principal[["finishedSqFt"]])),
                                lastsolddate = xmlValue(principal[["lastSoldDate"]]) ,
                                lastsoldprice = as.numeric(xmlValue(principal[["lastSoldPrice"]])),
                                zillowid = as.numeric(xmlValue(principal[["zpid"]])),
                                zillowaddress = toString(xmlValue(principal[["address"]][["street"]]))
                                )
              
                              )
            }else{
              output <- rbind(output,data.frame(
                unique = addresses[q, "unique"],
                address = addresses[q,"address"], 
                city = NA,
                state = NA,
                zip = addresses[q, "zip"],
                county = NA,
                zestimate = NA,
                rentzestimate = NA,
                yb = NA,
                beds = NA,
                baths = NA,
                sqft = NA,
                lastsolddate = NA,
                lastsoldprice = NA,
                zillowid = NA,
                zillowaddress = NA)
              )
            }
          }
          output$unique <- as.character(output$unique)
          output$zip <- as.character(output$zip)
          output$zestimate <- paste("$", format(output$zestimate, big.mark=",", scientific=F), sep="")
          output$rentzestimate <- paste("$", format(output$rentzestimate, big.mark=",", scientific=F), sep="")
          output$yb <- as.character(output$yb)
          output$beds <- as.character(output$beds)
          output$baths <- as.character(output$baths)
          output$sqft <- format(output$sqft, big.mark=",", scientific=F)
          output$lastsoldprice <- paste("$", format(output$lastsoldprice, big.mark=",", scientific=F), sep="")
          output$zillowid <- as.character(output$zillowid)
          if(!is.na(output[1,"address"]) & !is.na(output[1, "zip"])){output}
        }
      }
    }else{
      output$downloadReport <- downloadHandler(
        filename = "Zillowed Output.csv",
        content = function(file){write.csv(output, file, row.names = F)}
      )
      template <- data.frame(unique = "", address = "", zip ="")
      
      output$downloadTemplate <- downloadHandler(
        filename = "Addresses to Zillow.csv",
        content = function(file){write.csv(template, file, row.names = F)}
      )
      
      addresses <- read.csv(inFile$datapath, sep=",")
      addresses$address <- as.character(addresses$address)
      addresses$zip <- as.numeric(addresses$zip)
      
      output <- data.frame(unique = numeric(0),address = character(0), city = character(0),state = character(0),zip = numeric(0),county = character(0), zestimate = numeric(0), rentzestimate = numeric(0), yb = numeric(0), beds = numeric(0), baths = numeric(0),   sqft = numeric(0), lastsoldprice = numeric(0), lastsolddate = numeric(0), zillowid = numeric(0), zillowaddress = character(0))
      for(q in 1:nrow(addresses)){
        if(input$apikey==T){
          apikey <- input$ownapi
        }else{
          apikey <- "X1-ZWz1dvy6tisft7_60bkd"
        }
        txt <- getForm("http://www.zillow.com/webservice/GetDeepSearchResults.htm",
                       address = addresses[q,"address"], citystatezip = addresses[q, "zip"], 'zws-id' = apikey, rentzestimate = T)
        
        if(nchar(txt) > 850){
          doc = xmlParse(txt, asText = TRUE)
          principal = doc[["//response/results/result"]]
          output <- rbind(output,
                          data.frame(
                            unique = addresses[q, "unique"],
                            address = addresses[q,"address"],
                            city = xmlValue(principal[["address"]][["city"]]),
                            state = xmlValue(principal[["address"]][["state"]]),
                            zip = addresses[q, "zip"],
                            county = if(length(county[county$fips == as.numeric(xmlValue(principal[["FIPScounty"]])),"county"])==1){as.character(county[county$fips == as.numeric(xmlValue(principal[["FIPScounty"]])),"county"])}else{NA},
                            zestimate = as.numeric(xmlValue(principal[["zestimate"]][["amount"]])),
                            rentzestimate = as.numeric(xmlValue(principal[["rentzestimate"]][["amount"]])),
                            yb = as.numeric(xmlValue(principal[["yearBuilt"]])),
                            beds = as.numeric(xmlValue(principal[["bedrooms"]])),
                            baths = as.numeric(xmlValue(principal[["bathrooms"]])),
                            sqft = as.numeric(xmlValue(principal[["finishedSqFt"]])),
                            lastsolddate = xmlValue(principal[["lastSoldDate"]]) ,
                            lastsoldprice = as.numeric(xmlValue(principal[["lastSoldPrice"]])),
                            zillowid = as.numeric(xmlValue(principal[["zpid"]])),
                            zillowaddress = toString(xmlValue(principal[["address"]][["street"]])))
          )
        }else{
          output <- rbind(output,data.frame(
            unique = addresses[q, "unique"],
            address = addresses[q,"address"], 
            city = NA,
            state = NA,
            zip = addresses[q, "zip"],
            county = NA,
            zestimate = NA,
            rentzestimate = NA,
            yb = NA,
            beds = NA,
            baths = NA,
            sqft = NA,
            lastsolddate = NA,
            lastsoldprice = NA,
            zillowid = NA,
            zillowaddress = NA)
          )
        }
      }
      output$unique <- as.character(output$unique)
      output$zip <- as.character(output$zip)
      output$zestimate <- paste("$", format(output$zestimate, big.mark=",", scientific=F), sep="")
      output$rentzestimate <- paste("$", format(output$rentzestimate, big.mark=",", scientific=F), sep="")
      output$yb <- as.character(output$yb)
      output$beds <- as.character(output$beds)
      output$baths <- as.character(output$baths)
      output$sqft <- format(output$sqft, big.mark=",", scientific=F)
      output$lastsoldprice <- paste("$", format(output$lastsoldprice, big.mark=",", scientific=F), sep="")
      output$zillowid <- as.character(output$zillowid)
      output
    }
  }, include.rownames=F)
})