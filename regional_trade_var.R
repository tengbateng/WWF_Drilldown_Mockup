
regional_trade_var <- function(year=2015, ind = "KG"){
      library(plyr)
      
      setwd("C:/R Stuff/LPC")
      alerts <- read.csv("LPC WWF Alerts.csv", header = TRUE)
      
      ## Convert the Export and Import quantities into numerical values
      alerts$ExportedValue <- as.numeric(alerts$ExportedValue)
      alerts$ExportedVolume <- as.numeric(alerts$ExportedVolume)
      alerts$ImportedValue <- as.numeric(alerts$ImportedValue)
      alerts$ImportedVolume <- as.numeric(alerts$ImportedVolume)
      
      ## Extract data for the year
      yearalerts <- alerts[alerts$Year==year & 
                                 alerts$ExporterCode != 0 &
                                 alerts$ImporterCode != 0,]
      
      rm(alerts)
      
      ## Compute import export discrepancies in new table columns
      yearalerts$TradeDiscrepancyKG <- yearalerts$ImportedVolume - yearalerts$ExportedVolume 
      yearalerts$TradeDiscrepancyUSD <- yearalerts$ImportedValue - yearalerts$ExportedValue 
      
      ## Filter only the tradeflows with Imports > exports
      #yearalerts <- yearalerts[yearalerts$TradeDiscrepancyKG > 0, ]
      
      ##Read country mapping file
      countrymapping <- read.csv("UN Comtrade Country List.csv", header = TRUE)
      
      countrymapping$UN.Geoscheme.level.1 <- as.character(countrymapping$UN.Geoscheme.level.1)
      
      
      
      ##Map Importer codes to their corresponding region names
      yearalerts$ImporterRegion <- " "

      for (i in unique(yearalerts$ImporterCode)){
            
            if (length(countrymapping$ctyCode[countrymapping$ctyCode==i]) > 0){
                  yearalerts$ImporterRegion[yearalerts$ImporterCode == i] <- 
                        countrymapping$UN.Geoscheme.level.1[countrymapping$ctyCode == i]
            }
            else{
                  yearalerts$ImporterRegion[yearalerts$ImporterCode == i] <- "Unidentified"
  
            }
            
      }
      
      if (ind == "KG"){
            
            ## COMPUTE NETWEIGHT/ VOLUME DISCREPANCIES
            
            ## Get Global Tradeflow Discrepancy
            worlddiscrepancyKG = sum(yearalerts$TradeDiscrepancyKG)
            
            ##Get Regional totals of trade Volume discrepancies
            regvarKG <- aggregate(yearalerts$TradeDiscrepancyKG, 
                                  by=list(yearalerts$ImporterRegion), 
                                  FUN=sum)
            
            
            ##Get Regional totals of Import Volume 
            regtotKG <- aggregate(yearalerts$ImportedVolume, 
                                  by=list(yearalerts$ImporterRegion), 
                                  FUN=sum)
            
            ## Combine regtotKG and regvarKG by region name
            regKG <- merge(x=regvarKG, y=regtotKG, by="Group.1", all=TRUE)
            
            ## Compute discrepancies as % of world total, and as % or regional total
            regKG$PercentOfWorld <- round(100*regKG$x.x/worlddiscrepancyKG,2)
            regKG$Percentofregion <-  round(100*regKG$x.x/regKG$x.y ,2)
            
            ## Convert KGs to Tonnes
            regKG$x.x <- round(regKG$x.x/1000, 2)
            regKG$x.y <- round(regKG$x.y/1000, 2)
            
            ## Rename columns
            regKG <- plyr::rename( regKG, c("Group.1" ="Region", 
                                                "x.x"="Total Discrepancy (T)",
                                                "x.y" = "Total Imports (T)",
                                                "PercentOfWorld" = "Disc As % of World",
                                                "Percentofregion" ="Disc AS % of Reg Imports"))
            
            ## Order table by discount % of world, descending order
            regKG <- regKG[order(-regKG$`Disc As % of World`),]
            
            return(regKG)
            
            
      }
      
      else if (ind == "USD"){
            
            ## COMPUTE VALUE/ USD DISCREPANCIES
            ##This will probably not be used
            
            ## Get Global Tradeflow Discrepancy
            worlddiscrepancyUSD = sum(yearalerts$TradeDiscrepancyUSD)
      
            ##Get Regional levels of Import Value discrepancies
            regvarUSD <- aggregate(yearalerts$TradeDiscrepancyUSD, 
                                   by=list(yearalerts$ImporterRegion), 
                                   FUN=sum)
            
            regvarUSD$PercentOfWorld <- round(100*regvarUSD$x/worlddiscrepancyUSD, 2)
            
            
            regvarUSD$TotalRegionalImports <- " "
            regvarUSD$PercentOfRegionalImports <- " "
            regvarUSD$TotalRegionalImports <- " "
            for (i in unique(regvarUSD$Group.1)){
                  regionsum <- sum(yearalerts$ImportedValue[yearalerts$ImporterRegion == i])
                  
                  regvarUSD$TotalRegionalImports[regvarUSD$Group.1 == i] <- regionsum
                  regvarUSD$PercentOfRegionalImports[regvarUSD$Group.1 == i] <- 
                        round((100*regvarUSD$x[regvarUSD$Group.1 == i])/regionsum, 2)
            }
            
            regvarUSD$x <- round(regvarUSD$x/1000000, 2)
            regvarUSD$TotalRegionalImports <- round(as.numeric(regvarUSD$TotalRegionalImports)/1000000, 2)
            
            regvarUSD <- plyr::rename( regvarUSD, c("Group.1" ="Region", 
                                              "x"="Discrepancy(M USD)",
                                              "PercentOfWorld" = "%OfWorld", 
                                              "PercentOfRegionalImports" = "%OfRegionalImports",
                                              "TotalRegionalImports" = "TotalRegionalImports(M USD)"))
            
            regvarUSD <-  regvarUSD[order(-regvarUSD$`%OfWorld`),]      
      }
      else{
            
            return(NULL)
      }
     

}