
subregional_trade_var <- function(year=2015, 
                               region = "Americas", ind = "KG"){
      
      setwd("C:/R Stuff/LPC")
      alerts <- read.csv("LPC WWF Alerts.csv", header = TRUE)
      library(plyr)
      
      countrymapping <- read.csv("UN Comtrade Country List.csv", header = TRUE)
      
      ## Convert the Export and Import quantities into numerical values
      alerts$ExportedValue <- as.numeric(alerts$ExportedValue)
      alerts$ExportedVolume <- as.numeric(alerts$ExportedVolume)
      alerts$ImportedValue <- as.numeric(alerts$ImportedValue)
      alerts$ImportedVolume <- as.numeric(alerts$ImportedVolume)
      
      ## Extract data for the year and region
      
      regioncodes <- countrymapping$ctyCode[countrymapping$UN.Geoscheme.level.1 == region]
      
      if (length(regioncodes) == 0 ){
            stop("Invalid region")
      }
      
      yearalerts <- alerts[alerts$Year==year & 
                                 alerts$ExporterCode != 0 &
                                 alerts$ImporterCode != 0 &
                                 alerts$ImporterCode %in% regioncodes,]
      
      rm(alerts)
      
      if (length(yearalerts) == 0 ){
            stop("Either (1) invalid year, or (2) no data for that year and region")
      }
      
      
      ## Compute import export discrepancies in new table columns
      yearalerts$TradeDiscrepancyKG <- yearalerts$ImportedVolume - yearalerts$ExportedVolume 
      yearalerts$TradeDiscrepancyUSD <- yearalerts$ImportedValue - yearalerts$ExportedValue 
      
      
      ##Map Exporter codes to their corresponding subregion names
      
      countrymapping$UN.Geoscheme.level.2 <- as.character(countrymapping$UN.Geoscheme.level.2)
      
      
      
      ##Map Importer codes to their corresponding sub region names
      yearalerts$ImporterSubRegion <- " "
      
      for (i in unique(yearalerts$ImporterCode)){
            
            if (length(countrymapping$ctyCode[countrymapping$ctyCode==i]) > 0){
                  
                  yearalerts$ImporterSubRegion[yearalerts$ImporterCode == i] <- 
                        countrymapping$UN.Geoscheme.level.2[countrymapping$ctyCode == i]
                  
            }
            else{
                  
                  yearalerts$ImporterSubRegion[yearalerts$ImporterCode == i] <- "Unidentified"
                  
            }
            
      }

      if( ind == "KG"){
            ## COMPUTE NETWEIGHT/ VOLUME DISCREPANCIES
            
            ## Get total Regional Tradeflow Discrepancy
            regiondiscrepancyKG = sum(yearalerts$TradeDiscrepancyKG)
            
            ##Get Sub Regional levels of Import Volume discrepancies
            regvarKG <- aggregate(yearalerts$TradeDiscrepancyKG, 
                                  by=list(yearalerts$ImporterSubRegion), 
                                  FUN=sum)
            
            ##Get Sub Regional totals of Import Volume 
            regtotKG <- aggregate(yearalerts$ImportedVolume, 
                                  by=list(yearalerts$ImporterSubRegion), 
                                  FUN=sum)
            
            ## Combine regtotKG and regvarKG by subregion name
            regKG <- merge(x=regvarKG, y=regtotKG, by="Group.1", all=TRUE)
            
            ## Compute discrepancies as % of region total, and as % or subregional total
            regKG$Percentofregion <- round(100*regKG$x.x/regiondiscrepancyKG,2)
            regKG$Percentofsubregion <-  round(100*regKG$x.x/regKG$x.y ,2)
            
            ## Convert KGs to Tonnes
            regKG$x.x <- round(regKG$x.x/1000, 2)
            regKG$x.y <- round(regKG$x.y/1000, 2)
            
            ## Rename columns
            regKG <- plyr::rename( regKG, c("Group.1" ="Sub-Region", 
                                            "x.x"="Total Discrepancy (T)",
                                            "x.y" = "Total Imports (T)",
                                            "Percentofregion" = "Disc As % of Reg Discrepancy",
                                            "Percentofsubregion" ="Disc AS % of Sub Reg Imports"))
            
            ## Order table by discount % of world, descending order
            regKG <- regKG[order(-regKG$`Disc As % of Reg Discrepancy`),]
            
            return(regKG)
            
      }
      else if( ind == "USD"){
            
            ## COMPUTE VALUE/ USD DISCREPANCIES
            
            ## Get regional total of Value/ USD discrepancy
            
            regiondiscrepancyUSD = sum(yearalerts$TradeDiscrepancyUSD)
            
            ##Get Regional levels of Import Value discrepancies
            regvarUSD <- aggregate(yearalerts$TradeDiscrepancyUSD, 
                                   by=list(yearalerts$ImporterSubRegion), 
                                   FUN=sum)
            
            regvarUSD$PercentOfWorld <- round(100*regvarUSD$x/regiondiscrepancyUSD, 2)
            
            
            regvarUSD$TotalRegionalImports <- " "
            regvarUSD$PercentOfRegionalImports <- " "
            regvarUSD$TotalRegionalImports <- " "
            for (i in unique(regvarUSD$Group.1)){
                  regionsum <- sum(yearalerts$ImportedValue[yearalerts$ImporterSubRegion == i])
                  
                  regvarUSD$TotalRegionalImports[regvarUSD$Group.1 == i] <- regionsum
                  regvarUSD$PercentOfRegionalImports[regvarUSD$Group.1 == i] <- 
                        round((100*regvarUSD$x[regvarUSD$Group.1 == i])/regionsum, 2)
            }
            
            regvarUSD$x <- round(regvarUSD$x/1000000, 2)
            regvarUSD$TotalRegionalImports <- round(as.numeric(regvarUSD$TotalRegionalImports)/1000000, 2)
            
            regvarUSD <- plyr::rename( regvarUSD, c("Group.1" ="SubRegion", 
                                              "x"="Discrepancy(M USD)",
                                              "PercentOfWorld" = "percentOfRegion", 
                                              "PercentOfRegionalImports" = "%OfSubRegionalImports",
                                              "TotalRegionalImports" = "TotalSubRegionalImports(M USD)"))
            
            regvarUSD <-  regvarUSD[order(-regvarUSD$percentOfRegion),]
            
            return(regvarUSD)
      }
      else{
            
            return(NULL)
      }
      
}