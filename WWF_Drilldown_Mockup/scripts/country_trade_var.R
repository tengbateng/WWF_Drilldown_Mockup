
country_trade_var <- function(year=2015, 
                                  subregion = "Northern Europe",
                              ind = "KG"){
      
      library(plyr)
      setwd("C:/R Stuff/LPC")
      alerts <- read.csv("LPC WWF Alerts.csv", header = TRUE)
      
      countrymapping <- read.csv("UN Comtrade Country List.csv", header = TRUE)
      
      ## Convert the Export and Import quantities into numerical values 
      alerts$ExportedValue <- as.numeric(alerts$ExportedValue)
      alerts$ExportedVolume <- as.numeric(alerts$ExportedVolume)
      alerts$ImportedValue <- as.numeric(alerts$ImportedValue)
      alerts$ImportedVolume <- as.numeric(alerts$ImportedVolume)
      
      ## Extract data for the year and region
      regioncodes <- countrymapping$ctyCode[countrymapping$UN.Geoscheme.level.2 == subregion]  
      
      if (length(regioncodes) == 0 ){
            stop("Invalid region")
      }
      
      yearalerts <- alerts[alerts$Year==year & 
                                 alerts$ExporterCode != 0 &
                                 alerts$ImporterCode != 0 &
                                 alerts$ImporterCode %in% regioncodes,]
      
      rm(alerts)
      
      if (length(yearalerts) == 0 ){
            stop("Either (1) invalid year, or (2) no data for that year and subregion")
      }
      
      
      ## Compute import export discrepancies in new table columns
      yearalerts$TradeDiscrepancyKG <- yearalerts$ImportedVolume - yearalerts$ExportedVolume 
      yearalerts$TradeDiscrepancyUSD <- yearalerts$ImportedValue - yearalerts$ExportedValue 
      
      
      ##Map Exporter codes to their corresponding country and region names
      
      countrymapping$Cty.Abbreviation <- as.character(countrymapping$Cty.Abbreviation )
      
      
      ##Map Importer codes to their corresponding country and region names
      yearalerts$ImporterCountry <- " "
      
      for (i in unique(yearalerts$ImporterCode)){
            
            if (length(countrymapping$ctyCode[countrymapping$ctyCode==i]) > 0){
                  
                  yearalerts$ImporterCountry[yearalerts$ImporterCode == i] <- 
                        countrymapping$Cty.Abbreviation[countrymapping$ctyCode == i]
         
            }
            else{
                  yearalerts$ImporterCountry[yearalerts$ImporterCode == i] <- "Unidentified"
                  
            }
            
      }
      
      
      if (ind == "KG"){
            
            ## COMPUTE NETWEIGHT/ VOLUME DISCREPANCIES
            
            ## Get total sub Regional Tradeflow Discrepancy
            subregiondiscrepancyKG = sum(yearalerts$TradeDiscrepancyKG)
            
            ##Get country totals of Import Volume discrepancies
            regvarKG <- aggregate(yearalerts$TradeDiscrepancyKG, 
                                  by=list(yearalerts$ImporterCountry), 
                                  FUN=sum)
            
            ##Get country totals of Import Volume 
            regtotKG <- aggregate(yearalerts$ImportedVolume, 
                                  by=list(yearalerts$ImporterCountry), 
                                  FUN=sum)
            
            ## Combine regtotKG and regvarKG by subregion name
            regKG <- merge(x=regvarKG, y=regtotKG, by="Group.1", all=TRUE)
            
            ## Compute discrepancies as % of sub region, and as % or country total
            regKG$Percentofsubregion <- round(100*regKG$x.x/subregiondiscrepancyKG,2)
            regKG$Percentofcountry <-  round(100*regKG$x.x/regKG$x.y ,2)
            
            ## Convert KGs to Tonnes
            regKG$x.x <- round(regKG$x.x/1000, 2)
            regKG$x.y <- round(regKG$x.y/1000, 2)
            
            ## Rename columns
            regKG <- plyr::rename( regKG, c("Group.1" ="Country", 
                                            "x.x"="Total Discrepancy (T)",
                                            "x.y" = "Total Imports (T)",
                                            "Percentofsubregion" = "Disc As % of Sub Reg Discrepancy",
                                            "Percentofcountry" ="Disc AS % of Country Imports"))
            
            ## Order table by discount % of world, descending order
            regKG <- regKG[order(-regKG$`Disc As % of Sub Reg Discrepancy`),]
            
            return(regKG)
            
      }
      else if( ind == "USD"){
            
            ## Get Regional Tradeflow Discrepancy
            regiondiscrepancyUSD = sum(yearalerts$TradeDiscrepancyUSD)
            
            ##Get Regional levels of Import Value discrepancies
            regvarUSD <- aggregate(yearalerts$TradeDiscrepancyUSD, 
                                   by=list(yearalerts$ImporterCountry), 
                                   FUN=sum)
            
            regvarUSD$PercentOfWorld <- round(100*regvarUSD$x/regiondiscrepancyUSD, 2)
            
            
            regvarUSD$TotalRegionalImports <- " "
            regvarUSD$PercentOfRegionalImports <- " "
            regvarUSD$TotalRegionalImports <- " "
            for (i in unique(regvarUSD$Group.1)){
                  regionsum <- sum(yearalerts$ImportedValue[yearalerts$ImporterCountry == i])
                  
                  regvarUSD$TotalRegionalImports[regvarUSD$Group.1 == i] <- regionsum
                  regvarUSD$PercentOfRegionalImports[regvarUSD$Group.1 == i] <- 
                        round((100*regvarUSD$x[regvarUSD$Group.1 == i])/regionsum, 2)
            }
            
            regvarUSD$x <- round(regvarUSD$x/1000000, 2)
            regvarUSD$TotalRegionalImports <- round(as.numeric(regvarUSD$TotalRegionalImports)/1000000, 2)
            
            regvarUSD <- plyr::rename( regvarUSD, c("Group.1" ="Country", 
                                              "x"="Discrepancy(M USD)",
                                              "PercentOfWorld" = "%OfSubRegion", 
                                              "PercentOfRegionalImports" = "%OfCountryImports",
                                              "TotalRegionalImports" = "TotalCountryImports(M USD)"))
            
            regvarUSD <-  regvarUSD[order(-regvarUSD$`%OfSubRegion`),]
            
            return(regvarUSD)
      }
      else{
            return(NULL)
      }
      
    
}