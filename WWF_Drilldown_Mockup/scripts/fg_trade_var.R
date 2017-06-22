
fg_trade_var <- function(alerts, year=2015, 
                              country = "Algeria",
                              ind = "KG"){
      
      library(plyr)
      
      countrymapping <- read.csv("data/UN Comtrade Country List.csv", header = TRUE)
      
      ## Convert the Export and Import quantities into numerical values 
      alerts$ExportedValue <- as.numeric(alerts$ExportedValue)
      alerts$ExportedVolume <- as.numeric(alerts$ExportedVolume)
      alerts$ImportedValue <- as.numeric(alerts$ImportedValue)
      alerts$ImportedVolume <- as.numeric(alerts$ImportedVolume)
      
      ## Extract data for the year and region
      ### If Americas, use the third level data in the UN Geoscheme
      
      
      regioncodes <- countrymapping$ctyCode[countrymapping$Cty.Abbreviation == country]  
      
      
      if (length(regioncodes) == 0 ){
            stop("Invalid region")
      }
      
      yearalerts <- alerts[alerts$Year==year & 
                                 alerts$ExporterCode != 0 &
                                 alerts$ImporterCode != 0 &
                                 alerts$ImporterCode == regioncodes,]
      
      rm(alerts)
      
      if (length(yearalerts) == 0 ){
            stop("Either (1) invalid year, or (2) no data for that year and subregion")
      }
      
      
      ## Compute import export discrepancies in new table columns
      yearalerts$TradeDiscrepancyKG <- yearalerts$ImportedVolume - yearalerts$ExportedVolume 
      yearalerts$TradeDiscrepancyUSD <- yearalerts$ImportedValue - yearalerts$ExportedValue 
      
      
      ##Map Exporter codes to their corresponding country and region names
      
      
      countrymapping$Cty.Abbreviation <- as.character(countrymapping$Cty.Abbreviation )
      
      
      ## Assign Country Name to alerts
      yearalerts$ImporterCountry <- country
      
      ##Map fish categories to the commodity codes
      fishmapping <- read.csv("data/family commodity map.csv", header = TRUE)
      
      fishmapping$Family.Or.Group <- as.character(fishmapping$Family.Or.Group)
      
      yearalerts$FishCategory <- " "
      
      for ( i in unique(yearalerts$CommodityCode)){
      
            if(length(fishmapping$Family.Or.Group[fishmapping$code == i]) > 0){
                  yearalerts$FishCategory[yearalerts$CommodityCode == i] <-
                        fishmapping$Family.Or.Group[fishmapping$code == i]    
            }
            else {
                  yearalerts$FishCategory[yearalerts$CommodityCode == i] <- "Unidentified"
            }
      }
      
      
     
      
      if (ind == "KG"){
            
            ## COMPUTE NETWEIGHT/ VOLUME DISCREPANCIES
            
            ## Get total Country Tradeflow Discrepancy
            countrydiscrepancyKG = sum(yearalerts$TradeDiscrepancyKG)
            
            ##Get fish totals of Import Volume discrepancies
            regvarKG <- aggregate(yearalerts$TradeDiscrepancyKG, 
                                  by=list(yearalerts$FishCategory), 
                                  FUN=sum)
            
            ##Get fish totals of Import Volume 
            regtotKG <- aggregate(yearalerts$ImportedVolume, 
                                  by=list(yearalerts$FishCategory), 
                                  FUN=sum)
            
            ## Combine regtotKG and regvarKG by subregion name
            regKG <- merge(x=regvarKG, y=regtotKG, by="Group.1", all=TRUE)
            
            ## Compute discrepancies as % of country, and as % of fish total
            regKG$Percentofcountry <- round(100*regKG$x.x/countrydiscrepancyKG,2)
            regKG$Percentoffish <-  round(100*regKG$x.x/regKG$x.y ,2)
            
            ## Convert KGs to Tonnes
            regKG$x.x <- round(regKG$x.x/1000, 2)
            regKG$x.y <- round(regKG$x.y/1000, 2)
            
            ## Rename columns
            regKG <- plyr::rename( regKG, c("Group.1" ="Fish Grouping", 
                                            "x.x"="Total Discrepancy (T)",
                                            "x.y" = "Total Imports (T)",
                                            "Percentofcountry" = "Disc As % of Country Discrepancy",
                                            "Percentoffish" ="Disc As % of FG Imports"))
            
            ## Order table by discount % of world, descending order
            regKG <- regKG[order(-regKG$`Disc As % of Country Discrepancy`),]
            
            return(regKG)
            
            
      }
      else if( ind == "USD"){
            
            ## Get Regional Tradeflow Discrepancy
            regiondiscrepancyUSD = sum(yearalerts$TradeDiscrepancyUSD)
            
            ##Get Regional levels of Import Value discrepancies
            regvarUSD <- aggregate(yearalerts$TradeDiscrepancyUSD, 
                                   by=list(yearalerts$FishCategory), 
                                   FUN=sum)
            
            regvarUSD$PercentOfWorld <- round(100*regvarUSD$x/regiondiscrepancyUSD, 2)
            
            
            regvarUSD$TotalRegionalImports <- " "
            regvarUSD$PercentOfRegionalImports <- " "
            regvarUSD$TotalRegionalImports <- " "
            for (i in unique(regvarUSD$Group.1)){
                  regionsum <- sum(yearalerts$ImportedValue[yearalerts$FishCategory == i])
                  
                  regvarUSD$TotalRegionalImports[regvarUSD$Group.1 == i] <- regionsum
                  regvarUSD$PercentOfRegionalImports[regvarUSD$Group.1 == i] <- 
                        round((100*regvarUSD$x[regvarUSD$Group.1 == i])/regionsum, 2)
            }
            
            regvarUSD$x <- round(regvarUSD$x/1000000, 2)
            regvarUSD$TotalRegionalImports <- round(as.numeric(regvarUSD$TotalRegionalImports)/1000000, 2)
            
            regvarUSD <- plyr::rename( regvarUSD, c("Group.1" ="FishCategory", 
                                              "x"="Discrepancy(M USD)",
                                              "PercentOfWorld" = "%OfCountry", 
                                              "PercentOfRegionalImports" = "%OfFishCategory",
                                              "TotalRegionalImports" = "TotalFishCategoryImports(M USD)"))
            
            regvarUSD <-  regvarUSD[order(-regvarUSD$`%OfCountry`),]
            
            return(regvarUSD)
      }
      else{
            return(NULL)
      }
      
      
      
}