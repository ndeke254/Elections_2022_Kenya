
# import data file
results <- fread(file ='data/results.csv')

# import the shape files
county_shp <-readOGR(dsn="data/shp/county.shp",
                     layer ="county", 
                     verbose = FALSE, 
                     stringsAsFactors = FALSE)

# decomma the numeric columns
results$registered <- decomma(results$registered)
results$raila <- decomma(results$raila)
results$ruto <- decomma(results$ruto)
results$waihiga <- decomma(results$waihiga)
results$wajackoya <- decomma(results$wajackoya)
results$valid <- decomma(results$valid)
results$rejected <- decomma(results$rejected)

# remove string in county names
results$county <- gsub('COUNTY TOTAL','',results$county)

# use title case in county names
results$county <- snakecase::to_title_case(results$county)

# check names match with the ones in shape file
setdiff(results$county, county_shp$ADM1_EN)

# rename the different names 
results$county[13] <- county_shp$ADM1_EN[40]
results$county[21] <- county_shp$ADM1_EN[29]
results$county[28] <- county_shp$ADM1_EN[5]
results$county[47] <- county_shp$ADM1_EN[30]

# extract the diaspora and totals row
totals <- results[c(48,49),]

# slice the two rows off
results <- results |> slice(-c(48,49))

# write the data
fwrite(results,'data/results.csv')
fwrite(totals,'data/totals.csv')
