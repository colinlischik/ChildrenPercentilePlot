# Plotting the height, weight and head circumference for your child in an interactive plot
The aim of this repository is to establish an interactive plot for your child regarding the height, weight and head circumference.

## Files
Example files can be found in "test data/". They contain one tab "rawdata" and one tab "meta data". The meta data should contain the headers "key" and "value". A template can be found in "template.xlsx" (https://github.com/colinlischik/ChildrenPercentilePlot/blob/main/test%20data/template.xlsx).

## Supported units
Currently, the units for weight need to be in grams, whereas the height and head circumference need to be supplied in cm.

## Percentile data
Currently available data:
  DE - Robert-Koch-Institut
  
It may be enhanced by adding additional data for your country, if you are supplying the data in a format that can be ingested.

## RShiny App
The deployed RShiny App can be found here: https://colinlischik.shinyapps.io/ChildrenPercentilePlot/

