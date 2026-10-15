# Exeter Crime Analytics Dashboard

An interactive R Shiny dashboard analysing street-level crime in Exeter using live data from the UK Police API.

## Features
- Clustered crime map
- Grid-based crime intensity map with adjustable resolution
- Top 10 crime hotspots with labelled markers
- Crime category bar chart
- 12-month crime trend line
- Full analytical street-level leaderboard
- Summary table of crime types

## How to run
1. Download the repository
2. Open R or RStudio
3. Run:

```r
shiny::runApp("exeter-crime-dashboard")

Data sources
- Uk Police API(live street-level crime data)
- Local summary dataset: Exeter_stats.csv

Author
Ewan Richards
