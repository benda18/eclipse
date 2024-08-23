# Eclipse Planning Tools

### BACKGROUND: 
The 2024 Total Eclipse that will cross North America on April 8th will be the last chance for many of us to see a total solar eclipse for the next 21 years.  NASA estimates that 150 million people live within 200 miles of totality of this eclipse, 5 times as many people as the 2017 eclipse that also crossed the continental US.  

### PURPOSE/GOALS:
Many useful web tools and resources exist to help plan where and when to go to best see the eclipse.  However, I kept looking for (and couldn't find) a simple tool that allowed you to enter a mailing address and would give you all the planning information you needed about the eclipse for that location - timeline of the eclipse for that location, whether totality could be seen, etc.  So I created the tool that I wanted. Based on all I've learned in the process, I've built additional tools to leverage the calculations to show what eclipses will happen in the future ~~and past~~.  

A secondary goal of this project was to improve my R programming skills, particularly in learning how to communicate better in interactive ways using [Shiny](https://shiny.posit.co/) and [Leaflet](https://leafletjs.com/) to create inspiring dashboards with interactive and useful maps. 

### HOW IT WORKS: 

2 web apps came out of this project (links provided directly below): 
1) [R-Shiny dashboard #1 | April 8, 2024 Total Eclipse Planning Tool](https://tim-bender.shinyapps.io/shiny_eclipse_planner/): A planning tool for the April 8th, 2024 total solar eclipse that takes almost any US postal address as an input and as an output provides information on whether that location was in the path of totality, how long it would experience totality (if any), the time of the eclipse first and last contact as well as totality (if any), how much of the sun would be eclipsed (< 100% if outside of the path of totality), and finally it would calculate and return the next date that the location would see totality of a total solar eclipse.
2) [R-Shiny dashboard #2 | Find all eclipses for the next 75 years for a location](https://tim-bender.shinyapps.io/shiny_all_eclipses/): A planning tool that uses your current location (calculated using your public ip address) that can tell you the date and time of every single lunar and solar eclipse (partial or total) visibile from your current location basically for the rest of your life (75 years from today).  This app is computationally heavy and requires a long time to load results after clicking the "Click to Start" button. 

### Web Apps Developed
* April 8, 2024 Total Eclipse Planning Tool ([link to shinyapps.io](https://tim-bender.shinyapps.io/shiny_eclipse_planner/)) | [link to code](https://github.com/benda18/eclipse/blob/main/shiny_eclipse_timer/app.R)
* Find all eclipses for the next 75 years for a location ([link to shinyapps.io](https://tim-bender.shinyapps.io/shiny_all_eclipses/)) | [link to code](https://github.com/benda18/eclipse/blob/main/shiny_all_eclipsesV2/app.R)
  
### Sources
* www.eclipsewise.com for information and images of solar and lunar eclipses. 
* leaflet: Create Interactive Web Maps with the JavaScript 'Leaflet' Library ([link to CRAN](https://cran.r-project.org/web/packages/leaflet/index.html))
* geoloc: Add geolocation inside your shiny app ([link to github](https://github.com/ColinFay/geoloc))
* swephR: High Precision Swiss Ephemeris ([link to CRAN](https://cran.r-project.org/package=swephR))
* ggmap: spatial visualization with ggplot2 ([link to CRAN](https://cran.r-project.org/package=ggmap)) ([link to D. Kahle and H. Wickham. ggmap: Spatial Visualization with ggplot2. The R Journal, 5(1), 144-161.](http://journal.r-project.org/archive/2013-1/kahle-wickham.pdf))
* censusxy: easy access to the U.S. Census Bureau Geocoding Tools ([link to github](https://github.com/chris-prener/censusxy)) ([link to Census Bureau](https://geocoding.geo.census.gov/geocoder/)) ([link to "Creating open source composite geocoders: Pitfalls and opportunities" by Prener & Fox](https://onlinelibrary.wiley.com/doi/abs/10.1111/tgis.12741))
* shiny: Web Application Framework for R ([link to CRAN](https://cran.r-project.org/web/packages/shiny/index.html)) (https://shiny.posit.co)
* tigris: Download TIGER/Line shapefiles from the United States Census Bureau ([link to CRAN](https://cran.r-project.org/package=tigris))
* lubridate: Make Dealing with Dates a Little Easier ([link to CRAN](https://cran.r-project.org/package=lubridate))

### Other Useful Links
* Photos of 8-21-2017 Total Eclipse I took from Durham, NC @ 92% obscuration through a Questar 3.5" with solar filter using a Nikon D5000 ([link to public facebook post](https://www.facebook.com/tim.bender.7543/posts/pfbid0b9kPrJcSrKqGLmAy3iEpcAAKDWNJimF3EehUwe1MvRNMeMyhns1wAnwTDmEjSc4Ql))
* https://www.linkedin.com/pulse/building-simple-eclipse-planning-tool-tim-bender-e0hrf
* https://www.space.com/why-99-percent-totality-does-not-exist-need-to-be-on-total-solar-eclipse-path-april-2024
* https://www.si.com/extra-mustard/2013/12/30/the-extra-mustard-trivia-hour-when-a-calendar-defeated-russia-in-the-1908-olympics
* History of the Gregorian Calendar ([from the American National Standards Institute blog](https://blog.ansi.org/2016/02/history-of-standard-gregorian-calendar/))
* [The American Astronomical Society's "Suppliers of Safe Solar Viewers & Filters" list with purchasing links](https://eclipse.aas.org/eye-safety/viewers-filters)
* [National Solar Observatory interactive eclipse map](https://nso.edu/for-public/eclipse-map-2024/)
* [NASA's 2024 Eclipse Website](https://science.nasa.gov/eclipses/future-eclipses/eclipse-2024/)
* NASA's Five Millennium Catalog of Solar Eclipses ([pdf download](http://eclipse.gsfc.nasa.gov/5MCSE/TP2009-214174.pdf))
* [Webpages on the History of Astronomy by Robert Harry van Gent](https://webspace.science.uu.nl/~gent0113/)

### Special Thanks
* Reddit users u/danielsixfive and u/QuackingUp23
