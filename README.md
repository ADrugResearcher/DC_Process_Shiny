## DC_Process_Shiny

###Drug Checking App Made with Shiny in R.

This is a proof of concept, to provide drug checking orgs with easy(ish) to use tools for the purpose of visualizing qualitative drug checking results.

The two main source files are dc_cleaner.R & app.R. The former pre-processes the data including the edgelist for the network analysis & benzos. This is done weekly using gitlab. The advantage of using gitlab is that you get 10gb of storage, in my experience it's faster to download a file from than github or heroku, and it's really designed for continuous integration/continuous deployment, making it really easy to schedule the data processing for 1 or 2 days every week for me. Many thanks to Tyler Williams for holding my hand through hours & hours of trying to learn how to do this. 

Do you need to understand what the above means to know how to do it? No! Because I've done the hard work for you. If you use R. But if you use python, this is likely going to be a lot easier and more straightforward for you to set up anyways.

##Over the next couple of months I will slowly be updating the ReadMe with information on how to use each of the R scripts, although they all have pretty heavy commenting that it worth reding


Files that go to gitlab for preprocessing:
1. dc_cleaner.R
2. Drug Classification.R
3. Day_making.R
4. cheque_days.csv
5. demo_gitlab-ci.yml - remove the word "demo"

Files that Run on Shiny
1. app.R
2. Day_making.R

##For Beginners
1. run R Script renv_install.R
2. Go create an account on gitlab
3. I recommend following this instruction list on how to connect to gitlab:
https://vickysteeves.gitlab.io/repro-papers/git.html
4. Create a shinyapps.io account - or run on your own computer


Many thanks to the many folks who have helped in creating this, including but not limited to:
Tyler Williams, Karen Ward, Adam Palayew, Andrew Prestidge, Marcel Rambo.


R packages used to make the darn thing:
- ggraph
- tidygraph
- shiny
- shinyjs
- shinydashboard
- shinyWidgets
- shinythemes
- tidyverse
- lubridate
- igraph
- RColorBrewer
- readxl
- lubridate
- RColorBrewer


All Scripts are licensed under GPL v3


Setting Up Gitlab:
1. make an account at www.gitlab.com
2. Use whatever email you want
3. When setting up your username, it's worth noting it will appear on everything gitlab Project
4. After you've made your account, you should be offered the opportunity to make a new project

![image](https://user-images.githubusercontent.com/96000770/155586192-ccff4012-0a2e-4656-b36e-5f3de2925ca1.png)



