**COVID FORECASTING**

**DESCRIPTION**

Infectious Disease Forecasting is a web application designed to allow users to generate case count forecasts using a variety of provided mathematical models. Users have the option to forecast infections using 1) provided country-based COVID-19 data (pre-populated from John Hopkins database) or 2) their own uploaded data. This data can be uploaded by the end user in .csv, .tsv or .txt format. The data should contain only 2 columns: date and new case count. Based on the model selected, the application projects the trend of the disease. Additionally, the user has the ability to fine tune model parameters like intrinsic growth rate, deceleration of growth, maximum cumulative case incidence, and a scaling parameter based on the chosen model. For convenience, default values are populated for all model parameters which can be adjusted by the end user if desired.

The application can be understood as a backend and a front end. The backend performs model training and forecasting and is written in R and Python. The front end Graphical User Interface (GUI) is generated with Shiny and allows the user to read general app instructions, select/upload data for modeling, adjust model parameters, select one of the three optimization algorithms (trf, dogbox or lm), and run the model. Running the model will provide you the following outputs, each in its own tab:

1. Parameter Estimates - the best parameter estimates for the selected model
2. Model Goodness of Fit - details on how well the model fit the set of observations using Mean Absolute Error (MAE) and Root Mean Squared Error (RMSE) as evaluation metrics
3. Forecasting - predicted case count values

The application can be accessed at https://covidforecasting.shinyapps.io/cse6242-project.

 

**INSTALLATION** - How to install and setup code

i. If you do not have R-Studio, please install it for your Operating System by going to https://www.rstudio.com/products/rstudio/download/

ii. Download the code for the application from GitHub at https://github.gatech.edu/jmanzione3/cse6242-project or use the attached zip file and extract it to a local folder.

iii. Open the project in R-studio (cse6242-project.Rproj)

iv. Open the file app.R and click the 'RunApp' button in RStudio 
(NOTE: Make sure that all required libraries are installed. If you do not have some necessary libraries installed, please install them using your RStudio environment. If you are missing the necessary Python libraries, you can uncomment line #15 py_install(c('pandas', 'numpy', 'scipy')).

This will bring up a widget that runs the Shiny App. It might take a couple of minutes for the application to start up. Once the application is running, can run the model using the predefined data or by uploading new data. 

Deploying to Web (Optional)

You can deploy your application to the web using the shinyapp.io website. For this, please go to https://www.shinyapps.io/ and create an account.

If you would like to deploy the changes to the web and make it available to others, click on the "Publish this Application" button on the top of the window in RStudio. This will prompt you to "Publish from an Account", pre-populating with your Shiny account if you already have one. If you do not have an account, you can click on "Add New Account" link and on the next screen ("Connect to an Account"), click on ShinyApps.io. Login to Shiny App website using your login credentials created earlier. Click on your name and choose "Tokens" from account menu. Click "Show" on the token you want to use then "Show Secret" and copy to clipboard. Paste the value in the dialog box in RStudio and you are ready to publish the app. Provide a title of your liking and click on "Publish" button. This will deploy your application to the web (It could take a few minutes) and will open the web application in you default browser window. Use the URL in the window to open the app in the future.


 

**EXECUTION** - How to run a demo using the app

  i. You can access the link for the shiny app production version or run the app on your local by entering 'runApp()' in the console.
  
  ii. Read the 'About the App' tab for details on what to expect in the app and details about the model.
  
  ii. Select the 'Data load & display' tab to determine the data to use for model. 
  
  iii. Select to the Data Source to use:
  
   a. 'Pre-loaded Data' uses the dataset pulled from Our World in Data that gathers the covid confirmed cases by date from John Hopkins.
   
   b. 'Upload Data' allows you to upload your own dataset via a csv, tsv, or txt file that includes the number of covid cases and corresponding date. You can use the 'Ebola-DRC-April9.csv' file if you do not have your own.
    
  iv. Select the 'country' that is associated with the data source selected.
  
  v. Click 'Update Chart' to display a graph with the historical data of confirmed cases by date.
  
  vi. Click 'Select Model' or select the 'Model & parameter selection' tab.
  
  vii. Choose the date range or time range to be used from the data you previously selected in the model. In addition, choose whether to use the raw data or 7-day rolling average that is automatically calculated from the selected data. 
  
  viii. Select the model inputs. By default, the 'Exponential Growth' model is selected with default parameters. You can change the model from the static list of models in the 'Model' dropdown. Each model has default parameters that are automatically selected but can be changed.
  
  ix. Enter the forecast inputs to specify the number of data points forecasted. By default, 5 data points are forecasted.
  
  x. Click 'Run Model.'
  
  xi. You will be taken to the 'Model forecast & display' tab as the model runs. Depending on the size of the dataset and parameters selected, the model may take a while to run. You can view the parameter estimates, model goodness-of-fit, and forecasting with the projected data points. 
  
  xii. To test different parameters, go back to the 'Model & parameter selection' tab and change the inputs and 'Run Model' again. Alternatively, you can download the forecasted chart as a png to save teh results.
