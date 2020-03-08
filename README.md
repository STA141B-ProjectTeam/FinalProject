# fProject

This application retrives actual flight prices and corresponding data from one of the industry recognized flight database Amadeus.

Airport Codes and Names

Airport IATA three letter codes are stored in a file. Ideally, the codes should be received from Amadeus. This functionality is in the project scope, but currently it is not implemented. 

Amadeus Authorization

Thier API uses two-step authorization. At the first step, access token is obtained sending POST request with user credentials (user id and secret code that are received during registration). The body of POST request is similar to GET request parameters string; i.e. it is a string contating name/values pairs ("name=value") separated by "&". At the second step, actual data request is sent using GET method. Two-step authorization provides additional flexibility and security. Access token can be used only once, and there is no need to keep it secret; consequently, data can be received from Amadeus directly by a client, and all JSON processing job could be done on the client side. Client JSON parsing is within the scope of further steps of the project.

User Interface

The application has two panels, the left panel and the main panel. On the left panel, there are several controls that allow user input. User can choose departure airport, arrival airport, and departure date. After data input, user can press "Update view" button for the data to passed to the server and then the reponse recived. The reponse is shown in the main panel. The main panel has several tabs. The first tab present all the offers found on Amadeus for given departure and destination airports for selected date. Teh table has three columns including offer price, transit airports for each iterinary, and total time of travel. Total travel time is a difference between the scheduled departure time of the initial leg and scheduled arrival time of the final leg of the trip. There is also a tab showing price distribution as a function of total flight time as a plot.

Flight Booking API Documentation
https://developers.amadeus.com/get-started/category?id=80&durl=335&parentId=NaN

User registration and key acquisition: 
https://developers.amadeus.com/my-apps/

To call the API:
GET https://test.api.amadeus.com/v1/shopping/flight-dates?origin=[orig]&destination=[dest]

GET https://test.api.amadeus.com/v1/shopping/flight-dates?origin=MAD&destination=MUC


