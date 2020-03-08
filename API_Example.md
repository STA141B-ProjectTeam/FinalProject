# FinalProject
STA141b Winter20 Final Project

Using GET https://test.api.amadeus.com/v1/shopping/flight-dates?origin=MAD&destination=MUC
This is an example of the JSON that's returned to us. 

You can use https://developers.amadeus.com/self-service/category/air/api-doc/flight-offers-price/api-reference#
 to see these results for yourself. 

From this, we can see our primary data is `type`,`origin`,`destination`,`departureDate`, `returnDate`, `price` with inline value `total`, 
and `links`. 

```
 "data": [
    {
      "type": "flight-date",
      "origin": "MAD",
      "destination": "MUC",
      "departureDate": "2020-03-09",
      "returnDate": "2020-03-12",
      "price": {
        "total": "120.58"
      },
      "links": {
        "flightDestinations": "https://test.api.amadeus.com/v1/shopping/flight-destinations?origin=MAD&departureDate=2020-03-09,2020-09-04&oneWay=false&duration=1,15&nonStop=false&viewBy=DURATION",
        "flightOffers": "https://test.api.amadeus.com/v2/shopping/flight-offers?originLocationCode=MAD&destinationLocationCode=MUC&departureDate=2020-03-09&returnDate=2020-03-12&adults=1&nonStop=false"
      }
    },
    {
      "type": "flight-date",
      "origin": "MAD",
      "destination": "MUC",
      "departureDate": "2020-03-09",
      "returnDate": "2020-03-14",
      "price": {
        "total": "120.58"
      },
      "links": {
        "flightDestinations": "https://test.api.amadeus.com/v1/shopping/flight-destinations?origin=MAD&departureDate=2020-03-09,2020-09-04&oneWay=false&duration=1,15&nonStop=false&viewBy=DURATION",
        "flightOffers": "https://test.api.amadeus.com/v2/shopping/flight-offers?originLocationCode=MAD&destinationLocationCode=MUC&departureDate=2020-03-09&returnDate=2020-03-14&adults=1&nonStop=false"
      }
    },
    
    ...
    ```
