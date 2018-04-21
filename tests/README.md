RCT test output
================
2018-04-21

-   [Are the base models estimating and without obvious mistakes?](#are-the-base-models-estimating-and-without-obvious-mistakes)
-   [Plot ARIMA forecasts](#plot-arima-forecasts)

Are the base models estimating and without obvious mistakes?
============================================================

|   IFP| Estimated |    h|  lambda| time\_period | data\_aggregated | partial\_train | partial\_outcome |
|-----:|:----------|----:|-------:|:-------------|:-----------------|:---------------|:-----------------|
|  1514| TRUE      |    1|      NA| month        | FALSE            | used           | FALSE            |
|  1451| TRUE      |    5|      NA| month        | FALSE            | used           | FALSE            |
|  1433| TRUE      |    5|      NA| month        | FALSE            | discarded      | FALSE            |
|  1406| TRUE      |    3|      NA| month        | FALSE            | used           | FALSE            |
|  1271| TRUE      |    2|      NA| fixed        | TRUE             | used           | FALSE            |
|  1235| TRUE      |    2|      NA| month        | FALSE            | used           | FALSE            |
|  1226| TRUE      |    5|      NA| month        | FALSE            | discarded      | FALSE            |
|  1217| TRUE      |    4|      NA| month        | FALSE            | discarded      | FALSE            |
|  1208| TRUE      |    1|      NA| fixed        | TRUE             | no             | TRUE             |
|  1190| TRUE      |    5|     0.0| month        | FALSE            | discarded      | FALSE            |
|  1172| TRUE      |    2|      NA| month        | FALSE            | discarded      | FALSE            |
|  1145| TRUE      |    2|      NA| month        | FALSE            | used           | FALSE            |
|  1136| TRUE      |    7|      NA| month        | FALSE            | discarded      | FALSE            |
|  1055| TRUE      |    1|      NA| fixed        | TRUE             | no             | TRUE             |
|  1037| TRUE      |    3|      NA| month        | FALSE            | used           | FALSE            |
|  1028| TRUE      |   14|      NA| day          | FALSE            | no             | FALSE            |
|  1019| TRUE      |    4|      NA| month        | FALSE            | discarded      | FALSE            |
|   938| FALSE     |   NA|      NA| NA           | NA               | NA             | NA               |
|   929| TRUE      |    3|      NA| month        | FALSE            | used           | FALSE            |
|   911| TRUE      |    3|     0.5| month        | FALSE            | discarded      | FALSE            |
|   902| TRUE      |    4|      NA| month        | FALSE            | discarded      | FALSE            |
|   893| TRUE      |   91|      NA| day          | FALSE            | no             | FALSE            |
|   884| TRUE      |   11|      NA| fixed        | FALSE            | discarded      | FALSE            |
|   875| TRUE      |    5|      NA| month        | FALSE            | discarded      | FALSE            |
|   866| TRUE      |    1|      NA| month        | FALSE            | no             | TRUE             |
|   839| TRUE      |    3|     0.0| month        | FALSE            | discarded      | FALSE            |
|   830| TRUE      |    3|      NA| month        | FALSE            | used           | FALSE            |
|   821| TRUE      |    2|      NA| month        | FALSE            | used           | FALSE            |

| IFP | Error                                                                                          |
|:----|:-----------------------------------------------------------------------------------------------|
| 938 | validate\_data(target, data\_period, question\_period): Historical data in request appear to n |

Plot ARIMA forecasts
====================

Request 1514
------------

How many earthquakes of magnitude 5 or stronger will occur worldwide in May 2018?

![](README_files/figure-markdown_github/arima-plots-1.png)

Request 1451
------------

How much crude oil will Nigeria produce in July 2018?

![](README_files/figure-markdown_github/arima-plots-2.png)

Request 1433
------------

What will be the short-term interest rate for the Czech Republic (CZE) in June 2018?

![](README_files/figure-markdown_github/arima-plots-3.png)

Request 1406
------------

What will be the monthly period-over-period change in the consumer price index (CPI) for Egypt in May 2018?

![](README_files/figure-markdown_github/arima-plots-4.png)

Request 1271
------------

How many United Nations Security Council Resolutions concerning Syria will be vetoed by Russia between 22 April 2018 and 22 August 2018?

![](README_files/figure-markdown_github/arima-plots-5.png)

Request 1235
------------

What will be the monthly period-over-period change in the consumer price index (CPI) for Benin in April 2018?

![](README_files/figure-markdown_github/arima-plots-6.png)

Request 1226
------------

Will ACLED record any civilian fatalities in Ghana in June 2018?

![](README_files/figure-markdown_github/arima-plots-7.png)

Request 1217
------------

What will be the approval rate for Japan's cabinet in NHK's monthly survey in June 2018?

![](README_files/figure-markdown_github/arima-plots-8.png)

Request 1208
------------

What will be the maximum sea ice extent on the Baffin Bay Gulf of St. Lawrence between 21 March 2018 and 10 April 2018?

![](README_files/figure-markdown_github/arima-plots-9.png)

Request 1190
------------

How many deaths perpetrated by Boko Haram will the Council on Foreign Relations report for July 2018?

![](README_files/figure-markdown_github/arima-plots-10.png)

Request 1172
------------

How many 'hacking or malware (HACK)' data breaches will Privacy Rights Clearinghouse record in April 2018?

![](README_files/figure-markdown_github/arima-plots-11.png)

Request 1145
------------

What will be the monthly Period-over-Period change in the consumer price index (CPI) for Malawi in April (Month 04) 2018?

![](README_files/figure-markdown_github/arima-plots-12.png)

Request 1136
------------

Will ACLED record any riot/protest events in Gambia in July 2018?

![](README_files/figure-markdown_github/arima-plots-13.png)

Request 1055
------------

What will be the maximum sea ice extent on the Bering Sea between 14 March 2018 and 10 April 2018?

![](README_files/figure-markdown_github/arima-plots-14.png)

Request 1037
------------

What will be the long-term interest rate for Portugal (PRT) in April 2018?

![](README_files/figure-markdown_github/arima-plots-15.png)

Request 1028
------------

What will be the daily closing price of gold on 26 April 2018 in USD?

![](README_files/figure-markdown_github/arima-plots-16.png)

Request 1019
------------

What will be the FAO Dairy Price Index in May 2018?

![](README_files/figure-markdown_github/arima-plots-17.png)

Request 938
-----------

What will be the maximum sea ice extent on the Barents Sea between 1 January 2018 and 10 April 2018?

![](README_files/figure-markdown_github/arima-plots-18.png)

Request 929
-----------

How much crude oil will Libya produce in May 2018?

![](README_files/figure-markdown_github/arima-plots-19.png)

Request 911
-----------

How many material conflict events involving Occupied Palestinian Territory will ICEWS record in March 2018?

![](README_files/figure-markdown_github/arima-plots-20.png)

Request 902
-----------

How many material conflict events involving India will ICEWS record in April 2018?

![](README_files/figure-markdown_github/arima-plots-21.png)

Request 893
-----------

What will be the South Korean Won to one U.S. Dollar daily exchange rate on 29 June 2018?

![](README_files/figure-markdown_github/arima-plots-22.png)

Request 884
-----------

How many positive influenza virus detections will FluNet record for China between 12 March 2018 and 18 March 2018 (epidemiological week 11)?

![](README_files/figure-markdown_github/arima-plots-23.png)

Request 875
-----------

What will be the FAO Sugar Price Index in June 2018?

![](README_files/figure-markdown_github/arima-plots-24.png)

Request 866
-----------

How many earthquakes of magnitude 5 or stronger will occur worldwide in March 2018?

![](README_files/figure-markdown_github/arima-plots-25.png)

Request 839
-----------

Will ACLED record any riot/protest events in Gabon in April 2018?

![](README_files/figure-markdown_github/arima-plots-26.png)

Request 830
-----------

How many battle deaths will ACLED record in Yemen in May 2018?

![](README_files/figure-markdown_github/arima-plots-27.png)

Request 821
-----------

How many battle deaths will ACLED record in Afghanistan in April 2018?

![](README_files/figure-markdown_github/arima-plots-28.png)
