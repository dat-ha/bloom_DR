The data and code were used for analysis and creating plots for the manuscript
"Quantifying Algal Blooms Using a Disturbance-Recovery Approach"

There is 1 R file and 4 csvs.

Experimental_lakes_chl.csv has data from the experimental lakes, Peter, Paul, and Tuesday Lakes. There are four columns - Lake, Year, Manual_Chl (chlorophyll concentration measured in micrograms per liter), and DOYtrunc (day of year). 

Monitored_Lakes_Chlorophyll_final.csv and Monitored_Lakes_Phycocyanin_final.csv have the daily averages of chlorophyll-a and phycocyanin concentration (micrograms per liter) calculated for twenty two monitored lakes. These csvs each have five columns - Lake, date, pigment (chlorophyll or phycocyanin in micrograms per liter), DOY (day of year), and Year.

FINAL_DISTURBANCES_zsubset.csv is the output file for running the disturbance recovery algorithm on the monitored lakes. The code to run the algorithm to produce this output file is in the R file (Bloom_DR_code.R) and the following analyses. This file has 11 columns - Lake, dist.date (the day of year when the bloom began), recov.date (the day of year when the bloom ended), Year, peak.z (the peak z score from the disturbance), Pigment (either chlorophyll-a or phycocyanin), dist_length (duration of the disturbance in days), log_peakz (log10 of peak.z), log_length (log10 of dist_length), and Trophic_Status (status of the waterbody based on existing literature). 
