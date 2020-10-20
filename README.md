#HARVARD ONLINE - EDX PLATFORM - MACHINE LEARNING - CAPSTONE - MOVIELENS PROJECT
	##This README file gives a brief information on the design strategies adopted in constrained technical environments.
		###COVID-19 - STUCK IN INDIA LOCKDOWN - LIMTED WiFi - Local laptop RStudio - R v3.3.3 - Not compatible with R Markdown.
		###Broken the large edx and validation data into smaller set, uploaded to pushed to Github, pulled into RStudio cloud for Rmd and PDF report generation.
			####
			####
		###RMSE-Data_Wrangling.R - This R script is coded to in local laptop with RStudio wiht R v3.3.3 for data wrangling features
		#### Some technical highlights - 
			#####Downloading the R data files (Rds) using URL (both train and validation files), Breaking the large data sets into smaller files (Caret package),
			#####Saving into CSV files using write.table function
			#####Pushing the CSV files into Github repository for pulling to RStudio Cloud
			#####
			#####
		###RMSE_Analysis.R & RMSE_Analysis-Rmd.Rmd - These files are used to code the algoorithms accommodating different biases to estimate the ratings of test data
		###and to generate a PDF file for report purposes
			####Comment lines in the code breifly highlight what that section/chunk does and computes
			####
			####
		###RMSE_Analysis-Rmd.pdf - Final report file generated knitting Rmd file
