# Thesis
Created many different models to predict the outcome of professional LoL matches. 

The thesis itself can be found in the PDF file or at this link: http://hdl.handle.net/10211.3/216025. The presentation that I created for my defense can be found in the `Presentation` folder. 

This is my master's thesis titled "Statistical Learning for Esports Match Prediction". The focus was on seeing which model was the best at predicting the outcome of professional League of Legends (LoL) matches based on information available up to the 15-minute mark. Many models were created, such as logistic regression, LASSO, SVM, LDA/QDA, and I put an emphasis on Gaussian Processes as they are lesser explored especially in the classification area. 

The code was done completely in R making use of the many libraries available on CRAN, and the data was obtained from Oracle's Elixir (https://oracleselixir.com/). 

The R script is slightly messy and does contain *some* unnecessary code that was for testing results and making sure everything checked out. In general, however, most of it is needed and what was used for the thesis, including plot generation. 

There were two versions of the R script initially, which I filtered down to be just this one. The older version resulted in many different plots and AUC curves (mostly seen in the `Plots` folder), but the new one cleans them up a bit and newer plots (mostly for the ROC curves) can be seen in `Plots2`. 
