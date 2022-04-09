# Digit Span Test
Digit Span Test deals with conducting psychological test on subjects to find out the working memory/short-term memory capacity. The test deals with simply displaying or speaking aloud numbers to each subject and giving them chance to repeat back the entire number said in correct order or reverse order. This test usually gives results with mean 7 and standard deviatiion 2. That's why 7 is many a times referred as the magic number of the human brain.\
The performance of test subjects in this test is explained best by the Modal Model of Memory, which assumes human memory to be divided into 3 parts, which are-
- Sensory Memory
- Short Term Memory
- Long Term Memory


The Digit Span Test is implemented here in `app.R` using R Shiny. The deployable code is implemented in `app-deploy.R`\
**Run**: You can also see the deployed version of this app at [Digit Span Test](https://rishidarkdevil.shinyapps.io/Digit-Span-Test/) or check the link in Repository's About. To run the application(i.e. webApp) locally one needs to install the following packages which are imported by using `library()` at the starting of the `app.R` file. Further download the `www` folder and keep it in same hierarchial form as in this repository to view images in the app while running which are added in the Theoretical backing of the Modal Model and Digit Span Test. `app-deploy.R` is slightly better as it contains better modal messages, progressbars, etc.

**FAQ**: What are the interesting thing about this implementation?
- It is fully autonomous, with proper use of notifications and progress bars.
- Makes Digit Span Test quite interactive and fun compared to the usual method.
- Comes with post Digit Span Test analysis which has a leaderboard to see your performance compared to others, round-wise performance analysis for each round and an overall rank.
- Comes with relevant theory and bacground materials which explains the Digit Span Test in greater details
- Comes with Results and Findings which displays what patterns and trends are there in already collected data in a very organised way along with their interpretations.
- Generates 4 files `user_data.csv`, `user_dig_seq.csv`, `user_digit_click_time.csv`, `user_restart_wrong.csv` which contains all possible aspects in which data can be captured on a single subject without increasing the number of questions or making this test as a part of bigger test(e.g. WAIS IQ Test). So, it allows using these files and data in your own analysis if needed.
- Provides opportunity to host this app on a server to allow large scale collection of data.

