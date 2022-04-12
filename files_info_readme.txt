Data Formats of the files generated:

1) user_data.csv = Stores user information, the most important category here is the ID, it binds the data of a particular
user to his data in all the other files.

2) user_dig_seq.csv = The unique ID represents to whom the data belongs. It stores the try number and the round number, where
try number is atmax 2 per round as one is allowed 2 mistakes, so at max 2 digit sequences will be generated, 
the corresponding digit sequence is mentioned, the index where the user goes wrong in the digit sequence is indicated in 
the last column(mis_ind)

3) user_restart_wrong.csv = Unique ID identifies individual. It stores the number of times the user hits the restart button
(which is allowed to be at max 1 per round) and the number of times he is wrong per round(which can be at max 2)

4) user_digit_click_time.csv = Unique ID identifies individual. It stores the time difference between clicks on the digits of the digit pad
after the user is allowed to guess the number he was shown. Similar to user_dig_seq we have try and rounds column.

FAQ: Why did I go for long form of storing the data rather than wide form?
One user may get 10 digit sequence correct whereas some other user may get 5 digit sequence correct. So, there will be different number of columns and a lots of NAs need to be padded to make the data tabular. Hence, I used long form of storing the data for users.
FAQ: Why did I generate multiple files instead of one?
Cause the data we collect is of varied type. So it makes more sense to store the data in different files instead of one.
