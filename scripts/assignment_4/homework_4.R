###### Scalability and Efficiency: Assignment 4 #####

# In this assignment, we will seek to speed up the data management taks we
# performed in the second assignment. My solution code for that assignment took
# about 45 minutes to run. Our goal is to drastically reduce the runtime while
# arriving at the same answer. I will be providing more guidance for this
# assignment than for others because the programming here is more challenging.
# In theory, you could complete this assignment in about five minutes working
# off of the code I gave you, but I encourage you to really think about how you
# might complete the tasks more efficiently. The solution code here will serve as
# a nice template for future work where you need to form similar aggregate
# datasets.

### Preliminaries ###
rm(list = ls())

# Set your working directory to the folder containing 471 .csv files. For me,
# this looks like:
setwd("~/Desktop/Multi_Datasets")

# You will want to make use of the following package, which you can download if
# you have not already:
# install.packages("rio", dependencies = TRUE)
library(rio)

# As in assignment 2, if you go and look at the working directory, you will find
# 471 .csv files. You will need to read these in and combine them. They record
# some information about bills introduced in the U.S. Congress over a 22 year
# period (1993-2014). Each bill is broken down into sections, so there are
# multiple observations for each bill. Here is an explanation of what each
# variable is:

# $session -- The session of Congress in which the bill was introduced.
# $chamber -- HR = House of Representatives, S = Senate.
# $number -- The number assigned to the bill in that session of Congress.
# $type -- IH = Introduced in House, IS = Introduced in Senate.
# $section -- The index of each section.
# $Cosponsr -- The number of Cosponsors for each bill.
# $IntrDate -- The date the bill was introduced.
# $Title -- The title of the bill.
# $NameFull -- The full name of the legislator who introduced the bill.
# $major_topic_label -- The topic of the bill.
# $party_name -- The party of the legislator that introduced the bill.




### Exercise 1 ###

# Use a 'for()' loop to read in the data and combine them into a single
# data.frame. The difference here is that we are going to try pre-allocating a
# data.frame to slot each additional chunk of data into, as opposed to using
# rbind() to create a growing data structure. We can time the difference here
# as well.



# Some hints:
# 1. Start by pre-allocating a data.frame with about 500,000 rows (as we know
# from assignment 2 we only need about 470,000). In general, you may not know
# exactly how many rows you need, so it is good practice to add some additional
# rows. You will also want to use one of the 471 datasets as a template for this
# dataset. The reason is that changing variable types (from string to numeric,
# for example) can slow you down.
# 2. Use the 'import()' function from the rio package.
# 3. You will need to keep track of the first and last rows you are inserting
# the current dataset into, and then chop off the unused rows at the end.

# To start out, we can try things the slow way. I suggest you take this code and
# modify it to make it more efficient.
system.time({
    # Initialize data to NULL
    sections <- NULL

    # Loop over 471 datasets. Alternatively we could only loop over 1:20, to make
    # everything go faster:
    for (i in 1:471) {
        # print out the current dataset to keep track of our progress:
        print(i)

        # import the current dataset and use 'paste()' to generate the filenames
        cur <- import(paste("dataset_",i,".csv", sep = ""))

        # 'rbind()' the current data onto the full data.frame
        sections <- rbind(sections,cur)
    }
})


# Here is a bit of code to get you started:
system.time({

    # We will need to create a dummy dataset where we can slot each new dataset
    # we load in to it. You could do this by hand (creating a data frame and
    # making sure it has all of the right columns), but this code is more
    # general. It was also a bit of pain to get working, so I thought I would
    # share this with you to get you started. We begin by loading in one chunk:
    temp <- import(paste("dataset_",1,".csv", sep = ""))

    # Next, we create a single column data.frame by repeating the first value
    # in the first column of temp 500,000 times (more than the number of rows
    # we are dealing with in total, which is about 470,000). We add this extra
    # buffer so we do not have to assume a priori that we know how many rows
    # will be in our final dataset. Call this sections2 so we can compare it
    # to sections from our previous bit of code.
    sections2 <- data.frame(var1 = rep(temp[1,1],500000),
                            stringsAsFactors = FALSE)

    # Now we loop through the rest of the columns in temp and cbind() them on to
    # "sections2" to build up our dummy data.frame one column at a time:
    for (i in 2:ncol(temp)) {
        sections2 <- cbind.data.frame(sections2,
                                      rep(temp[1,i],500000),
                                      stringsAsFactors = FALSE)
    }

    # Now we will need to keep track of the first and last row where we want to
    # insert the current chunk of data we are working with:
    start <- 1
    end <- 1
    # Loop over 471 datasets.
    for (i in 1:471) {
        # Print out the current dataset to keep track of our progress:
        print(i)

        # Import the current dataset and use 'paste()' to generate the filenames
        cur <- import(paste("dataset_",i,".csv", sep = ""))

        # Need to update the value of end based on the number of rows in cur:

        # Put cur in the right chunk of rows in sections2.

        # Update the value for start:

    }

    # Chop off the extra rows we do not need using 'end' as a guide:

    # Make sure that the column names are correct:
})


# Check to see if we get the same result:
all.equal(c(sections),c(sections2))

### Exercise 2 ###

# Create a new dataset that collapses the original dataset over bill sections so
# there is only one entry per unique bill. Add a field to this collapsed dataset
# called "Sections" that records the total number of sections associated with
# that bill. Removed the "section" variable from the data.frame as it is no
# longer necessary. How many rows are in this data.frame? Does the sum of the
# "Sections" variable in your new data.frame equal the number of rows in the old
# data.frame (with one entry per section)?

# Some hints:
# 1. Start with my slow solution code below, and then make modifications to try
# and speed it up.



# First, we can try things the slow way, but note that this will take more than
# 40 minutes on most computers:
system.time({
    # Get the bill IDs:
    Bill_ID <- paste(sections$session,
                     sections$chamber,
                     sections$number,
                     sep = "-")

    # Get the unique bills:
    unique_bills <- unique(Bill_ID)

    # Create a vector to hold the count of sections in each unique bill:
    Num_Sections <- rep(0, length(unique_bills))

    # Create a copy of sections, the make it have the right number of rows. We will
    # then overwrite these row values with the correct ones:
    bills <- sections
    bills <- bills[1:length(unique_bills),]

    # Loop over unique bills (this will take half an hour or so):
    for (i in 1:length(unique_bills)) {
        # print out the current dataset to keep track of our progress:
        if (i %% 100 == 0) {
            print(i)
        }

        # Find the rows associated with each unique bill:
        inds <- which(Bill_ID == unique_bills[i])

        # Record the number of sections associated with the current bill
        Num_Sections[i] <- length(inds)

        # Replace the row with the correct data
        bills[i,] <- sections[inds[1],]

    }

    # Now add on our "Sections" variable
    bills <- bills[,-5] # remove the fifth column("section")
    bills <- cbind(bills, Num_Sections)
})




# You will need to fill in code here:
system.time({
    # Generate Bill IDs:
    Bill_ID <- paste(sections2$session,
                     sections2$chamber,
                     sections2$number,
                     sep = "-")

    # Create a vector to hold the count of sections in each unique bill, as well
    # as the row index of the section we want to extract information from to
    # use for the bills dataset (I have been selecting the first or last row
    # associated with a given bill to serve as the representative for that bill).
    Num_Sections <- rep(0, length(unique_bills))
    bill_inds <- rep(0, length(unique_bills))

    # Create a number of temporary variables that we will use in our pass
    # through the data.

    # cur_bill will store the current bill ID, which we can check against the
    # i'th bill id to see if it has changed. If it has, then we will know we
    # have reached a new bill, and it is time to save the number of sections in
    # the previous bill:
    cur_bill <- Bill_ID[1]

    # cur_counter is a temporary variable to storing the count of the number of
    # sections in the current bill. This will get reset to zero when we hit a
    # new bill:
    cur_counter <- 0

    # bill_counter will store the index of the current bill we are working with.
    # This will tell us where to put information about the number of sections in
    # a bill, or the current bill ID.
    bill_counter <- 0

    # We are going to loop over the bill sections this time instead of the unique
    # bills. The idea is we only want to visit each bill section once. We can do
    # this because the bill sections are ordered (all bill sections from a given
    # bill are in a clump), so to count sections we only need to keep track of
    # how many sections from the same bill we hit in a row.
    for (i in 1:nrow(sections2)) {

        # If we have reached a new bill, then we will want to store information
        # about the previous bill.
        if (cur_bill != Bill_ID[i]) {
            # Increment the bill counter. We do this first because it starts at
            # zero.

            # Store the row index for the previous row in the bill_inds vector
            # at the spot denoted by bill_counter. We will use these indices
            # later to for a collapsed dataset from sections2:

            # Store the count of number of sections in the current bill:

            # Reset cur counter to one. The reason we do not reset it to zero is
            # that we are already on the first section of the next bill by the
            # time we get to this clause.

            # Update cur_bill to the current bill ID so we can start checking to
            # see if we have moved on to a new bill.

        } else {
            # If we ware still in the same bill, then we just need to increment
            # the counter for the number of sections in the current bill:
        }

    }

    # Once we have completed the loop, we will need to deal with the last bill,
    # for which we will have not saved any information (at least the way I wrote
    # this code) because there was no different bill after it:

    # Now we go in and grab all of the rows out of sections2 at once to form
    # bills2. This is a place where we save an enormous amount of time, as
    # selecting many rows at once from a data.frame is about as fast as selecting
    # just one:

    # Now add on our "Num_Sections" variable:

})

# Check to see if we get the same result:
all.equal(c(bills),c(bills2))















