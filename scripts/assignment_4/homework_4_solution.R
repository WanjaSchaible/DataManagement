###### Scalability and Efficiency: Assignment 4 SOLUTION #####


### Preliminaries ###
rm(list = ls())

# Set your working directory to the folder containing 471 .csv files. For me,
# this looks like:
setwd("~/Desktop/Multi_Datasets")

# You will want to make use of the following package, which you can download if
# you have not already:
# install.packages("rio", dependencies = TRUE)
library(rio)


### Exercise 1 ###

# Frist we can try things the slow way:
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
# On my computer, this takes:
#   user  system  elapsed
# 46.685  12.449   68.145

# Now lets try in a more efficient way:
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

        # import the current dataset and use 'paste()' to generate the filenames
        cur <- import(paste("dataset_",i,".csv", sep = ""))

        # Need to update the value of end based on the number of rows in cur:
        end <- start + nrow(cur) - 1

        # Put cur in the right chunk of rows in sections2.
        sections2[start:end,] <- cur

        # Update the value for start:
        start <- end + 1
    }

    # Now remove the extra rows:
    sections2 <- sections2[1:end,]

    # Make sure that the column names are correct:
    colnames(sections2) <- colnames(cur)
})
# On my computer, this takes:
#   user  system  elapsed
# 20.730   7.124   28.751
# The reason this goes faster is that it is more computationally efficient to
# insert blocks of rows into an existing dataset than to rbind() them on at the
# end of an existing one. The latter approach requires copying the entire
# data.frame, while this approach does not.

# Check to see if we get the same result:
all.equal(c(sections),c(sections2))



### Exercise 2 ###

# First, we can try things the slow way, but note that this will take a while:
system.time({
    # Alternatively, we could just do the following, which would be much faster:
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

    # Now add on our "Num_Sections" variable
    bills <- bills[,-5] # remove the fifth column("section")
    bills <- cbind(bills, Num_Sections)
})

#    user   system  elapsed
# 737.768  407.137 1153.607

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
            bill_counter <- bill_counter + 1

            # Store the row index for the previous row in the bill_inds vector
            # at the spot denoted by bill_counter. We will use these indices
            # later to for a collapsed dataset from sections2:
            bill_inds[bill_counter] <- i-1

            # Store the count of number of sections in the current bill:
            Num_Sections[bill_counter] <- cur_counter

            # Reset cur counter to one. The reason we do not reset it to zero is
            # that we are already on the first section of the next bill by the
            # time we get to this clause.
            cur_counter <- 1

            # Update cur_bill to the current bill ID so we can start checking to
            # see if we have moved on to a new bill.
            cur_bill <- Bill_ID[i]
        } else {
            # If we ware still in the same bill, then we just need to increment
            # the counter for the number of sections in the current bill:
            cur_counter <- cur_counter + 1
        }

    }

    # Once we have completed the loop, we will need to deal with the last bill,
    # for which we will have not saved any information (at least the way I wrote
    # this code) because there was no different bill after it:
    bill_inds[bill_counter +1] <- i
    Num_Sections[bill_counter + 1] <- cur_counter

    # Now we go in and grab all of the rows out of sections2 at once to form
    # bills2. This is a place where we save an enormous amount of time, as
    # selecting many rows at once from a data.frame is about as fast as selecting
    # just one:
    bills2 <- sections2[bill_inds,]

    # Now add on our "Num_Sections" variable:
    bills2 <- bills2[,-5] # remove the fifth column("section")
    bills2 <- cbind(bills2, Num_Sections)
})

# On my computer, this takes:
#  user  system elapsed
# 0.512   0.009   0.522
# That works out to about a 2,200x speedup! The reason we get such a huge
# speedup is that we are only visit each section once, and do not have to keep
# searching though the sections data.frame to try and find all of the sections
# that match the current bill. This is made possible by the data being in a
# sorted order. You can also use the order() function to to make work even if
# your data are not in a sorted order. The other major speedup we get is by
# only extracting rows from sections2 once at the very end (passing it a big
# vector of the row indices we want). This is much faster than going one row at
# a time.

# Check to see if we get the same result:
all.equal(c(bills),c(bills2))


