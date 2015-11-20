#!/usr/bin/python3
# bipartite.py by josh

import sys
import random
import string

infile = open("Example_OTUs.csv", "r")
infile.readline()
exampleOTUs = []
for line in infile:
    row = line.rstrip().split(",")
    row.pop(0)
    row = list(map(int, row))
#    print(row)
    exampleOTUs.append(row)

#for line in exampleOTUs:
#    print(line)

#sys.exit("Test")

##################################################
##################################################
# Define function: unshared_copy

def unshared_copy(inList):
    if isinstance(inList, list):
        return list( map(unshared_copy, inList) )
    return inList


##################################################
##################################################
# Define function: empty

def empty(dataframe, count=False):
    numcols = len(dataframe[0])
    numrows = len(dataframe)
    numemptycols = 0
    numemptyrows = 0
    emptycolids = []
    emptyrowids = []
    # Figure out which rows are empty and collect their indexes
    for rowindex in range(0,numrows):
#        print("Row index:", rowindex)
        row = dataframe[rowindex]
#        print("Row:", row)
#        print("Row sum:", sum(row))
        if sum(row) == 0:
            numemptyrows += 1
            emptyrowids.append(rowindex)
    # Figure out which columns are empty and collect their indexes
    for colindex in range(0,numcols):
#        print("Column index:", colindex)
        col = []
        for rowindex in range(0,numrows):
            col.append(dataframe[rowindex][colindex])
#        print("Column:", col)
#        print("Column sum:", sum(col))
        if sum(col) == 0:
            numemptycols += 1
            emptycolids.append(colindex)
    print("Found empty rows:", emptyrowids)
    print("Found empty cols:", emptycolids)
    emptyrowids.sort(reverse=True)
    emptycolids.sort(reverse=True)
    # Get rid of empty rows
    for emptyrow in emptyrowids:
        dataframe.pop(emptyrow)
    # Get rid of empty columns
    for emptycol in emptycolids:
        for rowindex in range(0,len(dataframe)):
            popped = dataframe[rowindex].pop(emptycol)
            if not popped == 0:
                sys.exit("Error, removing a nonzero value")
    emptyinfo = [numemptyrows, numemptycols]
    return [dataframe, emptyinfo]



##################################################
##################################################
# Define function: extinction

def extinction(dataframe, participant, method):
    numcols = len(dataframe[0])
    numrows = len(dataframe)
    rowsums = []
    colsums = []
    for rowindex in range(0,numrows):
#        print(dataframe[rowindex])
        rowsum = sum(dataframe[rowindex])
        rowsums.append(rowsum)
    for colindex in range(0,numcols):
        col = []
        for rowindex in range(0,numrows):
            col.append(dataframe[rowindex][colindex])
        colsum = sum(col)
        colsums.append(colsum)
    if participant == "both" and method == "random":
        # Randomly pick a participant:
        partindex = random.randint(0,1)
        participant = participants[partindex]
        print("Participant is now:",participant)
    if method == "random":
        rowextin = random.randint(0,numrows-1)
        colextin = random.randint(0,numcols-1)
        if participant == "lower":
            for colindex in range(0,numcols):
                dataframe[rowextin][colindex] = 0
        if participant == "higher":
            for rowindex in range(0,numrows):
                dataframe[rowindex][colextin] = 0
    elif method == "abundance":
        # Reshuffle columns
        randomcolorder = list(range(0,numcols))
        random.shuffle(randomcolorder)
#        print(randomcolorder)
        for rowindex in range(0,numrows):
            newrow = []
            for randomcol in randomcolorder:
                copyint = int(dataframe[rowindex][randomcol])
                newrow.append(copyint)
            dataframe[rowindex] = newrow[:]
        # Reshuffle rows
        random.shuffle(dataframe)
        rowseq = [i[0] for i in sorted(enumerate(rowsums), key=lambda x:x[1])]
        colseq = [i[0] for i in sorted(enumerate(colsums), key=lambda x:x[1])]
        if participant == "lower":
            for colindex in range(0,numcols):
                dataframe[rowseq[0]][colindex] = 0
        elif participant == "higher":
            for rowindex in range(0,numrows):
                dataframe[rowindex][colseq[0]] = 0
        elif participant == "both":
            if min(rowsums) < min(colsums):
                for colindex in range(0,numcols):
                    dataframe[rowseq[0]][colindex] = 0
            elif min(rowsums) > min(colsums):
                for rowindex in range(0,numrows):
                    dataframe[rowindex][colseq[0]] = 0
            elif min(rowsums) == min(colsums):
                if random.randint(0,1) == 0:
                    for colindex in range(0,numcols):
                        dataframe[rowseq[0]][colindex] = 0
                elif random.randint(0,1) == 1:
                    for rowindex in range(0,numrows):
                        dataframe[rowindex][colseq[0]] = 0
                else:
                    sys.exit("Error")
            else:
                sys.exit("Error")
        else:
            sys.exit("Error")
    elif method == "degree":
        if participant == "lower":
            toprows = [index for index, val in enumerate(rowsums) if val == max(rowsums)]
            if len(toprows) > 1:
                rowextin = toprows[random.randint(0,len(toprows)-1)]
            else:
                rowextin = toprows[0]
            for colindex in range(0,numcols):
                dataframe[rowextin][colindex] = 0
        if participant == "higher":
            topcols = [index for index, val in enumerate(colsums) if val == max(colsums)]
            if len(topcols) > 1:
                colextin = topcols[random.randint(0,len(topcols)-1)]
            else:
                colextin = topcols[0]
            for rowindex in range(0,numrows):
                dataframe[rowindex][colextin] = 0
    else:
        sys.exit("Error")
    return dataframe


##################################################
##################################################
# Define function: secondextinction and nested function onesecondextinction

def secondextinction(dataframe, participant, method):
    for line in dataframe:
        print(line)
    print("Above is your starting dataframe")
    numcols = len(dataframe[0])
    numrows = len(dataframe)
    print("Number of columns:",numcols)
    print("Number of rows:",numrows)
    input("Press enter to continue.\n")
    participants = ("lower", "higher", "both")
    methods = ("random", "abundance", "degree")
    while participant not in participants:
        participant = input("Please re-enter your participant, lower/higher/both...\n")
    while method not in methods:
        method = input("Please re-enter your method, random/abundance/degree...\n")
    def onesecondextinction(osDataframe, osParticipant, osMethod):
        dead = []
        dead.append(["no","ext.lower","ext.higher"])
        osDF = unshared_copy(osDataframe)
        repz = True
        i = 1
        while repz:
            # Run extinction() to kill off one taxon
            currentdf = extinction(dataframe=osDF, participant=osParticipant, method=osMethod)
            print("Dataframe after running extinction:")
            for line in currentdf:
                print(line)
            # Run empty() to clean up dataframe
            osDF, emptyoutput = empty(currentdf, count=True)
            print("Dataframe after clearing with empty:")
            for line in osDF:
                print(line)
            if not len(emptyoutput) == 2:
                print(emptyoutput)
                sys.exit("Error, output from function empty() not of correct length")
            deadrow = [i] + emptyoutput
            dead.append(deadrow)
            numcols = len(osDF[0])
            numrows = len(osDF)
            print("Number of columns:",numcols)
            print("Number of rows:",numrows)
            input("Press enter to continue.\n")
            if osParticipant == "lower" and numrows < 2:
                break
            if osParticipant == "higher" and numcols < 2:
                break
            if osParticipant == "both" and (numrows < 2 or numcols < 2):
                break
            if numrows <= 1 or numcols <= 1:
                break
            i += 1
        dead2 = unshared_copy(dead)
        deadrows = len(dead)
        dead2.append([deadrows,numrows,numcols])
#        for line in dead2:
#            print(line)
        return dead2
    seOutput = onesecondextinction(osDataframe=dataframe, osParticipant=participant, osMethod=method)
    return seOutput

myoutput = secondextinction(dataframe=exampleOTUs,participant="lower",method="abundance")

for line in myoutput:
    print(line)


quit()
        

    
    

            
