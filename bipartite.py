#!/usr/bin/python3
# bipartite.py by josh

import sys
import random
import string
import re

#infile = open("Example_OTUs.csv", "r")
infile = open("Texas_data.csv","r")
#infile.readline()
exampleOTUs = []
for idx, line in enumerate(infile):
    row = line.rstrip().split(",")
#    row.pop(0)
    if idx != 0:
        row[1:] = list(map(int, row[1:]))
        row[1:] = [1 if val >= 2 else val for val in row[1:]]
        print(row)
#        for indx in range(1,len(row)):
#            if row[indx] >= 2:
#                row[indx] = 1
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
#Define function: eval_df

def eval_df(dataframe, taxadict):
    predatorlist = dataframe[0][:]
    print("Predator list:",predatorlist)
    for colindex in range(1, len(predatorlist)):
        col = []
        predator = predatorlist[colindex]
        print("Predator:",predator)
        preygained = 0
        preylost = 0
        if predator not in taxadict:
            sys.exit("Error, predator not found in dictionary.")
        preybreadth = len(taxadict[predator]) - 1
        print("Number of prey species currently in dictionary for this predator:",preybreadth)
        for rowindex in range(1, len(dataframe)):
            preyOTU = dataframe[rowindex][0]
#            print("Prey name:",preyOTU)
            interactionval = dataframe[rowindex][colindex]
            if interactionval == 1:
                col.append(interactionval)
                if taxadict[predator].count(preyOTU) == 0:
                    taxadict[predator].append(preyOTU)
                    preygained += 1
                elif taxadict[predator].count(preyOTU) == 1:
                    continue
                else:
                    sys.exit("Error, prey found more than once under predator lookup.")
            elif interactionval == 0:
                col.append(interactionval)
                if taxadict[predator].count(preyOTU) == 0:
                    continue
                elif taxadict[predator].count(preyOTU) == 1:
                    taxadict[predator].remove(preyOTU)
                    preylost += 1
                else:
                    sys.exit("Error, prey found more than once under predator lookup.")
            else:
                sys.exit("Error, non-binary interaction value found.")
        colsum = sum(col)
        if colsum != len(taxadict[predator])-1:
            sys.exit("Error, mismatch in number of predator interactions.")
        if colsum != preybreadth + preygained - preylost:
            sys.exit("Error, mismatch in number of predator interactions.")
        taxadict[predator][0] = colsum
#    for key, vals in taxadict.items():
#        print(key, ":", vals)
    return taxadict
        

                    

##################################################
##################################################
# Define function: empty

def empty(dataframe, count=False):
    numcols = len(dataframe[0])-1
    numrows = len(dataframe)-1
    numemptycols = 0
    numemptyrows = 0
    emptycolids = []
    emptyrowids = []
    # Figure out which rows are empty and collect their indexes
    for rowindex in range(1,numrows+1):
#        print("Row index:", rowindex)
        row = dataframe[rowindex][1:]
#        print("Row:", row)
#        print("Row sum:", sum(row))
        if sum(row) == 0:
            numemptyrows += 1
#            emptyOTU = dataframe[rowindex][0]
            emptyrowids.append(rowindex)
    # Figure out which columns are empty and collect their indexes
    for colindex in range(1,numcols+1):
#        print("Column index:", colindex)
        col = []
        for rowindex in range(1,numrows+1):
            col.append(dataframe[rowindex][colindex])
#        print("Column:", col)
#        print("Column sum:", sum(col))
        if sum(col) == 0:
            numemptycols += 1
#            emptypred = dataframe[0][colindex]
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
            if rowindex == 0:
                continue
            else:
                if not popped == 0:
                    sys.exit("Error, removing a nonzero value")
    emptyinfo = [numemptyrows, numemptycols]
    return [dataframe, emptyinfo]



##################################################
##################################################
# Define function: extinction

def extinction(dataframe, participant, method, interactiondict, switchdict, extOTUdict):
    numcols = len(dataframe[0])-1
    numrows = len(dataframe)-1
    rowsums = []
    colsums = []
    for rowindex in range(1,numrows+1):
        row = dataframe[rowindex][1:]
#        print(dataframe[rowindex])
        rowsum = sum(row)
        rowsums.append(rowsum)
    for colindex in range(1,numcols+1):
        col = []
        for rowindex in range(1,numrows+1):
            col.append(dataframe[rowindex][colindex])
        colsum = sum(col)
        colsums.append(colsum)
    if participant == "both" and method == "random":
        # Randomly pick a participant:
        partindex = random.randint(0,1)
        participant = participants[partindex]
        print("Participant is now:",participant)
    if method == "random":
        rowextin = random.randint(1,numrows)
        colextin = random.randint(1,numcols)
        if participant == "lower":
            for colindex in range(1,numcols+1):
                dataframe[rowextin][colindex] = 0
        if participant == "higher":
            for rowindex in range(1,numrows+1):
                dataframe[rowindex][colextin] = 0
    elif method == "abundance":
        rowsums = []
        colsums = []
        # Reshuffle columns
        randomcolorder = list(range(1,numcols+1))
        random.shuffle(randomcolorder)
        randomcolorder = [0] + randomcolorder
        print("Randomised column order:", randomcolorder)
        for rowindex in range(0,numrows+1):
            newrow = []
            for randomcol in randomcolorder:
                if rowindex == 0:
                    copyval = str(dataframe[rowindex][randomcol])
                else:
                    if randomcol == 0:
                        copyval = str(dataframe[rowindex][randomcol])
                    else:
                        copyval = int(dataframe[rowindex][randomcol])
                newrow.append(copyval)
            dataframe[rowindex] = newrow[:]
        # Reshuffle rows
        headerrow = dataframe.pop(0)
        random.shuffle(dataframe)
        dataframe.insert(0, headerrow)
#        for line in dataframe:
#            print(line)
#        print("This is a re-shuffled dataframe")
#        input("Press enter to continue")
        # Caculate new row/column sums of shuffled dataframe
        for rowindex in range(1,numrows+1):
            row = dataframe[rowindex][1:]
            rowsum = sum(row)
            rowsums.append(rowsum)
        for colindex in range(1,numcols+1):
            col = []
            for rowindex in range(1,numrows+1):
                col.append(dataframe[rowindex][colindex])
            colsum = sum(col)
            colsums.append(colsum)
        print("Row sums:", rowsums)
        print("Column sums:", colsums)
        rowseq = [i[0]+1 for i in sorted(enumerate(rowsums), key=lambda x:x[1])]
        print("Row indexes, sorted by row sum:")
        print(rowseq)
        colseq = [i[0]+1 for i in sorted(enumerate(colsums), key=lambda x:x[1])]
        print("Column indexes, sorted by column sum:")
        print(colseq)
        if participant == "lower":
            expreyidx = rowseq[0]
            extOTU = dataframe[expreyidx][0]
            if extOTU in extOTUdict:
                sys.exit("Error, prey already made extinct")
            else:
                extOTUdict[extOTU] = 1
            print("Making OTU",extOTU,"on row",expreyidx,"extinct")
            for colindex in range(1,numcols+1):
                cpreda = dataframe[0][colindex]
                nonpreyidxs = [idx for idx in range(1,len(dataframe)) if (dataframe[idx][colindex] == 0 and dataframe[idx][0] not in extOTUdict)]
#                print("Current predator,",cpreda,"does not feed on non-extinct prey species in rows:",nonpreyidxs, ". Total:", (len(dataframe)-1) - len(nonpreyidxs), interactiondict[cpreda][0])
                if switchdict[cpreda] == "switcher" and len(nonpreyidxs) > 0:
                    print("This predator is a switcher")
                    newprey = nonpreyidxs[random.randint(0,len(nonpreyidxs)-1)]
                    if dataframe[newprey][colindex] == 0:
                        dataframe[newprey][colindex] = 1
                    else:
                        sys.exit("Error, unexpected interaction found")
                dataframe[expreyidx][colindex] = 0
#            for key, val in extOTUdict.items():
#                print(key, val)
        elif participant == "higher":
            expredidx = colseq[0]
            extpred = dataframe[0][expredidx]
            print("Making predator",extpred,"on col",expredidx,"extinct")
            for rowindex in range(1,numrows+1):
                dataframe[rowindex][colseq[0]] = 0
        elif participant == "both":
            if min(rowsums) < min(colsums):
                for colindex in range(1,numcols+1):
                    dataframe[rowseq[0]][colindex] = 0
            elif min(rowsums) > min(colsums):
                for rowindex in range(1,numrows+1):
                    dataframe[rowindex][colseq[0]] = 0
            elif min(rowsums) == min(colsums):
                if random.randint(0,1) == 0:
                    for colindex in range(1,numcols+1):
                        dataframe[rowseq[0]][colindex] = 0
                else:
                    for rowindex in range(1,numrows+1):
                        dataframe[rowindex][colseq[0]] = 0
            else:
                sys.exit("Error")
        else:
            sys.exit("Error")
    elif method == "degree":
        if participant == "lower":
            toprows = [index+1 for index, val in enumerate(rowsums) if val == max(rowsums)]
            if len(toprows) > 1:
                rowextin = toprows[random.randint(0,len(toprows)-1)]
            else:
                rowextin = toprows[0]
            for colindex in range(1,numcols+1):
                dataframe[rowextin][colindex] = 0
        if participant == "higher":
            topcols = [index+1 for index, val in enumerate(colsums) if val == max(colsums)]
            if len(topcols) > 1:
                colextin = topcols[random.randint(0,len(topcols)-1)]
            else:
                colextin = topcols[0]
            for rowindex in range(1,numrows+1):
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
    numcols = len(dataframe[0])-1
    numrows = len(dataframe)-1
    print("Number of columns:",numcols)
    print("Number of rows:",numrows)
    input("Press enter to continue")
    interactiondict = {}
    switchdict = {}
    extOTUdict = {}
    predatorlist = dataframe[0][1:]
    print("Starting predator list:",predatorlist)
    numpreds = len(predatorlist)
    for colindex in range(1, numpreds + 1):
        predator = predatorlist[colindex-1]
#        print("Predator:",predator)
        if predator in interactiondict or predator in switchdict:
            sys.exit("Error, predator already in dictionary")
        else:
            interactiondict[predator] = [0]
            if random.randint(0,1) == 0:
                switchdict[predator] = "non_switcher"
            else:
                switchdict[predator] = "switcher"
        for rowindex in range(1, len(dataframe)):
            preyOTU = dataframe[rowindex][0]
#            print("Prey name:",preyOTU)
            interactionval = dataframe[rowindex][colindex]
            if interactionval == 1:
                if predator not in interactiondict:
                    sys.exit("Error, predator not found in dictionary")
                else:
                    if interactiondict[predator].count(preyOTU) != 0:
                        sys.exit("Error, prey already found under predator lookup")
                    else:
                        interactiondict[predator].append(preyOTU)
                        interactiondict[predator][0] += 1
            elif interactionval == 0:
                continue
            else:
                sys.exit("Error, non-binary interaction value found")
#    for key, vals in interactiondict.items():
#        print(key, ":", vals)
#    input("Press enter to continue.\n")
    participants = ("lower", "higher", "both")
    methods = ("random", "abundance", "degree")
    while participant not in participants:
        participant = input("Please re-enter your participant, lower/higher/both...\n")
    while method not in methods:
        method = input("Please re-enter your method, random/abundance/degree...\n")
    def onesecondextinction(osDataframe, osParticipant, osMethod, currentdict):
        dead = []
        dead.append(["no","ext.lower","ext.higher"])
        osDF = unshared_copy(osDataframe)
        repz = True
        i = 1
        while repz:
            # Run extinction() to kill off one taxon
            currentdf = extinction(dataframe=osDF, participant=osParticipant, method=osMethod, interactiondict=currentdict, switchdict=switchdict, extOTUdict=extOTUdict)
            for line in currentdf:
                print(line)
            print("Above is the dataframe after running extinction().")
#            input("Press enter to continue")
            # Re-evaluate interaction dictionary
            currentdict = eval_df(dataframe=currentdf, taxadict=currentdict)
#            for key, vals in currentdict.items():
#                print(key, ":", vals)
#            print("Above is your updated dictionary.")
#            input("Press enter to continue")
            # Run empty() to clean up dataframe
            osDF, emptyoutput = empty(currentdf, count=True)
            for line in osDF:
                print(line)
            print("Above is the dataframe after clearing with empty().")
            if not len(emptyoutput) == 2:
                print(emptyoutput)
                sys.exit("Error, output from function empty() not of correct length")
            deadrow = [i] + emptyoutput
            dead.append(deadrow)
            numcols = len(osDF[0])-1
            numrows = len(osDF)-1
            print("Number of columns:",numcols)
            print("Number of rows:",numrows)
#            input("Press enter to continue.\n")
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
    seOutput = onesecondextinction(osDataframe=dataframe, osParticipant=participant, osMethod=method, currentdict=interactiondict)
    for key, val in switchdict.items():
        print(key,":", val)
    return seOutput

myoutput = secondextinction(dataframe=exampleOTUs,participant="lower",method="abundance")

for line in myoutput:
    print(line)


quit()
        

    
    

            
