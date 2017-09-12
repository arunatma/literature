# The Game:
# Total 48 cards
#   3, 4, 5, 6, 7, 8 are minors and 9, 10, J, Q, K, A are majors
#   Four flavors: Spades, Clubs, Diamonds and Hearts
# Teams:
#   2 teams - 3 person in each team seated alternatively
# Objective:
#   Collection of Minors and Majors.  Wins 1 and 2 points respectively
#   Gather maximum points together as a team
# Rules for asking:
#   1. At least one card should be there to ask further (called root card)
#   2. Can ask only from the opponent team members
#   3. Can ask from one of the other two from the same team only when the 
#       player is confident of finishing the set. If not, the set belongs to the
#       opponent team

# Six Players   : 0 to 5
# 4 Flavors     : 0 to 3        (Spades, Clubs, Hearts, Diamonds)
# Two Sets      : 0 and 1       (Minor, Major)
# Card Index    : 0 to 47       (MinorSpades, MajorSpades, MinorClubs ...)
# Order of Sets : MinorSpades, MajorSpades, MinorClubs ... (0 to 7)
# Card Names    : 3 to A in Spades, Clubs, Hearts and Diamonds
# Card Mapping  : 0 to 5 is 3 to 8 in Spades, 42 to 47 is 9 to A in Diamonds.

numPlayers <- 6
numFlavors <- 4
numPoles <- 2
numSets <- 8
setIndices <- c(1:numSets)
numCards <- 48
numCardsInSet <- 6
numCardsInFlavor <- numCardsInSet * 2
nameFlavors <- c("Spade", "Club", "Heart", "Diamond")
nameSets <- c("Minor", "Major")
nameCards <- list(
    c("3", "4",  "5", "6", "7", "8"),
    c("9", "10", "J", "Q", "K", "A"))
playerNames <- c("Abel", "Benz", "Carl", "Dira", "Elly", "Fema")
cardNums <- seq(numCards)
numTeams <- 2
teamNames <- c("Team 1", "Team 2")
    
cardToFlavor <- function(cardIndex){
    cardNum <- cardIndex - 1
    index <- (floor(cardNum / numCardsInFlavor))
    flavor <- index + 1
    return (flavor)
}

cardToSetNum <- function(cardIndex){
    cardNum <- cardIndex - 1
    index <- floor(cardNum / numCardsInSet)
    setNum <- index + 1
    return (setNum)
}

cardToSetIndex <- function(cardIndex){
    cardNum <- cardIndex - 1
    # To get to index id (1 to 6) a card belongs in the set 
    index <- cardNum %% numCardsInSet
    return (index + 1)
}

cardToSetPolarity <- function(cardIndex){
    cardNum <- cardIndex - 1
    # To identify minor or major
    index <- floor(cardNum / numCardsInSet)
    setPolarity <- (index %% numPoles) + 1
    return (setPolarity)
}

setToName <- function(setNum){
    highCard <- setNum * numCardsInSet
    flavorId <- cardToFlavor(highCard)
    flavorName <- nameFlavors[flavorId]
    setPolarity <- cardToSetPolarity(highCard)
    setName <- nameSets[setPolarity]
    comboName <- paste(flavorName, setName, sep="-")
    return(comboName) 
}

cardToName <- function(cardNum){
    polarity <- cardToSetPolarity(cardNum)
    setIndex <- cardToSetIndex(cardNum)
    cardName <- nameCards[[polarity]][setIndex]
    flavorId <- cardToFlavor(cardNum)
    flavorName <- nameFlavors[flavorId]
    comboName <- paste(cardName, flavorName, sep="-")
    return (comboName)
}

scoreBoard <- rep("", numSets)
names(scoreBoard) <- setToName(c(1:numSets))
teamBoard <- rep("", numSets)
names(teamBoard) <- names(scoreBoard)

initProb <- function (thePlayer){
    # This function updates the probTable
    # probTable is not passed inside; Assumed to be global

    # Initialize the probabilities:
    # For a viewer, if he is the holder, the probability of that card is 1
    # For a holder, with respect to other viewers, the probability is equally
    # distributed among other five players (0.2)
    
    playerIndex <- which(playerNames == thePlayer)
    playerCards <- cardsAndPlayers[[playerIndex]]
    
    # The viewer index of the current person
    curViewer <- viewerNames[which(playerNames == thePlayer)]
    # The viewer indices of all but the current person
    curNonViewers <- viewerNames[which(playerNames != thePlayer)]

    # The holder index of the current person
    curHolder <- holderNames[which(playerNames == thePlayer)]
    # The holder indices of all but the current person
    curNonHolders <- holderNames[which(playerNames != thePlayer)]

    # The viewer if he is the holder knows he has the card - Probability 1
    probTable[curViewer, curHolder, playerCards] <<- 1

    # The viewer also knows the others do not have his cards - Probability 0
    probTable[curViewer, curNonHolders, playerCards] <<- 0

    # But, the viewer has no idea about the other cards
    # He does not have those cards; So assigns himself probability 0
    # And, equally distributes the probability for other players
    otherCards <- setdiff(cardNums, playerCards)
    probTable[curViewer, curHolder, otherCards] <<- 0
    probTable[curViewer, curNonHolders, otherCards] <<- 0.2
}

transaction <- function(asker, giver, theCard){
    cardName <- cardToName(theCard)
    if (!(theCard %in% askableCards(asker))){
        print (paste(asker, "cannot ask this card:", theCard, cardName))
        return (asker)
    }
    
    askerIndex <- which(playerNames == asker)
    askerCards <- cardsAndPlayers[[askerIndex]]
    
    giverIndex <- which(playerNames == giver)
    giverCards <- cardsAndPlayers[[giverIndex]]
        
    print (paste(asker, "asks the card", theCard, cardName, "from", giver))
    
    txnDone <- theCard %in% giverCards
    nonHolders <- holderNames[which(playerNames != asker)]
    if (txnDone){
        # Taking the card giver set and placing it in the asker set
        cardsAndPlayers[[askerIndex]] <<- c(askerCards, theCard)
        cardsAndPlayers[[giverIndex]] <<- setdiff(giverCards, theCard)
            
        # Now, all know that the card was present with giver, now gone to asker
        # update probabilities accordingly
        probTable[view(playerNames), hold(asker), theCard] <<- 1
        probTable[view(playerNames), nonHolders, theCard] <<- 0
        
        handleEmptyHand(giver)
        
        print ("Transaction Done!")
        print (paste(asker, "gets the card", theCard, cardName, "from", giver))
        return (asker)
    }
    else{
        # All know that this card is not present with both the players
        probTable[view(playerNames), hold(asker), theCard] <<- 0
        probTable[view(playerNames), hold(giver), theCard] <<- 0
        
        # Everyone will know it is with someone else. Remember all holders
        # with non-zero probability; update equally
        sapply(viewerNames, updateProb, theCard)
        
        print ("Transaction Not Done!")
        print (paste(giver, "does not have the card", theCard, cardName,
            "to give to", asker))
        return (giver)
    }
    # The asker holds a root card. Everyone would know that.
    # Assume Abel has 4 cards (9, 10, J, Q Spades).  Now Carl gets A Spade 
    # from Benz.  K Spade of other 5 players (incl Carl) was 0.2 
    # Now, probability of K Spade for Carl, as viewed by Abel has to be 
    # updated to 1.  Abel, should know for sure Carl has K-Spade!
    # If anyone knows the holder of 5 cards in a set, the 6th card, he does not 
    # yet know, has to be with the asker. That probability be updated as 1, 
    # and the probability for the card with other folks be updated 0.
    # This has to be done for all players.
    
    setNum <- cardToSetNum(theCard)
    curSetCards <- setCards(setNum)
    for(theViewer in viewerNames){
        knownCards <- c()
        for (cardId in curSetCards){
            numOnes <- length(which(probTable[theViewer,holderNames,cardId]==1))
            if(numOnes == 1) {
                knownCards <- c(knownCards, cardId)
            }
        }
        if(length(knownCards == 5)){
            rootCard <- setdif(curSetCards, knownCards)
            probTable[theViewer, hold(asker), rootCard] <<- 1
            probTable[theViewer, nonHolders, rootCard] <<- 0
        }
    }
}

updateProb <- function(theViewer, theCard){
    nonZeroProb <- which(probTable[theViewer, holderNames, theCard] != 0)
    
    totalNonZero <- length(nonZeroProb)
    updatedProb <- 1 / totalNonZero
    
    probTable[theViewer, holderNames[nonZeroProb], theCard] <<- updatedProb
}

setCards <- function(setNum){
    maxLimit <- setNum * numCardsInSet
    minLimit <- maxLimit - numCardsInSet + 1
    givenSetCards <- c(minLimit:maxLimit)
    return(givenSetCards)
}

sameSetCards <- function(theCard){
    # Given a card, give all the cards in that set, including itself
    setNum <- cardToSetNum(theCard)
    givenSetCards <- setCards(setNum)
    return(givenSetCards)
}

askableCards <- function(thePlayer){
    playerIndex <- which(playerNames == thePlayer)
    playerCards <- cardsAndPlayers[[playerIndex]]
 
    relatedCards <- union(unlist(sapply(playerCards, sameSetCards)), c())
    askCards <- setdiff(relatedCards, playerCards)
    return(askCards)
}

canFinishSet <- function(theSet, thePlayer){
    rootPresent <- checkRootExists(theSet, thePlayer)
    cards <- setCards(theSet)
    # Get the probabilites for all cards in the set for all players, as the 
    # current player views - of what is knowledge of
    variousProbs <- c(probTable[view(thePlayer),,cards])
    probsNotZeroOne <- setdiff(variousProbs, c(1,0))
    # If a set is formed, he must have all 0 and 1 probabilites for the set
    hasSetFormed <- (length(probsNotZeroOne) == 0)
    return (rootPresent && hasSetFormed)
}

opponents <- function(playerIndex){
    if(playerIndex %in% c(1,3,5)){
        oppPlayers <- c(2,4,6)
    } else {
        oppPlayers <- c(1,3,5)
    }
    return(oppPlayers)
}

sameTeam <- function(playerIndex){
    if(playerIndex %in% c(1,3,5)){
        teamPlayers <- setdiff(c(1,3,5), playerIndex)
    } else {
        teamPlayers <- setdiff(c(2,4,6), playerIndex)
    }
    return(teamPlayers)
}

checkRootExists <- function(theSet, thePlayer){
    cards <- setCards(theSet)
    setCardsWithPlayer <- 
        sum(c(probTable[view(thePlayer), hold(thePlayer), cards]))
    doesRootExist <- (setCardsWithPlayer >= 1)
    return (doesRootExist)
}

finishPossibleSets <- function(thePlayer){

    # Process Only if the player has some cards with him
    playerIndex <- which(playerNames == thePlayer)
    if(cardsWithPlayer[playerIndex] == FALSE)
        return ()

    # When the chance comes, check if any set can be finished
    setFinishes <- sapply(setIndices, canFinishSet, thePlayer)
    finishableSets <- which(setFinishes == TRUE)
    finishPossible <- (length(finishableSets) > 0)

    if (finishPossible){
        for (theSet in finishableSets){
            cards <- setCards(theSet)
            giverCardCombo <- which(probTable[view(thePlayer),,cards]==1)
            for (ac in giverCardCombo){
                cardInSet <- (ac - 1) %/% 6 + 1       # Quotient
                giverIndex <- (ac - 1) %% 6 + 1       # Reminder
                theCard <- cards[cardInSet] 
                giver <- playerNames[giverIndex]
                
                if (thePlayer == giver)
                    next  
                    
                transaction(thePlayer, giver, theCard)
            }
            print(paste(thePlayer, "finishes the set", setToName(theSet)))
            doFinishingFormalities(thePlayer, theSet)
            setName <- setToName(theSet)
            theTeam <- teamNames[(playerIndex - 1) %% 2 + 1]
            scoreBoard[setName] <<- thePlayer
            teamBoard[setName] <<- theTeam
            print(scoreBoard)
            print(teamBoard)
        }
    }
}

handleEmptyHand <- function (thePlayer){
    playerIndex <- which(playerNames == thePlayer)
    playerCards <- cardsAndPlayers[[playerIndex]]
    # Update if the cards are done for this player
    if(length(playerCards) == 0){
        cardsWithPlayer[playerIndex] <<- FALSE
        
        probTable[viewerNames, hold(thePlayer), ] <<- 0
        # update probabilites for all cards for all viewers
        for (theCard in cardNums){
            sapply(viewerNames, updateProb, theCard)
        }
    }
}

doFinishingFormalities <- function(thePlayer, theSet){
    theCards <- setCards(theSet)
    playerIndex <- which(playerNames == thePlayer)
    playerCards <- cardsAndPlayers[[playerIndex]]
    
    updatedCards <- setdiff(playerCards, theCards)
    # Removing the cards of finished set from the player
    cardsAndPlayers[[playerIndex]] <<- updatedCards
    
    # Update the probability table:  Assign zero probability for these cards
    probTable[,,theCards] <<- 0
    # update probabilites for these cards for all viewers
    for (theCard in theCards){
        sapply(viewerNames, updateProb, theCard)
    }
    
    handleEmptyHand(thePlayer)
    
    print(paste0("Finished Set: ", theSet))
}

checkForPassOn <- function(thePlayer){
    # Gets Player Name and Returns Player Index 
    # Returns 0 if no more players are having cards 
    playerIndex <- which(playerNames == thePlayer)
    if (cardsWithPlayer[playerIndex]){
        return (playerIndex)
    } else {
        nextPlayerIndex <- findPlayerWithCards(playerIndex)
        return (nextPlayerIndex)
    }
}

findPlayerWithCards <- function(playerIndex){
    # See if team players are available
    teamPlayers <- sameTeam(playerIndex)
    
    # If one team (all 3 players) are left with no cards, game over!
    validPlayers <- which(cardsWithPlayer[teamPlayers] == TRUE)
    if(length(validPlayers) == 0){
        # No more team player has the cards!
        return (0)
    } else {
        nextPlayer <- teamPlayers[validPlayers[1]]
        return (nextPlayer)
    }
}

pickMax <- function (inArray){
    # pick the max value other than 1
    inArray[which(inArray == 1)] <- -1      # Assign lowest value to P = 1
    return (max (inArray))
}

gameGo <- function(curPlayer){
    print ("==================")
    print (paste0("      Round:", sprintf("%4s", roundNum), "  "))
    print ("==================")
    roundNum <<- roundNum + 1
    
    thePlayer <- "NOTSET"
    
    while(TRUE){
        finishPossibleSets(curPlayer)
        # The player may be left with zero cards after finishing the set.
        # Check and if so, pass on the turn to other players of the same team.
        playerIndex <- checkForPassOn(curPlayer)
        if (playerIndex == 0){
            break # Game Over
        }
        thePlayer <- playerNames[playerIndex]
        
        if (curPlayer == thePlayer){
            # All Done; Finishable sets by the player finished
            # Still cards are remaining with him to play
            break
        } else {
            # No more cards with curPlayer, new player got assigned in 
            # thePlayer, we need finish off the sets for him.
            curPlayer <- thePlayer
        }
    }
    
    if (playerIndex == 0){
        print ("Game Over!")
        return ()
    }
    
    cardsToAsk <- askableCards(thePlayer)
    
    currentHolders <- setdiff(holderNames, 
        holderNames[c(playerIndex, sameTeam(playerIndex))])
    lenHolders <- length(currentHolders)    
    theViewer <- view(thePlayer)
    maxValue <- pickMax(probTable[theViewer, currentHolders, cardsToAsk])
    maxPosn <- 
        which(probTable[theViewer, currentHolders, cardsToAsk] == maxValue)
    lmax <- length(maxPosn)
    # can ask anyone randomly who all have same max probabilites.
    randomIndex <- sample(lmax, 1)

    if(!autoMode && playerIndex == 1){
        cardToAsk   <- as.integer(readline(prompt="Enter Card (1 to 48): "))
        playerNumToAsk <- as.integer(readline(prompt="Enter Player (2, 4, 6): "))
        playerToAsk <- playerNames[playerNumToAsk]
        # Do validations on whether that player can ask that card
    } else {
        cardToAsk <- cardsToAsk[(maxPosn - 1) %/% lenHolders  + 1][randomIndex]
        holderToAsk <- currentHolders[(maxPosn - 1) %% lenHolders + 1][randomIndex]
        playerToAsk <- substring(holderToAsk, 2)
    }
    
    
    nextPlayer <- transaction(thePlayer, playerToAsk, cardToAsk)

    printCardDetails()
    if(!autoMode)
        readline(prompt="Press [enter] to next round.")    
    gameGo(nextPlayer)
}

printCardDetails <- function(){
    if(autoMode){
        print(cardsAndPlayers)
        print(probTable[view(thePlayer),,])
    } else {
        print(cardsAndPlayers[1])
        print(sapply(cardsAndPlayers[[1]], cardToName))
        flush.console()
    }
}

################################################################################
# Game Starts Here
################################################################################

# Card Numbers will be 1 to 48
# Each player will get 8 cards randomly - taken care by 'sample' function
cardsAndPlayers <- split(sample(cardNums), factor(seq(numPlayers)))
# This is a list; Access by cardsAndPlayers[[1]]

# This holds info on whether there are cards existing with the player
cardsWithPlayer <- rep(TRUE, 6)


# Set the autoMode to TRUE, to run automatically.
autoMode <- FALSE
# When autoMode is FALSE, human player represents instead of Abel
if (!autoMode){
    humanName <- readline(prompt="Enter name: ")
    playerNames[1] <- humanName
}

# Probability Matrix for each card; as a viewer(v) guesses the holder (h)
# Proabibility is what the viewer assigns to each card for each holder
# For a viewer, sum of probabilities of a card across holders will be 1
# Probability Matrix :: Dimension - Viewer, Holder and Card

probTable <- array(0, c(numPlayers, numPlayers, numCards))
view <- function(x){paste0("v", x)}
hold <- function(x){paste0("h", x)}
viewerNames <- paste0("v", playerNames)
holderNames <- paste0("h", playerNames)
dimnames(probTable) <- list(viewerNames, holderNames, paste(cardNums))
# Initialize all to NA values
probTable[,,] <- NA

# update all initial probabilities using initProb function
sapply(playerNames, initProb)

startPlayerIndex <- sample(numPlayers, 1)
thePlayer <- playerNames[startPlayerIndex]
roundNum <- 1

printCardDetails()
# This starts of the game looping
gameGo(thePlayer)

