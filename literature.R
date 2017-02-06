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
    
    print (paste(asker, "asks the card", theCard, cardName, "from", giver))
    askerIndex <- which(playerNames == asker)
    askerCards <- cardsAndPlayers[[askerIndex]]
    
    giverIndex <- which(playerNames == giver)
    giverCards <- cardsAndPlayers[[giverIndex]]
    
    txnDone <- theCard %in% giverCards
    
    if (txnDone){
        # Taking the card giver set and placing it in the asker set
        cardsAndPlayers[[askerIndex]] <<- c(askerCards, theCard)
        cardsAndPlayers[[giverIndex]] <<- 
            giverCards[-c(which(giverCards == theCard))]
            
        # Now, all know that the card was present with giver, now gone to asker
        # update probabilities accordingly
        probTable[view(playerNames), hold(asker), theCard] <<- 1
        nonHolders <- holderNames[which(playerNames != asker)]
        probTable[view(playerNames), nonHolders, theCard] <<- 0
        
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
    # When the chance comes, check if any set can be finished
    setFinishes <- sapply(setIndices, canFinishSet, thePlayer)
    finishableSets <- which(setFinishes == TRUE)
    print(finishableSets)
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
                
                transaction(thePlayer, giver, theCard)
            }
            print(paste(thePlayer, "finishes the set", setToName(theSet)))
        }
    }
}

# Card Numbers will be 1 to 48
# Each player will get 8 cards randomly - taken care by 'sample' function
cardsAndPlayers <- split(sample(cardNums), factor(seq(numPlayers)))
# This is a list; Access by cardsAndPlayers[[1]]

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


# Game Starts Here
startPlayerIndex <- sample(numPlayers, 1)
thePlayer <- playerNames[startPlayerIndex]

pickMax <- function (inArray){
    # pick the max value other than 1
    inArray[which(inArray == 1)] <- -1      # Assign lowest value to P = 1
    return (max (inArray))
}

gameGo <- function(thePlayer){
    finishPossibleSets(thePlayer)
    cardsToAsk <- askableCards(thePlayer)
    
    playerIndex <- which(playerNames == thePlayer)
    currentHolders <- setdiff(holderNames, 
        holderNames[c(playerIndex, sameTeam(playerIndex))])
    lenHolders <- length(currentHolders)    
    theViewer <- view(thePlayer)
    maxValue <- pickMax(probTable[theViewer, currentHolders, cardsToAsk])
    maxPosn <- which(probTable[theViewer, currentHolders, cardsToAsk] == maxValue)
    lmax <- length(maxPosn)
    # can ask anyone randomly who all have same max probabilites.
    randomIndex <- sample(lmax, 1)

    cardToAsk <- cardsToAsk[(maxPosn - 1) %/% lenHolders  + 1][randomIndex]
    holderToAsk <- currentHolders[(maxPosn - 1) %% lenHolders + 1][randomIndex]
    playerToAsk <- substring(holderToAsk, 2)
    nextPlayer <- transaction(thePlayer, playerToAsk, cardToAsk)

    print(cardsAndPlayers)
    print(probTable[,,cardToAsk])
    readline(prompt="Press [enter] to continue - Next Round!")
    gameGo(nextPlayer)
}
