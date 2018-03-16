module Lab41 where 

data Decision = Learn | Laze | Erasmus deriving Show

data Node = Dec Node Node Node | Failure | Success deriving Show

data Outcome = Win | Loss | ToBeContinued deriving Show

-- 1
setSuccess (Dec learn laze erasmus) (d:dx) =
    match d
    where 
        match Learn = setSuccess learn dx
        match Laze =  setSuccess laze dx
        match _ = setSuccess erasmus dx

setSuccess (Failure) (d:dx) =
    createNode d dx

setSuccess (Success) (d:dx) =
    createNode d dx

setSuccess _ [] =
    Success
    
createNode Learn dx =
    Dec (setSuccess Success dx) Failure Failure

createNode Laze dx =
    Dec Failure (setSuccess Success dx) Failure

createNode _ dx =
    Dec Failure Failure (setSuccess Success dx)

-- 2
strategyOutcome (Dec learn laze erasmus) (d:dx) =
    process d
    where
        process Learn = strategyOutcome learn dx
        process Laze = strategyOutcome laze dx
        process Erasmus = strategyOutcome erasmus dx

strategyOutcome Success _ =
    Success

strategyOutcome Failure _ =
    Failure

stategyOutcome _ _ =
    ToBeContinued


-- 3
instance Eq Node where
    Success == Success = True
    Failure == Failure = True
    (Dec learn laze erasmus) == (Dec learn2 laze2 erasmus2) = 
        learn == learn2 && laze == laze2 && erasmus == erasmus2
    _ == _ = False


-- 5
infiniteTree = 
    Dec (infiniteTree Failure Failure)
            

