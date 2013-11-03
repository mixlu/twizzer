module MyServer where

import TwizzerTypes
import System.IO
import System.Environment
import Network
import Control.Concurrent
import Control.Concurrent.STM
import Data.List
import Data.List.Split
import Data.Time.Clock
import Data.Time.Calendar
import Data.Time.Format
import Data.CSV
import Data.String.Utils
import System.Directory
import System.Locale

port = 8000
accountFile     = "Accounts.txt"
twizLogFile     = "TwizLog.txt"
solutionFile    = "solution_twiz"
reviewFile      = "review_twiz"
scoreFile       = "scores.csv"

-- create a socket and listen on the port
start :: IO ()
start = do
    sock <- listenOn $ PortNumber port
    sockHandler sock

-- get the handle of the socket
-- the buffering is set as NoBuffering so that any message in the handle is sent to the client immediately, no delay
-- forkIO allows concurrent communication from multiple clients, and for any client, the procedure starts with the function serveServer
-- the socket is active forever
sockHandler :: Socket -> IO()
sockHandler sock = do
            (handle, _, _) <- accept sock
            hSetBuffering handle NoBuffering
            forkIO $ serveServer handle
            sockHandler sock

-- serve each client by asking for username and password first. If the client provides correct infomation, provide it an option menu.
serveServer :: Handle -> IO()
serveServer handle = do
                name <- hGetLine handle
                password <- hGetLine handle
                print (name ++ " : " ++ password)
                permission <- checkPassword name password
                print permission
                hPutStrLn handle $ show permission
                if permission == UserNotExist || permission == NotMatch then serveServer handle 
                    else commandProcessor handle name permission

-- keep receiving command from the client and do the corresponding task. 
commandProcessor :: Handle -> String -> CheckPwdResult-> IO()
commandProcessor handle name permission = do
                                        cmd <- hGetLine handle
                                        print ("got task " ++ cmd)
                                        doTask handle cmd name permission
                                        commandProcessor handle name permission

-- complete each specifit task required by the client
doTask :: Handle -> String -> String -> CheckPwdResult -> IO()
doTask h "getPhase"         _       _       =   do
                                                (i, p) <- getPhase
                                                hPutStrLn h (show p)
doTask h "changePwd"        name    _       =   do
                                                oldPwd <- hGetLine h
                                                permission <- checkPassword name oldPwd
                                                if permission == NotMatch then hPutStrLn h "username and password do not match"
                                                    else do
                                                        hPutStrLn h "Ready"
                                                        newPwd <- hGetLine h                                                                
                                                        changePwd name oldPwd newPwd
                                                        hPutStrLn h "Password changed."   
doTask h "addAccount"       _       ADMIN   =   do
                                                userName <- hGetLine h 
                                                pwd <- hGetLine h
                                                permission <- hGetLine h                                 
                                                p <- checkPassword userName pwd                                     
                                                if p /= UserNotExist then hPutStrLn h "username already used. " 
                                                else 
                                                    do
                                                    appendFile accountFile (userName ++ "\t" ++ pwd ++ "\t" ++ permission ++ "\n")
                                                    hPutStrLn h "New account added. "
doTask h "createTwiz"       _       ADMIN   =   do
                                                (i, p) <- getPhase
                                                let newTwizFile = formTwizFileName (i+1)
                                                hPutStrLn h ("You are creating " ++ newTwizFile)                                      
                                                saveFile h newTwizFile
                                                deadlines <- forceDeadlines
                                                appendFile twizLogFile ((show (i+1)) ++ "\t" ++ (head deadlines) ++ "\t" ++ (last deadlines) ++ "\n")
                                                print (newTwizFile ++ " created.")                                        
                                                hPutStrLn h "New twiz created."
doTask h "getTwiz"          _       _       =   do
                                                (i, p) <- getPhase
                                                sendFile h (formTwizFileName i)
                                                hPutStrLn h "Twiz saved."
doTask h "submitSolution"   name    STUDENT =   do
                                                (i, p) <- getPhase
                                                if p == Task then do
                                                                    hPutStrLn h "Ready"
                                                                    saveFile h (formSolFileName i name)
                                                                    print "solution saved. "
                                                                    hPutStrLn h "Solution submitted. "
                                                    else do
                                                        hPutStrLn h "Illegal to submit solutions."
doTask h "readPeerSol"      name    STUDENT =   do
                                                (i, p) <- getPhase
                                                if p == Review then do
                                                                    hPutStrLn h "Ready"
                                                                    peers <- getPeers name      
                                                                    print ("sending solutions of " ++ (show peers) ++ " to " ++ name)
                                                                    temp1 <- getMatchedFiles (formSolFileName i (fst peers))
                                                                    temp2 <- getMatchedFiles (formSolFileName i (snd peers))
                                                                    let solFiles = temp1 ++ temp2
                                                                    hPutStrLn h (show $ length solFiles)
                                                                    sendFiles h solFiles
                                                                    print "Your peers' sols are sent. "   
                                                                    if solFiles == [] then hPutStrLn h "No reviews submitted. "
                                                                        else hPutStrLn h ((show $ length solFiles) ++ " peers' solutions sent. ")
                                                    else do
                                                        hPutStrLn h "Illegal to review peers' solutions."
doTask h "submitReview"     name    STUDENT =   do
                                                (i, p) <- getPhase
                                                if p == Review then do
                                                                    hPutStrLn h "Ready"
                                                                    peers <- getPeers name
                                                                    print ("receiving reviews from " ++ name ++ " to " ++ (show peers))
                                                                    hPutStrLn h (fst peers)
                                                                    let revFile = formRevFileName i name (fst peers)
                                                                    saveFile h revFile  
                                                                    hPutStrLn h (snd peers)
                                                                    let revFile = formRevFileName i name (snd peers)
                                                                    saveFile h revFile       
                                                                    print "Your reviews are saved. "
                                                                    hPutStrLn h "Reviews submitted. "
                                                    else do
                                                        hPutStrLn h "Illegal to submit reviews. "
doTask h "readPeerRev"      name    STUDENT =   do
                                                (i, p) <- getPhase
                                                if p == Idle then do
                                                                    hPutStrLn h "Ready"
                                                                    reviewers <- getReviewers name
                                                                    print ("sending reviews from " ++ (show reviewers) ++ " to " ++ name) 
                                                                    temp1 <- getMatchedFiles (formRevFileName i (fst reviewers) name)
                                                                    temp2 <- getMatchedFiles (formRevFileName i (snd reviewers) name)
                                                                    let revFiles = temp1 ++ temp2
                                                                    hPutStrLn h (show $ length revFiles)
                                                                    sendFiles h revFiles
                                                                    print "Reviews to you are sent. "
                                                                    if revFiles == [] then hPutStrLn h "No reviews submitted for you to review."
                                                                        else hPutStrLn h ((show $ length revFiles) ++ " peers' reviews sent.")
                                                    else do
                                                        hPutStrLn h "Illegal for you to read reviews."
doTask h "readAllSol"       _       ADMIN   =   do
                                                (i, p) <- getPhase
                                                allSolFiles <- getMatchedFiles (solutionFile++ (show i) ++ "_")
                                                print allSolFiles
                                                hPutStrLn h (show $ length allSolFiles)
                                                hPutStrLn h (formTwizFileName i)
                                                sendFiles h allSolFiles
                                                if allSolFiles == [] then hPutStrLn h "No solutions submitted. "
                                                    else hPutStrLn h "All solutions are sent. "
doTask h "readAllRev"       _       ADMIN   =   do
                                                (i, p) <- getPhase
                                                allRevFiles <- getMatchedFiles (reviewFile ++ (show i) ++ "_")
                                                print allRevFiles
                                                hPutStrLn h (show $ length allRevFiles)
                                                hPutStrLn h (formTwizFileName i)
                                                sendFiles h allRevFiles
                                                if allRevFiles == [] then hPutStrLn h "No reviews submitted. "
                                                    else hPutStrLn h "All reviews are sent."
doTask h "calculateScores"  _       ADMIN   =   do
                                                (i, p) <- getPhase
                                                calSaveScores i
                                                sendFile h scoreFile
                                                print "score file sent"
                                                hPutStrLn h "Scores calculated and saved."

-- save the incoming messages into destFile
saveFile :: Handle -> String -> IO()
saveFile handle destFile = do
                        message <- hGetLine handle
                        if message /= "$$$FileEnd$$$" 
                            then do
                                appendFile destFile (message ++ "\t\n")
                                saveFile handle destFile
                            else do
                                return ()

-- send a single one file as sending messages. The file name is delivered first. An ending symbol is sent at last.
sendFile :: Handle -> String -> IO()
sendFile handle sourceFile = do
                        hPutStrLn handle sourceFile -- send File name First
                        (readFile sourceFile) >>= hPutStrLn handle >> hFlush handle -- send file contents
                        hPutStrLn handle "$$$FileEnd$$$" -- send ending symbols

-- send multiple files one by one
sendFiles :: Handle -> [String] -> IO()
sendFiles _ []         = return ()
sendFiles handle (f:fs)     = do
                        sendFile handle f
                        sendFiles handle fs
 
-- whose solutions stu is going to review                       
getPeers :: String -> IO (String, String)
getPeers stu = do
            (i, p) <- getPhase
            studentNames <- getStudentNames
            let n       = length studentNames
            let nameIdx = getIndex stu studentNames
            let twizIdx = mod i (n - 1)
            let first   = mod (nameIdx + twizIdx + 1) n
            let second  = mod (first + 1) n
            return ((studentNames !! first), (studentNames !! second))
                     
-- who are reviewing the solution from stu
getReviewers :: String -> IO (String, String)
getReviewers stu = do
                (i, p) <- getPhase
                studentNames <- getStudentNames
                let n = length studentNames
                let nameIdx = getIndex stu studentNames
                let twizIdx = mod i n
                let first   = mod (nameIdx - i - 2) n
                let second  = mod (nameIdx - i - 1) n
                return ((studentNames !! first), (studentNames !! second))

-- get the index of name in the list names. Similar to elemIndex, but return Int rather than Maybe Int.
getIndex :: String -> [String] -> Int
getIndex name [] = error "no student in the list"
getIndex name names@(x:xs) = if (not $ elem name names) then error "cant find the student name"
                                else if x==name then 0 
                                else (1 + getIndex name xs)

-- get the list of files in current directory that begin with the argument str
getMatchedFiles :: String -> IO [String]
getMatchedFiles str = do
                    files <- getDirectoryContents "."                
                    return $ filter (isPrefixOf str) files  

-- get the list of all student names from Account.txt
getStudentNames :: IO [String]
getStudentNames = do
                accountsInfo <- readFile accountFile
                return $ map (head . (splitOn "\t")) (drop 2 $ lines accountsInfo)

-- return the current active twize index and phase
getPhase :: IO (Int, Phase)
getPhase = do
        info <- readFile twizLogFile
        if (length (lines info) == 1) then return (-1, Idle) -- no twiz is ever created yet
            else do
                    let line = (splitOn "\t") $ last $ lines info
                    let i = read (line !! 0) :: Int
                    let solDue = extractDeadline (line !! 1)
                    let reviewDue = extractDeadline (line !! 2)
                    cur <- getCurrentTime
                    if cur >= reviewDue then return (i, Idle)
                        else if cur >= solDue then return (i, Review)
                            else return (i, Task)

-- read a UTC time from a formatted string
extractDeadline :: String -> UTCTime
extractDeadline date = case result of
                             (Just x) -> x
                             _  -> error ("deadline format error!!! - " ++ date)
                        where
                            result = parseTime defaultTimeLocale "%c" date

-- force the following noon of current time to be the deadline for soluiton, and the next noon to be the deadline for reviews.
forceDeadlines :: IO [String]
forceDeadlines = do
                cur <- getCurrentTime
                let (year, month, day) = toGregorian (utctDay cur)
                let deadlines = [formeUTCdeadline year month (day+1), formeUTCdeadline year month (day+2)]              
                return $ map (formatTime defaultTimeLocale "%c") deadlines
-- format the date as a string for writing to the TwizLog file. 19 makes the time to be 19pm in UTC time, which is 12pm (noon) in PST.
formeUTCdeadline y m d = UTCTime (fromGregorian y m d) (fromIntegral $ 19 * 3600) 

-- calculate the score for each student of twiz-0 to twiz-i, save the result to scoreFile. 
calSaveScores ::  Int -> IO()
calSaveScores i = do
                students <- getStudentNames
                result <- calAllScores i students
                let header = (getTwizHeader i) ++ ["sum"]
                let output = genCsvFile (header:result)
                writeFile scoreFile output

-- calculate the score for all students of twiz-0 to twiz-i, format the result as a csv file
calAllScores :: Int -> [String] -> IO [[String]]
calAllScores _ []         = return [[""]]
calAllScores i (stu:stus) = do
                            score <- calScore stu i
                            let oneSum = sum score
                            scores <- calAllScores i stus
                            let oneLine = [stu] ++ (map show score) ++ [show oneSum]
                            return $ oneLine:scores

-- calculate the score of stu for twiz-0 to twiz-i
calScore :: String -> Int -> IO [Int]
calScore stu 0 = do                    
                    score <- checkTwizI stu 0
                    return $ score
calScore stu i = do                    
                    oneScore <- checkTwizI stu i
                    scores <- calScore stu (i-1)
                    return $ scores ++ oneScore

-- check the score of stu for twiz i
checkTwizI :: String -> Int -> IO [Int]
checkTwizI stu i = do
                    solFiles <- getMatchedFiles (solutionFile ++ (show i) ++ "_"++ stu)
                    revFiles <- getMatchedFiles (reviewFile ++ (show i) ++ "_"++ stu)
                    return $ map length [solFiles, revFiles]

-- the header line that is used in the scores.csv file
getTwizHeader :: Int -> [String]
getTwizHeader 0 = ["", ("solution" ++ (show 0)), ("review" ++ (show 0))]
getTwizHeader i = (getTwizHeader (i-1)) ++ [("solution" ++ (show i)), ("review" ++ (show i))]

-- if username and password match, return the permission; then if the username exists return NotMatch; else return UserNotExist.
checkPassword :: String -> String -> IO CheckPwdResult
checkPassword username password = do
                            fHandle <- openFile accountFile ReadMode
                            accountInfo <- hGetContents fHandle 
                            print $ length accountInfo 
                            hClose fHandle
                            let line = "\n" ++ username ++ "\t" ++ password ++ "\t"           
                            if (line ++ "0") `isInfixOf` accountInfo then return STUDENT
                                else if (line ++ "1") `isInfixOf` accountInfo then return ADMIN
                                    else if ("\n" ++ username ++ "\t") `isInfixOf` accountInfo then return NotMatch 
                                        else return UserNotExist                                                        

-- update password in Account.txt
changePwd :: String -> String -> String -> IO()
changePwd name oldPwd newPwd   = do
                            renameFile accountFile "tempAccount.txt"
                            contents <- readFile "tempAccount.txt"
                            let newContents = replace (name ++ "\t" ++ oldPwd) (name ++ "\t" ++ newPwd) contents
                            writeFile accountFile newContents
                            removeFile "tempAccount.txt"
                            print "password changed"

-- utility functions to get certain file names (string). They are called by multiple functions.
formTwizFileName :: Int -> String
formTwizFileName i = "Twiz" ++ (show i) ++ ".txt"

formSolFileName :: Int -> String -> String
formSolFileName i name = solutionFile ++ (show i) ++ "_"++ name ++ ".hs"

formRevFileName :: Int -> String -> String -> String
formRevFileName i name peer = reviewFile  ++ (show i) ++ "_" ++ name ++ "_" ++ peer ++".txt"

-- end of the file.

