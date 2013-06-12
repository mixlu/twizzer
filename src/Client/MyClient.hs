module MyClient where

import TwizzerTypes
import Network
import Network.Socket
import System.IO
import System.Environment
import Control.Concurrent
import Control.Concurrent.STM
import System.Exit

port = 8000
-- hostname = "127.0.0.1"  -- 128.193.36.18"

-- connect to the server
-- the buffering is set as NoBuffering so that any message in the handle is sent to the client immediately, no delay
-- start the service with function serveClient
start :: IO ()
start = do
    putStrLn "Enter the host IP:"
    hostname <- getLine
    putStrLn "Using default port number 8000"
    handle <- connectTo hostname (PortNumber port)
    hSetBuffering handle NoBuffering
    serveClient handle

-- serve the client by asking for username and password first. 
-- If the server approves, to to optionMenu. Othewise, keep asking for correct info.
serveClient :: Handle -> IO ()
serveClient handle = do
    putStrLn "Enter Your Name: "
    getLine >>= hPutStrLn handle >> hFlush handle
    putStrLn "Enter your password: "
    getLine >>= hPutStrLn handle >> hFlush handle
    message <- hGetLine handle
    let permission = readCheckPwdResult message
    --putStrLn ("Your permissionLevel is " ++ show permission)    
    optionMenu handle permission

-- for different user, ask the server about the phase, then provide corresponding option menu
optionMenu :: Handle -> CheckPwdResult -> IO()
optionMenu h STUDENT      = do
                            hPutStrLn h "getPhase"
                            phase <- hGetLine h
                            case phase of 
                                "Idle"      -> do
                                            printStuMenuIdle
                                            option <- getStuIdleOpt
                                            selectStudentFunction h option
                                "Task"   -> do
                                            printStuMenuTask
                                            option <- getStuTaskOpt 
                                            selectStudentFunction h option    
                                "Review" -> do
                                            printStuMenuReview
                                            option <- getStuReviewOpt
                                            selectStudentFunction h option
                            optionMenu h STUDENT
optionMenu h ADMIN        = do
                            printAdminMenu
                            option <- getLine
                            selectAdminFunction h option
                            optionMenu h ADMIN
optionMenu h NotMatch     = do
                            putStrLn "username and password do not match!"
                            serveClient h     
optionMenu h UserNotExist = do
                            putStrLn "username can not be found!"
                            serveClient h                     

-- serve the student with all kinds of options
selectStudentFunction :: Handle -> String -> IO()
selectStudentFunction h "changePwd"      = do
                                        hPutStrLn h "changePwd"                                     
                                        putStrLn "Enter your old password: "
                                        oldPwd <- getLine
                                        hPutStrLn h oldPwd
                                        message <- hGetLine h
                                        if message == "Ready" then do                                        
                                                                putStrLn "Enter your new password: "
                                                                newPwd <- getLine
                                                                hPutStrLn h newPwd
                                                                message <- hGetLine h
                                                                putStrLn message
                                            else putStrLn message
selectStudentFunction h "getTwiz"        = do 
                                        hPutStrLn h "getTwiz"       
                                        receiveFile h
                                        message <- hGetLine h
                                        putStrLn message
selectStudentFunction h "submitSolution" = do                                        
                                        hPutStrLn h "submitSolution"
                                        message <- hGetLine h                                        
                                        if message == "Ready" then do
                                                                putStrLn "What's the file name of your solution:"
                                                                solFileName <- getLine
                                                                sendFile h solFileName
                                                                message <- hGetLine h
                                                                putStrLn message
                                            else putStrLn message
selectStudentFunction h "readPeerSol"    = do                                        
                                        hPutStrLn h "readPeerSol"
                                        message <- hGetLine h
                                        if message == "Ready" then do
                                                                fileNum <- hGetLine h
                                                                let n = read fileNum:: Int
                                                                receiveNFiles h n
                                                                message <- hGetLine h
                                                                putStrLn message
                                            else do
                                                putStrLn message
selectStudentFunction h "submitReview"   = do
                                        hPutStrLn h "submitReview"
                                        message <- hGetLine h
                                        if message == "Ready" then do
                                                                peer <- hGetLine h
                                                                putStrLn ("What's the file name of your review to " ++ peer)
                                                                myReviewFileName <- getLine
                                                                sendFile h myReviewFileName
                                                                peer <- hGetLine h
                                                                putStrLn ("What's the file name of your review to " ++ peer)
                                                                myReviewFileName <- getLine
                                                                sendFile h myReviewFileName
                                                                message <- hGetLine h
                                                                putStrLn message
                                            else do
                                                putStrLn message
selectStudentFunction h "readPeerRev"    = do
                                        hPutStrLn h "readPeerRev"
                                        message <- hGetLine h
                                        if message == "Ready" then do
                                                                fileNum <- hGetLine h
                                                                let n = read fileNum:: Int
                                                                receiveNFiles h n
                                                                message <- hGetLine h
                                                                putStrLn message                                                                
                                            else do
                                                putStrLn message
selectStudentFunction h "exit"           = do
                                        putStrLn "exit"
                                        exitWith ExitSuccess                        
selectStudentFunction _ option           = putStrLn (option ++ "is an invalid selection.")

-- serve the admin with all kinds of options
selectAdminFunction :: Handle -> String -> IO()
selectAdminFunction h "0" = do
                            hPutStrLn h "changePwd"                                     
                            putStrLn "Enter your old password: "
                            oldPwd <- getLine
                            hPutStrLn h oldPwd
                            message <- hGetLine h
                            if message == "Ready" then do                                        
                                                    putStrLn "Enter your new password: "
                                                    newPwd <- getLine
                                                    hPutStrLn h newPwd
                                                    message <- hGetLine h
                                                    putStrLn message
                                else putStrLn message
selectAdminFunction h "1" = do
                            hPutStrLn h "addAccount"
                            putStrLn "Enter the new username: "
                            username <- getLine
                            putStrLn "The initial password is set as 1234: "
                            putStrLn "Enter the permissionLevel, 0 (student) or 1 (Admin)"
                            permission <- getLine 
                            hPutStrLn h username
                            hPutStrLn h "1234"
                            hPutStrLn h permission
                            message <- hGetLine h
                            putStrLn message
selectAdminFunction h "2" = do
                            hPutStrLn h "createTwiz"
                            message <- hGetLine h
                            putStrLn message
                            putStrLn "Enter the file name of the twiz source: "
                            twizFileName <- getLine
                            sendFile h twizFileName
                            message <- hGetLine h
                            putStrLn message
selectAdminFunction h "3" = do                            
                            hPutStrLn h "getTwiz"                            
                            receiveFile h
                            message <- hGetLine h
                            putStrLn message
selectAdminFunction h "4" = do
                            hPutStrLn h "readAllSol"
                            fileNum <- hGetLine h
                            let n = read fileNum:: Int
                            print n
                            twizFileName <- hGetLine h
                            receiveFilesToOne h n ("AllSolutions_" ++ twizFileName)                                                          
                            message <- hGetLine h
                            putStrLn message
selectAdminFunction h "5" = do
                            hPutStrLn h "readAllRev"    
                            fileNum <- hGetLine h
                            let n = read fileNum:: Int
                            twizFileName <- hGetLine h
                            receiveFilesToOne h n ("AllReviews_" ++ twizFileName)
                            message <- hGetLine h
                            putStrLn message
selectAdminFunction h "6" = do
                            hPutStrLn h "calculateScores"                            
                            receiveFile h
                            message <- hGetLine h
                            putStrLn message
selectAdminFunction h "7" = do
                            putStrLn "exit"
                            exitWith ExitSuccess
selectAdminFunction _ _   = putStrLn "Invalid selection"


-- send a single one file as sending messages. Then ending symbol is sent at last.
sendFile :: Handle -> String -> IO ()
sendFile handle filename = do                                          
                        (readFile filename) >>= hPutStrLn handle >> hFlush handle
                        hPutStrLn handle "$$$FileEnd$$$"

-- receive and save n files one by one, save each as the same file name as it was on the server     
receiveNFiles :: Handle -> Int -> IO()
receiveNFiles handle n = if n <= 0 then return () 
                            else do
                                receiveFile handle
                                receiveNFiles handle (n-1)

-- receive and save a single file, save it as the same file name as it was on the server; the first message is the file name
receiveFile :: Handle -> IO()
receiveFile handle = do
                    destFile <- hGetLine handle
                    saveFile handle destFile

-- save incoming messages into the destFile until an ending line is received
saveFile :: Handle -> String -> IO()
saveFile handle destFile = do
                        message <- hGetLine handle  
                        if message /= "$$$FileEnd$$$" 
                            then do
                                appendFile destFile (message ++ "\t\n")
                                saveFile handle destFile
                            else do
                                return ()

-- receive n files one by one, then save all files into one single file
receiveFilesToOne :: Handle -> Int -> String -> IO()
receiveFilesToOne handle n destFile = if n <= 0 then return ()
                                            else do
                                                saveFile handle destFile
                                                receiveFilesToOne handle (n-1) destFile

-- functions to print out a menu to a user at a certain phase 
printAdminMenu :: IO()
printAdminMenu   = do
                putStrLn "\nAs an admin, select one option: "
                putStrLn "-0- ChangePassword"
                putStrLn "-1- Add an account"
                putStrLn "-2- CreateTwiz"
                putStrLn "-3- GetTwiz"
                putStrLn "-4- ReadAllSolution"
                putStrLn "-5- ReadAllReview"
                putStrLn "-6- CalculateScore"
                putStrLn "-7- Exit"
printStuMenuIdle :: IO()
printStuMenuIdle = do
                putStrLn "\nAs a student, select one option: "
                putStrLn "-0- ChangePassword"
                putStrLn "-1- GetTwiz"
                putStrLn "-2- ReadReview"
                putStrLn "-3- Exit"
printStuMenuTask :: IO()
printStuMenuTask = do
                putStrLn "\nAs a student, select one option: "
                putStrLn "-0- ChangePassword"
                putStrLn "-1- GetTwiz"
                putStrLn "-2- SubmitSolution"
                putStrLn "-3- Exit"
printStuMenuReview :: IO()
printStuMenuReview = do
                putStrLn "\nAs a student, select one option: "
                putStrLn "-0- ChangePassword"
                putStrLn "-1- GetTwiz"
                putStrLn "-2- ReadSolution"
                putStrLn "-3- SubmitReview"
                putStrLn "-4- Exit"

-- for a user at a certain phase, convert the input number into a task that is recognizable by the server
getStuIdleOpt :: IO String
getStuIdleOpt       = do
                    input <- getLine
                    return $ case input of 
                            "0" -> "changePwd"
                            "1" -> "getTwiz"
                            "2" -> "readPeerRev"
                            "3" -> "exit"
getStuTaskOpt :: IO String
getStuTaskOpt    = do
                    input <- getLine
                    return $ case input of 
                            "0" -> "changePwd"
                            "1" -> "getTwiz"
                            "2" -> "submitSolution"
                            "3" -> "exit"
getStuReviewOpt :: IO String
getStuReviewOpt  = do
                    input <- getLine
                    return $ case input of 
                            "0" -> "changePwd"
                            "1" -> "getTwiz"
                            "2" -> "readPeerSol"
                            "3" -> "submitReview"
                            "4" -> "exit"

