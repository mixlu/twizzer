module TwizzerTypes where

data Phase = Idle | Task | Review deriving (Show, Eq)
data CheckPwdResult = STUDENT | ADMIN | NotMatch | UserNotExist deriving (Show, Eq)

readCheckPwdResult :: String -> CheckPwdResult
readCheckPwdResult str = case str of 
							"STUDENT" 		-> STUDENT
							"ADMIN"			-> ADMIN
							"NotMatch"		-> NotMatch
							"UserNotExist" 	-> UserNotExist
