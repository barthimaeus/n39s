{-# LANGUAGE DeriveDataTypeable #-}
import Text.JSON
import Text.JSON.Generic
import Network.HTTP
import System.Process
import Control.Concurrent

data SpaceStatus = SpaceStatus {
	api :: String,
	space :: String,
	url :: String,
	address :: String,
	contact :: Contact,
	logo :: String,
    open :: Bool,
    lastchange :: Int,
    lat :: Int,
    lon :: Int
} deriving (Eq, Show, Data, Typeable)


data Contact = Contact {
	email :: String,
	twitter :: String,
	ml :: String,
	irc :: String
} deriving (Eq, Show, Data, Typeable)


main = pollSpace False

pollSpace oldStatus = 
	do
		result <- simpleHTTP (getRequest "http://spaceapi.n39.eu/json")
		case result of
			Left _ -> 
				do
					putStrLn "error"
			Right response -> 
				do
					let body = rspBody response
					let decoded = decodeJSON body :: SpaceStatus
					let newStatus = open decoded
					if  (newStatus /= oldStatus)
						then if newStatus
							then do
								(_,_,_, p) <- createProcess $ shell ("notify-send \""++((show . space) decoded)++ " is Open!\"")
								waitForProcess p
								return ()
							else do
								(_,_,_, p) <- createProcess $ shell ("notify-send \""++((show . space) decoded)++ " is Closed!\"")
								waitForProcess p
								return ()
						else do
							-- createProcess $ shell ("notify-send \"Nothing Changed.\"")
							return ()
					threadDelay 5000000
					pollSpace newStatus