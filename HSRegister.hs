import System
import System.Time
import System.Locale
import System.Console.GetOpt
import Directory
import Control.Monad
import Database.HDBC
import Database.HDBC.Sqlite3
import Data.Maybe
import IO
import List
import Char

--Program Settings
versionString :: String
versionString = "HSRegister - Banking Register: Version 0.1"

headerString :: String
headerString = "Usage: ic [OPTION...] files..."

dbLocation :: FilePath
dbLocation = "~/.hsr.db"

timeFormat :: String
timeFormat = "%Y-%m-%d %H:%M:%S"

main = do
  --Get cmd line args
  args <- getArgs
  --Parse them
  let (actions, nonopts, errs) = getOpt RequireOrder options args
  --Set defaults to non-set opts
  opts <- foldl (>>=) (return defaultOptions) actions
  --Set variables to given/default values
  let Options { optAccount  = account
              , optAmount   = amount
              , optCheckNum = checkNum
              , optDesc     = description
              , optHelp     = help
              , optInit     = initialize
              , optTrans    = transaction
              , optView     = view
              , optVersion  = version
              } = opts

  --Do Stuff with the arguments
  --On Errors
  unless (null errs) $ showHelp' errs

  --On --help (-h)
  when help showHelp

  --Print Version String
  when version $ hPutStrLn stderr versionString

  --Initialize Database
  when initialize $ do
    continue <- confirmSetup
    when continue (dbSetup)

  --Make a transaction
  unless (isNothing amount) $ makeTransaction account (setSignOnTrans transaction (fromJust amount)) checkNum description

  --View account
  when view $ viewAccount account

--These keep things inline
data Transaction = Deposit | Withdrawl deriving (Eq, Show)
data Account = Checking | Savings | Credit deriving (Eq, Show)

--Options that can be set by cmd line
data Options = Options
  { optAccount  :: Account
  , optAmount   :: Maybe Double
  , optCheckNum :: Maybe Int
  , optDesc     :: Maybe String
  , optHelp     :: Bool
  , optInit     :: Bool
  , optTrans    :: Transaction
  , optView     :: Bool
  , optVersion  :: Bool
  } deriving (Show)

--The default options for arguments not used
defaultOptions = Options
  { optAccount  = Checking
  , optAmount   = Nothing
  , optCheckNum = Nothing
  , optDesc     = Nothing
  , optHelp     = False
  , optInit     = False
  , optTrans    = Withdrawl
  , optView     = False
  , optVersion  = False
  }

--This is the argument parser
options :: [OptDescr (Options -> IO Options)]
options = [ Option [] ["init","initialize"]
              (NoArg (\opts -> return opts {optInit = True}))
              "Initilize the database (deletes old, if exists)"
          , Option ['a']  ["amount"]
              (ReqArg (\num opts -> return opts {optAmount = moneyAsDbl num}) "VALUE")
              "The amount of money in the transaction (required to make a transaction)"
          , Option ['C'] ["credit"]
              (NoArg (\opts -> return opts {optAccount = Credit}))
              "Use credit account"
          , Option ['c'] ["checking"]
              (NoArg (\opts -> return opts {optAccount = Checking}))
              "Use checking account (Default)"
          , Option ['D'] ["deposit"]
              (NoArg (\opts -> return opts {optTrans = Deposit}))
              "Sets the transaction to be a deposit"
          , Option ['d'] ["description"]
              (ReqArg (\desc opts -> return opts {optDesc = Just desc}) "STRING")
              "Adds a description to the transaction"
          , Option ['h'] ["help"]
              (NoArg (\opts -> return opts {optHelp = True}))
              "Displays this help output"
          , Option ['k'] ["check"]
              (ReqArg (\num opts -> return opts {optCheckNum = checkAsInt num, optTrans = Withdrawl}) "VALUE")
              "Sets the check number used, sets the transaction type to withdrawl"
          , Option ['s'] ["savings"]
              (NoArg (\opts -> return opts {optAccount = Savings}))
              "Use savings account"
          , Option ['V'] ["version"]
              (NoArg (\opts -> return opts {optVersion = True}))
              "Display Version"
          , Option ['v'] ["view"]
              (NoArg (\opts -> return opts {optView = True}))
              "Display selected account"
          , Option ['W'] ["withdrawl"]
              (NoArg (\opts -> return opts {optTrans = Withdrawl}))
              "Sets the transaction to be a withdrawl (Default)"
          ]


showHelp' errors = ioError (userError (concat errors ++ usageInfo headerString options))
showHelp = hPutStrLn stdout (usageInfo headerString options)

setSignOnTrans :: Transaction -> Double -> Double
setSignOnTrans trans amount = if trans == Withdrawl then negate amount else amount

confirmSetup :: IO Bool
confirmSetup = do
  hPutStrLn stderr "This will ERASE the previous database if it exists!"
  hPutStrLn stderr "Are you sure you want to (re)create the database? (Y/n) "
  response <- getLine
  case response of
    "Y"   -> return True
    "yes" -> return True
    "Yes" -> return True
    otherwise -> return False

dbSetup :: IO()
dbSetup = do
  fExist <- (doesFileExist dbLocation)
  when fExist (removeFile dbLocation)
  conn <- connectSqlite3 dbLocation
  run conn "CREATE TABLE checking (id integer primary key, description text default null, amount integer not null, checkNum integer default null, date text not null)" []
  run conn "CREATE TABLE savings (id integer primary key, description text default null, amount integer not null, checkNum integer default null, date text not null)" []
  run conn "CREATE TABLE credit (id integer primary key, description text default null, amount integer not null, checkNum integer default null, date text not null)" []
  commit conn
  disconnect conn

makeTransaction :: Account -> Double -> Maybe Int -> Maybe String -> IO ()
makeTransaction ac am cn desc = do
  calendar <- getClockTime >>= toCalendarTime
  conn <- connectSqlite3 dbLocation
  case ac of
    Checking -> run conn "INSERT INTO checking (description, amount, checkNum, date) VALUES (?,?,?,?)" [toSql desc, toSql am, toSql cn, toSql (formatCalendarTime defaultTimeLocale timeFormat calendar)]
    Savings -> run conn "INSERT INTO savings (description, amount, checkNum, date) VALUES (?,?,?,?)" [toSql desc, toSql am, toSql cn, toSql (formatCalendarTime defaultTimeLocale timeFormat calendar)]
    Credit -> run conn "INSERT INTO credit (description, amount, checkNum, date) VALUES (?,?,?,?)" [toSql desc, toSql am, toSql cn, toSql (formatCalendarTime defaultTimeLocale timeFormat calendar)]
  commit conn
  disconnect conn

viewAccount :: Account -> IO ()
viewAccount ac = do
  conn <- connectSqlite3 dbLocation
  result <- case ac of
    Checking -> quickQuery' conn "SELECT * from checking" []
    Savings -> quickQuery' conn "SELECT * from savings" []
    Credit -> quickQuery' conn "SELECT * from credit" []
  let resultRows = map convertRow result
  mapM_ putStrLn resultRows
  disconnect conn

--Used to take the option to the "-a" argument
moneyAsDbl :: [Char] -> Maybe Double
moneyAsDbl str
  | value == [] = Nothing
  | otherwise   = Just . abs $ fst $ head value
  where value = reads str :: [(Double, String)]

--Used to take the option to the "-c" argument
checkAsInt :: [Char] -> Maybe Int
checkAsInt str
  | value == [] = Nothing
  | otherwise   = Just . abs $ fst $ head value
  where value = reads str :: [(Int, String)]

convertRow :: [SqlValue] -> String
convertRow [sqlId, sqlDesc, sqlAmt, sqlChk, sqlDate] = 
  show intid ++ " | " ++ desc ++ " | " ++ amt ++ " | " ++ chk ++ " | " ++ date
    where intid = (fromSql sqlId) :: String
          desc  = case fromSql sqlDesc of
                    Just x  -> x
                    Nothing -> "NULL"
          amt   = (fromSql sqlAmt) :: String
          chk   = case fromSql sqlChk of
                    Just x  -> x
                    Nothing -> "NULL"
          date  = (fromSql sqlDate) :: String
convertRow x = fail $ "Unexpected result: " ++ show x
