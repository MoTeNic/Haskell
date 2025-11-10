{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TupleSections #-}


import Data.Monoid (Sum(..))
import Data.Char (toUpper)
import Data.List (isInfixOf)
import qualified Data.Map.Strict as Map



newtype WriterW w a = WriterW { runWriterW :: (a, w) }

instance Functor (WriterW w) where
  fmap f (WriterW (a,w)) = WriterW (f a, w)

instance Monoid w => Applicative (WriterW w) where
  pure a = WriterW (a, mempty)
  WriterW (f,w1) <*> WriterW (a,w2) = WriterW (f a, w1 <> w2)

instance Monoid w => Monad (WriterW w) where
  WriterW (a,w1) >>= k =
    let WriterW (b,w2) = k a
    in WriterW (b, w1 <> w2)

tellW :: Monoid w => w -> WriterW w ()
tellW w = WriterW ((), w)

listenW :: Monoid w => WriterW w a -> WriterW w (a, w)
listenW (WriterW (a,w)) = WriterW ((a,w), w)

passW :: Monoid w => WriterW w (a, w -> w) -> WriterW w a
passW (WriterW ((a, f), w)) = WriterW (a, f w)

-- Reader
newtype ReaderR r a = ReaderR { runReaderR :: r -> a }

instance Functor (ReaderR r) where
  fmap f (ReaderR g) = ReaderR (f . g)

instance Applicative (ReaderR r) where
  pure x = ReaderR (const x)
  ReaderR rf <*> ReaderR ra = ReaderR (\r -> rf r (ra r))

instance Monad (ReaderR r) where
  ReaderR m >>= k = ReaderR $ \r ->
    let a = m r
        ReaderR m' = k a
    in m' r

askR :: ReaderR r r
askR = ReaderR id

localR :: (r -> r) -> ReaderR r a -> ReaderR r a
localR f (ReaderR m) = ReaderR (m . f)

-- State
newtype StateS s a = StateS { runStateS :: s -> (a, s) }

instance Functor (StateS s) where
  fmap f (StateS g) = StateS $ \s -> let (a, s') = g s in (f a, s')

instance Applicative (StateS s) where
  pure a = StateS $ \s -> (a, s)
  StateS sf <*> StateS sa = StateS $ \s ->
    let (f, s1) = sf s
        (a, s2) = sa s1
    in (f a, s2)

instance Monad (StateS s) where
  StateS sa >>= f = StateS $ \s ->
    let (a, s1) = sa s
        StateS sb = f a
    in sb s1

getS :: StateS s s
getS = StateS $ \s -> (s, s)

putS :: s -> StateS s ()
putS s = StateS $ \_ -> ((), s)

modifyS :: (s -> s) -> StateS s ()
modifyS f = StateS $ \s -> ((), f s)


runWriter :: WriterW w a -> (a, w)
runWriter = runWriterW

runReader :: ReaderR r a -> r -> a
runReader = runReaderR

runState :: StateS s a -> s -> (a, s)
runState = runStateS

-- =================
-- HC21T1
-- =================

addW :: Int -> Int -> WriterW [String] Int
addW x y = WriterW (x + y, ["add " ++ show x ++ " " ++ show y])

subW :: Int -> Int -> WriterW [String] Int
subW x y = WriterW (x - y, ["sub " ++ show x ++ " " ++ show y])

mulW :: Int -> Int -> WriterW [String] Int
mulW x y = WriterW (x * y, ["mul " ++ show x ++ " " ++ show y])

calcDemo :: WriterW [String] Int
calcDemo = do
  a <- addW 2 3
  b <- mulW a 4
  c <- subW b 5
  return c

-- =================
-- HC21T2
-- =================

writerLawChecks :: [(String, Bool)]
writerLawChecks =
  let m = WriterW (3, ["m"])
      k x = WriterW (x + 1, ["k" ++ show x])
      h x = WriterW (x * 2, ["h" ++ show x])
      assocOk = runWriter ((m >>= k) >>= h) == runWriter (m >>= (\x -> k x >>= h))
      leftIdOk = runWriter (pure 5 >>= k) == runWriter (k 5)
      rightIdOk = runWriter (m >>= pure) == runWriter m
  in [("Writer associativity", assocOk), ("Writer left identity", leftIdOk), ("Writer right identity", rightIdOk)]

-- =================
-- HC21T3
-- =================

redact :: [String] -> [String]
redact = map (\s -> if "secret" `isInfixOf` s then "[REDACTED]" else s)

demoListenPass :: WriterW [String] String
demoListenPass = passW $ do
  tellW ["public", "this contains secret", "ok"]
  return ("done", redact)

-- =================
-- HC21T4
-- =================

countStep :: Int -> WriterW (Sum Int) ()
countStep n = tellW (Sum n)

countDemo :: WriterW (Sum Int) Int
countDemo = do
  countStep 1
  countStep 2
  pure 42

-- =================
-- HC21T5/6
-- =================

data Config = Config { greetPrefix :: String, shout :: Bool } deriving Show

greet :: String -> ReaderR Config String
greet name = do
  cfg <- askR
  let base = greetPrefix cfg ++ " " ++ name
  if shout cfg then pure (map toUpper base) else pure base

greetWithLocal :: String -> ReaderR Config (String, String)
greetWithLocal name = do
  normal <- greet name
  loud <- localR (\c -> c { shout = not (shout c) }) (greet name)
  pure (normal, loud)

-- =================
-- HC21T7
-- =================

type Env = Map.Map String String

getDbHost :: Env -> String
getDbHost env = Map.findWithDefault "localhost" "DB_HOST" env

getDbPort :: Env -> Int
getDbPort env = read $ Map.findWithDefault "5432" "DB_PORT" env

getDbHostR :: ReaderR Env String
getDbHostR = ReaderR getDbHost

getDbPortR :: ReaderR Env Int
getDbPortR = ReaderR getDbPort

getDbPortOverrideR :: ReaderR Env Int
getDbPortOverrideR = localR (Map.insert "DB_PORT" "9999") getDbPortR

-- =================
-- HC21T8
-- =================

countChars :: Char -> StateS String Int
countChars c = StateS $ \s -> (length $ filter (== c) s, s)

-- =================
-- HC21T9
-- =================

mapCount :: (a -> b) -> [a] -> StateS Int [b]
mapCount f xs = sequenceA $ map (\x -> do
                                    cnt <- getS
                                    putS (cnt + 1)
                                    return (f x)
                                 ) xs

-- =================
-- HC21T10
-- =================

data VendingState = VendingState { items :: Int, credit :: Int } deriving Show

insertCoin :: Int -> StateS VendingState ()
insertCoin n = modifyS $ \st -> st { credit = credit st + n }

vend :: StateS VendingState String
vend = StateS $ \st ->
  if items st > 0 && credit st >= 1
    then ("Dispensed", st { items = items st - 1, credit = credit st - 1 })
    else ("Nothing", st)

getChange :: StateS VendingState Int
getChange = StateS $ \st -> (credit st, st { credit = 0 })

vendingSequence :: StateS VendingState (String, Int)
vendingSequence = do
  insertCoin 2
  insertCoin 1
  r <- vend
  ch <- getChange
  pure (r, ch)

-- =================
-- HC21T11
-- =================

type UndoState = (Int, [Int])

setValue :: Int -> StateS UndoState ()
setValue v = StateS $ \(cur, hist) -> ((), (v, cur:hist))

undo :: StateS UndoState ()
undo = StateS $ \(cur, hist) -> case hist of
  (h:hs) -> ((), (h, hs))
  []     -> ((), (cur, []))

-- =================
-- HC21T12
-- =================

lcgStep :: Int -> Int
lcgStep s = (s * 1103515245 + 12345) `mod` 2147483648

randomStep :: StateS (Int, (Int, Int)) ()
randomStep = StateS $ \(seed, (x,y)) ->
  let seed' = lcgStep seed
      d = seed' `mod` 4
      pos' = case d of
               0 -> (x + 1, y)
               1 -> (x - 1, y)
               2 -> (x, y + 1)
               3 -> (x, y - 1)
               _ -> (x, y)
  in ((), (seed', pos'))

randomWalk :: Int -> StateS (Int, (Int, Int)) [(Int, Int)]
randomWalk n = StateS $ \(seed, pos) ->
  let go 0 s p acc = (reverse acc, (s, p))
      go k s p acc =
        let s' = lcgStep s
            d = s' `mod` 4
            p' = case d of
                  0 -> (fst p + 1, snd p)
                  1 -> (fst p - 1, snd p)
                  2 -> (fst p, snd p + 1)
                  _ -> (fst p, snd p - 1)
        in go (k-1) s' p' (p':acc)
  in go n seed pos [pos]

-- =================
-- HC21T13
-- =================

stepLog :: String -> ReaderR Config (WriterW [String] ())
stepLog msg = ReaderR $ \cfg ->
  let prefix = if shout cfg then map toUpper (greetPrefix cfg) else greetPrefix cfg
  in tellW [prefix ++ ": " ++ msg]

-- =================
-- HC21T14
-- =================

incLogState :: Int -> StateS Int (WriterW [String] ())
incLogState n = do
  modifyS (+ n)
  _ <- getS
  pure (tellW ["inc by " ++ show n])

decLogState :: Int -> StateS Int (WriterW [String] ())
decLogState n = do
  modifyS (\x -> x - n)
  _ <- getS
  pure (tellW ["dec by " ++ show n])

incAsWriter :: Int -> WriterW [String] (StateS Int ())
incAsWriter n = WriterW (modifyS (+ n), ["incAsWriter " ++ show n])

-- =================
-- HC21T15
-- =================

data ThresholdConfig = ThresholdConfig { threshold :: Int } deriving Show

tick :: ReaderR ThresholdConfig (StateS Int Bool)
tick = ReaderR $ \cfg -> StateS $ \st ->
  let st' = st + 1
  in (st' >= threshold cfg, st')

-- =================
-- HC21T16
-- =================

readerLocalIdCheck :: ReaderR Config Bool
readerLocalIdCheck = do
  let m = ReaderR (\(Config p s) -> p)
  pure $ runReader m (Config "x" False) == runReader (localR id m) (Config "x" False)

-- =================
-- HC21T17
-- =================

legacy :: Env -> Int -> (String, [String], Int)
legacy env cnt =
  let host = Map.findWithDefault "localhost" "DB_HOST" env
      logs = ["using host: " ++ host, "counter was " ++ show cnt]
      cnt' = cnt + 1
  in (host, logs, cnt')

refactored :: ReaderR Env (WriterW [String] (StateS Int String))
refactored = ReaderR $ \env ->
  let host = Map.findWithDefault "localhost" "DB_HOST" env
      logs = ["using host: " ++ host]
      stateAction = StateS $ \cnt -> (host, cnt + 1)
  in WriterW (stateAction, logs)

-- =================
-- MAIN
-- =================

main :: IO ()
main = do
  putStrLn "=== HC21T1: Writer logging calculator ==="
  let (res1, log1) = runWriter calcDemo
  putStrLn $ "Result: " ++ show res1
  putStrLn "Log:"
  mapM_ putStrLn log1

  putStrLn "\n=== HC21T2: Writer law checks ==="
  mapM_ (\(n,b) -> putStrLn $ n ++ ": " ++ show b) writerLawChecks

  putStrLn "\n=== HC21T3: listen/pass demo (redact) ==="
  let (r3, l3) = runWriter demoListenPass
  putStrLn $ "Result: " ++ r3
  putStrLn "Log:"
  mapM_ putStrLn l3

  putStrLn "\n=== HC21T4: Switch log monoid (Sum Int) demo ==="
  let (v4, Sum n4) = runWriter countDemo
  putStrLn $ "Value: " ++ show v4 ++ " Count: " ++ show n4

  putStrLn "\n=== HC21T5/6: Reader greet and local demo ==="
  let cfg = Config "Hello" False
      (normal, loud) = runReader (greetWithLocal "Alice") cfg
  putStrLn $ "normal: " ++ normal
  putStrLn $ "with local flipped shout: " ++ loud

  putStrLn "\n=== HC21T7: Reader Env demo ==="
  let env = Map.fromList [("DB_HOST","db.example"), ("DB_PORT","5432")]
  putStrLn $ "DB_HOST: " ++ runReader getDbHostR env
  putStrLn $ "DB_PORT normal: " ++ show (runReader getDbPortR env)
  putStrLn $ "DB_PORT override: " ++ show (runReader getDbPortOverrideR env)

  putStrLn "\n=== HC21T8: State countChars demo ==="
  let (cntA, _) = runState (countChars 'a') "banana"
  putStrLn $ "count 'a' in \"banana\": " ++ show cntA

  putStrLn "\n=== HC21T9: mapCount demo ==="
  let (mapped, finalCount) = runState (mapCount (*2) [1,2,3,4]) 0
  putStrLn $ "mapped: " ++ show mapped ++ ", count: " ++ show finalCount

  putStrLn "\n=== HC21T10: Vending machine demo ==="
  let initialV = VendingState 2 0
      ((vendRes, change), finalV) = runState vendingSequence initialV
  putStrLn $ "vend result: " ++ show vendRes ++ ", change: " ++ show change ++ ", final state: " ++ show finalV

  putStrLn "\n=== HC21T11: Undo stack demo ==="
  let ((), finalUndo) = runState (do setValue 10; setValue 20; undo; undo) (0,[])
  putStrLn $ "final undo state: " ++ show finalUndo

  putStrLn "\n=== HC21T12: Deterministic random walk demo ==="
  let (path, (seedEnd, posEnd)) = runState (randomWalk 8) (42, (0,0))
  putStrLn $ "path: " ++ show path
  putStrLn $ "final seed/pos: " ++ show (seedEnd, posEnd)

  putStrLn "\n=== HC21T13: Reader + Writer logging demo ==="
  let cfgLog = Config "info" False
      -- Compose ReaderR program that returns a WriterW action combining steps
      readerProg :: ReaderR Config (WriterW [String] ())
      readerProg = do
        s1 <- stepLog "start"
        s2 <- stepLog "middle"
        s3 <- localR (\c -> c { shout = True }) (stepLog "all good")
        pure (s1 >> s2 >> s3) -- combine writer actions into one WriterW
      writerAction = runReader readerProg cfgLog           -- WriterW [String] ()
      ((), logs13) = runWriter writerAction                -- unwrap WriterW
  mapM_ putStrLn logs13

  putStrLn "\n=== HC21T14: State + Writer instrumented demo ==="
  let (writerA, sAfter) = runState (incLogState 3 >>= \w -> return w) 0
      ((), logsA) = runWriter writerA
  putStrLn $ "state after incLogState 3: " ++ show sAfter ++ ", logs: " ++ show logsA
  let (stateActionB, logsB) = runWriter (incAsWriter 5)
      (_, sB) = runState stateActionB 0
  putStrLn $ "incAsWriter produced logs: " ++ show logsB ++ ", final state: " ++ show sB
  putStrLn "  Note: State(Writer) vs Writer(State) affects API ergonomics (which side you access first)."

  putStrLn "\n=== HC21T15: Reader + State tick demo ==="
  let cfgLow = ThresholdConfig 2
      cfgHigh = ThresholdConfig 4
      (bLow, stLow) = runState (runReader tick cfgLow) 0
      (b1, stAfterTwo) = runState ((runReader tick cfgLow) >>= \_ -> runReader tick cfgLow) 0
  putStrLn $ "single tick under cfgLow triggered? " ++ show bLow ++ ", state: " ++ show stLow
  putStrLn $ "two ticks under cfgLow second tick result: " ++ show b1 ++ ", final state: " ++ show stAfterTwo

  putStrLn "\n=== HC21T16: Reader local/id check ==="
  putStrLn $ "reader local/id test: " ++ show (runReader readerLocalIdCheck (Config "x" False))

  putStrLn "\n=== HC21T17: Legacy refactor demo ==="
  let envLegacy = Map.fromList [("DB_HOST","legacy.host")]
      (hlegacy, logsLegacy, cntLegacy) = legacy envLegacy 0
  putStrLn $ "legacy: host=" ++ hlegacy ++ " logs=" ++ show logsLegacy ++ " cnt=" ++ show cntLegacy
  let WriterW (stateAct, logsNew) = runReader refactored envLegacy
      (hostNew, cntNew) = runState stateAct 0
  putStrLn $ "refactored: host=" ++ hostNew ++ " logs=" ++ show logsNew ++ " cnt=" ++ show cntNew

  
