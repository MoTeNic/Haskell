-- Haskell Chapter 8: Data Types, Synonyms, and Records
-- Combined code for HC8T1 – HC8T10
module Main where

-- HC8T1: Type Synonyms and Basic Function
type Address = String
type Value = Int

generateTx :: Address -> Address -> Value -> String
generateTx fromAddr toAddr val =
  "From: " ++ fromAddr ++ ", To: " ++ toAddr ++ ", Value: " ++ show val


-- HC8T2: New Types and Data Constructors
data PaymentMethod = Cash | Card | Cryptocurrency deriving (Show)

data Person1 = Person1 {
  pname :: String,
  paddress :: (String, Int),
  method :: PaymentMethod
} deriving (Show)

bob :: Person1
bob = Person1 "Bob" ("Main Street", 123) Cash


-- HC8T3: Algebraic Data Types and Functions
data Shape = Circle Float | Rectangle Float Float deriving (Show)

area :: Shape -> Float
area (Circle r) = pi * r * r
area (Rectangle w h) = w * h

circleArea = area (Circle 5)
rectArea = area (Rectangle 10 5)


-- HC8T4: Record Syntax for Employee
data Employee = Employee {
  name :: String,
  experienceInYears :: Float
} deriving (Show)

richard :: Employee
richard = Employee "Richard" 7.5


-- HC8T5: Record Syntax for Person
data Person = Person {
  personName :: String,
  age :: Int,
  isEmployed :: Bool
} deriving (Show)

person1 :: Person
person1 = Person "Alice" 30 True

person2 :: Person
person2 = Person "John" 25 False


-- HC8T6: Record Syntax for Shape Variants
data CircleShape = CircleShape {
  center :: (Float, Float),
  colorC :: String,
  radius :: Float
} deriving (Show)

data RectangleShape = RectangleShape {
  width :: Float,
  height :: Float,
  colorR :: String
} deriving (Show)

circle1 :: CircleShape
circle1 = CircleShape (0, 0) "Red" 10

rectangle1 :: RectangleShape
rectangle1 = RectangleShape 20 10 "Blue"


-- HC8T7: Data Types and Describing Animals
data Animal = Dog String | Cat String deriving (Show)

describeAnimal :: Animal -> String
describeAnimal (Dog name) = "This is a dog named " ++ name
describeAnimal (Cat name) = "This is a cat named " ++ name

dog1 = Dog "Rex"
cat1 = Cat "Whiskers"


-- HC8T8: Type Synonyms and Greeting Function
type Name = String
type Age = Int

greet :: Name -> Age -> String
greet n a = "Hello, my name is " ++ n ++ " and I am " ++ show a ++ " years old."


-- HC8T9: Record Type and Transaction Function
data Transaction = Transaction {
  from :: Address,
  to :: Address,
  amount :: Value,
  transactionId :: String
} deriving (Show)

createTransaction :: Address -> Address -> Value -> String
createTransaction f t v =
  let tx = Transaction f t v "TX12345"
  in transactionId tx


-- HC8T10: Deriving Show for Book
data Book = Book {
  title :: String,
  author :: String,
  year :: Int
} deriving (Show)

book1 :: Book
book1 = Book "Learn You a Haskell" "Miran Lipovaca" 2011


-- MAIN
main :: IO ()
main = do
  putStrLn "=== HC8T1: generateTx ==="
  putStrLn (generateTx "AliceAddr" "BobAddr" 50)

  putStrLn "\n=== HC8T2: Person with PaymentMethod ==="
  print bob

  putStrLn "\n=== HC8T3: Area of Shapes ==="
  putStrLn ("Circle area: " ++ show circleArea)
  putStrLn ("Rectangle area: " ++ show rectArea)

  putStrLn "\n=== HC8T4: Employee Record ==="
  print richard

  putStrLn "\n=== HC8T5: Persons ==="
  print person1
  print person2

  putStrLn "\n=== HC8T6: Circle and Rectangle Shapes ==="
  print circle1
  print rectangle1

  putStrLn "\n=== HC8T7: Animals ==="
  putStrLn (describeAnimal dog1)
  putStrLn (describeAnimal cat1)

  putStrLn "\n=== HC8T8: Greeting ==="
  putStrLn (greet "Lerato" 22)

  putStrLn "\n=== HC8T9: Transaction ==="
  putStrLn ("Transaction ID: " ++ createTransaction "Addr1" "Addr2" 100)

  putStrLn "\n=== HC8T10: Book ==="
  print book1
