data Person = Person { firstName :: String, lastName :: String, age :: Int } deriving (Show)

abbrFirstName :: Person -> Person
abbrFirstName p@(Person {firstName = name})
    | length name < 2 = p
    | otherwise       = p {firstName = head name : '.' : []}
