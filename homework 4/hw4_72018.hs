main :: IO ()
main = do
    print(getFeaturedStars "MGM" 1995 db)
    print(getFeaturedStars "USA Entertainm." 2001 db)
    print(getPresident "Paramount" db)
    print(getPresident "Fox" db)
    print(getPresident "USA Entertainm." db)
    print(getHigherProductions "Calvin Coolidge" db)
    print(getHigherProductions "Stephen Spielberg" db)
    print(getHigherProductions "George Lucas" db)
    print(toBinaryIndexed t1)
    print(toBinaryIndexed t2)


type Name = String
type Title = String
type Address = String
type Year = Int
type Gender = Char
type Length = Int
type ProducerID = Int
type Networth = Integer
data Movie = Movie Title Year Length Name ProducerID
    deriving Show
data MovieStar = MovieStar Name Gender
    deriving Show
data StarsIn = StarsIn Name Title
    deriving Show
data Studio = Studio Name Int
    deriving Show
data MovieExec = MovieExec Name ProducerID Networth
    deriving Show
type MovieDB = ([Movie], [MovieStar], [StarsIn], [Studio], [MovieExec])




studios :: [Studio]
studios =
  [ Studio "Disney" 199,
    Studio "USA Entertainm." 222,
    Studio "Fox" 333,
    Studio "Paramount" 123,
    Studio "MGM" 555
  ]
movieExecs :: [MovieExec]
movieExecs =
  [ MovieExec "George Lucas" 555 200000000,
    MovieExec "Ted Turner" 333 125000000,
    MovieExec "Stephen Spielberg" 222 100000000,
    MovieExec "Merv Griffin" 199 112000000,
    MovieExec "Calvin Coolidge" 123 20000000
  ]

movies :: [Movie]
movies =
  [ Movie "Pretty Woman" 1990 119 "Disney" 199,
    Movie "The Man Who Wasn't There" 2001 116 "USA Entertainm." 555,
    Movie "Logan's run" 1976 120 "Fox" 333,
    Movie "Star Wars" 1977 124 "Fox" 555,
    Movie "Empire Strikes Back" 1980 111 "Fox" 555,
    Movie "Star Trek" 1979 132 "Paramount" 222,
    Movie "Star Trek: Nemesis" 2002 116 "Paramount" 123,
    Movie "Terms of Endearment" 1983 132 "MGM" 123,
    Movie "The Usual Suspects" 1995 106 "MGM" 199,
    Movie "Gone With the Wind" 1938 238 "MGM" 123,
    Movie "The Fellowship of the Ring" 2001 178 "USA Entertainm." 222
  ]

stars :: [MovieStar]
stars =
  [ MovieStar "Jane Fonda" 'F',
    MovieStar "Alec Baldwin" 'M',
    MovieStar "Kim Basinger" 'F',
    MovieStar "Harrison Ford" 'M',
    MovieStar "Debra Winger" 'F',
    MovieStar "Jack Nicholson" 'M',
    MovieStar "Sandra Bullock" 'F',
    MovieStar "Orlando Bloom" 'M',
    MovieStar "Cate Blanchett" 'F',
    MovieStar "Liv Tyler" 'F',
    MovieStar "Billy Bob Thornton" 'M',
    MovieStar "Scarlett Johansson" 'F'
  ]

starsIn :: [StarsIn]
starsIn =
  [ StarsIn "Kim Basinger" "Star Wars",
    StarsIn "Alec Baldwin" "Star Wars",
    StarsIn "Harrison Ford" "Star Wars",
    StarsIn "Harrison Ford" "Empire Strikes Back",
    StarsIn "Jack Nicholson" "The Usual Suspects",
    StarsIn "Jane Fonda" "Terms of Endearment",
    StarsIn "Jack Nicholson" "Terms of Endearment",
    StarsIn "Sandra Bulloc" "The Usual Suspects",
    StarsIn "Billy Bob Thornton" "The Man Who Wasn't There",
    StarsIn "Scarlett Johansson" "The Man Who Wasn't There",
    StarsIn "Orlando Bloom" "The Fellowship of the Ring",
    StarsIn "Cate Blanchett" "The Fellowship of the Ring",
    StarsIn "Liv Tyler" "The Fellowship of the Ring"
  ]

db :: MovieDB
db = (movies, stars, starsIn, studios, movieExecs)

-- getters

getMovieTitle :: Movie -> Title
getMovieTitle (Movie x _ _ _ _) = x  -- gets the title of the movie

getMovieYear :: Movie -> Year
getMovieYear (Movie _ x _ _ _) = x -- gets the year of the movie

getMovieLength :: Movie -> Length
getMovieLength (Movie _ _ x _ _) = x -- gets the length of the movie

getMovieStudioName :: Movie -> Name
getMovieStudioName (Movie _ _ _ x _) = x -- gets the name of the studio in a movie

getMovieID :: Movie -> ProducerID
getMovieID (Movie _ _ _ _ x) = x -- gets the producer's ID in a movie

getMovieStarName :: MovieStar -> Name
getMovieStarName (MovieStar x _) = x -- gets the movie star's name

getMovieStarGender :: MovieStar -> Gender
getMovieStarGender (MovieStar _ x) = x -- gets the movie star's gender

getStarsInName :: StarsIn -> Name
getStarsInName (StarsIn x _) = x -- gets the movie star's name in a movie

getStarsInTitle :: StarsIn -> Title
getStarsInTitle (StarsIn _ x) = x -- gets the movie title in which a movie star performs

getStudioName :: Studio -> Name
getStudioName (Studio x _) = x -- gets the name of a studio

getStudioNum :: Studio -> Int
getStudioNum (Studio _ x) = x -- gets the studio's number

getMovieExecName :: MovieExec -> Name
getMovieExecName (MovieExec x _ _) = x -- gets the movie executive's name

getMovieExecID :: MovieExec -> ProducerID
getMovieExecID (MovieExec _ x _) = x -- gets the movie executive's ID

getMovieExecNetWorth :: MovieExec -> Networth
getMovieExecNetWorth (MovieExec _ _ x) = x -- gets the movie executive's net worth

-- task 1

getFeaturedStars :: Name -> Int -> MovieDB -> [Name]
getFeaturedStars studioName year (movies,_,starsIn,_,_) = [getStarsInName y| x <- movies, y <- starsIn ,getStarsInTitle y == getMovieTitle x, getMovieStudioName x == studioName, getMovieYear x == year]
-- through list comprehension we go through the movies list, starsIn list and we compare if the movie title is equal to te starsIn movie title and also if the studio names are equal and also the years 
-- if so we add the movie in the list
getPresident :: Name -> MovieDB -> Name
getPresident studioName (_,_,_,studios,movieExecs) = head [getMovieExecName y | x <- studios, y <- movieExecs, getStudioName x == studioName,getStudioNum x == getMovieExecID y]
-- through list comprehension we go through the studios list,movieExecs list and we compare if the studio name is equal to the movieExec's studio name and also the studioNum and movieExec's ID
-- if so we add the element to the list but the list is only with one element so we get the head of the list
getHigherProductions :: Name -> MovieDB -> [Name]
getHigherProductions movieExecName (movies,_,_,_,movieExecs) = [getMovieTitle z | x <- movieExecs, y <- movieExecs, getMovieExecNetWorth x > getMovieExecNetWorth y,getMovieExecName y == movieExecName,z <- movies,getMovieID z == getMovieExecID x]
-- through list comprehension we go through the movies list,movieExecs list and compare the net worth of movieExecs if the net worth is more than the movieExec name which is entered in the function 
--then we get the movies from movies list which are produced by that movie executive and add it to the list

-- task 2

data BTree a = Nil | Node a (BTree a) (BTree a)
  deriving Show

t1 :: BTree Char
t1 = Node 'a' (Node 'b' Nil (Node 'd' Nil Nil)) (Node 'c' (Node 'f' (Node 'e' Nil  Nil) Nil) Nil)

t2 :: BTree Int
t2 = Node 10 (Node 5 (Node 3 (Node 1 Nil Nil) Nil) (Node 7 (Node 6 Nil Nil) Nil)) (Node 15 (Node 13 Nil Nil) (Node 18 Nil Nil))

toBinaryIndexed :: (Eq a) => BTree a -> BTree (a,Int)
toBinaryIndexed Nil = Nil -- if  the tree is empty -> empty tree is returned
toBinaryIndexed (Node v lt rt) = helper (Node v lt rt) 0 -- if the tree is not empty then the helper with cnt is called
     where helper Nil _ = Nil -- if the tree is empty -> empty tree is returned
           helper (Node value left right) cnt = Node (value,cnt + countTheleft left) (helper left (cnt)) (helper right (1 + cnt + countTheleft left))
-- if the tree is not empty then the node  is created with the (value, and the counter + countThe left to check how many nodes are located more left than the node)
-- the helper is called for the left side of the tree with cnt
-- the helper is called for the right side of the tree with 1(because of the value which is already located left compared to the right side of the tree)
-- + cnt + countTheLeft left(to count the left side of the tree again)                     

countTheleft:: BTree a -> Int -- the function counts the elements of the tree it is used to check how many nodes are more left 
countTheleft Nil = 0 -- if the tree is empty -> empty tree is returned
countTheleft (Node _ lt  rt) = 1 + countTheleft lt + countTheleft rt -- if the tree is not empty -> it counts all the nodes it has       
          
