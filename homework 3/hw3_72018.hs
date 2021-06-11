main :: IO()
main = do
    print(getMoviesLongerThan "Star Wars" db)
    print(getMoviesLongerThan "The Fellowship of the Ring" db)
    print(getMaleActorsIn "Terms of Endearment" db)
    print(getMaleActorsIn "Star Wars" db)
    print (getFemaleActorsFrom 1983 db)
    print (getFemaleActorsFrom 2001 db)
    print "Int"
    print(degr t1 5)
    print(degr t1 6)
    print(degr t1 7)
    print(degr t1 18)
    print "Char"
    print(degr t2 's')
    print(degr t2 'k')
    print(degr t2 '1')

-- task 1

type Name = String
type Title = String
type Year = Int
type Gender = Char
type Length = Int

data Movie     = Movie Title Year Length deriving Show
data MovieStar = MovieStar Name Gender deriving Show
data StarsIn   = StarsIn Name Title deriving Show

type MovieDB = ([Movie],[MovieStar],[StarsIn])

movies :: [Movie]
movies =
  [ Movie "The Man Who Wasn't There" 2001 116,
    Movie "Logan's run" 1976 120,
    Movie "Star Wars" 1977 124,
    Movie "Empire Strikes Back" 1980 111,
    Movie "Star Trek" 1979 132,
    Movie "Star Trek: Nemesis" 2002 116,
    Movie "Terms of Endearment" 1983 132,
    Movie "The Usual Suspects" 1995 106,
    Movie "Gone With the Wind" 1938 238,
    Movie "The Fellowship of the Ring" 2001 178
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
db = (movies, stars, starsIn)

-- getters for the data

getMovieName :: Movie -> Title
getMovieName (Movie x _ _) = x  -- getter for movie name

getYear :: Movie -> Year
getYear (Movie _ x _) = x -- getter for movie year

getLength :: Movie -> Length
getLength (Movie _ _ x) = x -- getter for movie length

getActorName :: MovieStar -> Name
getActorName (MovieStar x _) = x -- getter for action name

getGender :: MovieStar -> Gender
getGender (MovieStar _ x) = x -- getter for actor gender

getStarInActorName :: StarsIn -> Name
getStarInActorName (StarsIn x _) = x -- getter for actor name in movies

getStarInMovieTitle :: StarsIn -> Title
getStarInMovieTitle (StarsIn _ x) = x -- getter for movie name in which actors performs


getMoviesLongerThan :: Title -> MovieDB -> [Title]
getMoviesLongerThan film ([],_,_) = [] -- if the list is empty -> empty list is returned
getMoviesLongerThan film (movies,_,_) = contains film movies movies -- first we check if the searched movie is in the list
  where contains _ [] _ = [] -- if the movie has not been found -> empty list is returned
        contains  title movies cpy -- we get the title , movies list and  cpy of the movies list
            |title == getMovieName (head movies) =  helperMovie (Movie (getMovieName (head movies)) (getYear (head movies)) (getLength (head movies))) cpy -- if movie name is found we go in the helper and we get the movie name nad the year and the length and also the cpy of movie list
            |otherwise = contains title (tail movies) cpy -- if the movie name has not been found we continue searching by going through the list of movies
            where helperMovie _ [] = [] -- if the cpy of movie list is empty -> empty list is returned
                  helperMovie name movies -- we get the movie and movies list
                      |getLength name < getLength (head movies) = getMovieName (head movies) : helperMovie name (tail movies) -- if the length of the head movie of the list is longer than the our movie -> the other movie is added in a new list and then the func is called again with tail movie list
                      |otherwise        = helperMovie name (tail movies) -- if the length of head movie list is not longer than our movie then the head movie is not added in a new list and only the funcion is called again


getMaleActorsIn :: Title -> MovieDB -> [Name]
getMaleActorsIn film (movies,actors,scenesIn) = helper film (actors,scenesIn) -- helper is called only with film name, actors and scenes
  where helper _ (_,[]) = [] -- if scenesIn list is empty -> empty list is returned
        helper film (list,scenes)
          |film == getStarInMovieTitle (head scenes) = checkActorGender (getStarInActorName (head scenes)) list -- if the name of the movie is found in the list then the gender of the actor is checked
          |otherwise  = helper film (list,tail scenes) -- if the name of the movie is not found then the function is called again with tail of the list
          where checkActorGender _ [] = [] -- if the actor has not been found then -> empty list is returned
                checkActorGender actor listOfActors
                  |actor == getActorName (head listOfActors) && getGender (head listOfActors) == 'F' = helper film (tail list,tail scenes) -- if the found actor is female then the helper is called again with the film and tail of actors list and tail  of scenes list
                  |actor == getActorName (head listOfActors) && getGender (head listOfActors) == 'M' = getActorName (head listOfActors) : helper film (tail list,tail scenes) -- if the found actor is male then the actor name is added in a new list and the helper is called with tail of actors list and tail scenes
                  |otherwise  = checkActorGender actor (tail listOfActors) -- if the name is not found yet then the function is called again with actor and the list of actors


getFemaleActorsFrom :: Year -> MovieDB -> [Name]
getFemaleActorsFrom year (movies,actors,scenesIn) = helper year (movies,actors,scenesIn) -- the function calles helper
    where helper _ ([],_,_) = [] -- if movies list is empty -> empty list is returned
          helper period (moviesList,actorsList,scenesList) 
            |period == getYear(head moviesList) = checkFemaleActor (getMovieName(head moviesList)) scenesList -- if the year is equal with the year  of the movie then check female actor function is called with the name of the movie and scenesIn list
            |otherwise  = helper period (tail moviesList,actorsList,scenesList) -- if the year is not equal the helper is called again with the tail of list of movies
            where checkFemaleActor _ [] = helper period (tail moviesList,actorsList,scenesList) -- if the movie name has not been found then the helper is called with the year,tail movieList,actorList and scenesList
                  checkFemaleActor movieName scenes
                      |movieName == getStarInMovieTitle (head scenes) = checkGender (getStarInActorName(head scenes)) actorsList -- if the movie name is samer with movie name in scenesIn list then the checkGender fucntion is called with actor's name and list of actors
                      |otherwise  = checkFemaleActor movieName (tail scenes) -- if the name of whe movie is not the same with the head of movies list then the tail is calles
                      where checkGender _ [] = [] -- if the actors list is empty -> empty list is returned
                            checkGender actorName listActors
                              |actorName == getActorName (head listActors) && getGender(head listActors) == 'M' = helper period (moviesList,actorsList,tail scenes) -- if the found actor is male then the helper is called again with the year,moviesList,actorsList and tail scenes
                              |actorName == getActorName (head listActors) && getGender(head listActors) == 'F' = getActorName(head listActors) : helper period (moviesList,actorsList,tail scenes) -- if the found actor is female then the actor's name is added in a new list and the helper function is called again with year,moviesList,actorsList and tail scenes
                              |otherwise  = checkGender actorName (tail listActors) -- if the actor name has not been found yet then the function is called again with the name of the actor and tail of listActors

-- task 2

data NTree a = Nil | Node a [NTree a] deriving Show

t1 :: NTree Int
t1 = Node 8 [Node 7 [Node 4 [Nil],Node 5 [Nil]],Node 6[Node 10 [Nil],Node 15 [Nil],Node 13 [Nil]],Node 18 [Nil]]

t2:: NTree Char
t2 = Node '1'[Node 'f'[Node 'H'[Nil],Node 'a' [Nil]],Node 'm'[Node 's' [Nil]],Node 'i' [Node 'k'[Node 'e' [Nil], Node 'l' [Nil], Node 'L' [Nil]]]]


degr :: (Eq a) => NTree a -> a -> Int
degr Nil _ = 0 -- if it's empty tree then -> 0 is returned
degr (Node a xs) x = helperDegr (Node a xs) x 0 -- otherwise helper is called with the Ntree searched element and cnt
    where helperDegr Nil _ _ = 0 -- if it's empty then 0 is returned
          helperDegr (Node a xs) x cnt
            |a == x && isLeaf (Node a xs) = 1 -- if the searched element is leaf then -> 1 is returned
            |a == x && cnt == 0 = length xs -- if the searched element is root then -> the length of the list is returned(the element is root if cnt == 0)
            |a == x = 1 + length  xs -- if the seached element is found then we then we return 1 + the length of element's list
            |otherwise = helper xs x -- if the searched element has not been found yet -> helper is called with the list of Node of the tree with the searched element
            where helper [] _ = 0 -- if the list is empty -> 0 is returned
                  helper xs x = helperDegr (head xs) x (cnt+1) + helper (tail xs) x -- the helperDegr is called with the head of the list and the element anc (cnt + 1) is increased by one also the helper is called again with the tail ot the list and also the seacherd element


isLeaf :: (Eq a) => NTree a -> Bool -- this function checks if an element in NTree is a leaf
isLeaf (Node a [Nil]) = True  -- returns True if the element does not have other elements
isLeaf (Node a xs)    = False  -- returns False if the element has other elements