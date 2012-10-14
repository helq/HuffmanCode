module HuffmanCode (
      encode
    , encodeWithTree
    , decode
    , createCodeTree
    , lorem
    --, convert
) where

import Data.List (sort, group, sortBy, foldr1, foldl', insertBy)
import Data.Function (on)

data CodeTree =
    Fork { 
        left   :: CodeTree,
        right  :: CodeTree
    } 
    | Leaf { char   :: Char }

instance Show CodeTree where
  show (Fork l r) = "Fork (" ++ show l ++ ") (" ++ show r ++ ")"
  show (Leaf c)   = "Leaf " ++ show c

type CharInt = (Char, Int)

createCodeTree :: String -> CodeTree
createCodeTree = combine . sortPair . map unreplicate . group . sort
    where
        unreplicate l@(x:_) = (x, length l) -- Strings to Pairs

        -- sortPair = sortBy (\(_,x) (_,y)->compare x y)
        sortPair = sortBy comparePair
        comparePair = (compare `on` snd)
        
        --combine :: [CharInt] -> CodeTree -- unoptimized
        --combine = foldr1 Fork . map pairToLeaf -- Is don't optime
        --      where pairToLeaf = (\(x,_)->Leaf x)
        
        -- optimization of combine
        combine :: [CharInt] -> CodeTree 
                                 -- (pairs, sum $ map snd pairs)
        combine = oneElement . iterate combine' . map (\(c,i)->(Leaf c, i))
            where 
                combine' :: [(CodeTree, Int)] -> [(CodeTree, Int)]
                combine' ((c1,n1):(c2,n2):cts) = insertBy comparePair (Fork c1 c2, n1+n2) cts

                oneElement = fst . head . head . dropWhile (not.len1)
                    where len1 [x] = True
                          len1 _   = False

          --optCombine pairs = combine' (pairs, foldl' (\x (_,y)->x+y) 0 pairs)
          --  where 
          --      combine' :: ([CharInt], Int) -> CodeTree
          --      combine' ([(c,_)], _) = Leaf c
          --      combine' chars        = Fork (combine' l) (combine' r)
          --          where (l,r) = cutMiddle chars

          --      cutMiddle :: ([CharInt], Int) -> ( ([CharInt], Int) , ([CharInt], Int) )
          --      cutMiddle ([c,c'], _) = ( ([c],snd c), ([c'],snd c) )
          --      cutMiddle (cs, n)     = ( (cs', i), (cs'', n-i) )
          --          where (cs',cs'',i) = break' 0 cs
                          
          --                break' :: Int -> [CharInt] -> ( [CharInt], [CharInt], Int )
          --                break' acc [x]      = ([],[x], acc)
          --                break' acc (x:xs)
          --                   | 2*acc >= n   = ([], x:xs, acc)
          --                   | otherwise = (x:xs', ys', acc')
          --                        where (xs',ys',acc') = break' (acc+snd x) xs


type CodeTable = [(Char, [Int])]
type Bit = Int

convert :: CodeTree -> CodeTable
convert (Leaf c)   = [(c, [])]
convert (Fork l r) = add 0 (convert l) ++ add 1 (convert r)
    where add n = map (\(a, ns)->(a, n:ns))

encodeWithTree :: CodeTree -> String -> [Bit]
encodeWithTree ct = encode'
    where codeTable = convert ct
          
          encode' []     = []
          encode' (c:cs) = bs ++ encode' cs
            where bs = encodeChar codeTable c

          encodeChar :: CodeTable -> Char -> [Bit]
          encodeChar ((c',bs):ct) c
              | c' == c   = bs
              | otherwise = encodeChar ct c

encode :: String -> (CodeTree, [Bit])
encode text = (codeTree, encodeWithTree codeTree text)
    where codeTree = createCodeTree text

decode :: CodeTree -> [Bit] -> String
decode _ [] = []
decode codeTree ns = c' : decode codeTree ns'
    where (c',ns') = decodeChar codeTree ns
          
          decodeChar :: CodeTree -> [Int] -> (Char, [Int])
          decodeChar (Leaf c)    ns     = (c, ns)
          decodeChar (Fork _ _)  []     = error "CodeTree or (text::[Bin]) invalid"
          decodeChar (Fork l r) (n:ns)
              | n==0      = decodeChar l ns
              | otherwise = decodeChar r ns

-- (ct,b) = encode lorem
-- decode ct b == lorem -- True

lorem = "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Donec a diam lectus.\
        \ Sed sit amet ipsum mauris. Maecenas congue ligula ac quam viverra nec consectetur\
        \ ante hendrerit. Donec et mollis dolor. Praesent et diam eget libero egestas mattis\
        \ sit amet vitae augue. Nam tincidunt congue enim, ut porta lorem lacinia consectetur.\
        \ Donec ut libero sed arcu vehicula ultricies a non tortor. Lorem ipsum dolor sit amet,\
        \ consectetur adipiscing elit. Aenean ut gravida lorem. Ut turpis felis, pulvinar a\
        \ semper sed, adipiscing id dolor. Pellentesque auctor nisi id magna consequat sagittis.\
        \ Curabitur dapibus enim sit amet elit pharetra tincidunt feugiat nisl imperdiet. Ut\
        \ convallis libero in urna ultrices accumsan. Donec sed odio eros. Donec viverra mi\
        \ quis quam pulvinar at malesuada arcu rhoncus. Cum sociis natoque penatibus et magnis\
        \ dis parturient montes, nascetur ridiculus mus. In rutrum accumsan ultricies. Mauris\
        \ vitae nisi at sem facilisis semper ac in est."

