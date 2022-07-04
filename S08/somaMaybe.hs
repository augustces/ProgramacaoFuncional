import Data.Maybe (isNothing, fromJust)
somaMaybe a b = case remover a + remover b of
                    0 -> Nothing 
                    y -> Just y
    where remover a
            | isNothing a = 0
            | otherwise = fromJust a

main = do
    print $ somaMaybe (Just 5) (Just 7) -- == Just(12)
    print $ somaMaybe (Just 5) Nothing -- == Just(5)
    print $ somaMaybe Nothing (Just 3) -- == Just(3)
    print $ somaMaybe Nothing Nothing -- == Nothing