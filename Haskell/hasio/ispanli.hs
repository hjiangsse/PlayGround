main = interact reverseLines

panliResponse contents = unlines (map (\xs -> if isPanli xs then "Palindrome" else "Not a palindrome")
                                 (lines contents))
                         where isPanli xs = xs == reverse xs

{-point.free-}
panliResponseFree = unlines . map (\xs -> if isPanli xs then "Palindrome" else "Not a palindrome") . lines
                         where isPanli xs = xs == reverse xs

{-reverse the lines-}
reverseLines = unlines . map reverse . lines

