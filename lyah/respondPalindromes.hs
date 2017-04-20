main = interact filterPalindromes

filterPalindromes :: String -> String
filterPalindromes =
  unlines . map replaceWithResponse . lines

replaceWithResponse :: String -> String
replaceWithResponse line =
  if isPalindrome line
  then "palindrome"
  else "not a palindrome"

isPalindrome :: String -> Bool
isPalindrome xs = xs == reverse xs
