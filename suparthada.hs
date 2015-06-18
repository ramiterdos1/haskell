wordbuilding :: [String] -> [[String]]
wordbuilding l= dd [] l
dd :: [String] -> [String] -> [[String]]
dd l xs | l==[] =concat [dd  (l++[(xs!!n)]) ((take (n) xs)++(drop (n+1) xs)) | n <- [0..((length xs)-1)]]
	| []== [dd  (l++[(xs!!n)]) ((take (n) xs)++(drop (n+1) xs)) | n <- [0..((length xs)-1)],last (last l) ==head (xs!!n)] = [l]
	| otherwise =concat [dd  (l++[(xs!!n)]) ((take n xs)++(drop (n+1) xs)) | n <- [0..((length xs)-1)],last (last l) ==head (xs!!n)]
