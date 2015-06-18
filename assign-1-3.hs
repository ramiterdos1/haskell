import Char
charToDigit::Char -> Int
charToDigit p|(('0'<=p)&&(p<='9'))=ord(p)-ord('0')
             |otherwise=0
charToNum::[Char]->Int
charToNum []=0
charToNum [x]=charToDigit x
charToNum (x:y:xs)=((charToDigit x)*(10^(length (x:y:xs)-1)))+charToNum(y:xs)
