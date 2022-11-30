module Foo where

import Numeric (showHex)

parse s = lines s

input = parse $
 "DULUDRDDDRLUDURUUULRRRURDRDULRUDDUDRULUDDUDRLDULRRLRDRUDUUULUUDLRURDUDDDDRDLLLLULRDLDRDLRLULRUURDDUULUDLRURRDDRDDRDDLDRDLLUURDRUULRRURURRDLRLLLUDULULULULUDRLLRUDUURLDRLRLRDRRDRLLLDURRDULDURDDRLURRDURLRRRLDLLLDRUUURLRDLDLLLLRDURRLDLULRLDDLDLURLRRDDRUDDUULRURRUDLRDLDUURDDDDRLRURUDULUDLRRLLLLLRDRURLLDLDULUUDLUDDDRLLDRRUDLLURRUUDDRRLLRRLDDDURLDRDRLURRRRDRRRDDUDULULDURRUUURRRDULUUUDDRULDRLLRDLDURLURRLLRUUUULRDURLLDDRLLDLRLRULUUDRURUDLLURUDDRDURLRDRRRDURLDDRDRLRLLURULUUULUDDDULDLRDDDRDLLRRLDRDULLUUUDLDDLDDDLLLLLLLDUDURURDURDRUURRRDDRDUDLULDURDUDURDDDRULDURURURRLURLURLUURLULDLLRUULURDDRLRDDLRDLRRR\n" ++
 "LUURLRUDRRUDLLDLUDDURULURLUUDUUDDRLUULRDUDDUULDUUDRURDDRRDRLULLRDRDLRLLUURRUULRLDRULUDLDUUDDDRDDLRDLULDRLDUULDLRDLLLDLDLRDUULUDURRULLRLDUDRLLLULUUUULUUDUUURRRDULLUURUDRRLDURRUULDRDULDUDRDUUULUUDDRLUDRLDLDRUUURDLDUDRUDUURLLRRLRLLRRLDULDDULUDUUURULDDUDUDRURRDLULRUDDURDLDLLRRRLDRLULLLRUULDUDLUUDURRLLLRLUDURRDDLDRDDDLURDLDRRUDUDLUDULULRUUUDLUURLLRLDDLURULDURDLRRDDDDURLDDLLDDULLLRLDLDULDUUDDRLDUURDDLDLUUDULRRLRLUURURUURLRLURUURLDRUURLLRDDUUUDULUDDDRDRLDRDRRLRLDULLRRUDLURULULRDRURURLULDUDLRURLRDDRULDDLRD\n" ++
 "LUDRULUULRRDDDDRRDUURUDDRLDDLDRDURRURULRDLDLDUUDRRDUUDUDLLLRRLDUDDRLDDLRRLRDRLUDLULUDDUUDULDUUULUDLDDURLDURUDLDRUUDRLRRLDLDDULDUUDDLDDLLURDRLRUURDDRUDDUDLDRRLRUDRUULRRRLRULULURDLRRURDRLRULDDDRDUULLURUUUURUDDLRRRRRDURLULDLUULUDRRUDUDRRDDRURDURLRLUDDLDLRRULUDLDDRLDDLDDDLLLLRDLLUULDDLULDLDRDDUDLURUDLDLDDRRUUDDDLRLLLDRRDDDUURDUDURUURRDRLLDUDLDUULLDLDLLUULLRRULDLDRURLDULDRUURDURRURDLRDLLLDRRUDRUUDRURLUDDRURLDURRDLUUDLUUDULLLDDDDRRDLLLDLURULDDRDLUUURRDRRUUDDUL\n" ++
 "DUUULDUDDDURLLULDDLLUDURLLLURULULURUURDRURLRULLLLDRDDULRRDRRLLLRDDDUULLRRURRULLDDURRRLRDDLULDULLDUDLURRDLDDLURDLRLLDRURLLRLLRRRDRRRURURUUDDLLDDLDDDLRLURUUUULRDLUDDDURLLDDRLDRRLLUDUUULRLLDRRRLRUUDLDUULRLUDRULLLLDUDLLUUDDRUURLURUDRDDDLRURUDRLULLULUUDLDURDULRRDRLDURUULRDRRRDRDRRLRLRDDUULLRDLDURDDDULURRLULDDURDURDDUDURDLLUUULUDULRDDLDRDRUDLLUURDLRDURURULURULLDRLLRRULDLULULDLULRURLRRLUDLLLRLUDLURLULDULDRLLLDLDDDDRDRLRRLRDULUUDULDDLDURDLLLDDDDLLUURRDURLDLUDDLULRUUUDDRRLDLLLRDLLDRRRDDLULLURDDRRRRLDLRLLLRL\n" ++
 "LULLRRDURRLDUUDRRURLURURRRLRDRUULUULURLLURRDRULRDURDDDDUULLLLDUULDLULURDRLDLULULDRLLDLLRLRULURUDRUUDULRULLLUDRULUDRLLUDLDRRDRUUURURLRDURDRLRDDDURLURRDLRUUUDUURULULDLUULRDLRRRDRDRLLLDLRRDRLLDDULDRUDRRLULLRDLDUDDULRDDLULRURULRLLLULDLLLLRDLDRURUDUURURLDRLUULLDUDULUDDDULUDLRUDDUDLULLUULUUURULURRULRDDURDDLURLRRDRDLDULRLRDRRRULRDDDRLLDDDDRRRRDRDLULUURDURULDLRDULDUDLDURUDLUDLUDDDUDURDURDDURLLRUDUURRRUDRRRRULLLLDDDLUULLUULRRRULDLURDLULRULDRLR"

app s 'U' | not (topRow s)      = s - 3
app s 'D' | not (botRow s)      = s + 3
app s 'L' | not (leftCol s)     = s - 1
app s 'R' | not (rightCol s)    = s + 1
app s _ = s

topRow s = s <= 3
botRow s = s >= 7
leftCol s = s `elem` [1,4,7]
rightCol s = s `elem` [3,6,9]

fold f s [] = []
fold f s (r:rs) = let s' = foldl f s r in s' : fold f s' rs

result1 = hexify $ fold app 5 input


canMoveUp s = s `notElem` [1,2,4,5,9]
canMoveDown s = s `notElem` [5,9,0xA,0xC,0xD]
canMoveLeft s = s `notElem` [1,2,5,0xA,0xD]
canMoveRight s = s `notElem` [1,4,9,0xC,0xD]

app2 s 'U' | s `elem` [3,0xD] = s - 2
app2 s 'U' | s `elem` [6,7,8,0xA,0xB,0xC] = s - 4
app2 s 'D' | s `elem` [1,0xB] = s + 2
app2 s 'D' | s `elem` [2,3,4,6,7,8] = s + 4
app2 s 'L' | s `elem` [3,4,6,7,8,9,0xB,0xC] = s - 1
app2 s 'R' | s `elem` [2,3,5,6,7,8,0xA,0xB] = s + 1
app2 s _ = s

result2 = hexify $ fold app2 5 input

hexify [] = ""
hexify (c:cs) | c < 10  = show c ++ hexify cs
hexify (c:cs) =
  let c' = case c of 
             10 -> 'A'
             11 -> 'B'
             12 -> 'C'
             13 -> 'D'
             14 -> 'E'
             15 -> 'F'
   in c' : hexify cs
