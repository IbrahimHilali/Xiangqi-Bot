# Xiangqi Bot 

Xiangqi Bot is a Haskell Bot to deal with fen String to produce list of moves or get a move

## Intro
- [FEN](https://en.wikipedia.org/wiki/Forsyth–Edwards_Notation) : "rheagaehr/9/1c5c1/s1s1s1s1s/9/9/S1S1S1S1S/1C5C1/9/RHEAGAEHR r"
- rows [a..i] 
- colums [0...9]
- Move in format of "e2-e9" 
- listMove fen  -> "[e3-e4,e5-e4,f3-f5............]"

## Usage

```haskell
# run Tests XianggiTest 
 > ghci  XianggiTest.hs
 > main
# get all possiable moves r-> red , b-> black
> listMoves "rheagaehr/9/1c5c1/s1s1s1s1s/9/9/S1S1S1S1S/1C5C1/9/RHEAGAEHR r"
# get Random Move
> getMove "rheagaehr/9/1c5c1/s1s1s1s1s/9/9/S1S1S1S1S/1C5C1/9/RHEAGAEHR r"
```


## License
[MIT](https://choosealicense.com/licenses/mit/)

## Made By Ibrahim Hilali
[![paypal](https://www.paypalobjects.com/en_US/i/btn/btn_donate_SM.gif)](https://www.paypal.com/donate/?hosted_button_id=P8XXZB2BCWTXS)
