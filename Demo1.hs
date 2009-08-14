import Transition

main = printCommands $
       runTransition 200 $
       do light (Both ALL) white
          during 50 100 $ do light (Both A) red
          during 60 70 $ do light (Front A) blue
