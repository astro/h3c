import Transition

main = printCommands $
       runTransition 70 $
       do light (Both ALL) white
          during 30 60 $ do light (Both A) red
          during 40 50 $ do light (Front A) blue
