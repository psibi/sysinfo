{-#LANGUAGE CPP#-}

import Test.Hspec
import Test.Hspec.Expectations.Contrib (isRight)
import System.SysInfo
import Foreign.C.Error
#if __GLASGOW_HASKELL__ <= 784
import Control.Applicative ((<$>))
#endif

instance Show Errno where
    show (Errno val) = show val

getRight :: (Show a, Show b) => Either a b -> b
getRight (Right b) = b
getRight x@(Left a) = error $ "Invalid value " ++ show x

main :: IO ()
main =
  hspec $
  do describe "sysInfo function" $
       do it "retuns valid data" $
            do val <- sysInfo
               val `shouldSatisfy` isRight
          it "uptime > 0" $
            do val <- getRight <$> sysInfo
               (uptime val) `shouldSatisfy` (> 0)
          it "freeram > 0" $
            do val <- getRight <$> sysInfo
               (freeram val) `shouldSatisfy` (> 0)

