import System.Environment   
import Data.List 


traverseSteps ops = takeSteps (foldl folder ([],[]) ops)
takeSteps (x, y) = x


folder (xs, x:y:ys) "*" = (xs ++[generateOperand x y "*"],(x * y):ys)  
folder (xs,x:y:ys) "+" =(xs ++[generateOperand x y "+" ],(x + y):ys)
folder (xs,x:y:ys) "-" =(xs++[generateOperand x y "-"],(y - x):ys)
folder (xs, ys) numberString = (xs ,(read numberString):ys) 


generateOperand x y ops= 
  show(x) ++ ops ++ show (y) 
main = do  
    args <- getArgs  
    putStrLn "operations:"
    steps <- return(traverseSteps args)
    mapM putStrLn steps  