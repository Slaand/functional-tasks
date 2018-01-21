import Data.Char (toUpper)
import Data.String

type Todo     = String
type TodoList = [Todo]

toDoList :: TodoList -> Char -> IO TodoList
toDoList xs 'V' = putStrLn (showTodoList xs) >> return xs
toDoList xs 'A' = do
  putStrLn "Что бы вы хотели добавить?"
  addTodo xs <$> getLine
toDoList [] 'D' = putStrLn (showTodoList []) >> return []
toDoList xs 'D' = do
    printStrs $ numberList xs
    putStr "Какую строку вы хотите удалить? "
    removeTodo xs <$> readLn
toDoList xs _ = do
    putStrLn "Операция не поддерживается"
    return xs

numberList :: TodoList -> [String]
numberList = zipWith (\a b -> show a ++ ". " ++ b) [1..]

printStrs :: [String] -> IO ()
printStrs = mapM_ putStrLn

showTodoList :: TodoList -> String
showTodoList [] = "Нету записей"
showTodoList xs = putStr ( unlines numberList )

addTodo :: TodoList -> Todo -> TodoList
addTodo = flip (:)

removeTodo :: TodoList -> Int -> TodoList
removeTodo = flip removeElem

removeElem :: Int -> [a] -> [a]
removeElem i xs = let (as, bs) = splitAt i xs
                  in as ++ drop 1 bs