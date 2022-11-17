import Unit  -- crude assertions for unit tests

----------------------------- Employee Type -----------------------------
type Name = String
type Age = Int
type Department = String
type Salary = Float

-- an Employee is a Haskell record.  Note that Haskell creates
-- accessor functions for each field.  For example, given e :: Employee,
-- (emplDept e) :: Department
data Employee = Employee {
  emplName :: Name,
  emplAge :: Age,
  emplDept :: Department,
  emplSalary :: Salary
} deriving ( Eq, Show )

employees :: [Employee]
employees = [
    Employee "tom" 33 "cs" 85000.00,
    Employee "joan" 23 "ece" 110000.00,
    Employee "bill" 29 "cs" 69500.00,
    Employee "john" 28 "me" 58200.00,
    Employee "sue" 19 "cs" 22000.00
  ]
  

----------------------------- deptEmployees -----------------------------
-- #1: 10-points
-- Given a list empls of Employee and a Department dept, 
-- (deptEmployees empls dept) must return those employees in
-- empls which have emplDept == dept.
-- Restriction: MUST use recursion
deptEmployees :: [Employee] -> Department -> [Employee]

deptEmployees [] dept = []
deptEmployees (h:t) dept
  | emplDept h == dept = h: deptEmployees t dept
  | otherwise =  deptEmployees t dept



testDeptEmployees = do
  assertEq "deptEmployees cs"
           (deptEmployees employees "cs")
           (filter (\e -> emplDept e == "cs") employees)
  assertEq "deptEmployees ece"
           (deptEmployees employees "ece")
           (filter (\e -> emplDept e == "ece") employees)
  assertEq "deptEmployees ce"
           (deptEmployees employees "ce")
           (filter (\e -> emplDept e == "ce") employees)


-------------------------- comprDeptEmployees ---------------------------
-- #2: 10-points
-- comprDeptEmployees has same spec as deptEmployees.
-- Restriction: MUST be implemented using list comprehension.
comprDeptEmployees :: [Employee] -> Department -> [Employee]

comprDeptEmployees bs dept = [b | b<-bs, emplDept b == dept]


testComprDeptEmployees = do
  assertEq "comprDeptEmployees cs"
           (comprDeptEmployees employees "cs")
           (filter (\e -> emplDept e == "cs") employees)
  assertEq "comprDeptEmployees ece"
           (comprDeptEmployees employees "ece")
           (filter (\e -> emplDept e == "ece") employees)
  assertEq "comprDeptEmployees ce"
           (comprDeptEmployees employees "ce")
           (filter (\e -> emplDept e == "ce") employees)

------------------------ employeesSalarySum -----------------------------
-- #3: 10-points
-- Given a list empls of Employee, (employeesSalarySum empls) must
-- return the sum of the emplSalary of all the employees in empls.
-- Restriction: May NOT use recursion or list comprehension.
employeesSalarySum :: [ Employee ] -> Float

employeesSalarySum x = sum (map (\ (Employee _ _ _ sal) -> sal ) x)

testEmployeesSalarySum = do
  assertEq "employeesSalarySum all" (employeesSalarySum employees) 344700.0
  assertEq "employeesSalarySum empty" (employeesSalarySum []) 0.0

--------------------------- quadraticRoots ------------------------------
-- #4: 10-points
-- Given quadratic equation a*x**2 + b*x + c, (quadraticRoots a b c)
-- should return the pair containing the roots of the equation.
-- The first element in the returned pair should use the positive 
-- square-root of the discriminant, the second element should use the 
-- negative square-root of the discriminant.  Need not handle complex
-- roots.
quadraticRoots :: Floating t => t -> t -> t -> (t, t)

quadraticRoots a b c = (x, y)
                            where
                              x = e + sqrt d / (2 * a)
                              y = e - sqrt d / (2 * a)
                              d = b * b - 4 * a * c
                              e = - b / (2 * a)


testQuadraticRoots = do
  assertEq "quadraticRoots \"x**2 - 25 = 0\""
           (quadraticRoots 1 0 (-25))
           (5.0, (-5.0))
  assertEq "quadraticRoots \"x**2 + 5x + 6 = 0\""
           (quadraticRoots 1 5 6)
           ((-2.0), (-3.0))
  assertEq "quadraticRoots \"x**2 - 5x + 6 = 0\""
           (quadraticRoots 1 (-5) 6)
           (3.0, 2.0)
  assertEq "quadraticRoots \"2*x**2 + 10x - 12 = 0\""
           (quadraticRoots 2 10 (-12))
           (1.0, (-6.0))

--------------------------------- expn ---------------------------------
-- #5: 15-points
-- Given a x (expn x) should return the infinite series
-- 1 + x + x**2/2! + x**3/3! + x**4/4!
-- expn :: (Floating a)  => a -> [a]
-- Hint: use a local auxiliary function

expn x = expn2 0 x
   where expn2 acc x = ((x**acc / (product [1..acc])) : expn2 (acc+1) x)
         

-- epsilon equality for floats; does not work if close to 0.0
floatEq f1 f2 = (abs (f2 - f1)/f1) < 0.0001
allFloatEq f1s f2s = all (\(a, b) -> floatEq a b) (zip f1s f2s)

testExpn = do
  assertTrue "expn 1 first 3" (allFloatEq (take 3 (expn 1)) [1.0, 1.0, 0.5])
  assertTrue "expn 1 first 6"
             (allFloatEq (take 5 (expn 1))
                         [1.0, 1.0, 0.5, 1/6.0, 1/24.0, 1/120.0])
  assertTrue "expn 1 sum e"
             (floatEq (foldl (+) 0 (take 50 (expn 1))) (exp 1))
  assertTrue "expn 2 first 3" (allFloatEq (take 3 (expn 2)) [1.0, 2.0, 2.0])
  assertTrue "expn 2 first 6"
             (allFloatEq (take 5 (expn 2))
                         [1.0, 2.0, 2.0, 8/6.0, 16/24.0, 32/120.0])
  assertTrue "expn sum e**2"
             (floatEq (foldl (+) 0 (take 50 (expn 2))) (exp 2))
 
------------------------------ charIndexes ------------------------------
-- #6: 15-points
-- Given a string s and char c, charIndexes s c should return a
-- list of indexes in s at which c occurs.
-- Restriction: May NOT use recursion.
-- Hint: pair each char in the string with its index and then select
-- those indexes with matching char.
charIndexes :: String -> Char -> [Int]

charIndexes str char = [ b | (a, b) <- zip str [0..], a == char ] 

testCharIndexes = do
  assertEq "charIndexes \"hello world\" 'o'"
           (charIndexes "hello world" 'o')
           [4, 7]
  assertEq "charIndexes \"hello world\" 'x'"
           (charIndexes "hello world" 'x')
           []

------------------------------- Tree Type -------------------------------
-- A Tree of some type t is either a Leaf containing a value of type t,
-- or it is an internal node (with constructor Tree) with some left
-- sub-tree, a value of type t and a right sub-tree.
data Tree t = Leaf t
            | Tree (Tree t) t (Tree t)
            deriving (Eq, Show)

------------------------------- foldTree --------------------------------
-- #7: 10-points
-- Fold tree to a single value using a ternary treeFn and unary
-- leafFn.  Specifically, (foldTree treeFn leafFn tree) will
-- fold tree as follows:
-- If tree is a Tree node, then it's folded value is the result of
-- applying ternary treeFn to the result of folding the left sub-tree,
-- the value stored in the Tree node and the result of folding the
-- right sub-tree.
-- If tree is a Leaf node, then the result of the fold is the result of
-- applying the unary leafFn to the value stored within the Leaf node.
-- May use recursion.
foldTree :: (t1 -> t -> t1 -> t1) -> (t -> t1) -> Tree t -> t1

foldTree treeFn leafFn tree =
  case tree of
    Leaf v -> leafFn v
    Tree leftTree lq rightTree -> treeFn (foldTree treeFn leafFn leftTree) lq (foldTree treeFn leafFn rightTree)


testFoldTree = do
  assertEq "foldTree (left + v + right) (2*v)"
           (foldTree (\left v right -> left + v + right)
                    (\v -> 2*v)
                    (Tree (Tree (Leaf 1) 2 (Leaf 3))
                          4
                          (Tree (Leaf 4) 5 (Leaf 6))))
           39
  assertEq "foldTree (left + (length v) + right) (2*(length v))"
           (foldTree (\left v right -> left + (length v) + right)
                    (\v -> 2*(length v))
                    (Tree (Tree (Leaf [1]) [1, 2] (Leaf [1, 2, 1]))
                          [1, 2, 3, 4]
                          (Tree (Leaf [1, 2, 3, 4])
                                [1, 2, 3, 4, 5]
                                (Leaf [1, 2, 3, 4, 5, 6]))))
           39

------------------------------ flattenTree ------------------------------
-- #8: 10-points
-- Return list containing flattening of tree.  The elements of the
-- list correspond to the elements stored in the tree ordered as per 
-- an in-order traversal of the tree. 
-- Restriction: May NOT use recursion.  MUST be implemented using foldTree.
flattenTree :: Tree a -> [a]

flattenTree = foldTree (\ts t rs -> ts ++ t : rs) (\t -> [t])


testFlattenTree = do
  assertEq "flattenTree Tree Int"
           (flattenTree (Tree (Tree (Leaf 1) 2 (Leaf 3))
                              4
                              (Tree (Leaf 4) 5 (Leaf 6))))
           [1, 2, 3, 4, 4, 5, 6]
  assertEq "flattenTree Tree [Int]"
           (flattenTree (Tree (Tree (Leaf [1]) [1, 2] (Leaf [1, 2, 1]))
                              [1, 2, 3, 4]
                              (Tree (Leaf [1, 2, 3, 4])
                                    [1, 2, 3, 4, 5]
                                    (Leaf [1, 2, 3, 4, 5, 6]))))
           [[1], [1, 2], [1, 2, 1], [1, 2, 3, 4],
            [1, 2, 3, 4], [1, 2, 3, 4, 5], [1, 2, 3, 4, 5, 6]]
  assertEq "flattenTree Tree String"
           (flattenTree (Tree (Tree (Leaf "twas") "brillig" (Leaf "and"))
                              "the"
                              (Tree (Leaf "slighty") "toves" (Leaf "did"))))
           [ "twas", "brillig", "and", "the", "slighty", "toves", "did" ]

---------------------------- catenateTree -------------------------------
-- #9: 10-points
-- Given tree of type (Tree [t]) return list which is concatenation
-- of all lists in tree.
-- Restriction: May NOT use recursion. MUST be implemented using flattenTree.
catenateTreeLists :: Tree [a] -> [a]

catenateTreeLists tree = foldr (++) [] (flattenTree tree)

testCatenateTreeLists = do
  assertEq "catenateTreeLists Tree [Int]"
           (catenateTreeLists (Tree (Tree (Leaf [1]) [1, 2] (Leaf [1, 2, 1]))
                              [1, 2, 3, 4]
                              (Tree (Leaf [1, 2, 3, 4])
                                    [1, 2, 3, 4, 5]
                                    (Leaf [1, 2, 3, 4, 5, 6]))))
           [1, 1, 2, 1, 2, 1, 1, 2, 3, 4,
            1, 2, 3, 4, 1, 2, 3, 4, 5, 1, 2, 3, 4, 5, 6]
  assertEq "catenateTreeLists Tree String"
           (catenateTreeLists (Tree (Tree (Leaf "twas") "brillig" (Leaf "and"))
                              "the"
                              (Tree (Leaf "slighty") "toves" (Leaf "did"))))
           "twasbrilligandtheslightytovesdid"


----------------------------- Run All Tests -----------------------------

testAll = do
  testDeptEmployees
  testComprDeptEmployees
  testEmployeesSalarySum
  testQuadraticRoots
  testExpn
  testCharIndexes
  testFoldTree
  testFlattenTree
  testCatenateTreeLists


