open System

// Define the transcendental algebraic equation
let equation1 x = x * Math.Tan(x) - 1.0/3.0
let equation2 x = (x + 1.0) * Math.Cos(x) - 1.0
let equation3 x = Math.Sin(x) + Math.Cos(x) - 1.0

// Iterations method
let rec iterationsMethod (func: float -> float) (guess: float) (tolerance: float) (maxIterations: int) (i: int) =
    let nextGuess = func guess
    if abs (guess - nextGuess) < tolerance || i >= maxIterations then
        nextGuess
    else
        iterationsMethod func nextGuess tolerance maxIterations (i + 1)

// Dichotomy method
let rec dichotomyMethod (func: float -> float) (a: float) (b: float) (tolerance: float) =
    let c = (a + b) / 2.0
    if abs (b - a) < tolerance then
        c
    else
        let fa = func a
        let fb = func b
        let fc = func c
        if fa * fc < 0.0 then
            dichotomyMethod func a c tolerance
        else
            dichotomyMethod func c b tolerance

// Newton's method
let rec newtonsMethod (func: float -> float) (deriv: float -> float) (guess: float) (tolerance: float) (maxIterations: int) (i: int) =
    let nextGuess = guess - (func guess / deriv guess)
    if abs (guess - nextGuess) < tolerance || i >= maxIterations then
        nextGuess
    else
        newtonsMethod func deriv nextGuess tolerance maxIterations (i + 1)

// Derivative of the equations
let derivative1 x = (Math.Tan x) + x / ((Math.Cos x) ** 2.0)
let derivative2 x = -Math.Sin(x) - (x + 1.0) * Math.Sin(x)
let derivative3 x = Math.Cos(x) - Math.Sin(x)

// Initial intervals for roots
let a1, b1 = 0.2, 1.0
let a2, b2 = -1.0, 0.0
let a3, b3 = 0.0, 1.0

// Personal number
let personalNumber = 123

// Perform calculations for each method and each equation
let resultIterations1 = iterationsMethod equation1 ((a1 + b1) / 2.0) 1e-8 100 0
let resultDichotomy1 = dichotomyMethod equation1 a1 b1 1e-8
let resultNewton1 = newtonsMethod equation1 derivative1 ((a1 + b1) / 2.0) 1e-8 100 0

let resultIterations2 = iterationsMethod equation2 ((a2 + b2) / 2.0) 1e-8 100 0
let resultDichotomy2 = dichotomyMethod equation2 a2 b2 1e-8
let resultNewton2 = newtonsMethod equation2 derivative2 ((a2 + b2) / 2.0) 1e-8 100 0

let resultIterations3 = iterationsMethod equation3 ((a3 + b3) / 2.0) 1e-8 100 0
let resultDichotomy3 = dichotomyMethod equation3 a3 b3 1e-8
let resultNewton3 = newtonsMethod equation3 derivative3 ((a3 + b3) / 2.0) 1e-8 100 0

// Output results
printf " %.10f      " resultIterations1
printf " %.10f      " resultDichotomy1
printfn " %.10f     " resultNewton1

printf " %.10f      " resultIterations2
printf " %.10f      " resultDichotomy2
printfn " %.10f     " resultNewton2

printf " %.10f      " resultIterations3
printf " %.10f      " resultDichotomy3
printf " %.10f      " resultNewton3