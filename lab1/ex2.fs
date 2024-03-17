let tolerance = 0.000001

let rec dichotomyMethod f a b = 
    if (b - a) <= tolerance then  
        ((a + b) / 2.)
    else
        let c = (a + b) / 2.
        if ((f b) * (f c)) < 0. then 
            dichotomyMethod f c b
        else
            dichotomyMethod f a c

let rec iterateMethod f acc =
    let currentResult = f acc
    if (abs (currentResult - acc)) <= tolerance then 
        currentResult
    else 
        iterateMethod f currentResult

let iterations phi x0 = iterateMethod (fun acc -> (phi acc)) x0

let newtonMethod f f' x0 = iterateMethod (fun acc -> acc - (f acc) / (f' acc)) x0

let equation1 x = 0.6 * 3.0 ** x - 2.3 * x - 3.0
let equation2 x = x * x - log(1.0 + x) - 3.0
let equation3 x = 2.0 * x * sin(x) - cos(x)

let equation1' x = 0.6 * 3.0 ** x - 2.3
let equation2' x = 2.0 * x - 1.0 / (1.0 + x)
let equation3' x = 2.0 * x * cos(x) + 2.0 * sin(x)

let phiFunction1 x = log(x) + 1.8
let phiFunction2 x = sqrt(log(1.0 + x) + 3.0)
let phiFunction3 x = acos(cos(x) / (2.0 * x))

let main = 
    printfn "%10.5f  %10.5f %10.5f" (dichotomyMethod equation1 2. 3.) (iterations phiFunction1 2.) (newtonMethod equation1 equation1' 2.5)
    printfn "%10.5f  %10.5f %10.5f" (dichotomyMethod equation2 2. 3.) (iterations phiFunction2 1.) (newtonMethod equation2 equation2' 0.5)
    printfn "%10.5f  %10.5f %10.5f" (dichotomyMethod equation3 0.4 1.) (iterations phiFunction3 1.5) (newtonMethod equation3 equation3' 1.5)