let rec calculateNaiveTaylorSeries x n =
    if n = 0 then x
    else
        let term = if n % 2 = 0 then 0.0 else if n % 4 = 1 then -1.0 else 1.0
        let num = float (n * 2 - 1)
        let rec factorial acc = function
            | 0 -> acc
            | n -> factorial (acc * float n) (n - 1)
        let denom = factorial 1.0 n
        term * (x ** num / denom) + calculateNaiveTaylorSeries x (n - 1)

let rec calculateSmartTaylorSeries x eps n term acc =
    if abs term < eps then acc
    else calculateSmartTaylorSeries x eps (n + 1) (term * (-x * x / ((float n) * (float (n + 1))))) (acc + term)

let printTable a b =
    printfn "x\tBuiltin\tSmart Taylor\tNaive Taylor"
    let eps = 1e-6
    let rec loop x =
        if x > b then ()
        else
            let builtinValue = sin x
            let smartValue = calculateSmartTaylorSeries x eps 1 x 0.0
            let naiveValue = calculateNaiveTaylorSeries x 10
            printfn "%f\t%f\t%f\t%f" x builtinValue smartValue naiveValue
            loop (x + 0.1)
    loop a

printTable 0.0 1.0
