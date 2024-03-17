let rec calculateNaiveTaylorSeries x n =
    if n = 0 then -1.0/5.0
    else
        let term = - (float (n * 2)) * x ** (float n) / (5.0 ** (float (n + 1)))
        term + calculateNaiveTaylorSeries x (n - 1)

let rec calculateSmartTaylorSeries x eps n term acc =
    if abs term < eps then acc, n
    else calculateSmartTaylorSeries x eps (n + 1) (term * (-2.0 * x / 5.0)) (acc + term)

let builtinFunction x = 1.0 / (2.0 * x - 5.0)

let printTable a b =
    printfn "x\tBuiltin\tSmart Taylor\t# terms\tNaive Taylor\t# terms"
    let eps = 1e-6
    let rec loop x =
        if x > b then ()
        else
            let builtinValue = builtinFunction x
            let smartValue, smartTerms = calculateSmartTaylorSeries x eps 1 (-1.0/5.0) 0.0
            let naiveValue = calculateNaiveTaylorSeries x 10
            let naiveTerms = 10
            printfn "%f\t%f\t%f\t%d\t%f\t%d" x builtinValue smartValue smartTerms naiveValue naiveTerms
            loop (x + 0.1)
    loop a

printTable 0.0 2.0
