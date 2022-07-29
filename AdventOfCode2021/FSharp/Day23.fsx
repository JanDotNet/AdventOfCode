// let rec sieve xs count maxNumb = 
//     match count = maxNumb with
//     | true -> xs
//     | false when (Array.contains count xs) -> 
//         xs
//         |> Array.filter (fun ys -> ys % count <> 0. || ys = count)
//         |> fun filteredArr -> sieve filteredArr (count + 1.) maxNumb
//     | false -> sieve xs (count + 1.) maxNumb


let rec sieve xs count maxNumb = 
    if count = maxNumb then xs
    else
        let xs' = if xs |> Array.contains count 
                  then xs |> Array.filter (fun ys -> ys % count <> 0. || ys = count)
                  else xs
        sieve xs' (count + 1.) maxNumb

let findPrimes maxNumb = 
    sieve [|2. .. maxNumb|] 2. maxNumb


#time
findPrimes 30000.
