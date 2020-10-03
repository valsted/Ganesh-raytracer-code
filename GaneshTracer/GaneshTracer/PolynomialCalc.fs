namespace GaneshTracer

open System

module PolynomialCalc =
    //Function that solves any first degree polynomial
    let solveFirstDegree a b = 
        if a = 0.0 
        then None
        else let dist = -b / a
             if dist < 0.0
             then None
             else Some(dist)
    
    //Function that solves any second degree polynomial and returns the shortest solution
    let solveSecondDegree a b c= 
        let d = b*b-4.0*a*c
        match d with
            | n when n<0.0 -> None
            | n when n=0.0 -> let t = -b/(2.0*a)
                              if t < 0.0 then None else Some(t)
            | n            -> let sqrtD = sqrt(d)
                              let divideVal = 1.0/(2.0*a)
                              let t1 = (-b+sqrtD) * divideVal
                              let t2 = (-b-sqrtD) * divideVal 
                              if t1 < 0.0
                              then
                                if t2 < 0.0 then None else Some (t2)
                              else
                                if t2 < 0.0 then Some(t1) else Some(min t1 t2)

    //Function that solves any second degree polynomial and returns both solutions
    let solveSecondDegreeBothValues a b c : ((float option)*(float option)) option= 
        let d = b*b-4.0*a*c
        match d with
            | n when n<0.0 -> None
            | n when n=0.0 -> if a=0.0 then None else
                              let t = -b/(2.0*a)
                              if t < 0.0 then None else Some (Some t, None)
            | n            -> if a=0.0 then None else
                              let sqrtD = sqrt(d)
                              let divideVal = 1.0/(2.0*a)
                              let t1 = (-b+sqrtD) * divideVal  
                              let t2 = (-b-sqrtD) * divideVal  
                              Some (Some t1, Some t2)
    
    let getValueOrDefault (map:Map<int,float>) (n:int) =
        let value = map.TryFind n
        match value with
        | None    -> 0.0
        | Some(f) -> f
    
    let findBiggestKey (map:Map<int, float>) =
        let mutable maxkey = -1
        for(KeyValue(i,f)) in map do
            if i > maxkey then maxkey <- i
        maxkey

    let differentiateMap (map:Map<int, float>) : Map<int,float> =
        let list = Map.fold (fun list exp fval -> if exp = 0 then list else (exp - 1, fval * (float) exp) :: list) [] map
        Map.ofList list
    
    let polynomialMult map1 map2 =
        let innerFold tmpMap outerExp outerVal  =
            Map.fold (fun (m:Map<int,float>) exp fval -> 
                            let result = fval * outerVal
                            let priorval = getValueOrDefault m (outerExp + exp)
                            m.Add ((outerExp + exp), priorval + result)) tmpMap map2
        Map.fold innerFold Map.empty map1

    let polynomialSubtract map1 map2 =
        Map.fold (fun (m:Map<int,float>) exp fval -> 
                    let newVal = getValueOrDefault map1 exp - fval
                    if abs newVal < 0.00000001 //Epsilon value
                    then m.Remove(exp)
                    else m.Add(exp, newVal)) map1 map2

    let polynomialAddition map1 map2 =
        Map.fold (fun (m:Map<int,float>) exp fval -> 
                    let newVal = getValueOrDefault map1 exp + fval
                    m.Add(exp, newVal)) map1 map2 

    let divideTerms (i1, (f1:float)) (i2, f2) =
        let newexp = i1 - i2
        (newexp, f1/f2)

    (*Based on pseudo-code from https://en.wikipedia.org/wiki/Polynomial_long_division
        function n / d:
            require d ≠ 0
            q ← 0
            r ← n       # At each step n = d × q + r
             while r ≠ 0 AND degree(r) ≥ degree(d):
                t ← lead(r)/lead(d)     # Divide the leading terms
                q ← q + t
                r ← r − t * d
        return (q, r)*)
    let polynomialLongDivision (map1:Map<int,float>) (map2:Map<int,float>) : (Map<int,float> * Map<int,float>) =
        let maxD = findBiggestKey map2
        let maxDVal = Map.find maxD map2
        let rec inner (r:Map<int,float>) (d:Map<int,float>) (q:Map<int,float>) : (Map<int,float>*Map<int,float>) =
            let maxR = findBiggestKey r
            if maxR = -1 || maxR < maxD
            then (q, r)
            else let t = Map.ofList [divideTerms (maxR, Map.find maxR r) (maxD, maxDVal)]
                 inner (polynomialSubtract r (polynomialMult t d)) d (polynomialAddition q t)
        inner map1 map2 Map.empty
    
    //Sturm sequence algorithm
    let sturmSequence (poly: Map<int, float>) : (Map<int, float> list) =
        let dif = differentiateMap poly
        let rec completeSequence (seq:Map<int, float> list) =
            match seq with
            | m1 :: m2 :: _ -> if findBiggestKey m1 < 1
                               then seq
                               else completeSequence ((polynomialMult (Map.ofList [(0, -1.0)]) (snd (polynomialLongDivision m2 m1))) :: seq)
        completeSequence [dif; poly]
    
    let calculateValue (map:Map<int, float>) (x:float) = 
        Map.fold (fun acc exp fval -> 
                    let xe = match exp with
                             | 0 -> 1.0
                             | 1 -> x
                             | 2 -> x * x
                             | 3 -> x * x * x
                             | 4 -> let x2 = x * x
                                    x2 * x2
                             | 5 -> let x2 = x * x
                                    x2 * x2 * x
                             | 6 -> let x2 = x * x
                                    x2 * x2 * x2
                             | n -> x ** (float)n
                    acc + fval * xe) 0.0 map
    
    //Counts the number of sign changes found in a Sturm sequence
    let countSignChanges (seq: Map<int,float> list) (x:float) =
        let rec runthrough (inner: Map<int, float> list) (negative:bool) (acc:int) =
            match inner with
            | [] -> acc
            | next :: rst -> let neg = calculateValue next x < 0.0
                             if neg = negative
                             then runthrough rst neg acc
                             else runthrough rst neg acc + 1
        match seq with
        | []           -> 0
        | first :: rst -> let negative = calculateValue first x < 0.0
                          runthrough rst negative 0
    
    //Counts the number of roots found by running a Sturm sequence
    let countRoots (seq: Map<int,float> list) (min:float) (max: float) = abs(countSignChanges seq max - countSignChanges seq min)
    
    //Newton-Ralphson algorithm that converges on the closest root within a specified interval
    let newtonRalphson (poly: Map<int,float>) (initial: float) =
        let valCalc = calculateValue poly
        let difCalc = calculateValue (differentiateMap poly)

        let rec inner (guess: float) (guesses: int) =
            let value = valCalc guess
            if abs(value) < 0.000001
            then Some(guess)
            else if guesses > 55
                 then System.Console.WriteLine("Newton-Ralphson didn't terminate after 55 tries")
                      None              //Newton-Raphson should fail, when it cannot find a root.
                 else let newGuess = guess - value / (difCalc guess)
                      inner newGuess (guesses + 1)    
        inner initial 0
    
    //Finds a smaller interval for the root that exist in the given interval
    let betterGuess (lo:float) (hi:float) (sturmSeq:Map<int,float> list) = 
        //Finds a better guess for the minimum value of the interval
        let rec minGuess (low:float) (high:float) (value:float) (sturm:Map<int,float> list) (depth:int) = 
            let min = low + value
            let max = high
            if max-min < 0.00001 then low
            else
                 let roots = countRoots sturm min max
                 if roots < 1 then low
                 else 
                      if depth < 50 then minGuess min max value sturm (depth+1)
                      else min
        
        //Finds a better guess for the maximum value of the interval
        let rec maxGuess (low:float) (high:float) (value:float) (sturm:Map<int,float> list) (depth:int) = 
            let min = low
            let max = high - value
            if max-min < 0.00001 then high
            else
                 let roots = countRoots sturm min max
                 if roots < 1 then high
                 else 
                      if depth < 50 then minGuess min max value sturm (depth+1)
                      else max

        let oneMinPct = lo*0.01
        let oneMaxPct = hi*0.01

        let minimum = minGuess lo hi oneMinPct sturmSeq 0
        let maximum = maxGuess lo hi oneMaxPct sturmSeq 0
        (minimum,maximum)
    
    //Given an interval containing one root the function will improve that interval to find the smallest possible interval where the root can be found
    let rec improveInterval (min:float) (max:float) (sturm:Map<int,float> list) (depth:int) mincount maxcount =
        if max-min < 0.00001 then None
        else
            if mincount = maxcount then None
            else if depth >= 15 then Some(min,max) //Cutoff
            else let mid = min + (max - min) * 0.5
                 let first = improveInterval min mid sturm (depth+1) mincount (countSignChanges sturm mid)
                 if first = None
                 then let second = improveInterval mid max sturm (depth+1) (countSignChanges sturm mid) maxcount
                      if second = None then Some(min,max) //Did not find a better interval, return recent best known interval
                      else second //Reached cutoff
                 else first       //Reached cutoff
    
    //Function that can be used to solve polynomials of any degree
    let solveLargePolynomial (map1:Map<int,float>) : float option =
        let map = Map.fold(fun (m:Map<int, float>) i f -> if abs f < 0.001
                                                          then m
                                                          else m.Add((i, f))) Map.empty map1
        //Calculates the Sturm Sequence
        let sturm = sturmSequence map
        let signcount = countSignChanges sturm
        //Finds the smallest possible interval with one root
        let rec roughInterval (min:float) (max:float) mincount maxcount : (float * float) option =
            if max-min < 0.000001 then None
            else
                let roots = abs(mincount - maxcount)
                if roots = 0 then None
                else if roots = 1 then improveInterval min max sturm 0 mincount maxcount
                else let mid = min + (max - min) * 0.5
                     let first = roughInterval min mid mincount (signcount mid)
                     if first = None then roughInterval mid max (signcount mid) maxcount
                     else first
        
        let intervalOpt = roughInterval 0.0 100.0 (signcount 0.0) (signcount 100.0)
        if intervalOpt = None then None     //No roots were found within the 0.0 - 100.0 interval
        else let interval = Option.get intervalOpt
             let minimum = fst interval
             let maximum = snd interval
             let mutable guess = newtonRalphson map (minimum + (maximum - minimum)*0.5)                     //First valid guess
             if guess.IsSome && (guess.Value < minimum - 0.0000875 || guess.Value > maximum + 0.00000875)  //Checks whether the guess is within the desired interval
             then let interval2 = betterGuess minimum maximum sturm
                  let betterMin = fst interval2
                  let betterMax = snd interval2
                  guess <- newtonRalphson map (betterMin + (betterMax - betterMin)*0.5)
                  if guess.IsSome && (guess.Value < betterMin || guess.Value > betterMax) 
                  then guess <- Some(betterMin+(betterMax-betterMin)*0.5)    
             guess

    //Used to solve a polynomial of any degree
    let solvePolynomial (values:Map<int, float>) : float option =
        let func = getValueOrDefault values
        match (findBiggestKey values) with
        | 0            -> if func 0 = 0.0 then Some(0.0) else None
        | 1            -> solveFirstDegree (func 1) (func 0)
        | 2            -> solveSecondDegree (func 2) (func 1) (func 0)
        | n when n > 0 -> solveLargePolynomial values
        | _            -> failwith "solvePolynomial: Invalid polynomial. No values included."
