namespace GaneshTracer


module ExprToPoly =
    open System
    open Base
    open ExprParse

    (* Converts expression to a string representation *)
    let rec printExpr = function
      | FNum c -> string(c)
      | FVar s -> s
      | FAdd(e1, e2) -> "(" + (printExpr e1) + " + " + (printExpr e2) + ")"
      | FSubtract(e1, e2) -> "(" + (printExpr e1) + " - " + (printExpr e2) + ")"
      | FMult(e1, e2) -> (printExpr e1) + " * " + (printExpr e2)
      | FDiv(e1, e2) -> (printExpr e1) + " / " + (printExpr e2)
      | FExponent(e, n) -> "(" + (printExpr e) + ")^" + string(n)
      | FRoot(e, n) -> "(" + (printExpr e) + ")_" + string(n)

    (* Substitutes variables with arbitrary expressions *)
    let rec subst e (v, ex) =
      match e with
      | FNum c -> FNum c
      | FVar s -> if s.Equals(v) then ex else FVar s
      | FAdd(ex1, ex2) -> FAdd((subst ex1 (v, ex)), (subst ex2 (v, ex)))
      | FSubtract(ex1, ex2) -> FSubtract((subst ex1 (v, ex)), (subst ex2 (v, ex)))
      | FMult(ex1, ex2) -> FMult((subst ex1 (v, ex)), (subst ex2 (v, ex)))
      | FDiv(ex1, ex2) -> FDiv((subst ex1 (v, ex)), (subst ex2 (v, ex)))
      | FExponent(ex1, i) -> FExponent((subst ex1 (v, ex)), i)
      | FRoot(ex1, i) -> FRoot((subst ex1 (v, ex)), i)

    (* Substitutes variables with arbitrary expressions *)
    let rec substFunc e f =
      match e with
      | FNum c -> FNum c
      | FVar s -> f s
      | FAdd(ex1, ex2) -> FAdd((substFunc ex1 f), (substFunc ex2 f))
      | FSubtract(ex1, ex2) -> FSubtract((substFunc ex1 f), (substFunc ex2 f))
      | FMult(ex1, ex2) -> FMult((substFunc ex1 f), (substFunc ex2 f))
      | FDiv(ex1, ex2) -> FDiv((substFunc ex1 f), (substFunc ex2 f))
      | FExponent(ex, i) -> FExponent((substFunc ex f), i)
      | FRoot(ex, i) -> FRoot((substFunc ex f), i)

    (*The datatypes used to store an expression after simplification*)
    type atom = ANum of float | AExponent of string * int 
                         | ADiv of atom list list * atom list list | ARoot of atom list list * int
    type atomGroup = atom list  
    type simpleExpr = SE of atomGroup list
    let isSimpleExprEmpty (SE ags) = ags = [] || ags = [[]]

    (* Converts atom to a string representation *)
    let rec printAtom = function
      | ANum c -> string(c)
      | AExponent(s, 1) -> s
      | AExponent(s, n) -> s+"^"+(string(n))
      | ADiv(al1, al2)  -> "(" + ppSimpleExpr (SE(al1)) + ")/(" + ppSimpleExpr (SE(al2)) + ")"
      | ARoot(ex, n) -> "(" + ppSimpleExpr (SE(ex)) + ")_"+(string(n))


    (* Converts atom group to a string representation *)
    and printAtomGroup ag = String.concat "*" (List.map printAtom ag)

    (* Converts simple expression to a string representation *)
    and ppSimpleExpr (SE ags) = String.concat "+" (List.map printAtomGroup ags)

    (* Converts two expressions being multiplied into their product *)
    let rec combine xss = function
      | [] -> []
      | ys::yss -> List.map ((@) ys) xss @ combine xss yss

    (*Evaluate an atom list as a single constant if possible*)
    let evaluateAlAsConstant (expr: atom list) : float option =
        let rec product (expr: atom list) acc =
            match expr with
            | []               -> Some(acc)
            | ANum(f) :: rst   -> product rst (acc * f)
            | _                -> None
        product expr 1.0

    (*Evaluates an atom list list as a single constant if possible*)
    let evaluateExprAsConstant (expr: atom list list) : float option =
        let rec sum (expr: atom list list) acc =
            match expr with
            | []        -> Some(acc)
            | al :: rst -> let alVal = evaluateAlAsConstant al
                           if alVal = None
                           then None
                           else sum rst (acc + alVal.Value)
        sum expr 0.0

    (* Simplifies expressions and converts to atom list list*)
    let rec simplify = function
      | FNum c when c = 0.0              -> []
      | FNum c                           -> [[ANum c]]
      | FVar s                           -> [[AExponent(s,1)]]
      | FAdd(e1,e2) when e1 = FNum(0.0)  -> simplify e2
      | FAdd(e1,e2) when e2 = FNum(0.0)  -> simplify e1
      | FAdd(e1,e2)                      -> simplify e1 @ simplify e2
      | FSubtract(e1, e2)                -> simplify e1 @ List.map(fun (a:atom list) -> ANum -1.0 :: a) (simplify e2)
      | FMult(e1,e2) when e1 = FNum(0.0) -> []
      | FMult(e1,e2) when e2 = FNum(0.0) -> []
      | FMult(e1,e2)                     -> combine (simplify e1) (simplify e2)
      | FDiv(e1,e2)                      -> let divisor = simplify e2
                                            match evaluateExprAsConstant divisor with
                                            | None      -> [[ADiv ((simplify e1), divisor)]]
                                            | Some(0.0) -> failwith "Simplify: Division by zero is not allowed"
                                            | Some(f)   -> simplify (FMult(e1, FNum (1.0 / f)))
      | FExponent(e,n) when n < 0        -> [[ADiv ([[ANum 1.0]], (simplify (FExponent(e, -n))))]]
      | FExponent(e,0)                   -> [[ANum 1.0]]
      | FExponent(e,1)                   -> simplify e
      | FExponent(e,n) when n >= 2       -> simplify (FMult(e, (FExponent(e, n - 1))))
      | FRoot(e, n) when n < 1           -> simplify (FDiv(FNum (0.0), FRoot(e, -n)))
      | FRoot(e, 0)                      -> failwith "Simplify: Root of degree 0 is undefined"
      | FRoot(e, 1)                      -> simplify e
      | FRoot(e, n)                      -> let radical = simplify e
                                            match evaluateExprAsConstant radical with
                                            | None                 -> [[ARoot(radical, n)]]
                                            | Some(f) when f < 0.0 -> failwith "Simplify: Roots of negative values not supported."
                                            | Some(f)              -> [[ANum(f ** (1.0/(float)n))]]
      | _                                -> failwith "simplify failed"
    
    
    //The following functions are used to simplify an expression

    (* Updates a map with values based on how many times a specific exponent is present in the atom group *)
    let updateMapValues exp v (values:Map<string, int>) = 
        let va = Map.tryFind exp values
        match va with
        | None       -> (values.Add(exp, v))
        | Some(some) -> (values.Add(exp, (some + v)))
    
    (*Splits an atomGroup into the a constant and a list of variables*)
    let rec splitAg (ag:atomGroup) : (atom list * float) =
        let rec iter (ag:atomGroup) (complex: atom list) (map:Map<string, int>) (acc:float) =
            match ag with
            | []                  -> let list = Map.fold (fun list s exp -> AExponent(s, exp) :: list) complex map
                                     (list, acc)
            | ANum(f)::rst        -> iter rst complex map (acc * f)
            | AExponent(s,i)::rst -> iter rst complex (updateMapValues s i map) acc
            | ARoot(al, n) :: rst -> iter rst (ARoot(joinAtomLists al, n) :: complex) map acc
            | ADiv(al1, al2) :: rst -> iter rst (ADiv(joinAtomLists al1, joinAtomLists al2) :: complex) map acc
        iter ag []  Map.empty 1.0

    (* Groups atom groups according to how many times a specific atom group is present in the map  *)
    and addAGToMap map (ag:atomGroup, f: float) =
      match Map.tryFind ag map with
      | None   -> map.Add(ag, f)
      | Some v -> map.Add(ag, (v+f))

    (*Joins atomlists with identical variables into one*)
    and joinAtomLists (origin: atom list list) : atom list list =
        let mapping = List.fold(fun map al -> addAGToMap map (splitAg al)) Map.empty origin
        Map.fold(fun list al f -> if f = 0.0
                                  then list
                                  else if f = 1.0
                                       then if al = []
                                            then ([ANum(f)]::list)
                                            else (al::list)
                                       else (ANum(f)::al)::list) [] mapping

    //The function below are used to compress a simpleExpr

    (*Converts the map into an atom group that is now simplified *)
    let mapToAtomList (roots, map:Map<string, int>, c) =
      if c = 0.0 then [] else
      let al = Map.foldBack (fun k v s -> AExponent(k, v)::s) map []
      let tmp = if c = 1.0 then if map = Map.empty 
                                then ANum(c)::al //The 1 has an effect on the result, and are therefore added.
                                else al //The 1 does not have an effect on the result, and are thrown away.
                else ANum(c)::al
      roots @ tmp
    
    
    (* Simplifies an atom group by reducing exponents and multiplying constants*)
    let rec simplifyAtomList (al: atom list) : atom list =
      let rec inner agx complex map acc =
        match agx with
        | []                   -> (complex, map, acc)
        | ANum(x)::tail        -> inner tail complex map (x * acc)
        | AExponent(s,i)::tail -> inner tail complex (updateMapValues s i map) acc
        | ARoot(al, n) :: tail -> inner tail (ARoot(compress al, n)::complex) map acc
        | ADiv(al1, al2)::tail -> inner tail (ADiv(compress al1, compress al2)::complex) map acc
      mapToAtomList (inner al [] Map.empty 1.0)

    (*Compresses similar values of an expression*)
    and compress atll =
        joinAtomLists (List.fold (fun list al -> simplifyAtomList al :: list) [] atll)


    //The functions below are used to remove divisions

    //The following functions are used to multiply and add divisions

    (*Converts a division into a tuple for access to the dividend and divisor*)
    let getDivValues (div:atom) =
        match div with
        | ADiv(al1, al2) -> (al1, al2)
        | _              -> failwith "getDivValues: Called with nondiv"

    (*Multiplies a list of divisions to create a single division*)
    let multiplyDivisions (divs: atom list) : atom=
        let rec joinNext (divs: atom list) acc =
            let (t, b) = getDivValues acc
            match divs with
            | []                    -> ADiv(t, b)
            | ADiv(top, btm) :: rst -> joinNext rst (ADiv(combine t top, combine b btm))
            | _                     -> failwith "MultiplyDivisions: This should only be called with divisions"
        match divs with
        | div :: rst -> joinNext rst div
        | _          -> failwith "MultiplyDivisions: This function should only be called with divisions"
    

    (*Helper function used to add divisions to each other*)
    let joinDivisions (divs: atom list) =
        let rec joinNext (divs: atom list) div =
            let (t, b) = getDivValues div
            match divs with
            | []                    -> ADiv(t, b)
            | ADiv(top, btm) :: rst -> joinNext rst (ADiv(combine t btm @ combine b top, combine b btm))
            | _                     -> failwith "joinDivisions: This should only be called with divisions"
        match divs with
        | []         -> None
        | div :: rst -> Some(joinNext rst div)


    (*Inserts an empty map as the value if the key was unused*)
    let insertEmpty divs divisorKey =
        if Map.tryFind divisorKey divs = None then divs.Add(divisorKey, Map.empty) else divs

    (*Updates the division map used to join divisions with identical divisors*)
    let updateDivMap (divs:Map<atom list list, Map<atom list, float>>) (dividend: atom list list) (divisor: atom list list) =
        let divisor' = joinAtomLists divisor
        
        let divs' = insertEmpty divs divisor'
        let newVal = List.fold(fun (values: Map<atom list, float>) (al: atom list) -> 
                         match splitAg al with
                         | (_, 0.0) -> values
                         | (key, f) -> let va = Map.tryFind key values
                                       match va with
                                       | None       -> (values.Add(key, f))
                                       | Some(some) -> (values.Add(key, some + f))) (Map.find divisor' divs') dividend
        divs'.Add(divisor', newVal)


    (*Joins simular divisions and adds all divisions to eachother. Will return None if all divisions cancels eachother out*)
    let addDivisions (divs: atom list) : atom option =
        let rec mapDivisions (divs: atom list) (map:Map<atom list list, Map<atom list, float>>) =
            match divs with
            | []                    -> map
            | ADiv(top, btm) :: rst -> mapDivisions rst (updateDivMap map top btm)
            | _                     -> failwith "joinDivisions: This should only be called with divisions"
        
        let joinedDivs = Map.fold(fun list btm (top: Map<atom list, float>) -> 
                                          let topExpr = Map.fold(fun list (al:atom list) (f:float) -> 
                                                if f = 0.0
                                                then list
                                                else (ANum(f)::al)::list) [] top
                                          let topvalue = evaluateExprAsConstant topExpr
                                          if Option.isSome topvalue && topvalue.Value = 0.0
                                          then list
                                          else ADiv(topExpr, btm)::list) [] (mapDivisions divs Map.empty)

        joinDivisions joinedDivs

    (*Converts an expression and a list of divisions into once single division*)
    let compressDivisions (agl: atomGroup list, divs: atom list)=
        match divs with
        |[] -> (agl, None)
        |d  -> let bigDiv = addDivisions divs
               let rec joinNext (original: atomGroup list) (divisor: atom list list) (acc: atom list list) =
                    match original with
                    | [] -> acc
                    | al :: rst -> joinNext rst divisor ((combine [al] divisor) @ acc)
               match bigDiv with
               | None            -> (agl, None)
               | Some(ADiv(t,b)) -> (joinNext agl b t, Some(b))
               | _               -> failwith "eliminateDivisions: bigDiv should always be a division"

    (*Splits an atom list into lists of all nondivisions and all division*)
    let splitDivNondiv (ag: atomGroup) : (atom list * atom list) =
        let rec splitter (ag: atomGroup) (nondiv: atom list) (divs: atom list) =
            match ag with
            | []               -> (nondiv, divs)
            | ADiv(a,b) :: rst -> splitter rst nondiv (ADiv(a,b)::divs)
            | atom      :: rst -> splitter rst (atom::nondiv) divs
        splitter ag [] []


    (*Recursively eliminates inner divisions by using common mathematical division rules*)
    let rec cleanInnerDivision div : atom =
        let (top, btm) = getDivValues div
        match (createBigDivision top, createBigDivision btm) with
        | ((agl1, None),       (agl2, None))       -> ADiv(agl1, agl2)
        | ((agl1, Some(div1)), (agl2, None))       -> ADiv(agl1, combine agl2 div1)
        | ((agl1, None),       (agl2, Some(div2))) -> ADiv(combine agl1 div2, agl2)
        | ((agl1, Some(div1)), (agl2, Some(div2))) -> ADiv(combine agl1 div2, combine agl2 div1)

    (*Simplifies a list of atoms and a list of divisions all being multiplied into a single division*)
    and simplifyDivisions (nondiv: atom list, div: atom list) =
        let divs = List.map cleanInnerDivision div
        let bigDiv = multiplyDivisions divs    
        match bigDiv with
        | ADiv(t,b) -> match nondiv with
                       | [] -> bigDiv
                       | _  -> ADiv(List.map(fun (a:atom list) -> nondiv @ a) t, b)
        | _         -> failwith "This should be a division"
    
    (*Splits an expression into an expression and a list of divisions*)
    and findDivisions (ags: atomGroup list) : (atomGroup list * atom list) =
        let rec splitter (agl: atomGroup list) (nondiv: atomGroup list) (div: atom list) =
            match agl with
            | []        -> (nondiv, div)
            | ag :: rst -> match splitDivNondiv ag with
                           |(_, []) -> splitter rst (ag::nondiv) div
                           |(nd, d) -> splitter rst nondiv (simplifyDivisions (nd, d) :: div)
        splitter ags [] []

    (*Converts an expression into a tuple representing a single division. If no division is needed, the second item is None*)
    and createBigDivision (original: atom list list) : (atom list list * atom list list option) =
        match findDivisions original with
        | (nondiv, [])  -> (nondiv, None)
        | (nondiv, div) -> compressDivisions (nondiv, div)
        
    (*Eliminates all divisions possible from an expression*)
    let eliminateDivisions (original: atom list list) : atom list list =
        fst (createBigDivision original)


    //The functions below are used to remove a root

    (*Returns whether or not an atom list contains an ARoot*)
    let rec containsRoot (al: atom list) =
        match al with
        | []                -> false
        | ARoot(_,_) :: rst -> true
        | _ :: rst          -> containsRoot rst

    (*Joins roots that are muliplied with eachother. Roots of different degrees are not supported*)
    let rec joinRoots (roots: atom list) (acc: atom) = 
        match (roots, acc) with
        | ([],acc) -> acc
        | (ARoot(rad1, n1)::rst, ARoot(rad2,n2)) -> if n1 = n2
                                                    then joinRoots rst (ARoot(combine rad1 rad2, n1))
                                                    else failwith "joinRoots: Does not support multiplying roots of different degrees"
        | _ -> failwith "joinRoots: Should only be called with roots"

    (*Helper function used to add a root to a list a specified number of times*)
    let rec addToList root roots timestoadd =
        match timestoadd with 
        | 0            -> roots
        | n when n > 0 -> addToList root (root::roots) (n - 1)
        | _            -> failwith "addToList: Cannot add a negative amount of times"

    (*Simplifies a single atom list containing a root as far as possible. The root might be removed completely.*)
    let simplifyRoot (roots: atom list) : (atom list list * atom option) =
        let rec map (list: atom list) (nonroots: atom list) (acc: Map<(atom list list * int), int>) =
            match list with
            | []                   -> (nonroots, acc)
            | ARoot(rad, n) :: rst -> let rad' = joinAtomLists rad
                                      let newAcc = let va = Map.tryFind (rad', n) acc
                                                   match va with
                                                   | None       -> (acc.Add((rad', n), 1))
                                                   | Some(some) -> (acc.Add((rad', n), (some + 1)))
                                      map rst nonroots newAcc
            | atom          :: rst -> map rst (atom :: nonroots) acc
        let (alt, roots) = map roots [] Map.empty
        let split = Map.fold(fun (all, roots) (atll, n) i -> 
                                    if n = i
                                    then (atll @ all, roots)
                                    else let newRoots = addToList (ARoot(atll, n)) roots i
                                         (all, newRoots)) ([],[]) roots
        match split with
        | (at, [])      -> if alt = []
                           then (at, None)
                           else (alt :: at, None)
        | (at, r :: []) -> if alt = []
                           then (at, Some(r))
                           else (alt :: at, Some(r))
        | (at, r :: rst)-> if alt = []
                           then (at, Some(joinRoots rst r))
                           else (alt :: at, Some(joinRoots rst r))

    (*Simplefies all roots as far as possible*)
    let simplifyRoots (nonroots:atomGroup list, roots:atom list list) : (atomGroup list * (atom * atom list list) list) =
        let rec simplRoot (nonrootlist: atomGroup list) (rootlist: atom list list) (acc:Map<atom, atom list list>)=
            match rootlist with
            | [] -> (nonrootlist, Map.toList acc)
            | root :: rst -> match simplifyRoot root with
                             | (agl, None) ->  simplRoot (agl @ nonrootlist) rst acc
                             | (agl, Some(ARoot(rad, n))) when agl = [[]] || agl = [] -> 
                                    let newAcc = let va = Map.tryFind (ARoot(rad, n)) acc
                                                 match va with
                                                 | None       -> (acc.Add(ARoot(rad, n), [[ANum(1.0)]]))
                                                 | Some(some) -> (acc.Add(ARoot(rad, n), ([ANum(1.0)] :: some)))
                                    simplRoot nonrootlist rst newAcc
                             | (agl, Some(ARoot(rad, n))) -> 
                                    let newAcc = let va = Map.tryFind (ARoot(rad, n)) acc
                                                 match va with
                                                 | None       -> (acc.Add(ARoot(rad, n), agl))
                                                 | Some(some) -> (acc.Add(ARoot(rad, n), (agl @ some)))
                                    simplRoot nonrootlist rst newAcc
                             | _                          -> failwith "simplifyRoots: simplifyRoot should only return root"
        simplRoot nonroots roots Map.empty
        
    
    (*Splits an atom list list into a list of all groups without roots and a list of all roots*)
    let findRoots (ags: atom list list) : (atom list list * (atom * atom list list) list) =
        let rec splitter (agl: atomGroup list) (nonroots: atomGroup list) (roots: atom list list) =
            match agl with
            | []        -> (nonroots, roots)
            | ag :: rst -> if containsRoot ag
                           then splitter rst nonroots (ag::roots)
                           else splitter rst (ag::nonroots) roots
        let tmp = splitter ags [] []
        simplifyRoots tmp

    (*Removes the root from a function of [nonroots] + [root] = 0*)
    let removeRoot (nonroots: atom list list) (root: atom list) : atom list list=
        match root with
        | ARoot(rad, n) :: []  -> let rightside = combine [[ANum(-1.0)]] nonroots
                                  let rec mult (original: atom list list) (r: int) (acc: atom list list) =
                                     match r with
                                     | 0 -> acc
                                     | n -> mult original (n - 1) (combine original acc)
                                  rad @ (combine [[ANum(-1.0)]] (mult rightside (n - 1) rightside))
        | _                    -> failwith "removeRoot: root should only contain roots"
    
    (*Removes the advanced root from a function of [nonroots] + [root] = 0*)
    let removeAdvancedRoot (nonroots: atom list list) (root: (atom * atom list list)) : atom list list=
        match root with
        | (ARoot(r, n), [[]])   -> removeRoot nonroots ([ARoot(r, n)])
        | (ARoot(r, n), [[ANum(1.0)]])   -> removeRoot nonroots ([ARoot(r, n)])
        | (ARoot(r, n), all)  -> let rightside = combine [[ANum(-1.0)]] nonroots
                                 let rec mult (original: atom list list) (r: int) (acc: atom list list) =
                                    match r with
                                    | 0 -> acc
                                    | n -> mult original (n - 1) (combine original acc)
                                 (combine (mult all (n - 1) all) r) @ (combine [[ANum(-1.0)]] (mult rightside (n - 1) rightside))
        | _                   -> failwith "RemoveAdvancedRoot: The atom should always be a root"
    
    (*Removes one root from the atom list list and tries to elimate divisions and roots again*)
    let rec eliminateRoots (agl: atom list list) : atom list list = 
        let (nonroots, roots) = findRoots agl
        match roots with
        | []          -> nonroots
        | root :: []  -> eliminateDivsAndRoots (removeAdvancedRoot nonroots root)
        | root :: rst -> let roots = List.fold (fun list ((a:atom), (all:atom list list)) -> 
                                                               if all = []
                                                               then [[a]] @ list
                                                               else (combine [[a]] all) @ list) [] rst
                         eliminateDivsAndRoots (removeAdvancedRoot (roots @ nonroots) root)
    
    (*Recursively switches between removing divisions and roots*)
    and eliminateDivsAndRoots (all: atom list list) : (atom list list) =
        //elimates all divisions
        let ags = eliminateDivisions all

        //elimates roots and might call this recursily
        eliminateRoots ags



    (*Simplifies a simpleExpr. Removes all roots and divisions*)
    let simplifySimpleExpr (SE ags) : simpleExpr = 
        //Eliminates all atomgroups containing divisions and roots
        let atll = eliminateDivsAndRoots ags
        
        //Simplifyes all atomgroups and joins all simular atomlists. Each atomlist should be sorted somehow
        SE(compress atll)
    

    (*Simplifies a simpleExpr. Does not remove divisions or roots*)
    let simplifySimpleExprSafe (SE ags) : simpleExpr = 
        //Simplifyes all atomgroups and joins all simular atomlists. Each atomlist should be sorted somehow
        SE(compress ags)

    (*Converts an expreesion into a simple expression*)
    let exprToRawSimpleExpr e = simplifySimpleExprSafe (SE (simplify e))

    (* Converts an expression into a simple expression and removes roots and divisions*)
    let exprToSimpleExpr e = simplifySimpleExpr (SE (simplify e))


    (*A datatype used to represent a polynomial*)
    type poly = P of Map<int, simpleExpr>

    (*Multiplies a simpleexpr with a float value*)
    let rec multiplySimpleExpr (SE(agl)) (f:float) : simpleExpr =
        let rec list all = match all with
                           | []        -> []
                           | ag :: rst -> (ANum(f) :: ag) :: (list rst)
        simplifySimpleExpr (SE(list agl))

    (*Derives a polynomial*)
    let derivePoly (P(map)) : poly = 
        let list = Map.fold (fun list exp se -> 
                                if exp = 0 
                                then list 
                                else (exp - 1, multiplySimpleExpr se ((float)exp)) :: list) [] map
        P(Map.ofList list)

    (*Converts polygon to a string representation*)
    let ppPoly v (P p) =
      let pp (d, ags) =
        let prefix = if d=0 then "" else printAtom (AExponent(v,d))
        let postfix = if isSimpleExprEmpty ags then "" else "(" + (ppSimpleExpr ags) + ")"
        prefix + postfix
      String.concat "+" (List.map pp (Map.toList p))

    (* Collect atom groups into groups with respect to one variable v *)
    let splitAG v (m:Map<int, simpleExpr>) = function
      | [] -> m
      | ag ->
        let eqV = function 
          | AExponent(v', _) -> v = v' 
          | _                -> false
        let addMap (d:int) ag m = 
          match (Map.tryFind d m) with
          | Some (SE list) -> m.Add(d, ((SE(ag::list))))
          | None           -> m.Add(d, (SE([ag])))
        match List.tryFind eqV ag with
          | Some (AExponent(_, d)) ->
            let ag' = List.filter (not << eqV) ag
            match ag' with
            | []    -> addMap d [ANum(1.0)] m
            | group -> addMap d group m
          | Some _                -> failwith "splitAG: Must never come here! - ANum will not match eqV"
          | None                  -> addMap 0 ag m

    (* Converts a simple expression into a polygon*)
    let simpleExprToPoly (SE ags) v =
      P (List.fold (splitAG v) Map.empty ags)

    (* Converts an expression into a polygon*)
    let exprToPoly e v = (exprToSimpleExpr >> simpleExprToPoly) e v

    (*A function used to substitute the variables x, y and z in an expr with the corresponding variables of ray*)
    let replaceWithRay s =
        match s with
        | "x" -> FAdd(FVar("px"), FMult(FVar("t"), FVar("vx")))
        | "y" -> FAdd(FVar("py"), FMult(FVar("t"), FVar("vy")))
        | "z" -> FAdd(FVar("pz"), FMult(FVar("t"), FVar("vz")))
        | _   -> failwith "Only x, y and z variables supported"
    
    (*Converts an expr to a polynomial with the values of a ray*)
    let convertToRayPoly e = 
        exprToPoly (substFunc e replaceWithRay) "t"
        

    let polyFromString s = 
        exprToPoly (substFunc (parseExpressionString s) replaceWithRay) "t"
        
    (*Extracts all relevant values from a ray*)
    let valuesFromRay (r:Ray) =
        Map.ofList([("px", r.Origin.X);("py", r.Origin.Y);("pz", r.Origin.Z);
                     ("vx", r.Direction.X);("vy", r.Direction.Y);("vz", r.Direction.Z)])
    
    
    (*Calculates the value of an atomgroup based on the values of a map*)
    let rec calculateAtomGroup (ag: atomGroup) (values:Map<string,float>) (acc: float)=
        match ag with
        | []       -> acc
        | ANum(f) :: rst         -> calculateAtomGroup rst values acc*f
        | AExponent(v, 1) :: rst -> calculateAtomGroup rst values acc*(values.Item v)
        | AExponent(v, 2) :: rst -> let it = values.Item v
                                    calculateAtomGroup rst values acc*(it * it)
        | AExponent(v, 3) :: rst -> let it = values.Item v
                                    calculateAtomGroup rst values acc*(it * it * it)
        | AExponent(v, 4) :: rst -> let it = values.Item v
                                    let it2 = it * it
                                    calculateAtomGroup rst values acc*(it2 * it2)
        | AExponent(v, n) :: rst -> calculateAtomGroup rst values acc*(values.Item v ** (float) n)
        | ADiv(al1, al2)  :: rst -> calculateAtomGroup rst values acc*(calculateAGList al1 values 0.0 /(calculateAGList al2 values 0.0))
        | ARoot(al, n)    :: rst -> calculateAtomGroup rst values acc*(calculateAGList al values 0.0 ** (1.0/(float)n))
    
    (*Calculates the values of a list of atomgroups based on the values of a map*)
    and calculateAGList (se: atomGroup list) (values:Map<string,float>) (acc:float)=
        match se with
        | []        -> acc
        | ag :: rst -> calculateAGList rst values acc+(calculateAtomGroup ag values 1.0)
    
    (*Calculates the values of a simpleExpr based on the values of a map*)   
    let calculateSimpleExpr (se:simpleExpr) (values:Map<string,float>)=
        match se with
        |SE(agl) -> calculateAGList agl values 0.0
    
    (*Calculates the values of a poly of variable s based on the values of a map*) 
    let calculatePoly (P(m)) (s:string) (values: Map<string, float>) : float =
        let singleSeCalc (i:int) (se:simpleExpr) =
            let coefficient = calculateSimpleExpr se values
            let sval = Map.find s values
            match i with
            | 0 -> coefficient
            | 1 -> coefficient * sval
            | 2 -> coefficient * (sval * sval)
            | 3 -> coefficient * (sval * sval * sval)
            | 4 -> let sval2 = sval * sval
                   coefficient * (sval2 * sval2)
            | 5 -> let sval2 = sval * sval
                   coefficient * (sval2 * sval2 * sval)
            | 6 -> let sval2 = sval * sval
                   coefficient * (sval2 * sval2 * sval2)
            | n -> coefficient * (sval ** (float) n)
        Map.fold (fun acc int se -> acc + (singleSeCalc int se)) 0.0 m

    (*Calculates the values of a polynomial when given the values from a ray*)
    let calcPolyValues (r:Ray) (p:poly)=
        let values = valuesFromRay r
        match p with
        |P(is) -> Map.map(fun i se -> calculateSimpleExpr se values) is
        