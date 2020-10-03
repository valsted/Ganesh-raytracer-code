namespace GaneshTracer

module ExprParse =
    type terminal =
      Add | Subtract | Mul | Div | Pwr | Lpar | Rpar | Root | Int of int | Float of float | Var of string
    
    (*Simple functions to evaluate a character*)
    let isblank c = System.Char.IsWhiteSpace c
    let isdigit c  = System.Char.IsDigit c
    let isletter c = System.Char.IsLetter c
    let isletterdigit c = System.Char.IsLetterOrDigit c

    (*Function to evaluate the value of a char*)
    let floatval (c:char) = float((int)c - (int)'0')
    let intval (c:char) = (int)c - (int)'0'

    exception Scanerror

    (*Scans a char list to determine the value of the number starting at the beginning of the list using an accumulator*)
    let rec scnum (cs, value) = 
      match cs with 
        '.' :: c :: cr when isdigit c -> scfrac(c :: cr, (float)value, 0.1)
      | c :: cr when isdigit c -> scnum(cr, 10 * value + intval c)
      | _ -> (cs,Int value)    (* Number without fraction is an integer. *)
    and scfrac (cs, value, wt) =
      match cs with
        c :: cr when isdigit c -> scfrac(cr, value+wt*floatval c, wt/10.0)
      | _ -> (cs, Float value)
    
    (*Scans a char list to determine the name of the variable*)
    let rec scname (cs, value) =
      match cs with
        c :: cr when isletterdigit c -> scname(cr, value + c.ToString())
      | _ -> (cs, value)
    
    (*Scans a char list and converts it to a terminal list*)
    let rec scanList cs = 
        match cs with
          [] -> []
        | '+' :: cr -> Add :: scanList cr    
        | '-' :: cr -> Subtract :: scanList cr  
        | '*' :: cr -> Mul :: scanList cr      
        | '/' :: cr -> Div :: scanList cr      
        | '^' :: cr -> Pwr :: scanList cr
        | '_' :: cr -> Root :: scanList cr
        | '(' :: cr -> Lpar :: scanList cr     
        | ')' :: cr -> Rpar :: scanList cr     
        | c :: cr when isdigit c -> let (cs1, t) = scnum(cr, intval c) 
                                    t :: scanList cs1
        | c :: cr when isblank c -> scanList cr
        | c :: cr when isletter c -> let (cs1, n) = scname(cr, (string)c)
                                     Var n :: scanList cs1
        | _ -> raise Scanerror
    
    (*Converts a string to a terminal list by scanning through it*)
    let scan (s:string) = scanList (List.ofArray(s.ToCharArray()))
    
    (*Inserts multiplication at any point of the expression, where multiplication is implicit*)
    let rec insertMult = function
        | Float r :: Var x :: ts -> Float r :: Mul :: insertMult (Var x :: ts)
        | Float r1 :: Float r2 :: ts -> Float r1 :: Mul :: insertMult (Float r2 :: ts)
        | Float r :: Int i :: ts -> Float r :: Mul :: insertMult (Int i :: ts)
        | Var x :: Float r :: ts -> Var x :: Mul :: insertMult (Float r :: ts)
        | Var x1 :: Var x2 :: ts -> Var x1 :: Mul :: insertMult (Var x2 :: ts)
        | Var x :: Int i :: ts -> Var x :: Mul :: insertMult (Int i::ts)
        | Int i :: Float r :: ts -> Int i :: Mul :: insertMult (Float r :: ts)
        | Int i :: Var x :: ts -> Int i :: Mul :: insertMult (Var x :: ts)
        | Int i1 :: Int i2 :: ts -> Int i1 :: Mul :: insertMult (Int i2 :: ts)
        | Float r :: Lpar :: ts -> Float r :: Mul :: insertMult (Lpar :: ts)
        | Var x :: Lpar :: ts -> Var x :: Mul :: insertMult (Lpar :: ts)
        | Int i :: Lpar :: ts -> Int i :: Mul :: insertMult (Lpar :: ts)
        | t :: ts -> t :: insertMult ts
        | [] -> []
    
    (*The datastructure used to store the parsed expression in a treestructure*)
    type expr = 
      | FNum of float
      | FVar of string
      | FAdd of expr * expr
      | FSubtract of expr * expr
      | FDiv of expr * expr
      | FMult of expr * expr
      | FExponent of expr * int
      | FRoot of expr * int
    
    exception Parseerror of string

    (*Recursively calls functions that satisfies the grammar*)
    let rec E (ts:terminal list) = (T >> Eopt) ts
    and Eopt (ts, inval) =
      match ts with
        | Add :: tr      -> let (tl, ex) = T tr
                            Eopt(tl, FAdd(inval, ex))
        | Subtract :: tr -> let (tl, ex) = T tr
                            Eopt(tl, FSubtract(inval, ex))
        | _ -> (ts, inval)
    and T ts = (F >> Topt) ts
    and Topt (ts, inval:expr) = 
        match ts with
        |Mul :: tr -> let (tl, ex) = F tr
                      Topt(tl, FMult(inval, ex))
        |Div :: tr -> let (tl, ex) = F tr
                      Topt(tl, FDiv(inval, ex))
        | _ -> (ts, inval)
    and F ts = (P >> Fopt) ts
    and Fopt (ts, inval:expr) = 
        match ts with
        | Pwr  :: Int(i) :: tr -> (tr,  FExponent(inval, i)) 
        | Root :: Int(i) :: tr -> (tr,  FRoot(inval, i)) 
        | _ -> (ts, inval)
    and P ts = 
        match ts with
        | Int(i) :: tr   -> (tr, FNum(float i))
        | Float(f) :: tr -> (tr, FNum f)
        | Var(s) :: tr   -> (tr, FVar s)
        | Lpar :: tr     -> let (tl, ex) = E tr
                            match tl with
                            | Rpar :: rst -> (rst, ex)
                            | _ -> raise (Parseerror "Left parenteses not matched")
        | Subtract :: tr -> let (tl, ex) = P tr
                            (tl, FMult(FNum(-1.0), ex))   
        | _              -> raise (Parseerror ("P: cannot parse " + ts.ToString()))

    (*Parses a terminal list into an expr*)
    let parse ts = 
      match E ts with
        ([], result) -> result
      | _ -> raise (Parseerror "Terminal list not empty after parsing")
    
    (*Parses a string to an expr*)
    let parseExpressionString (s:string) =
        (scan s |> insertMult) |> parse
