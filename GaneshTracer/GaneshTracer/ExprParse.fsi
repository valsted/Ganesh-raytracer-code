namespace GaneshTracer

module ExprParse =
    [<Sealed>]
    
    type terminal
    exception Scanerror
    /// <summary>
    /// Converts a string into a list of terminals.
    /// </summary>
    val scan: expressionSeq: string -> terminal list
    /// <summary>
    /// Inserts explicits multiplications in place of any implicit multiplication
    /// </summary>
    val insertMult: initialList: terminal list -> terminal list

    (*A datastructure to represent an expression*)
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
    /// <summary>
    /// Converts a correct terminal list into an expr.
    /// </summary>
    val parse: terminals: terminal list -> expr

    /// <summary>
    /// Parses a string to an expr
    /// </summary>
    val parseExpressionString: expression: string -> expr
