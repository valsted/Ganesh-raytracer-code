namespace GaneshTracer

module ExprToPoly =
    open Base
    open ExprParse
    /// <summary>
    /// Substitutes a variable name with an expr
    /// </summary>
    val subst: initialEx: expr -> subFunc: (string * expr) -> expr

    type simpleExpr
    /// <summary>
    /// Respresents a simpleExpr as a string
    /// </summary>
    /// <remark>
    /// This is taken from assignment6
    /// </remark>
    val ppSimpleExpr: simpleExpr -> string
    /// <summary>
    /// Converts an expr to a simpleExpr
    /// </summary>
    val exprToRawSimpleExpr: expr -> simpleExpr
    /// <summary>
    /// Converts an expr to a simpleExpr witout roots and divisions
    /// </summary>
    val exprToSimpleExpr: expr -> simpleExpr

    type poly

    /// <summary>
    /// Creates the derivative polynomial
    /// </summary>
    val derivePoly: poly -> poly
    /// <summary>
    /// Converts a polynomial to a string representation with the given coefficient
    /// </summary>
    val ppPoly: string -> poly -> string
    /// <summary>
    /// Creates a polynomial based on the specified variable name
    /// </summary>
    val simpleExprToPoly: simpleExpr -> string -> poly

    /// <summary>
    /// Creates a polynomial from an expr based on a specified variable name
    /// </summary>
    val exprToPoly: expr -> string -> poly

    /// <summary>
    /// Converts an expr to a polynomial, which can be used to calculate the distance to the surface based on a ray
    /// </summary>
    val convertToRayPoly: expr -> poly

    /// <summary>
    /// Creates a polynomial from an expr based on the standard ray
    /// </summary>
    val polyFromString: string -> poly

    /// <summary>
    /// Calculates the float value of a simpleExpr based on the values of a map
    /// </summary>
    val calculateSimpleExpr: simpleExpr -> Map<string,float> -> float

    /// <summary>
    /// Calculates the float value of a poly with the specified coefficient based on the values of a map
    /// </summary>
    val calculatePoly: poly -> string -> Map<string, float> -> float

    /// <summary>
    /// Calculates the float values needed to create a simple polynomial function
    /// </summary>
    val calcPolyValues: Ray -> poly -> Map<int,float>