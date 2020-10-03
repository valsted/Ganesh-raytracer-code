namespace GaneshTracer

module PolynomialCalc =
    //This module is used to calculate the solution to a polynomial function. Each function returns the smallest non-negative solution    

    /// <summary>
    /// Solves a 1st degree polynomial
    /// </summary>
    val solveFirstDegree: float -> float -> float option
    /// <summary>
    /// Solves a 2nd degree polynomial
    /// </summary>
    val solveSecondDegree: float -> float -> float -> float option
    /// <summary>
    /// Solves a 2nd degree polynomial and return both solutions
    /// </summary>
    val solveSecondDegreeBothValues: float -> float -> float -> ((float option)*(float option)) option
    /// <summary>
    /// Used for degrees greater than two
    /// </summary>
    val solveLargePolynomial: values:Map<int, float> -> float option
    /// <summary>
    /// Solves a polynomial of any degree
    /// </summary>
    val solvePolynomial: values:Map<int, float> -> float option