namespace GaneshTracer

module BaseShape =
    open Base
    open ExprParse
    open ExprToPoly
    open PolynomialCalc
    open Material
    open Texture
    open Shape
    open Datastructure

    type BaseShape = 
        | Implicit of hitfunc:poly * normalvector:(poly * poly * poly) * insidefunc:simpleExpr
        | PLY of KD

    val mkImplicit: string -> BaseShape
    val insideFunction: BaseShape -> Point -> bool
    val shapeFunction: BaseShape -> (Material -> Ray -> (float * (unit -> Material * Vector * Point))option)

    /// <summary>
    /// Constructs a shape from the given baseShape with the applied material
    /// </summary>
    val mkShape: b:BaseShape -> tex:Texture -> Shape

    /// Loads a .ply file and parses it to triangle mesh ready to be textured
    val mkPLY: string -> bool -> BaseShape