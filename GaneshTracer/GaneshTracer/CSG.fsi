namespace GaneshTracer

module CSG =
    open Shape
    //This module is used for Constructive Solid Geometry to group, union, intersect and subtract two shapes.    

    /// <summary>
    /// Group two shapes together - also rendering internal edges.
    /// </summary>
    val group : Shape -> Shape -> Shape

    /// <summary>
    /// Union two shapes - without rendering internal edges.
    /// </summary>
    val union : Shape -> Shape -> Shape

    /// <summary>
    /// Intersect two shapes - only rendering the overlap of the two shapes.
    /// </summary>
    val intersection : Shape -> Shape -> Shape

    /// <summary>
    /// Subtract one shape (s2) from another shape (s1).
    /// </summary>
    val subtraction : Shape -> Shape -> Shape