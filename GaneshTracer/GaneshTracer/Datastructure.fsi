namespace GaneshTracer
module Datastructure =
    open Base
    open Shape

    /// <summary>Datastructure, supports shapes (R)</summary>
    type Tree    
    /// <summary>Datastructure, supports tris and shapes (KD)</summary>
    type KD
     
    (* Beginning of the epic R-Tree Legacy *)
    /// <summary>R-tree (legacy) | used for large scenes</summary>
    val hitShapes : Tree -> Ray -> (float * (unit -> Material.Material * Vector * Point)) option
    /// <summary>R-tree (legacy) | converts a list of shapes to a (R)-Tree</summary>
    val shapesToTree : Shape list -> Tree
    (* End of the epic R-Tree Legacy *)

    
    (* Beginning of the KD adventure *)
    /// <summary>Returns the boundingbox of a KD tree</summary>
    val getBB : KD -> BoundingBox
    /// <summary>Hit function used for meshes, based on KD trees</summary>
    val meshHitFunc : KD -> Ray -> (float * (unit -> Point * Vector * (float * float))) option
    /// <summary>Creation of the tree used for meshes</summary>
    val meshTree : Tris[] -> KD
    /// <summary>Hit function used for meshes, based on KD trees</summary>
    val sceneHitFunc : KD -> Ray -> float -> (float * (unit -> Material.Material * Vector * Point) * float) option
    /// <summary>Creation of the tree used for meshes (KD)</summary>
    val sceneTree : Shape[] -> KD