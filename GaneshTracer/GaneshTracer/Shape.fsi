namespace GaneshTracer

module Shape =

    open Base
    open Material

    [<Sealed>]
    type Shape =
        class
            /// <summary>
            /// Creates a shape with a given hitfunction, a BoundingBox and a offset precision parameter.
            /// This is the prefered constructor for implicit surfaces
            /// </summary>
            new : hitFunc: (Ray -> ((float * (unit -> Material * Vector * Point))) option) * inside: (Point -> bool) * bounds: BoundingBox option * precision: float -> Shape
            /// <summary>
            /// Creates a shape with a given hitfunction and a BoundingBox.
            /// This is the prefered constructor for hardcoded Shapes with precise hit functions.
            /// </summary>
            new : hitFunc: (Ray -> ((float * (unit -> Material * Vector * Point))) option) * inside: (Point -> bool) * bounds: BoundingBox option -> Shape
            /// <summary>
            /// The Shape's hitfunction
            /// </summary>
            member updatedHitFunction: (Ray -> ((float * (unit -> Material * Vector * Point))) option)
            /// <summary>
            /// Checks if the given point is inside this shape.
            /// </summary>
            member inside : (Point -> bool)
            /// <summary>
            /// The axis-aligned boundingbox surrounding this shape
            /// </summary>
            member bounds : BoundingBox
            member boundsOpt : BoundingBox option
            /// <summary>
            /// Parameter describing how much offset the shape needs in terms of shading precision etc.
            /// </summary>
            member precision : float
        end
    
    /// <summary>
    /// Creates a transformed Shape by altering the incoming Ray and the resulting normal
    /// </summary>
    val transformWithNormal : (Ray -> Ray) -> (Vector-> Vector) -> (Point -> Point) -> (BoundingBox -> BoundingBox) -> Shape -> Shape
    /// <summary>
    /// Gets the BoundingBox for the given Shape
    /// </summary>
    val getShapeBoundingBox : Shape -> BoundingBox    

    /// Shape-like data type specialized for meshes. A triangle that allows for texturing after the hitdetails have been calculated
    type Tris = 
        class
            new : intersectFunc: (Ray -> float -> float * (unit -> Point * Vector * (float*float)) option) * bounds: BoundingBox -> Tris
            /// The customized hitfunction for this triangle. Computes ray-intersection, normal at the hitpoint and the uv-coords
            member intersect : (Ray -> float -> float * (unit -> Point * Vector * (float*float)) option)
            /// The bounding box surrounding this tris
            member bounds :  BoundingBox
        end

    /// Constructs a Tris from 3 vertices and a smooth toggle
    val mkTris : (Vertex * Vertex * Vertex) -> bool -> Tris