namespace GaneshTracer
/// <summary>The base module is a collection of commenly used types in the GaneshTracer</summary>
module (*All your*) Base (*are belong to us*) =
    [<Struct>]
    type Vector =
      /// <summary>Creates a new 3D Vector</summary>
      new : x:float * y:float * z:float -> Vector
      /// <summary>Returns the X coordinate of the vector as a float</summary>
      val X : float
      /// <summary>Returns the Y coordinate of the vector as a float</summary>
      val Y : float
      /// <summary>Returns the Z coordinate of the vector as a float</summary>
      val Z : float
      /// <summary>Flips the direction of the vector</summary>
      static member ( ~- ) : Vector -> Vector
      /// <summary>Add two vectors together/summary>
      static member ( + ) : Vector * Vector -> Vector
      /// <summary>Substracts two vectors</summary>
      static member ( - ) : Vector * Vector -> Vector
      /// <summary>Scale the vector</summary>
      static member ( * ) : float * Vector -> Vector
      /// <summary>Scale the vector</summary>
      static member ( * ) : int * Vector -> Vector
      /// <summary>Scale the vector</summary>
      static member ( * ) : Vector * float -> Vector
      /// <summary>Returns the (right-handed) dot product of the vectors</summary>
      static member ( * ) : Vector * Vector -> float
      /// <summary>Returns the cross product of the vectors</summary>
      static member ( % ) : Vector * Vector -> Vector
      /// <summary>Scales the vector (division)</summary>
      static member ( / ) : Vector * float -> Vector
    and [<Struct>] Point =
      /// <summary>Creates a new Point 3D</summary>
      new : x:float * y:float * z:float -> Point
      /// <summary>Returns the X coordinate of the point as a float</summary>
      val X : float
      /// <summary>Returns the Y coordinate of the point as a float</summary>
      val Y : float
      /// <summary>Returns the Z coordinate of the point as a float</summary>
      val Z : float
      /// <summary>Moves the point</summary>
      static member ( + ) : Point * Vector -> Point
      /// <summary>Returns the vector between the two points</summary>
      static member ( - ) : Point * Point -> Vector
    
    /// <summary>Returns the (right-handed) dot product of two vectors</summary>
    val dotProduct : Vector -> Vector -> float
    /// <summary>Returns the cross products of two vectors</summary>
    val crossProduct : Vector -> Vector -> Vector   
    /// <summary>Returns the normalized cross products of two vectors</summary>
    val normCrossProduct : Vector -> Vector -> Vector
    /// <summary> Computes the angle between two vectors in the interval [0; pi]</summary>    
    val angle : Vector -> Vector -> float
    /// <summary>Returns the distance between two points as a vector</summary>
    val distance : Point -> Point -> Vector
    /// <summary>Returns the normalized distance between two points as a vector</summary>
    val direction : Point -> Point -> Vector
    /// <summary>Returns the point as a vector</summary>
    val ToVector : Point -> Vector

    [<Struct>]
    type Ray =
       /// <summary>Creates a new Ray with a point as origin and a vector as direction</summary>
       new: Point * Vector -> Ray
       /// <summary>Returns the origin point of the ray</summary>
       val Origin : Point
       /// <summary>Returns the direction of the ray</summary>
       val Direction : Vector
       /// <summary>Returns the inverse direction vector</summary>
       val InverseDir : Vector
    
    [<Struct>]
    type BoundingBox =
        /// <summary>Creates a new bouding box from a min point and a max point</summary>
        new: Point * Point -> BoundingBox
        /// <summary>Returns the minimum point of the bounding box</summary>
        val MinimumX : float
        val MinimumY : float
        val MinimumZ : float
        /// <summary>Returns the maximum point of the bounding box</summary>
        val MaximumX : float
        val MaximumY : float
        val MaximumZ : float   
        /// <summary>Creates a new bounding box containing the two given bounding boxes</summary>
        static member ( + ) : BoundingBox * BoundingBox -> BoundingBox
    
    /// <summary>Creates a bounding box from the min and max point</summary>
    val mkBoundingBox : min:Point -> max:Point -> BoundingBox
    /// <summary>Creates a bounding Box from min til max float</summary>
    val minMaxBounds: BoundingBox
    /// <summary>Returns a bounding box around all the given bounding boxes</summary>
    val combineBoundingBoxes : BoundingBox list -> BoundingBox
    /// <summary>Calculates the distance to the entry and exit point of the bounding box</summary>
    val boxIntersection : float -> float -> float -> float -> float * float
    /// <summary>Calculates the distance to the entry and exit point of the bounding box</summary>
    val isHit : Ray -> BoundingBox -> (float * float) option

    /// <summary> Extracts the corner points of the bounding box. 
    /// Iteration is clockwise in the xy plane, starting in the bottom left corner. 
    /// Then it moves to the plane behind, again clockwise </summary>
    val getCorners : BoundingBox -> (Point * Point * Point * Point * Point * Point * Point * Point)

    /// <summary>Takes 8 corner points of a transformed boundingbox and creates a new one surrounding these. (Making it axis aligned)</summary>
    val bbFromCorners : (Point[]) -> BoundingBox

    [<Struct>]
    type Vertex =
        /// <summary>Constructor for the vertex</summary>
        new : point:Point * normal:Vector * uvCoords:(float*float) -> Vertex
        val P : Point
        val Norm : Vector
        val UV : float * float

    /// <summary>Create a new vertex</summary>
    val mkVertex : Point -> Vector -> Vertex
    /// <summary>Create a new vertex containing UV coordinates</summary>
    val mkVertexWithTexCoords : Point -> Vector -> float -> float -> Vertex
    /// <summary>Return the position of a vertex</summary>
    val getVertexPoint : Vertex -> Point
    /// <summary>Return the normal of a vertex</summary>
    val getVertexNormal : Vertex -> Vector
    /// <summary>Return the UV-coordinates of the vertex</summary>
    val getVertexUVs : Vertex -> float * float
    /// <summary>Print vertex information to console</summary>
    val printVertex : Vertex -> unit

    /// <summary>Return the first element in a tuple as an option</summary>
    val fstTOpt : ('a * 'b * 'c) option -> 'a option
    /// <summary>Return the second element in a tuple as an option</summary>
    val sndTOpt : ('a * 'b * 'c) option -> 'b option
    /// <summary>Return the third element in a tuple as an option</summary>
    val trdTOpt : ('a * 'b * 'c) option -> 'c option
    /// <summary>Return the first element in a tuple</summary>
    val fstT : ('a * 'b * 'c)  -> 'a
    /// <summary>Return the second element in a tuple</summary>
    val sndT : ('a * 'b * 'c)  -> 'b
    /// <summary>Return the third element in a tuple</summary>
    val trdT : ('a * 'b * 'c)  -> 'c
    
    /// <summary>Create a new vector that is normalized</summary>
    val inline mkNormVector : float -> float -> float -> Vector
    /// <summary>Normalize the given vector</summary>
    val inline normalize : Vector -> Vector
    /// <summary>Returns the magnitude (length) of the vector. Use SquaredMagnitude for fast comparison of lengths.</summary>
    val Magnitude : Vector -> float
    /// <summary>Returns the squared magnitude, which is faster than the magnitude</summary>
    val SquaredMagnitude : Vector -> float
    /// <summary>Returns this vector as a point</summary>
    val ToPoint : Vector -> Point
