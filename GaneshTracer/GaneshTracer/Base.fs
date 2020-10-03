namespace GaneshTracer
module Base =
    (* Vector is quite straight forward, see the .fsi for doc *)
    type Vector =
        struct
            val X: float
            val Y: float
            val Z: float
            new (x: float, y: float, z: float) = {X = x; Y = y; Z = z}
        end
       
    (* Point is quite straight forward, see the .fsi for doc *)
    and Point =
        struct
            val X: float
            val Y: float
            val Z: float
            new (x: float, y: float, z: float) = {X = x; Y = y; Z = z}
        end
   
   (* Vector functions *)
    let inline mkNormVector x y z =
        let length = 1.0 / sqrt(x*x+y*y+z*z)
        Vector(x * length, y * length, z * length)
    let inline normalize (v: Vector) =
        mkNormVector v.X v.Y v.Z  
    let SquaredMagnitude (v: Vector) = 
        v.X * v.X + v.Y * v.Y + v.Z * v.Z
    let Magnitude (v: Vector) = 
        sqrt (SquaredMagnitude v)
    let ToPoint (v: Vector) =
        Point(v.X, v.Y, v.Z)
    (* Required to be here in the file, used by Vector operators *)
    let dotProduct   (a : Vector) (b : Vector)  = a.X * b.X + a.Y * b.Y + a.Z * b.Z
    let normCrossProduct (a : Vector) (b : Vector) = mkNormVector (a.Y * b.Z - a.Z * b.Y) (a.Z * b.X - a.X * b.Z) (a.X * b.Y - a.Y * b.X)
    let crossProduct (a : Vector) (b : Vector)  = Vector (a.Y * b.Z - a.Z * b.Y, a.Z * b.X - a.X * b.Z, a.X * b.Y - a.Y * b.X)
    let angle (a: Vector) (b: Vector) = (dotProduct a b) / ((Magnitude a) * (Magnitude b))

    (* Vector operators... *) 
    type Vector with
        static member ( ~- ) (v : Vector) = Vector(-v.X,-v.Y,-v.Z)
        static member ( + ) (v : Vector, u : Vector) = Vector(v.X + u.X, v.Y + u.Y, v.Z + u.Z)
        static member ( - ) (v : Vector, u : Vector) = Vector(v.X - u.X, v.Y - u.Y, v.Z - u.Z)
        static member ( * ) (f : float, v : Vector) = Vector(v.X * f, v.Y * f, v.Z * f)
        static member ( * ) (i : int, v : Vector) = Vector(v.X * (float i), v.Y * (float i), v.Z * (float i))
        static member ( * ) (v:Vector, f) = Vector(v.X * f, v.Y * f, v.Z * f)
        static member ( / ) (v : Vector, f) = let speed = 1.0/f
                                              Vector(v.X*speed, v.Y*speed, v.Z*speed)
        static member ( * ) (u, v) = dotProduct u v
        static member ( % ) (u, v) = crossProduct u v
    
    (* Point operators... *)
    type Point with
        static member ( + ) (p : Point, v : Vector) = Point (p.X + v.X, p.Y + v.Y, p.Z + v.Z) 
        static member ( - ) (p : Point, q : Point) = Vector (p.X - q.X, p.Y - q.Y, p.Z - q.Z)

    (* Point functions *)
    let distance (p : Point) (q : Point) = q - p
    let direction (p : Point) (q : Point) = mkNormVector (q.X-p.X) (q.Y-p.Y) (q.Z-p.Z)
    let ToVector (p : Point) = Vector(p.X, p.Y, p.Z)

    (* Ray is currently quite simple *)
    type Ray =
       struct
        val Origin: Point
        val Direction: Vector
        val InverseDir: Vector
        new(p: Point, v :Vector) = {Origin = p; Direction = v; InverseDir = Vector((1.0/v.X),(1.0/v.Y),(1.0/v.Z))}
       end

    type BoundingBox =
        struct 
            val MinimumX : float
            val MinimumY : float
            val MinimumZ : float
            val MaximumX : float
            val MaximumY : float
            val MaximumZ : float
            new (min: Point, max:Point) = {MinimumX = min.X; MinimumY = min.Y; MinimumZ = min.Z;
                                           MaximumX = max.X; MaximumY = max.Y; MaximumZ = max.Z}
            static member ( + ) (bb : BoundingBox, bc : BoundingBox) = let minX = (min (bb.MinimumX) (bc.MinimumX))
                                                                       let minY = (min (bb.MinimumY) (bc.MinimumY))
                                                                       let minZ = (min (bb.MinimumZ) (bc.MinimumZ))
                                                                       let maxX = (max (bb.MaximumX) (bc.MaximumX))
                                                                       let maxZ = (max (bb.MaximumZ) (bc.MaximumZ))
                                                                       let maxY = (max (bb.MaximumY) (bc.MaximumY))
                                                                       BoundingBox(Point(minX,minY,minZ), Point(maxX, maxY, maxZ))
        end
    
    let mkBoundingBox (min : Point) (max : Point) = 
        let e' = 0.000001
        BoundingBox(Point(min.X-e',min.Y-e',min.Z-e'), Point(max.X+e',max.Y+e',max.Z+e'))

    let minMaxBounds = 
        let min = System.Double.MinValue
        let max = System.Double.MaxValue
        BoundingBox(Point(min, min, min), Point(max, max, max))
                                                                        
    (* Boinding Box methods *)
    let combineBoundingBoxes (bb : BoundingBox list) =
        if bb.IsEmpty then failwith "combineBoundingBoxes :: Does not support empty list"
        if bb.Length = 1 then bb.Head else 
            let rec inner minX minY minZ maxX maxY maxZ (list : BoundingBox list) =
                match list with
                | [] -> BoundingBox(Point(minX, minY, minZ), Point(maxX, maxY, maxZ))
                | element :: rest -> inner (System.Math.Min(minX, element.MinimumX)) (System.Math.Min(minY, element.MinimumY)) (System.Math.Min(minZ, element.MinimumZ)) (System.Math.Max(maxX, element.MaximumX)) (System.Math.Max(maxY, element.MaximumY)) (System.Math.Max(maxZ, element.MaximumZ)) rest
            inner System.Double.MaxValue System.Double.MaxValue System.Double.MaxValue System.Double.MinValue System.Double.MinValue System.Double.MinValue bb
    
    let alternativeBox (r:Ray) (bb:BoundingBox) =
        // Calculate X axis hitpoint
        let tx = (bb.MinimumX - r.Origin.X) * r.InverseDir.X
        let tx' = (bb.MaximumX - r.Origin.X) * r.InverseDir.X
 
        let t = min tx tx'
        let t' = max tx tx'

        // Calculate Y axis hitpoint
        let ty = (bb.MinimumY - r.Origin.Y) * r.InverseDir.Y
        let ty' = (bb.MaximumY - r.Origin.Y) * r.InverseDir.Y
 
        // Check if a hit is possible
        let low = min ty ty'
        let high = max ty ty'
        if(high < t) then None else
        if(low > t') then None else
        let t  = max t low
        let t' = min t' high

        // Calculate Z axis hitpoint
        let tz = (bb.MinimumZ - r.Origin.Z) * r.InverseDir.Z
        let tz' = (bb.MaximumZ - r.Origin.Z) * r.InverseDir.Z
 
        // Not worth checking...
        let t = max t (min tz tz')
        let t' = min t' (max tz tz')

        if(t' >= t) then Some(t, t') else None

    let boxIntersection (L:float) (H:float) (o:float) (d:float) =
        let divideD = 1.0 / d
        if d >= 0.0 then
             let t = (L - o) * divideD
             let t' = (H - o) * divideD
             (t, t')
         else
             let t = (H - o) * divideD
             let t' = (L - o) * divideD
             (t, t')

    let isHit (r : Ray) (bb : BoundingBox) = alternativeBox r bb

    let getCorners (bb: BoundingBox) =
        let p1 = Point(bb.MinimumX, bb.MinimumY, bb.MinimumZ)
        let p2 = Point(bb.MinimumX, bb.MaximumY, bb.MinimumZ)
        let p3 = Point(bb.MaximumX, bb.MaximumY, bb.MinimumZ)
        let p4 = Point(bb.MaximumX, bb.MinimumY, bb.MinimumZ)
        let p5 = Point(bb.MinimumX, bb.MinimumY, bb.MaximumZ)
        let p6 = Point(bb.MinimumX, bb.MaximumY, bb.MaximumZ)
        let p7 = Point(bb.MaximumX, bb.MaximumY, bb.MaximumZ)
        let p8 = Point(bb.MaximumX, bb.MinimumY, bb.MaximumZ)
        (p1, p2, p3, p4, p5, p6, p7, p8)

    let bbFromCorners (points: Point[]) = 
        let L = Point ( (Array.minBy(fun (p: Point) -> p.X) points).X, (Array.minBy(fun (p: Point) -> p.Y) points).Y, (Array.minBy(fun (p: Point) -> p.Z) points).Z )
        let H = Point ( (Array.maxBy(fun (p: Point) -> p.X) points).X, (Array.maxBy(fun (p: Point) -> p.Y) points).Y, (Array.maxBy(fun (p: Point) -> p.Z) points).Z )
        mkBoundingBox L H

    type Vertex = 
        struct
            val P: Point
            val Norm: Vector
            val UV: float*float
            new (point: Point, normal: Vector, uvCoords: float*float) = {P = point; Norm = normal; UV = uvCoords}
        end
 
    let mkVertex (p:Point) (norm:Vector) = Vertex(p, norm, (0.0, 0.0))
    let mkVertexWithTexCoords (p:Point) (norm:Vector) (u:float) (v:float) = Vertex(p, norm, (u,v))
    let getVertexPoint (v: Vertex) = v.P
    let getVertexNormal (v: Vertex) = v.Norm
    let getVertexUVs (v: Vertex) = v.UV
    let printVertex (v: Vertex) = printfn "Vertex (P: %f , %f, %f)" v.P.X v.P.Y v.P.Z
        
    let fstTOpt t =
        match t with
        | Some (x,_,_) -> Some x
        | None         -> None

    let sndTOpt t =
        match t with
        | Some (_,x,_) -> Some x
        | None         -> None

    let trdTOpt t =
        match t with
        | Some (_,_,x) -> Some x
        | None         -> None

    let fstT (x,_,_) = x
    let sndT (_,x,_) = x
    let trdT (_,_,x) = x