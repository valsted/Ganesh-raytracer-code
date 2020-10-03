namespace GaneshTracer
module Datastructure =
    open Base
    open Shape
    open System.Threading.Tasks

    type Axis = X | Y | Z
    
    type Output =
    | TrisOutput of (unit -> Point * Vector * (float * float))
    | ShapeOutput of (unit -> Material.Material * Vector * Point)
    | EmptyOutput

    type TreeShape =
    | TrisNode of Tris
    | ShapeNode of Shape

    (* Used for KD Datastructure *)
    type KD =
    | KDNode of float * Axis * BoundingBox * KD * KD
    | RSplit of BoundingBox * KD
    | KDLeaf of BoundingBox * TreeShape[]
    | KDEmpty

    (* Used for R* Datastructure *)
    /// This structre is used for both Mesh Shapes and scenes - this should not be exposed!
    type Tree =
    | Node of BoundingBox * Tree * Tree
    | LeafShape of BoundingBox * Shape
    | Empty

    (* Used for R* Datastructure *)
    // Returns the bounding box for the given node
    let RgetBB (t : Tree) =
        match t with
        | Node (bb, _, _) -> bb
        | LeafShape (bb, _) -> bb
        | Empty -> failwith "getBoundingBox :: The empty tree does not contain a bounding box" 

    (* Used for R* Datastructure *)
    // Helper method to get a single dimension of the bb
    let unwrapBounding (bb : BoundingBox) (dimension : int) =
        match dimension with
        | 0 -> bb.MinimumX + (0.5 * (bb.MaximumX - bb.MinimumX))
        | 1 -> bb.MinimumY + (0.5 * (bb.MaximumY - bb.MinimumY))
        | 2 -> bb.MinimumZ + (0.5 * (bb.MaximumZ - bb.MinimumZ))
        | _ -> failwith "unwrapBounding :: Vectors only keep track of three dimensions"
        
    (* Used for R* Datastructure *)
    // Divides array in chunks of n (with a rest)
    let rec divide (a : 'a[]) (index : int) (n : int) acc =
        if (index+n) < a.Length then
            divide a (index+n) n ((Array.sub a index n) :: acc)
        else
            ((Array.sub a index (a.Length - index)) :: acc)
            
    (* Used for R* Datastructure *)
    // Creates a node from a Tree array, with the correct bounding box
    let mkNode (n : Tree[]) =
        let rec inner (list : Tree list) =
            match list with
            | head :: tail when tail.IsEmpty        -> head
            | fst :: snd :: tail when tail.IsEmpty  -> Node(RgetBB fst + RgetBB snd, fst, snd)
            | _                                     -> failwith "mkNode :: Unexpected"
        Array.toList n |> inner
    
    (* Used for R* Datastructure *)
    // Generate a full R* tree from a set of nodes!
    let rec findParent (size : int) (nodes : Tree[]) =
        if (size < 2) then failwith "findParent :: Datastructure does not support chunks of size 1!"
        if nodes.Length = 1 then nodes.[0] else // Found parent node
            Array.sortInPlaceBy(fun element -> unwrapBounding (RgetBB element) 0) nodes                                         // Sort by X
            let n3 = divide nodes 0 (size*size*size) List.empty                                                                 // Divide in N^3
            List.iter(fun array -> Array.sortInPlaceBy(fun element -> unwrapBounding (RgetBB element) 1) array) n3              // Sort by Y
            let n2 = List.fold(fun acc array -> (divide array 0 (size*size) List.empty) @ acc) List.empty n3                    // Divide in N^2
            List.iter(fun array -> Array.sortInPlaceBy(fun element -> unwrapBounding (RgetBB element) 2) array) n2              // Sort by Z 
            let n = (List.fold(fun acc array -> (divide array 0 (size) List.empty) @ acc) List.empty n2)                        // Divide in N
            findParent size (List.toArray (List.map(fun element -> mkNode element) n))                                          // Recursion

    (* Used for R* Datastructure *)
    // Only here because the types of Tree should'nt be exposed
    let shapeToNode (s : Shape) =
        LeafShape (s.bounds, s)
     
    (* Used for R* Datastructure *)
    // Only here to not expose leaf size
    let nodesToTree list =
        findParent 2 list
        
    (* Used for R* Datastructure *)
    // Returns all shapes that might be hit by the ray
    let hitShapes (t : Tree) (r : Ray) =
        let isHit = Base.isHit r
        let rec inner (t : Tree) : (float * (float * (unit -> Material.Material * Vector * Point))) option =
            match t with
            | Node(bb, right, left) -> match (RgetBB right |> isHit, RgetBB left |> isHit) with
                                       | (a, b) when a.IsNone && b.IsNone      -> None
                                       | (a, _) when a.IsNone                  -> inner left
                                       | (_, b) when b.IsNone                  -> inner right
                                       | (a, b) when snd a.Value < fst b.Value -> NonOverlap right left
                                       | (a, b) when fst a.Value > snd b.Value -> NonOverlap left right
                                       | (a, b) when fst a.Value < fst b.Value -> Overlap right left
                                       | (a, b) when fst a.Value > fst b.Value -> Overlap left right
                                       | (a, b)                                -> EqualHitDist right left
            | LeafShape (bb, s) -> let result = s.updatedHitFunction r
                                   if result.IsNone then None else Some(fst result.Value, result.Value) // Actual hit
            | Empty -> failwith "hitShapes :: Does not handle type Tree.Empty well..."

        and NonOverlap (first : Tree) (second : Tree) =
            let result = inner first
            // Only evaluate other branch if this does not return a hit...
            if result.IsSome then result else inner second

        and Overlap (first : Tree) (second : Tree) =
            match inner first with
            | None            -> // Did not hit first, try second
                                 inner second
            | Some(hit, func) -> // Check if hit is closer than boundingbox entry
                                 let t = RgetBB second |> isHit
                                 if t.IsNone then Some(hit, func) else
                                 if (hit < fst t.Value) then Some(hit, func) else
                                 // The other 'branch' must be evaluated...
                                 let altResult = inner second
                                 if (altResult.IsSome) && fst altResult.Value < hit then altResult else Some(hit, func)     
                                  
        and EqualHitDist (a : Tree) (b : Tree) =
            // Both must be evaluated - closest hit is returned
            match (inner a, inner b) with
            | None, None                         -> None
            | Some(t,r), None                    -> Some(t,r)
            | None, Some(t,r)                    -> Some(t,r)
            | Some(t,r), Some(t2,r2) when t < t2 -> Some(t, r)
            | Some(t,r), Some(t2,r2) when t > t2 -> Some(t2, r2)
            | Some(t,r) as result, _             -> result

        let result = inner t
        if (result).IsNone then None else Some(snd result.Value)

    (* Used for R* Datastructure *)
    let shapesToTree s = List.map(fun element -> shapeToNode element) s |> List.toArray |> nodesToTree



    // ############ START OF THE NEW KD ERA ############ //
    //
    //             o - - - -x - - - - o
    //             | \      |\       | \
    //             |  \     | \      |  \
    //             |   \    |  \     |   \
    //             |    o - | - x - -|- - o
    //             |    |   |   |    |    |
    //             |    |   |   |    |    |
    //             o - -| - x - | - -o    |
    //              \   |    \  |     \   |
    //               \  |     \ |      \  |
    //                \ |      \|       \ |
    //                  o - - - x - - - - o
    //

    (* Used for KD Datastructure *)
    let bounds (t : TreeShape) =
        match t with
        | TrisNode(tris)    -> tris.bounds
        | ShapeNode(shape)  -> shape.bounds

    (* Used for KD Datastructure *)
    /// Retrieves origin component of the ray for the given axis
    let rayOrig (ray: Ray) (axis: Axis) = 
        match axis with 
        | X -> ray.Origin.X
        | Y -> ray.Origin.Y
        | Z -> ray.Origin.Z

    (* Used for KD Datastructure *)
    /// Retrieves direction component of the ray for the given axis
    let rayDir (ray: Ray) (axis: Axis) = 
        match axis with 
        | X -> ray.Direction.X
        | Y -> ray.Direction.Y
        | Z -> ray.Direction.Z

    (* Used for KD Datastructure *)
    let axisBound (bb: BoundingBox) (axis: Axis) (min: bool) = 
        match axis with 
        | X -> if min then bb.MinimumX else bb.MaximumX
        | Y -> if min then bb.MinimumY else bb.MaximumY
        | Z -> if min then bb.MinimumZ else bb.MaximumZ
        
    (* Used for KD Datastructure *)
    /// Get the combined bounding box of the triangle meshes
    let KDcombineBB (array : TreeShape[]) =
        let someBB = array.[0] |> bounds

        let mutable minPoint = Point(someBB.MinimumX, someBB.MinimumY, someBB.MinimumZ)
        let mutable maxPoint = Point(someBB.MaximumX, someBB.MaximumY, someBB.MaximumZ)
        for shape in array do
            let bb = shape |> bounds
            minPoint <- Point(min minPoint.X bb.MinimumX, min minPoint.Y bb.MinimumY, min minPoint.Z bb.MinimumZ) 
            maxPoint <- Point(max maxPoint.X bb.MaximumX, max maxPoint.Y bb.MaximumY, max maxPoint.Z bb.MaximumZ)

        BoundingBox (minPoint, maxPoint)

    (* Used for KD Datastructure *)
    // Splits the given triangles by the given plane
    let splitByPlane (axis: Axis) (divPlane: float) (shapes: TreeShape[]) = 
        let left = new ResizeArray<TreeShape>()
        let right = new ResizeArray<TreeShape>()
        let mutable overlap = 0
        for shape in shapes do
            let goLeft = (axisBound (shape |> bounds) axis true) <= divPlane
            let goRight = (axisBound (shape |> bounds) axis false) > divPlane
            overlap <- if goLeft && goRight then overlap + 1 else overlap
            if goLeft then left.Add(shape)
            if goRight then right.Add(shape)
        (overlap, (left.ToArray(), right.ToArray()))    

    (* Used for KD Datastructure *)
    // Fine selection of split functions:
    let minFunc (axis: Axis) (s: TreeShape) = (axisBound (s |> bounds) axis true)
    let midFunc (axis: Axis) (s: TreeShape) = (axisBound (s |> bounds) axis true) + 0.5 * ((axisBound (s |> bounds) axis false) - (axisBound (s |> bounds) axis true))
    let maxFunc (axis: Axis) (s: TreeShape) = (axisBound (s |> bounds) axis false) + 0.0000001
    
    (* Used for KD Datastructure *)
    // Performs the split with the least overlap
    let split (bb: BoundingBox) (shapes: TreeShape[]) (sortFunc : Axis -> TreeShape -> float) = 
        let findSplit (axis: Axis) (sortFunc) = 
            Array.sortInPlaceBy(sortFunc axis) shapes
            let splitVal = sortFunc axis (shapes.[shapes.Length/2])
            (splitVal, splitByPlane axis splitVal shapes) |> fun (sv, (overlap, tpl)) -> (sv, overlap, tpl)

        // By pragmatic testing it seems that middle splitting works the best
        let xRes = findSplit X midFunc 
        let yRes = findSplit Y midFunc
        let zRes = findSplit Z midFunc
            
        let xOverlap = sndT xRes 
        let yOverlap = sndT yRes
        let zOverlap = sndT zRes

        if xOverlap < (min yOverlap zOverlap) then (X, xRes) // X split is best
        else if yOverlap < zOverlap then (Y, yRes) // Y split is best
        else (Z, zRes) // Z split is best

    (* Used for KD Datastructure *)
    let mkKDleaf bb (s: TreeShape[]) = 
        KDLeaf (bb,s)
     
    (* Used for KD Datastructure *)
    // Empty Heuristic
    let clampBB (splitVal: float) (axis: Axis) (left: bool) (bb: BoundingBox) =
        if left then
            match axis with
            | X -> BoundingBox(Point(bb.MinimumX, bb.MinimumY, bb.MinimumZ), Point(min splitVal bb.MaximumX, bb.MaximumY, bb.MaximumZ))
            | Y -> BoundingBox(Point(bb.MinimumX, bb.MinimumY, bb.MinimumZ), Point(bb.MaximumX, min splitVal bb.MaximumY, bb.MaximumZ))
            | Z -> BoundingBox(Point(bb.MinimumX, bb.MinimumY, bb.MinimumZ), Point(bb.MaximumX, bb.MaximumY, min splitVal bb.MaximumZ))
        else   
            match axis with
            | X -> BoundingBox(Point(max splitVal bb.MinimumX, bb.MinimumY, bb.MinimumZ), Point(bb.MaximumX, bb.MaximumY, bb.MaximumZ))
            | Y -> BoundingBox(Point(bb.MinimumX, max splitVal bb.MinimumY, bb.MinimumZ), Point(bb.MaximumX, bb.MaximumY, bb.MaximumZ))
            | Z -> BoundingBox(Point(bb.MinimumX, bb.MinimumY, max splitVal bb.MinimumZ), Point(bb.MaximumX, bb.MaximumY, bb.MaximumZ))
        
    (* Used for KD Datastructure *)
    /// <summary>Converts a list of shapes to a Tree</summary>
    let mkKDTree (all : TreeShape[]) (sortFunc : Axis -> TreeShape -> float) =
        let rec inner (array : TreeShape[]) (levelCounter: int) =
            let startTask = levelCounter < 3
            let bb = KDcombineBB array
            let (axis, (splitVal, overlap, (left, right))) = split bb array sortFunc

            let n = array.Length
            let overlapPercentage = ((float) overlap / (float) n)
            // Check for 70% overlap!
            if (left.Length = n) || (right.Length = n) then mkKDleaf bb array else
                if overlapPercentage > 0.70 then mkKDleaf bb array                
                else
                    if startTask then 
                        let leftNode = Task.Factory.StartNew(fun () -> emptyHeuristic left splitVal axis bb true (levelCounter+1))
                        let rightNode = Task.Factory.StartNew(fun () -> emptyHeuristic right splitVal axis bb false (levelCounter+1))
                        KDNode(splitVal, axis, bb, leftNode.Result, rightNode.Result)
                    else 
                    KDNode(splitVal, axis, bb, (emptyHeuristic left splitVal axis bb true (levelCounter+1)), (emptyHeuristic right splitVal axis bb false (levelCounter+1))) 
        and emptyHeuristic (array: TreeShape[]) (splitVal: float) (axis: Axis) (bb: BoundingBox) (left: bool) (levelCounter: int) =
            let threshold = match levelCounter with
                            | 0 -> 0.30
                            | 1 -> 0.20
                            | 2 -> 0.15
                            | 3 -> 0.12
                            | _ -> 0.10

            let parentBB = clampBB splitVal axis left bb
            let currentBB = clampBB splitVal axis left <| KDcombineBB array
            
            let deltaTopX = parentBB.MaximumX - currentBB.MaximumX
            let deltaTopY = parentBB.MaximumY - currentBB.MaximumY
            let deltaTopZ = parentBB.MaximumZ - currentBB.MaximumZ
            
            let deltaBtmX = currentBB.MinimumX - parentBB.MinimumX 
            let deltaBtmY = currentBB.MinimumY - parentBB.MinimumY 
            let deltaBtmZ = currentBB.MinimumZ - parentBB.MinimumZ 

            // Required by R*
            let xTopPercentage = deltaTopX / (parentBB.MaximumX-parentBB.MinimumX)
            let yTopPercentage = deltaTopY / (parentBB.MaximumY-parentBB.MinimumY)
            let zTopPercentage = deltaTopZ / (parentBB.MaximumZ-parentBB.MinimumZ)

            let xBtmPercentage = deltaBtmX / (parentBB.MaximumX-parentBB.MinimumX)
            let yBtmPercentage = deltaBtmY / (parentBB.MaximumY-parentBB.MinimumY)
            let zBtmPercentage = deltaBtmZ / (parentBB.MaximumZ-parentBB.MinimumZ)
            // End R*

            let xPercentage = max xTopPercentage xBtmPercentage
            let yPercentage = max yTopPercentage yBtmPercentage 
            let zPercentage = max zTopPercentage zBtmPercentage
            
            // START R* NODE
            let maxPercentage = max xPercentage (max yPercentage zPercentage)
            if (xTopPercentage + yTopPercentage + zTopPercentage + xBtmPercentage + yBtmPercentage + zBtmPercentage) > (max (maxPercentage * 2.5) (threshold * 2.5)) then
                RSplit (currentBB, (inner array levelCounter))
            else
            // END R* NODE

            if maxPercentage > threshold then
                if xPercentage > (max yPercentage zPercentage) then // X
                    if (deltaTopX > deltaBtmX) then
                        KDNode(currentBB.MaximumX, X, currentBB, inner array levelCounter, KDEmpty)
                    else
                        KDNode(currentBB.MinimumX, X, currentBB, KDEmpty, inner array levelCounter)
                else if yPercentage > zPercentage then // Y
                   if (deltaTopY > deltaBtmY) then
                        KDNode(currentBB.MaximumY, Y, currentBB, inner array levelCounter, KDEmpty)
                    else
                        KDNode(currentBB.MinimumY, Y, currentBB, KDEmpty, inner array levelCounter)
                else // Z
                    if (deltaTopZ > deltaBtmZ) then
                        KDNode(currentBB.MaximumZ, Z, currentBB, inner array levelCounter, KDEmpty)
                    else
                        KDNode(currentBB.MinimumZ, Z, currentBB, KDEmpty, inner array levelCounter)
            else // Could not optimize with KD split
                inner array levelCounter
        inner all 0
        
    // ############ USED FOR SEARCHING ############ //
    
    (* Used for KD Datastructure *)
    // Returns the bounding box for the given node
    let getBB (t: KD) =
        match t with
        | KDNode (_,_,bb,_,_) -> bb
        | KDLeaf (bb, _) -> bb
        | KDEmpty -> failwith "KDgetBB :: The empty item does not contain a bounding box" 
        | RSplit (bb, _) -> bb
    
    (* Used for KD Datastructure *)
    let KDdist (r: Ray) (splitVal: float) (axis: Axis) =
        match axis with
        | X  -> (splitVal - r.Origin.X) * r.InverseDir.X
        | Y  -> (splitVal - r.Origin.Y) * r.InverseDir.Y
        | Z  -> (splitVal - r.Origin.Z) * r.InverseDir.Z
        
    (* Used for KD Datastructure *)       
    let order (rayComponent: float) (left: KD) (right: KD) = 
        if rayComponent > 0.0 then (left, right) else (right, left)

    (* Used for KD Datastructure *)
    let hitTreeshape (s : TreeShape) (r : Ray) (dist : float) = 
        match s with
        | TrisNode(t)  -> let (d, res) = t.intersect r dist
                          if res.IsNone then (d, EmptyOutput)
                          else if d < dist then (d, TrisOutput(res.Value)) else (d, EmptyOutput)
        | ShapeNode(s) -> let res = s.updatedHitFunction r
                          if res.IsNone then (dist, EmptyOutput) else
                          let (d, res) = res.Value
                          if d < dist then (d, ShapeOutput(res)) else (d, EmptyOutput)

    (* Used for KD Datastructure *)
    let isNoneOutput (o : Output) =
        match o with
        | EmptyOutput -> true
        | _           -> false

    let getPrecision (t : TreeShape) =
        match t with
        | TrisNode(_)  -> 0.00001
        | ShapeNode(s) -> s.precision

    (* Used for KD Datastructure *)
    let rec search (node : KD) (r : Ray) (t : float) (t' : float) (bestDist : float) : (float * float) * Output = 
        match node with 
        | KDNode(splitVal,axis,bb,left,right) 
                                    -> // No shape can be closer to the current hitpoint in this node...
                                       if bestDist < t then ((bestDist,0.0), EmptyOutput) else
        
                                       // Best case possible!
                                       if (rayDir r axis) > -0.000001 && (rayDir r axis) < 0.000001 then
                                            if rayOrig r axis <= splitVal then search left r t t' bestDist
                                            else search right r t t' bestDist
                                       else 
                                       
                                       // Calculate...
                                       let dist = KDdist r splitVal axis
                                       let (first, second) = order (rayDir r axis) left right

                                       // Missed first completely or hit "before" this bb
                                       if dist <= t || dist <= 0.0 then
                                        search second r t t' t'
                                       else
                                        
                                       // Missed second completely
                                       if dist >= t' then
                                        search first r t t' dist
                                       else
                                       
                                       // Both are possible, check both!
                                       let r1 = search first r t dist dist
                                       if (snd r1) |> isNoneOutput then search second r dist t' t'
                                       else
                                            let r2 = search second r dist t' t'
                                            if fst r1 < fst r2 then r1 else r2
                                                                                                            
        | KDLeaf(bb,shapes)         -> // Hit the closest shape!
                                       let (dist, res, precision) = Array.fold(fun (closest : float * Output * float) (element : TreeShape) ->
                                                                                 match sndT closest with
                                                                                 | EmptyOutput  -> let (f, res) = hitTreeshape element r (fstT closest)
                                                                                                   (f, res, element |> getPrecision)
                                                                                 | _            -> let (dist, res) = hitTreeshape element r (fstT closest)
                                                                                                   if dist < fstT closest then (dist, res, element |> getPrecision) else closest
                                                                              ) (bestDist, EmptyOutput, 0.0) shapes
                                       ((dist, precision), res)
                                                
        | RSplit(bb,KD)             -> // R* split (special case)
                                       let bbHit = isHit r <| bb
                                       if bbHit.IsNone then ((bestDist, 0.0), EmptyOutput) else
                                       let (t, t') = bbHit.Value
                                       search KD r t t' bestDist
                                                                  
        | KDEmpty                   -> ((bestDist, 0.0), EmptyOutput)

    (* Used for KD Datastructure *)
    let meshHitFunc (tree : KD) (r : Ray) =
        let hit = isHit r <| getBB tree
        if hit.IsNone then None else
        let (t, t') = hit.Value

        match search tree r t t' t' with
        | (_, EmptyOutput)             -> None
        | ((dist,_), TrisOutput(func)) -> Some(dist, func)
        | _                            -> failwith "meshHitFunc : Did you use a scene tree as a mesh?"

    (* Used for KD Datastructure *)
    let sceneHitFunc (tree : KD) (r : Ray) (distToBeat : float) =
        let hit = isHit r <| getBB tree
        if hit.IsNone then None else
        let (t, t') = hit.Value

        if(t > distToBeat) then None else

        match search tree r t t' distToBeat with
        | (_, EmptyOutput)                             -> None
        | ((dist, precision), ShapeOutput(func))       -> Some(dist, func, precision)
        | _                                            -> failwith "sceneHitFunc : Check if ID is being set properly!"

    (* Used for KD Datastructure *)
    let TrisNodeKingdom (triangles : Tris[]) =
        Array.init triangles.Length (fun index -> TrisNode(triangles.[index]))
     
    (* Used for KD Datastructure *)
    let ShapeNodeKingdom (shapes : Shape[]) =
        Array.init shapes.Length (fun index -> ShapeNode(shapes.[index]))

    (* Used for KD Datastructure *)
    let meshTree (triangles : Tris[]) =
        let tree = TrisNodeKingdom triangles |> mkKDTree <| midFunc
        tree

    (* Used for KD Datastructure *)
    let sceneTree (shapes : Shape[]) =
        let tree = ShapeNodeKingdom shapes |> mkKDTree <| maxFunc
        tree
        