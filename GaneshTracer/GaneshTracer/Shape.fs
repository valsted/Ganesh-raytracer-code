namespace GaneshTracer

open Base
open Material

module Shape =
    [<Sealed>]
    (*Type used to represent a Shape with member functions to access properties of the Shape*)
    type Shape (hitFunc: Ray -> ((float * (unit -> Material * Vector * Point)) option), insideFunc: (Point -> bool), bounding: BoundingBox option, precision: float) = 
        new (hitFunc: Ray -> ((float * (unit -> Material * Vector * Point)) option), insideFunc: (Point -> bool), bounding: BoundingBox option) = 
            Shape(hitFunc, insideFunc, bounding, 0.000001)
        member this.updatedHitFunction = hitFunc
        member this.inside = insideFunc 
        member this.bounds = if bounding = None
                             then minMaxBounds
                             else bounding.Value
        member this.boundsOpt = bounding
        member this.precision = precision

    (*Transform a Shape creating a new Shape*)
    let transformWithNormal (rayfunc:(Ray -> Ray)) (normalfunc:(Vector -> Vector)) (pointfunc:(Point -> Point)) (BBfunc:(BoundingBox -> BoundingBox)) (oldShape : Shape) : Shape = 
        let newHitFunction (ray:Ray)  = 
            let tempHitOption = (rayfunc >> oldShape.updatedHitFunction) ray
            if Option.isNone tempHitOption then None 
            else
                let tempHit = Option.get tempHitOption
                let dist = fst (tempHit)
                let newUnitFunc () =
                    let unitTriple = snd (tempHit) ()
                    let mat = fstT unitTriple
                    let norm = normalfunc (sndT unitTriple)
                    let hitP = ray.Origin + ray.Direction*dist  
                    (mat, norm, hitP)
                Some(dist,newUnitFunc)
        
        let newInside (p : Point) : bool = oldShape.inside (pointfunc p)
        let transformedBB = if oldShape.boundsOpt.IsNone then None else Some(BBfunc oldShape.boundsOpt.Value)
        Shape(newHitFunction, newInside, transformedBB, oldShape.precision)

    let getShapeBoundingBox (s : Shape) = s.bounds

    
    
    (*********************************************)
    (*         Mesh only functions               *)
    (*********************************************)

    type Tris(intersectFunc: Ray -> float -> float * (unit -> Point * Vector * (float*float)) option, bounds: BoundingBox) = 
        member this.intersect = intersectFunc
        member this.bounds = bounds
    
    let mkTris (ov:Vertex, pv:Vertex, qv:Vertex) (smooth: bool) = 
        //Extracting Vertex data
        let o = ov.P
        let p = pv.P
        let q = qv.P
        let oNorm = ov.Norm
        let pNorm = pv.Norm
        let qNorm = qv.Norm
        let oUV = ov.UV
        let pUV = pv.UV
        let qUV = qv.UV

        // Boundingbox: 
        let minP = Point(min (min o.X p.X) q.X, min (min o.Y p.Y) q.Y, min (min o.Z p.Z) q.Z)
        let maxP = Point(max (max o.X p.X) q.X, max (max o.Y p.Y) q.Y, max (max o.Z p.Z) q.Z)
        let bounds = mkBoundingBox minP maxP
        
        // Hitfunc precalc
        let u = distance o p
        let v = distance o q
        let a = (o.X - p.X)
        let b = (o.X - q.X)
        let e = (o.Y - p.Y)
        let f = (o.Y - q.Y)
        let i = (o.Z - p.Z)
        let j = (o.Z - q.Z)
        let precalc = e * j - f * i
        let planeNorm = normCrossProduct u v

        //Calculating the hit function
        let intersect (r: Ray) (bestDist: float) =
            // Triangle intersection check ray-constants: 
            let c = (r.Direction.X)
            let d = (o.X - r.Origin.X)
            let g = (r.Direction.Y)
            let h = (o.Y - r.Origin.Y)
            let k = (r.Direction.Z)
            let l = (o.Z - r.Origin.Z)

            // Find intersection with triangle plane to beat distance first: 
            let pB = planeNorm * r.Direction
            if abs pB < 0.000001 then (bestDist, None)
            else 
            let pA = -(planeNorm * (-Vector(d,h,l)))
            let planeDist = pA / pB
            if planeDist < 0.0 || planeDist > bestDist then (bestDist, None)
            else

            // Continue with fullblown intersection check
            match (a*(f * k - g * j) + b*(g * i - e * k) + c * precalc) with
            | D when D = 0.0 -> (bestDist, None)
            | D ->
                // Solve using cramers rule
                let speedD = 1.0 / D
                let beta = (d*(f*k-g*j)+b*(g*l-h*k)+c*(h*j-f*l)) * speedD
                let gamma = (a*(h*k-g*l)+d*(g*i-e*k)+c*(e*l-h*i)) * speedD                
                let theta = beta + gamma 
                // Is found values outside triangle bounds?
                if beta < 0.0 || beta > 1.0 || gamma < 0.0 ||gamma > 1.0 || theta < 0.0 || theta > 1.0 then (bestDist, None)
                else
                let details () = // Compute hit details
                    let hitPoint = o + beta*u + gamma*v
                    let alpha = 1.0 - theta
                    let normal = if smooth then //Creates a normal based on the vertex normals
                                    normalize (alpha*oNorm + beta*pNorm + gamma*qNorm)
                                 else planeNorm //The standard normal
                    let u = alpha * (fst oUV) + beta * (fst pUV) + gamma * (fst qUV)
                    let v = alpha * (snd oUV) + beta * (snd pUV) + gamma * (snd qUV)
                    (hitPoint, normal, (u,v))
                (planeDist, Some(details))
        Tris(intersect, bounds)