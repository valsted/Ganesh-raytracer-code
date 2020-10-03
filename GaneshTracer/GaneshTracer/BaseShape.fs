namespace GaneshTracer

module BaseShape =
    open Base
    open ExprParse
    open ExprToPoly
    open PolynomialCalc
    open Material
    open Shape
    open Texture
    open Datastructure
    
    type BaseShape = 
        | Implicit of hitfunc:poly * normalvector:(poly * poly * poly) * insidefunc:simpleExpr
        | PLY of KD
    
    (*Extracts the polynomial used for the hitfunction from an implicit baseShape*)
    let getPoly (Implicit(p,_,_)) = p

    (*Creates an Implicit BaseShape from a stringrepresentation of an expression*)
    let mkImplicit (s:string) : BaseShape = 
        let originalExpr = parseExpressionString s
        let simple = exprToSimpleExpr originalExpr
        let difXPoly = derivePoly (simpleExprToPoly simple "x")
        let difYPoly = derivePoly (simpleExprToPoly simple "y")
        let difZPoly = derivePoly (simpleExprToPoly simple "z")
        let distPoly = convertToRayPoly originalExpr
        Implicit(distPoly, (difXPoly, difYPoly, difZPoly), exprToRawSimpleExpr originalExpr)
    
    (*Creates the insidefunction from an Implicit BaseShape*)
    let insideFunction (Implicit(_,_,ins)) (p:Point) : bool =
        let values = Map.ofList [("x", p.X); ("y", p.Y); ("z", p.Z)]
        calculateSimpleExpr ins values <= 0.0
    
    (*Calculates the NormalVector at a point of an Implicit BaseShape*)
    let calculateNormal (Implicit(_,df,_)) (hp: Point) : Vector =
        let values = Map.ofList [("x", hp.X); ("y", hp.Y); ("z", hp.Z)]
        match df with
        |(x, y, z) -> Base.mkNormVector (calculatePoly x "x" values) (calculatePoly y "y" values) (calculatePoly z "z" values) 

    (*Creates a function that can be converted to a hitfunction from a BaseShape, when given a Material*)
    let shapeFunction (b:BaseShape) : (Material -> Ray -> (float * (unit -> Material * Vector * Point))option) =
        let hitfunc (mat:Material) (r:Ray) =
            let values = ExprToPoly.calcPolyValues r (getPoly b)
            let dist = solvePolynomial values
            if dist = None
            then None
            else let details () =
                    let hitpoint = r.Origin + r.Direction * Option.get dist
                    let norm = calculateNormal b hitpoint
                    (mat, norm, hitpoint)
                 Some(Option.get dist, details)
        hitfunc
    
    (*Creates a Shape from a BaseShape with the given Texture*)
    let mkShape (shape: BaseShape) (tex: Texture) = 
        match shape with
        | Implicit(_, _,_) -> Shape(shapeFunction shape (getMaterialAt tex (0.0, 0.0)), insideFunction shape, None, 0.000175) 
        | PLY(tree)        -> let hitfunc (r: Ray) =  // Map Tris hitfunc to match shape API
                                  match meshHitFunc tree r with
                                  | None                   -> None
                                  | Some(dist, detailFunc) -> let details () = 
                                                                  let (hp, norm, uv) = detailFunc ()
                                                                  ((getMaterialAt tex uv), norm, hp)
                                                              Some(dist, details)
                              Shape(hitfunc, (fun p -> false), Some(getBB tree)) 
    
    (*Creates a PLY BaseShape of the .ply file at the specified filepath*)
    let mkPLY (filename: string) (smooth: bool)= 
        match Import.parsePLY filename smooth with
        | None        -> failwith "mkPLY :: Parser returned 0 faces"
        | Some(faces) -> 
            let tris = Array.Parallel.map(fun (v0,v1,v2) -> mkTris (v0, v1, v2) smooth) faces // Construct tris shapes from vertex triples
            PLY(meshTree(tris)) //Construct KD tree for mesh
