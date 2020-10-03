namespace GaneshTracer

open Camera
open Base
open Scene
open Transformation

module API = 
  type vector = Vector
  type point = Point
  type colour = Colour.Colour
  type material = Material.Material
  type shape = Shape.Shape
  type baseShape = BaseShape.BaseShape
  type texture = Texture.Texture
  type camera = Camera
  type scene = Scene
  type light = Light.Light
  type ambientLight = AmbientLight.AmbientLight
  type transformation = AffineTransformation

  let mkVector (x : float) (y : float) (z : float) : vector = Vector(x, y, z) 
  let mkPoint (x : float) (y : float) (z : float) : point = Point(x, y, z) 
  let fromColor (c : System.Drawing.Color) : colour = Colour.fromColor c 
  let mkColour (r : float) (g : float) (b : float) : colour = Colour.mkColour r g b 

  let mkMaterial (c : colour) (r : float) : material = Material.create c r
  let mkTexture (f : float -> float -> material) : texture = Texture.mkTexture f
  let mkMatTexture (m : material) : texture = Texture.mkMatTexture m

  let mkShape (b : baseShape) (t : texture) : shape = BaseShape.mkShape b t                                                    
  let mkSphere (p : point) (r : float) (m : texture) : shape = ShapeFactory.mkSphere p r m 
  let mkRectangle (bottomLeft : point) (topLeft : point) (bottomRight : point)  (t : texture) : shape = ShapeFactory.mkRectangle bottomLeft topLeft bottomRight t
  let mkTriangle (a:point) (b:point) (c:point) (m : material) : shape = ShapeFactory.mkTriangle a b c m 
  let mkPlane (m : texture) : shape = ShapeFactory.mkTexturedGround m
  let mkImplicit (s : string) : baseShape = BaseShape.mkImplicit s
  let mkPLY (filename : string) (smooth : bool) : baseShape = BaseShape.mkPLY filename smooth

  let mkHollowCylinder (c : point) (r : float) (h : float) (t : texture) : shape = ShapeFactory.mkOpenCylinder c r h t
  let mkSolidCylinder (c : point) (r : float) (h : float) (t : texture) (top : texture) (bottom : texture) : shape
      = ShapeFactory.mkClosedCylinder c r h t top bottom
  let mkDisc (c : point) (r : float) (t : texture) : shape = ShapeFactory.mkDisc c r t
  let mkBox (low : point) (high : point) (front : texture) (back : texture) (top : texture) (bottom : texture) (left : texture) (right : texture) : shape
      = ShapeFactory.mkBox low high front back top bottom left right
 
  let group (s1 : shape) (s2 : shape) : shape = CSG.group s1 s2
  let union (s1 : shape) (s2 : shape) : shape = CSG.union s1 s2
  let intersection (s1 : shape) (s2 : shape) : shape = CSG.intersection s1 s2 
  let subtraction (s1 : shape) (s2 : shape) : shape = CSG.subtraction s1 s2

  let mkCamera (pos : point) (look : point) (up : vector) (zoom : float) (width : float)
    (height : float) (resX : int) (resY : int) : camera = mkCamera pos look up zoom width height resX resY
  let mkThinLensCamera (pos : point) (look : point) (up : vector) (zoom : float) (width : float)
    (height : float) (pwidth : int) (pheight : int) (lensRadius : float) (fpDistance : float) : camera = failwith "mkThinCamera not implemented"
  
  let mkLight (p : point) (c : colour) (i : float) : light = Light.mkPointLight p c i
  let mkAmbientLight (c : colour) (i : float) : ambientLight = AmbientLight.mkAmbientLight c i 

  let mkScene (s : shape list) (l : light list) (a : ambientLight) (m : int) : scene = mkScene s l a m 
  let renderToScreen (sc : scene) (c : camera) : unit = BitMapRenderer.renderToScreen (trace sc c TraceMode.Hybrid None)
  let renderToFile (sc : scene) (c : camera) (path : string) : unit = BitMapRenderer.writeToFile (trace sc c TraceMode.Hybrid None) path 

  let translate (x : float) (y : float) (z : float) : transformation = Move(x,y,z)
  let rotateX (angle : float) : transformation = Rotate(angle, X)
  let rotateY (angle : float) : transformation = Rotate(angle, Y)
  let rotateZ (angle : float) : transformation = Rotate(angle, Z)
  let sheareXY (distance : float) : transformation = Shear(X,Y,distance)
  let sheareXZ (distance : float) : transformation = Shear(X,Z,distance)
  let sheareYX (distance : float) : transformation = Shear(Y,X,distance)
  let sheareYZ (distance : float) : transformation = Shear(Y,Z,distance)
  let sheareZX (distance : float) : transformation = Shear(Z,X,distance)
  let sheareZY (distance : float) : transformation = Shear(Z,Y,distance)
  let scale (x : float) (y : float) (z : float) : transformation = Scale(x,y,z)
  let mirrorX : transformation = Mirror(X)
  let mirrorY : transformation = Mirror(Y)
  let mirrorZ : transformation = Mirror(Z)
  let mergeTransformations (ts : transformation list) : transformation = combineTranformation ts
  let transform (sh : shape) (tr : transformation) : shape = tranformShapeAPI sh tr