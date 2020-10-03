open System.Drawing
open System
open Runner.Video
open GaneshTracer
open GaneshTracer.API
open Transformation
open GaneshTracer.Texture
open GaneshTracer.Import
open System.Windows.Forms

type Render = 
        { scene : scene;
          camera : camera}

[<EntryPoint>]
let main argv =
#if circle_to_sceen
    let renderSphere () =
        let light = mkLight (Base.Point(0.0, 0.0, 4.0)) (Colour.fromColor Color.White) 1.0 in
        let ambientLight = mkAmbientLight (Colour.fromColor Color.White) 0.1 in
        let sphere = mkSphere (Base.Point(0.0, 0.0, 0.0)) 0.5 (Texture.mkMatTexture (Material.create (Colour.fromColor Color.Green) 0.0)) in
        let triangle = mkTriangle (mkPoint 0.5 0.0 -0.5) (mkPoint 0.55 0.5 0.5) (mkPoint 0.55 0.0 0.5) (Material.create (Colour.fromColor Color.DarkKhaki) 0.7) in
        let disc = mkDisc (Base.Point(0.0, 1.0, 0.0)) 0.5 (Texture.mkCheckeredTexture) in
        { camera = mkCamera (Base.Point(0.0, 0.0, 4.0)) (Base.Point(0.0, 0.0, 0.0)) (Base.Vector(0.0, 1.0, 0.0)) 1.0 4.0 4.0 800 800;
          scene = mkScene [sphere; disc; triangle] [light] ambientLight 0}
    let r = renderSphere ()
    let bitmap = BitMapRenderer.giveMeBitmap (Camera.rayGun r.scene r.camera)

    let form = new Form(Height = bitmap.Height, Width = bitmap.Width, Text = "Jens er en flot mand", BackgroundImage = bitmap) 
    form.TopMost <- true
    //form.Show()
    do Application.Run(form)
#endif

#if kd_test
    let renderSpheres () =
        let light = mkLight (Base.Point(-5.0, 5.0, 5.0)) (Colour.fromColor Color.White) 1.0 in
        let ambientLight = mkAmbientLight (Colour.fromColor Color.White) 0.1 in
        let sphere1 = mkSphere (Base.Point(-2.0, -2.5, -1.0)) 1.0 (Texture.mkMatTexture (Material.create (Colour.fromColor Color.Red) 0.0)) in
        let sphere2 = mkSphere (Base.Point(0.0, -1.5, 0.0)) 1.0 (Texture.mkMatTexture (Material.create (Colour.fromColor Color.Green) 0.0)) in
        let sphere3 = mkSphere (Base.Point(2.0, -1.5, 1.0)) 1.0 (Texture.mkMatTexture (Material.create (Colour.fromColor Color.Blue) 0.0)) in
        let sphere4 = mkSphere (Base.Point(2.0, -2.5, -1.0)) 1.0 (Texture.mkMatTexture (Material.create (Colour.fromColor Color.Red) 0.0)) in
        let sphere5 = mkSphere (Base.Point(-2.0, -1.5, 1.0)) 1.0 (Texture.mkMatTexture (Material.create (Colour.fromColor Color.Blue) 0.0)) in
        let sphere6 = mkSphere (Base.Point(-2.0, 0.0, -1.0)) 1.0 (Texture.mkMatTexture (Material.create (Colour.fromColor Color.Red) 0.0)) in
        let sphere7 = mkSphere (Base.Point(0.0, 1.0, 0.0)) 1.0 (Texture.mkMatTexture (Material.create (Colour.fromColor Color.Green) 0.0)) in
        let sphere8 = mkSphere (Base.Point(2.0, 2.0, 1.0)) 1.0 (Texture.mkMatTexture (Material.create (Colour.fromColor Color.Blue) 0.0)) in
        let sphere9 = mkSphere (Base.Point(2.0, 0.0, -1.0)) 1.0 (Texture.mkMatTexture (Material.create (Colour.fromColor Color.Red) 0.0)) in
        let sphere10 = mkSphere (Base.Point(-2.0, 2.0, 1.0)) 1.0 (Texture.mkMatTexture (Material.create (Colour.fromColor Color.Blue) 0.0)) in
        { camera = mkCamera (Base.Point(0.0, 0.0, 2.5)) (Base.Point(0.0, 0.0, 0.0)) (Base.Vector(0.0, 1.0, 0.0)) 1.0 4.0 4.0 800 800;
          scene = mkScene [sphere1; sphere2; sphere3; sphere4; sphere5; sphere6; sphere7; sphere8; sphere9; sphere10] [light] ambientLight 2}
    
    let r = renderSpheres ()
    let folderPath = System.Environment.GetFolderPath(System.Environment.SpecialFolder.DesktopDirectory) + "\kd_test.png"
    BitMapRenderer.writeToFile (Camera.rayGun r.scene r.camera) folderPath
#endif


#if sphere_to_file
    let renderSphere () =
        let light = mkLight (mkPoint 0.0 0.0 4.0) (fromColor Color.White) 1.0 in
        let ambientLight = mkAmbientLight (fromColor Color.White) 0.1 in
        let sphere = mkSphere (mkPoint 0.0 0.0 0.0) 1.0 (mkMatTexture (mkMaterial (fromColor Color.Blue) 0.0)) in
        { camera = mkCamera (mkPoint 0.0 0.0 4.0) (mkPoint 0.0 0.0 0.0) (mkVector 0.0 1.0 0.0) 1.0 2.0 2.0 500 500;
          scene = mkScene [sphere] [light] ambientLight 0}
    
    let r = renderSphere ()
    let folderPath = System.Environment.GetFolderPath(System.Environment.SpecialFolder.DesktopDirectory) + "\FISK.png"
    BitMapRenderer.writeToFile (Camera.rayGun r.scene r.camera) folderPath
#endif

#if BB_transformation_test
    let renderSphere () =
        let light = mkLight (Base.Point(3.0, 2.0, 2.0)) (Colour.fromColor Color.White) 1.0 in
        let ambientLight = mkAmbientLight (Colour.fromColor Color.White) 0.1 in
        let sphere = mkSphere (Base.Point(0.0, 0.0, 0.0)) 1.0 (Texture.mkMatTexture (Material.create (Colour.fromColor Color.Green) 0.0)) in
        
        let bounds = sphere.bounds
        System.Console.WriteLine("Untransformed: minX;"+bounds.MinimumX.ToString()+"   minY;"+bounds.MinimumY.ToString()+"   minZ;"+bounds.MinimumZ.ToString())
        System.Console.WriteLine("Untransformed: maxX;"+bounds.MaximumX.ToString()+"   maxY;"+bounds.MaximumY.ToString()+"   maxZ;"+bounds.MaximumZ.ToString())
        System.Console.WriteLine()

        let trans = Transformation.transformShape [Transformation.Shear(0.0,0.0,0.0,0.0,0.0,2.0)] sphere in
        let transBounds = trans.bounds
        System.Console.WriteLine("Transformed: minX;"+transBounds.MinimumX.ToString()+"   minY;"+transBounds.MinimumY.ToString()+"   minZ;"+transBounds.MinimumZ.ToString())
        System.Console.WriteLine("Transformed: maxX;"+transBounds.MaximumX.ToString()+"   maxY;"+transBounds.MaximumY.ToString()+"   maxZ;"+transBounds.MaximumZ.ToString())
        System.Console.WriteLine()

        { camera = mkCamera (Base.Point(0.0, 1.0, 3.0)) (Base.Point(0.0, 0.0, 0.0)) (Base.Vector(0.0, 1.0, 0.0)) 1.0 1.0 1.0 800 800;
          scene = mkScene [trans] [light] ambientLight 0}
    
    let r = renderSphere ()
    let folderPath = System.Environment.GetFolderPath(System.Environment.SpecialFolder.DesktopDirectory) + "\FISK.png"
    BitMapRenderer.writeToFile (Camera.rayGun r.scene r.camera) folderPath

    Console.WriteLine("Press a key to close...")
    Console.ReadKey() |> ignore
#endif

#if meshTriangle_test
    let stopWatch = Diagnostics.Stopwatch.StartNew();
    let renderPorsche () =
        let baseporsche = mkPLY (System.Environment.GetFolderPath(System.Environment.SpecialFolder.DesktopDirectory) + "\porsche.ply") true
        let t = mergeTransformations
                  [rotateY (-Math.PI / 4.0);
                   scale 2.0 2.0 2.0;
                   translate -2.0 4.5 0.0] in
        let white = fromColor Color.White
        let porsche = mkShape baseporsche (mkMatTexture (mkMaterial white 0.0))
        let affineporsche = transform porsche t in
        let l1 = mkLight (mkPoint 12.0 4.0 14.0) white 0.4
        let l2 = mkLight (mkPoint -6.0 2.0 6.0) (fromColor Color.Red) 0.5
        let l3 = mkLight (mkPoint -3.5 12.0 4.0) (fromColor Color.Blue) 0.5
        let p = transform (mkPlane (mkMatTexture (mkMaterial (fromColor Color.Green) 0.5)))
                  (rotateX (System.Math.PI/2.0))
        let ambientLight = mkAmbientLight (fromColor Color.Green) 0.1
        { camera = mkCamera (mkPoint 4.0 8.0 25.0) (mkPoint 0.0 0.0 -5.0) (mkVector 0.0 1.0 0.0) 4.0 4.0 4.0 1000 1000;
          scene = mkScene [p; affineporsche] [l1; l2; l3] ambientLight 2}
    let r = renderPorsche ()
    stopWatch.Stop();
    let conTime = stopWatch.Elapsed.TotalMilliseconds / 1000.0
    stopWatch.Restart();

    let folderPath = System.Environment.GetFolderPath(System.Environment.SpecialFolder.DesktopDirectory) + "\meshTriangletest.png"
    renderToFile r.scene r.camera folderPath

    stopWatch.Stop();
    let renderTime = stopWatch.Elapsed.TotalMilliseconds / 1000.0
    Console.WriteLine("Construction time: " + conTime.ToString())
    Console.WriteLine("Rendering time:    " + renderTime.ToString())
    Console.WriteLine("Done - Press a key to close...")
    Console.ReadKey() |> ignore
#endif

#if implicit_sphere_and_ground
    let renderSphere () =
        let light = mkLight (Base.Point(0.0, 0.0, 4.0)) (Colour.fromColor Color.White) 1.0 in
        let ambientLight = mkAmbientLight (Colour.fromColor Color.White) 0.1 in
        let sphere = Shape.mkShape (BaseShape.mkImplicit "x^2+y^2+z^2-1") (Material.create (Colour.fromColor Color.Red) 0.7)
        let ground = Shape.mkShape (BaseShape.mkImplicit "y+2") (Material.create (Colour.fromColor Color.Green) 0.7)
        { camera = mkCamera (Base.Point(0.0, 0.5, 4.0)) (Base.Point(0.0, 0.0, 0.0)) (Base.Vector(0.0, 1.0, 0.0)) 1.0 4.0 4.0 800 800;
          scene = mkScene [sphere; ground] [light] ambientLight 4}
    
    let r = renderSphere ()
    let folderPath = System.Environment.GetFolderPath(System.Environment.SpecialFolder.DesktopDirectory) + "\IMPLICIT.png"
    BitMapRenderer.writeToFile2 (Camera.rayGun r.scene r.camera) folderPath
#endif

#if implicit_sphere_in_box
    let renderSphere () =
        let light = mkLight (Base.Point(4.0, 4.0, 4.0)) (Colour.fromColor Color.White) 1.0 in
        let ambientLight = mkAmbientLight (Colour.fromColor Color.White) 0.1 in
        let sphere = BaseShape.mkShape (BaseShape.mkImplicit "x^4+y^4+z^4-2(x^2+y^2+z^2)+1") (Texture.mkMatTexture (Material.create (Colour.fromColor Color.Red) 0.4))
        //let sphere2 = BaseShape.mkShape (BaseShape.mkImplicit "(x^2 + y^2 + z^2)_2 - 1") (Texture.mkMatTexture (Material.create (Colour.fromColor Color.Red) 0.7))
        
        let left = BaseShape.mkShape (BaseShape.mkImplicit "x+10") (Texture.mkMatTexture (Material.create (Colour.fromColor Color.Green) 0.1))
        let right = BaseShape.mkShape (BaseShape.mkImplicit "x-10") (Texture.mkMatTexture (Material.create (Colour.fromColor Color.Yellow) 0.1))
        let ground = BaseShape.mkShape (BaseShape.mkImplicit "y+10") (Texture.mkMatTexture (Material.create (Colour.fromColor Color.Blue) 0.1))
        let top = BaseShape.mkShape (BaseShape.mkImplicit "y-10") (Texture.mkMatTexture (Material.create (Colour.fromColor Color.Orange) 0.1))
        let back = BaseShape.mkShape (BaseShape.mkImplicit "z+10") (Texture.mkMatTexture (Material.create (Colour.fromColor Color.Fuchsia) 0.1))
        let front = BaseShape.mkShape (BaseShape.mkImplicit "z-10") (Texture.mkMatTexture (Material.create (Colour.fromColor Color.White) 0.1))
        { camera = mkCamera (Base.Point(1.0, 1.5, 3.5)) (Base.Point(0.0, 0.0, 0.0)) (Base.Vector(0.0, 1.0, 0.0)) 1.0 4.0 4.0 800 800;
          scene = mkScene [sphere; left; right; ground; top; back; front] [light] ambientLight 1}
    
    let r = renderSphere ()
    let folderPath = System.Environment.GetFolderPath(System.Environment.SpecialFolder.DesktopDirectory) + "\ImplicitBox.png"
    BitMapRenderer.writeToFile (Camera.rayGun r.scene r.camera) folderPath
#endif

#if moo
    let renderSphere () =
        let light = mkLight (Base.Point(0.0, 8.0, 0.0)) (Colour.fromColor Color.White) 1.0 in
        let ambientLight = mkAmbientLight (Colour.fromColor Color.White) 0.1 in
        let tmpcow = mkPLY (System.Environment.GetFolderPath(System.Environment.SpecialFolder.DesktopDirectory) + "\cow.ply") false
        let redcow = mkShape tmpcow Texture.mkRedTexture
        let whitecow = mkShape tmpcow Texture.mkWhiteTexture
        //Leftside
        let white1 = Transformation.transformShape[(Transformation.Move (3.0, 0.0, 0.0))] whitecow
        let red1 = Transformation.transformShape[Transformation.Scale (1.0, 1.0,2.3);(Transformation.Move (3.0, 0.0, -2.25))] redcow
        let red3 = Transformation.transformShape[Transformation.Scale (1.0, 1.0,2.3);(Transformation.Move (3.0, 0.0, 2.25))] redcow
        
        //RightSide
        let white1b = Transformation.transformShape[(Transformation.Move (-3.0, 0.0, 0.0))] (Transformation.transformShape [Transformation.Rotate (Math.PI, Axis.Y)] whitecow)
        let red1b = (Transformation.transformShape[Transformation.Scale (1.0, 1.0,2.3);Transformation.Rotate (Math.PI, Axis.Y);(Transformation.Move (-3.0, 0.0, -2.25))] redcow)
        let red3b = (Transformation.transformShape[Transformation.Scale (1.0, 1.0,2.3);Transformation.Rotate (Math.PI, Axis.Y);(Transformation.Move (-3.0, 0.0, 2.25))] redcow)

        let long = Transformation.transformShape[Transformation.Rotate (Math.PI/2.0, Axis.Y);Transformation.Scale (1.24, 1.0,1.8)] whitecow

        let left = BaseShape.mkShape (BaseShape.mkImplicit "x+10") (Texture.mkMatTexture (Material.create (Colour.fromColor Color.White) 1.0))
        let right = BaseShape.mkShape (BaseShape.mkImplicit "x-10") (Texture.mkMatTexture (Material.create (Colour.fromColor Color.White) 1.0))
        let ground = BaseShape.mkShape (BaseShape.mkImplicit "y") (Texture.mkMatTexture (Material.create (Colour.fromColor Color.Green) 0.0))
        let back = BaseShape.mkShape (BaseShape.mkImplicit "z+10") (Texture.mkMatTexture (Material.create (Colour.fromColor Color.White) 1.0))
        let front = BaseShape.mkShape (BaseShape.mkImplicit "z-10") (Texture.mkMatTexture (Material.create (Colour.fromColor Color.White) 1.0))
        { camera = mkCamera (Base.Point(3.0, 8.0, 4.5)) (Base.Point(0.0, 6.5, 0.0)) (Base.Vector(0.0, 1.0, 0.0)) 1.0 4.0 4.0 800 800;
          scene = mkScene [white1; red1; red3;white1b; red1b; red3b; long; left; right; ground; back; front] [light] ambientLight 10}

    let r = renderSphere ()
    let folderPath = System.Environment.GetFolderPath(System.Environment.SpecialFolder.DesktopDirectory) + "\Moo.png"
    BitMapRenderer.writeToFile (Camera.rayGun r.scene r.camera) folderPath
#endif

#if unknown
    let renderSphere () =
        let light = mkLight (Base.Point(2.0, 8.0, 30.0)) (Colour.fromColor Color.White) 1.0 in
        let ambientLight = mkAmbientLight (Colour.fromColor Color.White) 0.1 in
        let tmpunknown = mkPLY (System.Environment.GetFolderPath(System.Environment.SpecialFolder.DesktopDirectory) + "\unknown.ply") false
        let unknown = Transformation.transformShape [Rotate (-Math.PI/2.0, Axis.X)]  (mkShape tmpunknown (Texture.mkWhiteTexture))

        let ground = BaseShape.mkShape (BaseShape.mkImplicit "y+1") (Texture.mkMatTexture (Material.create (Colour.fromColor Color.Blue) 0.6))
        { camera = mkCamera (Base.Point(2.0, 8.0, 15.0)) (Base.Point(-15.0, 6.5, 0.0)) (Base.Vector(0.0, 1.0, 0.0)) 1.3 4.0 4.0 3840 2160;
          scene = mkScene [unknown; ground;] [light] ambientLight 1}
    
    let r = renderSphere ()
    let folderPath = System.Environment.GetFolderPath(System.Environment.SpecialFolder.DesktopDirectory) + "\unknown.png"
    BitMapRenderer.writeToFile (Camera.rayGun r.scene r.camera) folderPath
#endif


#if rectangle_test
    let renderRect () =
        let light = mkLight (Base.Point(0.0, 0.0, 4.0)) (Colour.fromColor Color.White) 1.0 in
        let ambientLight = mkAmbientLight (Colour.fromColor Color.White) 0.1 in
        let rect = mkRectangle (Base.Point(-4.0, -4.0, 0.0)) (Base.Point(-4.0, 4.0, 0.0)) (Base.Point(4.0, -4.0, 0.0)) (Texture.mkMatTexture (Material.create (Colour.fromColor Color.Green) 0.0)) in
        { camera = mkCamera (Base.Point(0.0, 0.0, 4.0)) (Base.Point(0.0, 0.0, 0.0)) (Base.Vector(0.0, 1.0, 0.0)) 1.0 4.0 4.0 800 800;
          scene = mkScene [rect] [light] ambientLight 0}
    let r = renderRect ()
    let bitmap = BitMapRenderer.giveMeBitmap (Camera.rayGun r.scene r.camera)

    let form = new Form(Height = bitmap.Height, Width = bitmap.Width, Text = "TestFrame", BackgroundImage = bitmap) 
    form.TopMost <- true
    //form.Show()
    do Application.Run(form)
#endif

#if box_to_file
    let renderBox () =
        let light = mkLight (Base.Point(0.0, 0.0, 4.0)) (Colour.fromColor Color.White) 1.0 in
        let ambientLight = mkAmbientLight (Colour.fromColor Color.White) 0.5 in
        let tex = (Texture.mkMatTexture (Material.create (Colour.fromColor Color.Purple) 0.0))
        let tbox = mkBox (Base.Point(-1.0,-1.0,-1.0)) (Base.Point(1.0,1.0,1.0)) tex tex tex tex tex tex in
        let box = Transformation.transformShape [Transformation.Rotate(0.1, 0.1, 0.0) ] tbox in

        //let transformed1 = Transformation.transformShape [Transformation.Rotate(0.3, 0.3, 0.4)] box in
        { camera = mkCamera (Base.Point(0.0, 0.0, 4.0)) (Base.Point(0.0, 0.0, 0.0)) (Base.Vector(0.0, 1.0, 0.0)) 1.0 4.0 4.0 500 500;
          scene = mkScene [box] [light] ambientLight 5}
           
    let r = renderBox ()
    let folderPath = System.Environment.GetFolderPath(System.Environment.SpecialFolder.DesktopDirectory) + "\BOX.png"
    //BitMapRenderer.writeToFile (Camera.rayGun r.scene r.camera) folderPath //To file
    BitMapRenderer.renderBitmapToScreen (Camera.rayGun r.scene r.camera)      //To screen
#endif

#if open_cylinder_to_file
    let renderCylinder () =
        let light = mkLight (Base.Point(0.0, 0.0, 4.0)) (Colour.fromColor Color.White) 1.0 in
        let ambientLight = mkAmbientLight (Colour.fromColor Color.White) 0.5 in
        let tex = (Texture.mkMatTexture (Material.create (Colour.fromColor Color.Purple) 0.0))
        let cylinder = mkSolidCylinder (Base.Point(2.0, 0.0, 0.0)) 0.5 1.0 tex tex tex in //Point is ignored for now
        let tCylinder = Transformation.transformShape [Transformation.Move(0.0, 2.0, 0.0)] cylinder in
        //let transformed1 = Transformation.transformShape [Transformation.Rotate(0.3, 0.3, 0.4)] box in
        { camera = mkCamera (Base.Point(0.0, 0.0, 4.0)) (Base.Point(0.0, 0.0, 0.0)) (Base.Vector(0.0, 1.0, 0.0)) 1.0 4.0 4.0 500 500;
          scene = mkScene [tCylinder] [light] ambientLight 5 }
           
    let r = renderCylinder ()
    let folderPath = System.Environment.GetFolderPath(System.Environment.SpecialFolder.DesktopDirectory) + "\Cylinder.png"
    BitMapRenderer.writeToFile (Camera.rayGun r.scene r.camera) folderPath
#endif

#if TA_Test
    let renderTest () =
        let light = mkLight (Base.Point(0.0, 0.0, 4.0)) (Colour.fromColor Color.White) 1.0 in
        let ambientLight = mkAmbientLight (Colour.fromColor Color.White) 0.5 in
        let disc = mkDisc (Base.Point(0.0,0.0,0.0)) 1.0 (Texture.mkMatTexture (Material.create (Colour.fromColor Color.Purple) 0.0))
        let transformed1 = Transformation.transformShape [Transformation.Move(1.0,0.0,0.0) ; Transformation.Rotate(0.78, Z)] disc in
        { camera = mkCamera (Base.Point(0.0, 0.5, 2.0)) (Base.Point(0.0, 0.0, 0.0)) (Base.Vector(0.0, 1.0, 0.0)) 1.0 4.0 4.0 500 500;
          scene = mkScene [transformed1] [light] ambientLight 0 }
           
    let r = renderTest ()
    let folderPath = System.Environment.GetFolderPath(System.Environment.SpecialFolder.DesktopDirectory) + "\TA_TEST.png"
    BitMapRenderer.writeToFile (Camera.rayGun r.scene r.camera) folderPath
#endif

#if transformationSphere
    let renderSphere =
     let light = mkLight (Base.Point(0.0, 0.0, 4.0)) (Colour.fromColor Color.White) 1.0 in
     let ambientLight = mkAmbientLight (Colour.fromColor Color.White) 0.3 in
     let sphere1 = mkSphere (Base.Point(0.0, 0.0, 0.0)) 0.2 (Texture.mkMatTexture (Material.create (Colour.fromColor Color.Red) 0.5)) in
     let sphere2 = mkSphere (Base.Point(0.0, 0.0, 0.0)) 0.2 (Texture.mkMatTexture (Material.create (Colour.fromColor Color.AntiqueWhite) 0.8)) in 
     let sphere3 = mkSphere (Base.Point(0.0, 0.0, 0.0)) 0.2 (Texture.mkMatTexture (Material.create (Colour.fromColor Color.Blue) 0.5)) in
     let sphere4 = mkSphere (Base.Point(0.0, 0.0, 0.0)) 0.2 (Texture.mkMatTexture (Material.create (Colour.fromColor Color.Violet) 0.8)) in
     let transformed1 = Transformation.transformShape [Transformation.Move(0.2, 0.2, 0.2)] sphere1 in
     let transformed2 = Transformation.transformShape [Transformation.Move(-0.2, 0.2, -0.2)] sphere2 in
     let transformed3 = Transformation.transformShape [Transformation.Move(-0.2, -0.2, 0.2)] sphere3 in
     let transformed4 = Transformation.transformShape [Transformation.Move(0.2, -0.2, -0.2)] sphere4 in
     { camera = mkCamera (Base.Point(0.0, 0.0, 4.0)) (Base.Point(0.0, 0.0, 0.0)) (Base.Vector (0.0, 1.0, 0.0)) 1.0 2.0 2.0 666 666;
       scene = mkScene [transformed1 ; transformed2 ; transformed3 ; transformed4 ] [light] ambientLight 5}

    let r = renderSphere
    let folderPath = System.Environment.GetFolderPath(System.Environment.SpecialFolder.DesktopDirectory) + "\KAT.png"
    BitMapRenderer.writeToFile (Camera.rayGun r.scene r.camera) folderPath
#endif

#if testing_room
    let stopWatch = Diagnostics.Stopwatch.StartNew();

    let renderSphere =
        let light1 = Light.mkFallOff (Base.Point(-0.9, 0.8, 0.8)) (Colour.fromColor Color.White) 1.0 in
        let light2 = Light.mkFallOff (Base.Point(0.8, -0.2, 0.5)) (Colour.fromColor Color.Blue) 0.4 in
        let ambientLight = mkAmbientLight (Colour.fromColor Color.White) 0.1 in
        let sphere  = mkSphere (Base.Point(0.0, 0.0, 0.0)) 0.5 (Texture.mkMatTexture (Material.create (Colour.fromColor Color.Blue) 0.0)) in
        let floor =   mkPlane (Texture.mkMatTexture (Material.create (Colour.fromColor Color.White) 0.0))
        let ceiling = mkPlane (Texture.mkMatTexture (Material.create (Colour.fromColor Color.White) 0.0))
        let left =    mkPlane (Texture.mkMatTexture (Material.create (Colour.fromColor Color.White) 0.0))
        let right =   mkPlane (Texture.mkMatTexture (Material.create (Colour.fromColor Color.White) 0.0))
        let back =    mkPlane (Texture.mkMatTexture (Material.create (Colour.fromColor Color.White) 0.0))
        let floor'    = Transformation.transformShape [Transformation.Rotate(1.57079633, Transformation.Axis.X); Transformation.Move(0.0, -1.0, 0.0)] floor in
        let ceiling'  = Transformation.transformShape [Transformation.Rotate(1.57079633, Transformation.Axis.X); Transformation.Move(0.0, 1.0, 0.0)] ceiling in
        let left'     = Transformation.transformShape [Transformation.Rotate(1.57079633, Transformation.Axis.Y); Transformation.Move(-1.0, 0.0, 0.0)] left in
        let right'    = Transformation.transformShape [Transformation.Rotate(1.57079633, Transformation.Axis.Y); Transformation.Move(1.0, 0.0, 0.0)] right in
        let back'     = Transformation.transformShape [Transformation.Move(0.0, 0.0, -1.0)] back in
        { camera = mkCamera (Base.Point(0.0, 0.0, 1.5)) (Base.Point(0.0, 0.0, 0.0)) (Base.Vector (0.0, 1.0, 0.0)) 1.0 2.0 2.0 1280 960;
          scene = mkScene [sphere; floor'; ceiling'; left'; right'; back'] [light1; light2] ambientLight 4}

    let r = renderSphere
    stopWatch.Stop();
    let conTime = stopWatch.Elapsed.TotalMilliseconds / 1000.0
    stopWatch.Restart();

    let folderPath = System.Environment.GetFolderPath(System.Environment.SpecialFolder.DesktopDirectory) + "\profiling.png"
    renderToFile r.scene r.camera folderPath

    stopWatch.Stop();
    let renderTime = stopWatch.Elapsed.TotalMilliseconds / 1000.0
    Console.WriteLine("Construction time: " + conTime.ToString())
    Console.WriteLine("Rendering time:    " + renderTime.ToString())
    Console.WriteLine("Done - Press a key to close...")
    Console.ReadKey() |> ignore
#endif

#if profiling_scene
    let stopWatch = Diagnostics.Stopwatch.StartNew();

    let renderSphere =
        let light1 = mkLight (Base.Point(10.0, 5.0, 5.0)) (Colour.fromColor Color.Red) 1.0 in
        let light2 = mkLight (Base.Point(-10.0, 5.0, 5.0)) (Colour.fromColor Color.Blue) 1.0 in
        let light3 = mkLight (Base.Point(10.0, 5.0, -5.0)) (Colour.fromColor Color.Green) 1.0 in
        let light4 = mkLight (Base.Point(-10.0, 5.0, -5.0)) (Colour.fromColor Color.Yellow) 0.8 in
        let ambientLight = mkAmbientLight (Colour.fromColor Color.White) 0.1 in
        let sphere1 = mkSphere (Base.Point(0.0, 0.0, 0.0)) 0.3 (Texture.mkMatTexture (Material.create (Colour.fromColor Color.White) 0.5)) in
        let sphere2 = mkSphere (Base.Point(0.0, 0.0, 0.0)) 0.5 (Texture.mkMatTexture (Material.create (Colour.fromColor Color.White) 0.8)) in 
        let sphere3 = mkSphere (Base.Point(0.0, 0.0, 0.0)) 0.3 (Texture.mkMatTexture (Material.create (Colour.fromColor Color.White) 0.5)) in
        let sphere4 = mkSphere (Base.Point(0.0, 0.0, 0.0)) 0.3 (Texture.mkMatTexture (Material.create (Colour.fromColor Color.White) 0.8)) in
        let plane = mkPlane (Texture.mkMatTexture (Material.create (Colour.fromColor Color.White) 0.0))
        let transformed1 = Transformation.transformShape [Transformation.Move(1.2, 0.6, -1.3)] sphere1 in
        let transformed2 = Transformation.transformShape [Transformation.Move(-1.1, 1.1, -1.5)] sphere2 in
        let transformed3 = Transformation.transformShape [Transformation.Move(-1.0, -0.3, -0.3)] sphere3 in
        let transformed4 = Transformation.transformShape [Transformation.Move(0.9, -0.2, -0.2)] sphere4 in
        let transformed5 = Transformation.transformShape [Transformation.Rotate(1.57079633, Transformation.Axis.X); Transformation.Move(0.0, -1.0, 0.0)] plane in
        { camera = mkCamera (Base.Point(0.0, 0.0, 1.0)) (Base.Point(0.0, 0.0, 0.0)) (Base.Vector (0.0, 1.0, 0.0)) 1.0 2.0 2.0 1280 960;
          scene = mkScene [transformed1 ; transformed2 ; transformed3 ; transformed4; transformed5] [light1; light2; light3; light4] ambientLight 3}

    let r = renderSphere
    stopWatch.Stop();
    let conTime = stopWatch.Elapsed.TotalMilliseconds / 1000.0
    stopWatch.Restart();

    let folderPath = System.Environment.GetFolderPath(System.Environment.SpecialFolder.DesktopDirectory) + "\profiling.png"
    renderToFile r.scene r.camera folderPath

    stopWatch.Stop();
    let renderTime = stopWatch.Elapsed.TotalMilliseconds / 1000.0
    Console.WriteLine("Construction time: " + conTime.ToString())
    Console.WriteLine("Rendering time:    " + renderTime.ToString())
    Console.WriteLine("Done - Press a key to close...")
    Console.ReadKey() |> ignore
#endif

#if videoRender

    let mkScene() =
     let light = mkLight (Base.Point(0.0, 3.0, 0.0)) (Colour.fromColor Color.White) 1.5 in
     let ambientLight = mkAmbientLight (Colour.fromColor Color.White) 0.3 in
     let sphere1 = mkSphere (Base.Point(0.0, 0.0, 0.0)) 1.2 (Texture.mkMatTexture (Material.create (Colour.fromColor Color.Red) 0.5))
     let sphere3 = mkSphere (Base.Point(0.0, 0.0, 0.0)) 1.1 (Texture.mkMatTexture (Material.create (Colour.fromColor Color.Blue) 0.5))
     let sphere4 = mkSphere (Base.Point(0.0, 0.0, 0.0)) 1.0 (Texture.mkMatTexture (Material.create (Colour.fromColor Color.HotPink) 0.5))
     let transformed1 = Transformation.transformShape [Transformation.Move(1.2, -1.3, 1.3)] sphere1 in
     let transformed3 = Transformation.transformShape [Transformation.Move(-1.3, 0.0, 1.3)] sphere3 in
     let transformed4 = Transformation.transformShape [Transformation.Move(1.3, 0.0, -1.3)] sphere4 in
     mkScene [transformed1 ; transformed3 ; transformed4 ] [light] ambientLight 2

    let s = mkScene()
    let radius = 4.0
    let images = 50
    Runner.Video.render360 s radius images
    Console.WriteLine("Press a key to close...")
    Console.ReadKey() |> ignore
#endif

#if csg
    let mkColourTexture c r = mkMatTexture (mkMaterial (fromColor c) r)
    let mkUnitBox t = mkBox (mkPoint -1.0 -1.0 -1.0) (mkPoint 1.0 1.0 1.0) t t t t t t
    let mkUnitCylinder t = mkSolidCylinder (mkPoint 0.0 0.0 0.0) 1.0 2.0 t t t
    let l1 () = mkLight (mkPoint 4.0 0.0 4.0) (fromColor Color.White) 1.0 in
    let l2 () = mkLight (mkPoint -4.0 0.0 4.0) (fromColor Color.White) 1.0 in
    let l3 () = mkLight (mkPoint 0.0 0.0 0.0) (fromColor Color.White) 1.0 in
    let l4 () = mkLight (mkPoint 2.0 4.0 4.0) (fromColor Color.White) 1.0 in
    let l5 () = mkLight (mkPoint 0.0 -4.0 0.0) (fromColor Color.White) 1.0 in
    let l6 () = mkLight (mkPoint 0.8 0.0 0.0) (fromColor Color.White) 1.0 in
    let ambientLight () = mkAmbientLight (fromColor Color.White) 0.2 in
    let camera = mkCamera (mkPoint 16.0 16.0 16.0) (mkPoint 0.0 0.0 0.0) (mkVector -1.0 1.0 -1.0) 8.0 1.4 1.4 500 500 in
    let camera2 = mkCamera (mkPoint 0.0 0.0 4.0) (mkPoint 0.0 0.0 0.0) (mkVector 0.0 1.0 0.0) 2.0 2.0 2.0 500 500 in
    let camera3 = mkCamera (mkPoint 0.3 0.3 0.3) (mkPoint 0.0 0.0 0.0) (mkVector -1.0 1.0 -1.0) 8.0 1.4 1.4 500 500 in
    let camera4 = mkCamera (mkPoint 0.3 0.0 0.0) (mkPoint 0.0 1.0 0.0) (mkVector 0.0 0.0 1.0) 1.0 2.0 2.0 500 500 in

    let cube () = mkUnitBox (mkMatTexture (mkMaterial (fromColor Color.Red) 0.0))
    let sphere () = mkSphere (mkPoint 0.0 0.0 0.0) 1.3 (mkMatTexture (mkMaterial (fromColor Color.Blue) 0.0))
    let sphere1 () = mkSphere (mkPoint 0.5 0.0 0.0) 1.0 (mkMatTexture (mkMaterial (fromColor Color.Blue) 0.0))
    let sphere2 () = mkSphere (mkPoint -0.5 0.0 0.0) 1.0 (mkMatTexture (mkMaterial (fromColor Color.Red) 0.0))
    let sphere3 () = mkSphere (mkPoint -0.5 0.0 0.0) 0.2 (mkMatTexture (mkMaterial (fromColor Color.Yellow) 0.0))
    let tex = mkColourTexture Color.Cyan 1.0
    let box = mkBox (mkPoint 0.0 0.0 0.0) (mkPoint 1.0 1.0 1.0) tex tex tex tex tex tex
    let cylinder = mkSolidCylinder (mkPoint 1.0 0.0 0.0) 0.5 3.0 tex tex tex

    let degrees_to_radians (d : float) = d * Math.PI / 180.0

    let cross () =
        let cy = transform (mkUnitCylinder (mkColourTexture Color.Yellow 0.0)) (scale 0.7 1.5 0.7)  in
        let cx = transform cy (rotateX (degrees_to_radians 90.0)) in
        let cz = transform cy (rotateZ (degrees_to_radians 90.0)) in 
        union cy (union cz cx)

    let renderUnion () =
        { scene = mkScene [union (sphere1 ()) (sphere2 ())] [l1 (); l2 ()] (ambientLight ()) 0;
          camera = camera2 }

    let renderUnion2 () =
        { camera = mkCamera (mkPoint 0.9 0.0 0.0) (mkPoint 0.0 0.0 0.0) (mkVector 0.0 1.0 0.0) 0.4 0.9 0.9 500 500;
          scene = mkScene [union (sphere1 ()) (sphere2 ()); sphere3 ()] [l3 ()] (ambientLight ()) 0 }
    
    let renderUnion3 () =
        { camera = mkCamera (mkPoint 0.9 0.0 0.0) (mkPoint 0.0 0.0 0.0) (mkVector 0.0 1.0 0.0) 0.4 0.9 0.9 500 500;
          scene = mkScene [group (sphere1 ()) (sphere2 ()); sphere3 ()] [l3 ()] (ambientLight ()) 0 }

    let renderIntersection () =
        let cube = mkUnitBox (mkMatTexture (mkMaterial (fromColor Color.Red) 0.0)) in
            { scene = mkScene [intersection (sphere1 ()) (sphere2 ())] [l1 (); l2 ()] (ambientLight ()) 0
              camera = camera2}

    let renderIntersection2 () =
        { scene = mkScene [intersection (cube ()) (sphere ())] [l1 (); l2 ()] (ambientLight ()) 0
          camera = camera}
    
    let renderCross () =
        { scene = mkScene [cross ()] [l1 (); l2 ()] (ambientLight ()) 0;
          camera = camera}

    let renderLantern () =
        { scene = mkScene [subtraction (intersection (cube ()) (sphere ())) (cross ())] [l1 (); l2 (); l3 ()] (ambientLight ()) 0
          camera = camera}

    let renderSubtraction () =
        let cube = mkUnitBox (mkMatTexture (mkMaterial (fromColor Color.Red) 0.0)) in
        { scene = mkScene [subtraction (sphere ()) cylinder] [l1 (); l2 ()] (ambientLight ()) 0 ;
          camera = camera}

    let renderUnionTorus (R : float) (r : float) () =
        let s1 = mkShape (mkImplicit ("(((x^2 + y^2)_2 - " + (string R) + ")^2 + z^2)_2 - " + (string r))) (mkMatTexture (mkMaterial (fromColor Color.Blue) 0.0))
        let s2 = mkShape (mkImplicit ("(((x^2 + y^2)_2 - " + (string (R - (3.0 * r))) + ")^2 + z^2)_2 - " + (string r))) (mkMatTexture (mkMaterial (fromColor Color.Red) 0.0))
        let s3 = union (transform s2 (translate 0.0 (3.0 * r) 0.0)) s1
        { camera = mkCamera (mkPoint 0.0 0.0 4.0) (mkPoint 0.0 0.0 0.0) (mkVector 0.0 1.0 0.0) 2.0 4.0 4.0 500 500
          scene = mkScene [s3] [l4 ()] (ambientLight ()) 0}

    let renderIntersectionTorus (R : float) (r : float) () =
        let s1 = mkShape (mkImplicit ("(((x^2 + y^2)_2 - " + (string R) + ")^2 + z^2)_2 - " + (string r))) (mkMatTexture (mkMaterial (fromColor Color.Blue) 0.0))
        let s2 = mkShape (mkImplicit ("(((x^2 + y^2)_2 - " + (string (R - (3.0 * r))) + ")^2 + z^2)_2 - " + (string r))) (mkMatTexture (mkMaterial (fromColor Color.Red) 0.0))
        let s3 = intersection (transform s2 (translate 0.0 (3.0 * r) 0.0)) s1
        { camera = mkCamera (mkPoint 0.0 0.0 4.0) (mkPoint 0.0 0.0 0.0) (mkVector 0.0 1.0 0.0) 2.0 4.0 4.0 500 500
          scene = mkScene [s3] [l4 ()] (ambientLight ()) 0}

    let renderSubtractionTorus (R : float) (r : float) () =
        let s1 = mkShape (mkImplicit ("(((x^2 + y^2)_2 - " + (string R) + ")^2 + z^2)_2 - " + (string r))) (mkMatTexture (mkMaterial (fromColor Color.Blue) 0.0))
        let s2 = mkShape (mkImplicit ("(((x^2 + y^2)_2 - " + (string (R - (3.0 * r))) + ")^2 + z^2)_2 - " + (string r))) (mkMatTexture (mkMaterial (fromColor Color.Red) 0.0))
        let s3 = subtraction s1 (transform s2 (translate 0.0 (3.0 * r) 0.0))
        { camera = mkCamera (mkPoint 0.0 0.0 4.0) (mkPoint 0.0 0.0 0.0) (mkVector 0.0 1.0 0.0) 2.0 4.0 4.0 500 500
          scene = mkScene [s3] [l4 ()] (ambientLight ()) 0}

    let renderUnionTorus2 (R : float) (r : float) () =
        let s1 = mkShape (mkImplicit ("(((x^2 + y^2)_2 - " + (string R) + ")^2 + z^2)_2 - " + (string r))) (mkMatTexture (mkMaterial (fromColor Color.Blue) 0.0))
        let s2 = mkShape (mkImplicit ("(((x^2 + y^2)_2 - " + (string (R - (3.0 * r))) + ")^2 + z^2)_2 - " + (string r))) (mkMatTexture (mkMaterial (fromColor Color.Red) 0.0))
        let s3 = union (transform s2 (translate 0.0 (3.0 * r) 0.0)) s1
        { camera = mkCamera (mkPoint 0.0 -4.0 0.0) (mkPoint 0.0 0.0 0.0) (mkVector 0.0 0.0 1.0) 2.0 4.0 4.0 500 500
          scene = mkScene [s3] [l5 ()] (ambientLight ()) 0}

    let renderIntersectionTorus2 (R : float) (r : float) () =
        let s1 = mkShape (mkImplicit ("(((x^2 + y^2)_2 - " + (string R) + ")^2 + z^2)_2 - " + (string r))) (mkMatTexture (mkMaterial (fromColor Color.Blue) 0.0))
        let s2 = mkShape (mkImplicit ("(((x^2 + y^2)_2 - " + (string (R - (3.0 * r))) + ")^2 + z^2)_2 - " + (string r))) (mkMatTexture (mkMaterial (fromColor Color.Red) 0.0))
        let s3 = intersection (transform s2 (translate 0.0 (3.0 * r) 0.0)) s1
        { camera = mkCamera (mkPoint 0.0 -2.0 0.0) (mkPoint 0.0 0.0 0.0) (mkVector 0.0 0.0 1.0) 2.0 4.0 4.0 500 500
          scene = mkScene [s3] [l5 ()] (ambientLight ()) 0}

    (* Union, Substraction, Intersection Stuff*)
    let blue = mkMatTexture (mkMaterial (fromColor Color.Blue) 0.0)
    let red = mkMatTexture (mkMaterial (fromColor Color.Red) 0.0)
    let cube = ShapeFactory.mkBox (mkPoint -10.0 -10.0 -10.0) (mkPoint 10.0 10.0 10.0) red red red red red red
    let sphere = ShapeFactory.mkSphere (mkPoint 0.0 0.0 0.0) 12.0 blue


    let UnionCubeSphere = 
        let s = union cube sphere
        let l = mkLight (mkPoint -20.0 20.0 15.0) (fromColor Color.White) 1.0
        { camera = mkCamera (mkPoint 20.0 20.0 20.0) (mkPoint 0.0 0.0 0.0) (mkVector 0.0 1.0 0.0) 1.0 2.0 2.0 2000 2000
          scene = mkScene [s] [l] (ambientLight ()) 5}

    let SubstractionCubeSphere = 
        let s = subtraction cube sphere
        let l = mkLight (mkPoint -20.0 20.0 15.0) (fromColor Color.White) 1.0
        { camera = mkCamera (mkPoint 20.0 20.0 20.0) (mkPoint 0.0 0.0 0.0) (mkVector 0.0 1.0 0.0) 1.0 2.0 2.0 2000 2000
          scene = mkScene [s] [l] (ambientLight ()) 5}

    let IntersectionCubeSphere = 
        let s = intersection cube sphere
        let l = mkLight (mkPoint -20.0 20.0 15.0) (fromColor Color.White) 1.0
        { camera = mkCamera (mkPoint 20.0 20.0 20.0) (mkPoint 0.0 0.0 0.0) (mkVector 0.0 1.0 0.0) 1.0 2.0 2.0 2000 2000
          scene = mkScene [s] [l] (ambientLight ()) 5}
        

    let r = renderLantern ()
    let folderPath = System.Environment.GetFolderPath(System.Environment.SpecialFolder.DesktopDirectory) + "\CSG.png"
    BitMapRenderer.writeToFile (Camera.rayGun r.scene r.camera) folderPath
#endif
    0   // return an integer exit code