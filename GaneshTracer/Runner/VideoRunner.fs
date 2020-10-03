namespace Runner

open System.Drawing
open System
open GaneshTracer
open GaneshTracer.API
open GaneshTracer.Transformation

module Video = 
 type Render = 
        { scene : scene;
          camera : camera}

 let angleCam (r : float) (distance : float) (width : float) (height : float) (resX : int) (resY : int) (step : int) (current : int) =
    let angle = (Convert.ToDouble(current-1) * (2.0 * Math.PI / Convert.ToDouble(step)))
    let x = r * Math.Sin(angle)
    let z = r * Math.Cos(angle)
    printfn "CAM AT %f | %f | %f " x 0.0 z
    mkCamera (Base.Point(x, 0.0, z)) (Base.Point(0.0,0.0,0.0)) (Base.Vector(0.0, 1.0, 0.0)) distance width height resX resY
        
 let render360 (s : scene) (radius : float) (step : int) =
  let stop = new Diagnostics.Stopwatch()
  stop.Start()
  let folderPath = System.Environment.GetFolderPath(Environment.SpecialFolder.DesktopDirectory)
  for i in 1 .. step do
    let cam = angleCam radius 1.0 2.0 2.0 500 500 step i
    printfn "Rendering frame #%i" i
    BitMapRenderer.writeToFile (Camera.rayGun s cam) (folderPath + "\\video" + i.ToString() + ".png")
  stop.Start()
  printfn "Done... %d" stop.ElapsedMilliseconds