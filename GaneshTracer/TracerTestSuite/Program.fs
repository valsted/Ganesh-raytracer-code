namespace TracerTestSuite

open System
open System.Drawing
open GaneshTracer.API
open Shapes

module Program = 
    let allTargets : Target list =
      List.concat 
        [
         Shapes.render;
         AffineTransformations.render true;
         AffineTransformations.render false;
         ImplicitSurfaces.render;
         Meshes.render;
         Texture.render;
         Light.render;
         CSG.render;
         // The test group below is only needed for teams of 7 students.
         // Teams of 6 students can uncomment the line below.
         //ThinLens.render;
         ]


    let renderAll (toScreen : bool) : unit = 
      List.iter (Util.renderTarget toScreen) allTargets
    let renderTests (toScreen : bool) (group : string) (tests : string list) : unit = 
      Util.renderTests toScreen allTargets group tests
    let renderGroups (toScreen : bool) (groups : string list) : unit =
      Util.renderGroups toScreen allTargets groups


    
    [<EntryPoint>]
    let main argv =
        Util.init();

        // Run all test cases
        renderAll false;

        // To only run some test groups, use the following
        //renderGroups false ["implicitSurfaces"]

        // To only run some test cases of a group, use the following
        //renderTests false "csg" ["intersectTorus2"]

        Util.finalize();

        Console.WriteLine("Press a key to close...")
        Console.ReadKey() |> ignore
        0 // return an integer exit code