namespace GaneshTracer

open Colour
open System
open System.Drawing
open System.Windows.Forms

module BitMapRenderer =
    /// Converts a tracing result to a bitmap and saves to disk at the given filepath
    let writeToFile (pixels: Color[][]) (filepath: string) = 
        let bitmap = new Bitmap((pixels.Length), (pixels.[0].Length))
        Array.iteri (fun x cA -> 
            Array.iteri (fun y c -> bitmap.SetPixel(x, y, c)) cA ) pixels   // Move traceresult to bitmap
        bitmap.Save(filepath);

    /// Converts a tracing result to a bitmap and displays in a windows form
    let renderToScreen (pixels: Color[][]) = 
        let bitmap = new Bitmap((pixels.Length), (pixels.[0].Length))
        Array.iteri (fun x cA -> 
            Array.iteri (fun y c -> bitmap.SetPixel(x, y, c)) cA ) pixels   // Move traceresult to bitmap
        let form = new Form(Height = bitmap.Height, Width = bitmap.Width, Text = "Blessed by Ganesh", BackgroundImage = bitmap) 
        form.TopMost <- true
        do Application.Run(form)