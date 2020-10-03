namespace GaneshTracer

open Material
open Colour
open System.Drawing

module Texture =
    type Texture = T of (float -> float -> Material)

    //Takes in a function, which applies a material to x y coordinates, and returns texture.
    let mkTexture f = T (f)

    //Returns a texture, where the same material is applied to all x y coordinates.
    let mkMatTexture m = mkTexture (fun x y -> m)

    //Extracts the mapping function for a given Texture.
    let getMappingFunc (T(f): Texture) = f

    //Returns the material at the given pair of surface coordinates.
    let getMaterialAt (T(f): Texture) (x, y) = f x y;
    
    //Returns a texture, where the same material with the specified color is applied to all x y coordinates.
    let mkBlackTexture = mkMatTexture black
    let mkWhiteTexture = mkMatTexture white
    let mkGrayTexture  = mkMatTexture grey 
    let mkRedTexture   = mkMatTexture red  
    let mkGreenTexture = mkMatTexture green
    let mkBlueTexture  = mkMatTexture blue 
    
    //Textures for testing purposes.
    let mkCheckeredTexture =
        let white = Material.create (fromColor Color.Red) 0.5
        let black = Material.create (fromColor Color.Green) 0.5
        let checker x y =
            let abs' s f = if f < 0.0 then 1.0 - (f * s) else f * s
            if (int (abs' 64.0 x) + int (abs' 32.0 y)) % 2 = 0
            then white
            else black
        mkTexture checker
    
    let mkMixedTexture = 
        let mixer (x:float) (y:float) = 
            Material.createFlat (Colour.mkColour y (y*0.5) 0.5)
        mkTexture mixer
    
    //Returns a texture based on a coordinate map and a file.
    let mkTextureFromFile (file : string) =
        let img = new Bitmap(file)
        let width = img.Width - 1
        let height = img.Height - 1
        let widthf = float width
        let heightf = float height
        let texture x y =
          let x', y' = int (widthf * x), int (heightf * (1.0 - y)) //'(1.0 - ' added to adjust v coordinate.
          let c = lock img (fun () -> img.GetPixel(x',y'))
          create (fromColor c) 0.0
        mkTexture texture



