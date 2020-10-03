namespace GaneshTracer

open FParsec
open Base
open System
open System.IO

module Import =
    
    /// Overall type of ply file
    type PLYtype = 
        | ASCII
        | BINARY of bool  //Flag for needing to flip

    /// PLY scalar data types
    type Format = 
        | Char | UChar | Short | UShort | Int | UInt | Float | Double | PList of Format * Format

    /// Union type for supported vertex properties by name
    type PropertyName = 
        | X | Y | Z | NX | NY | NZ | U | V | R | G | B | Indices | Other of string

    /// Vertex property type mapping a property format with a corresponding name. 
    type Property = Property of Format * PropertyName
    type PropertyResult = PResult of float * PropertyName

    /// Extracting fields of property
    let propName (Property(f, n)) = n
    let propFormat (Property(f, n)) = f    

    /// Container for parsed header-element information. Is either vertex or face
    type Element = 
        | Vertices of int * Property List
        | Face of int * Format * Format

    type Tri = T of int * int * int
    
    let getIDx (T(a,b,c)) = (a,b,c)

    /// Union type for 3 and 4 vertex faces
    type Face =
        | Tri of Tri
        | Quad of Tri * Tri

    let getPropName (p: PropertyName) = 
        match p with
        | X         -> "x"
        | Y         -> "y"
        | Z         -> "z"
        | NX        -> "nx"
        | NY        -> "ny"
        | NZ        -> "nz"
        | U         -> "u"
        | V         -> "v"
        | R         -> "r"
        | G         -> "g"
        | B         -> "b"
        | Indices   -> "vertex_indices"
        | Other(s)  -> s

    /// Shorthand for creating a specific string Parser
    let str s = pstring s

    /// Parser for consuming a string without saving it
    let skip s = skipString s

    /// Shorthand for whitespace and newline etc.
    let wsn = spaces 

    /// Shorthand for space
    let ws = skip " "

    /// Determines if a byte array reverse
    let getBinaryFlip (little: bool) = if little = (BitConverter.IsLittleEndian)
                                       then BINARY(false) else BINARY(true)

    /// Comparator function to tell if a format is the (element only) PList type
    let isPList (form: Format) = 
        match form with
        | PList(_,_) -> true
        | _          -> false

    /// Parses a comment tag -> ignore
    let pComment = (skip "comment " >>. skipRestOfLine true) <|> (skip "obj_info" >>. skipRestOfLine true)

    /// Parser mapping PLY-formats to internal format qualifier
    let pFormat = 
        choice [ ( skip "int8 "    <|> skip "char "   ) |>> fun () -> Char;
                 ( skip "uint8 "   <|> skip "uchar "  ) |>> fun () -> UChar;
                 ( skip "int16 "   <|> skip "short "  ) |>> fun () -> Short;
                 ( skip "uint16 "  <|> skip "ushort " ) |>> fun () -> UShort;
                 ( skip "int32 "   <|> skip "int "    ) |>> fun () -> Int;
                 ( skip "uint32 "  <|> skip "uint "   ) |>> fun () -> UInt;
                 ( skip "float32 " <|> skip "float "  ) |>> fun () -> Float;
                 ( skip "float64 " <|> skip "double " ) |>> fun () -> Double ] >>= preturn

    /// Parser mapping ply string typed propertynames to internal PropertyName qualifier
    let pPropName = 
        choice [ ( skip "vertex_indices" )      |>> fun () -> Indices;
                 ( skip "x" )                   |>> fun () -> X;
                 ( skip "y" )                   |>> fun () -> Y;
                 ( skip "z" )                   |>> fun () -> Z;
                 ( skip "nx" )                  |>> fun () -> NX;
                 ( skip "ny" )                  |>> fun () -> NY;
                 ( skip "nz" )                  |>> fun () -> NZ;
                 ( skip "u" )                   |>> fun () -> U;
                 ( skip "v" )                   |>> fun () -> V; 
                 ( skip "red"   <|> skip "r" )  |>> fun () -> R; 
                 ( skip "green" <|> skip "g" )  |>> fun () -> G; 
                 ( skip "blue"  <|> skip "b" )  |>> fun () -> B; 
                 ( restOfLine false ) |>> fun s -> Other(s) ] >>= preturn

    /// Parses a property tag from header
    let pProperty = 
        let pVert = (skip "property ") >>. pFormat .>>. (pPropName .>> skipRestOfLine true)                |>> fun(f, n) -> Property(f,n)
        let pFace = (skip "property list ") >>. tuple3 pFormat pFormat (pPropName .>> skipRestOfLine true) |>> fun(f, g, n) -> Property(PList(f,g),n)
        pFace <|> pVert

    /// Parses a property tag for a Vertex element
    let pVertexProperty = (skip "property ") >>. pFormat .>>. (pPropName .>> skipRestOfLine true) |>> fun(f, n) -> Property(f,n)

    /// Parses a property list tag for a face element
    let pFaceProperty = skip "property list " >>. pFormat .>>. (pFormat .>> skipRestOfLine true)

    /// Parse an element vertex tag
    let pVertexElement =  
        (skip "element vertex " >>. pint32 .>> wsn) .>>. (many pVertexProperty) |>> fun (i, ps) -> Vertices(i, ps)

    /// Parse an element face tag
    let pFaceElement =
        (skip "element face " >>. pint32 .>> wsn) .>>. (many pProperty)
        |>> fun (i, lst) -> match (List.find(fun prop -> isPList (propFormat prop)) lst) with
                            | Property(PList(f,g), n) -> Face(i, f, g)
                            | _        -> failwith "Face element missing list property"

    /// Parses the dataformat of the ply file. Either binary or ascii
    let pDataFormat = 
        skip "format " >>. choice [ ( skip "ascii " )                |>> fun () -> ASCII;
                                    ( skip "binary_little_endian " ) |>> fun () -> getBinaryFlip true;
                                    ( skip "binary_big_endian ")     |>> fun () -> getBinaryFlip false ] .>> skipRestOfLine true
                                    
    /// Fetches the header tags contained within the header delimiters, returning vertex and face element info
    let pHeader = 
        let preParser = (skip "ply" >>. wsn) >>. pDataFormat
        let endParser = (skip "end_header" .>> wsn)
        tuple3 (preParser .>> many pComment) pVertexElement (pFaceElement .>> endParser) 
    
    /// Parses a ply number of the given format and returns as F# float
    let pNumber (form: Format) =
        match form with
        | Char       -> pint8    |>> fun n -> (float n)
        | UChar      -> puint8   |>> fun n -> (float n)
        | Short      -> pint16   |>> fun n -> (float n)
        | UShort     -> puint16  |>> fun n -> (float n)
        | Int        -> pint32   |>> fun n -> (float n)
        | UInt       -> puint32  |>> fun n -> (float n)
        | Float      -> pfloat   |>> fun n -> (float n)
        | Double     -> pfloat   |>> fun n -> (float n)
        | PList(_,_) -> failwith "pNumber: PList received - this is only a wrapper and cannot be interpreted as a value"

    /// Parses ply number while retaining property name information    
    let pPropertyResult (Property(form, name)) = pNumber form |>> fun n -> (n, name)

    /// Converts a map of parsed vertx properties to a Vertex type
    let mapToVertex (map: Map<PropertyName, float>) =  
        let get (k: PropertyName) = match map.TryFind k with 
                                    | None    -> 0.0
                                    | Some x  -> x
        let point = Point(get X, get Y, get Z)
        let norm = Vector(get NX, get NY, get NZ)
        let UV = match (map.TryFind U, map.TryFind V) with
                 | Some v, Some u -> (u,v)
                 | _, _           -> (0.0, 0.0)
        let tuple = (point, norm) 
        Vertex(point,norm,UV)

    /// Parses a vertex with the supplied properties. Maps to supported types and discards the rest
    let pVertex (pList: Property List) =
        let map = Map.empty<PropertyName, float>
        let update prop map = pPropertyResult prop |>> fun (f,n) -> Map.add n f map
        List.fold(fun pars prop -> pars .>> (optional ws) >>= fun m -> update prop m) (update (pList.Head) map) pList.Tail
        .>> skipRestOfLine true |>> mapToVertex 

    /// Computes the normal of a face and adds it to the Vertices bordering it (for smooth shading)
    let addToNormal (i1:int) (i2:int) (i3:int) (arr:Vertex[]) : unit =         
         let vert1 = arr.[i1]
         let vert2 = arr.[i2]
         let vert3 = arr.[i3]
         let v = distance vert1.P vert3.P
         let u = distance vert1.P vert2.P
         let normal = normCrossProduct v u
         arr.[i1] <- Vertex(vert1.P,(vert1.Norm + normal),vert1.UV)
         arr.[i2] <- Vertex(vert2.P,(vert2.Norm + normal),vert2.UV)
         arr.[i3] <- Vertex(vert3.P,(vert3.Norm + normal),vert3.UV)

    /// Helper function for iterating a vertex[] and make sure the normals are normalized
    let normalizeAll (arr:Vertex[]) =
        Array.Parallel.map(fun (v:Vertex) -> Vertex(v.P, normalize v.Norm, v.UV)) arr    

    /// Parser for checking the first digit of a face payload. Determines if the face requires conversion or is not supported at all 
    let checkFaceDigit (form: Format) = 
        (pNumber form) .>> ws |>> fun d -> match int d with
                                           | 3 -> false
                                           | 4 -> true
                                           | _ -> failwith "Face has more vertices than supported"

    /// Parses a tris payload (with first digit removed)
    let pTris (verts: Vertex[]) (listForm: Format) (computeNorm: bool)= 
        tuple3 (pNumber (listForm) .>> ws) (pNumber (listForm) .>> ws) (pNumber (listForm))
        .>> skipRestOfLine true |>> fun (a,b,c) ->  if computeNorm then addToNormal (int a) (int b) (int c) verts
                                                                        Tri(T(int a, int b, int c)) 
                                                                   else Tri(T(int a, int b, int c))

    /// Parses a quad payload (with first digit removed)
    let pQuad (verts: Vertex[]) (listForm: Format) (computeNorm: bool) = 
        tuple4 (pNumber (listForm) .>> ws) (pNumber (listForm) .>> ws) (pNumber (listForm) .>> ws) (pNumber (listForm))
        .>> skipRestOfLine true |>> fun (a,b,c,d) -> if computeNorm then addToNormal (int a) (int b) (int c) verts
                                                                         addToNormal (int b) (int c) (int d) verts
                                                     Quad(T(int a, int b, int c), T(int b, int c, int d))

    /// Parser for reading a face payload in the given format. Handles both tris & quad. Calculates normals if not supplied
    let pFace (count: int) (verts: Vertex[]) (checkForm: Format) (listForm: Format) (computeNorm: bool)= 
        let innerParser = (checkFaceDigit checkForm) >>= fun b -> if b then pQuad verts listForm computeNorm
                                                                  else pTris verts listForm computeNorm
        parray count innerParser |>> fun ar -> 
            let dymAr = new ResizeArray<(int*int*int)>()
            Array.iter(fun face -> match face with 
                                   | Tri(a)    -> dymAr.Add(getIDx a)
                                   | Quad(a,b) -> dymAr.Add(getIDx a)
                                                  dymAr.Add(getIDx b)) ar |> fun () -> dymAr.ToArray()

    /// Converts vertices and face ids to tris'
    let assembleFaces (verts:Vertex[]) (faces: (int*int*int)[]) =
        printfn "PLY parser found :  %d  Vertices  -  %d Faces" verts.Length faces.Length
        Array.Parallel.map(fun (a, b, c) -> (verts.[a], verts.[b], verts.[c])) faces

    (*****************************************************)
    (*   START OF BINARY PARSER FUNCTIONS                *)
    (*****************************************************)
    
    /// Returns the amount of bytes required to describe a number in the supplied format
    let byteAmount (form: Format) = 
        match form with
        | Char  | UChar        -> 1
        | Short | UShort       -> 2
        | Int   | UInt | Float -> 4
        | Double               -> 8
        | PList(_,_)           -> failwith "byteAmount: Undefined for PList"

    /// Attemtps to read a number from a binarystream, parsing it with the format given 
    /// Relies on the reader to be at the correct spot in the stream at the time of calling. (NB: moves cursor of reader)
    let readNum (form: Format) (reader: BinaryReader) (flip: bool) = 
        let buffer = if flip then Array.rev (reader.ReadBytes(byteAmount form)) else reader.ReadBytes(byteAmount form)
        match form with 
        | Char   -> float (buffer.[0])              
        | UChar  -> float (buffer.[0])                    
        | Short  -> float (BitConverter.ToInt16(buffer, 0)) 
        | UShort -> float (BitConverter.ToUInt16(buffer, 0))
        | Int    -> float (BitConverter.ToInt32(buffer, 0)) 
        | UInt   -> float (BitConverter.ToUInt32(buffer, 0))
        | Float  -> float (BitConverter.ToSingle(buffer, 0))
        | Double -> float (BitConverter.ToDouble(buffer, 0))
        | PList(_,_) -> failwith "readNum: PList got this far. Shouldn't have happened"
                    
    /// Reads a vertex with given properties from a binary stream. 
    /// Relies on the reader to be at the correct spot in the stream at the time of calling. (NB: moves cursor of reader)
    let readVertex (props: Property list) (reader: BinaryReader) (flip: bool) = 
        let map = Map.empty<PropertyName, float>
        let propResult (Property(form,name)) (reader: BinaryReader) (map) = 
                readNum form reader flip |> fun num -> Map.add name num map
        List.fold(fun m p -> propResult p reader m) map props |> fun m -> mapToVertex m
    
    /// Reads 3 * no. of bytes for the given format and converts into a Tri-face type 
    /// Relies on the reader to be at the correct spot in the stream at the time of calling.(NB: moves cursor of reader)
    let readTris (verts: Vertex[]) (reader: BinaryReader) (lForm: Format) (flip: bool) (compNorm: bool) = 
        let triple = (int (readNum lForm reader flip), int (readNum lForm reader flip), int (readNum lForm reader flip))
        if compNorm then addToNormal (fstT triple) (sndT triple) (trdT triple) verts else ()
        Tri(T(triple))

    /// Reads 4 * no. of bytes for the given format and converts into a Quad-face type 
    /// Relies on the reader to be at the correct spot in the stream at the time of calling. (NB: moves cursor of reader)
    let readQuad (verts: Vertex[]) (reader: BinaryReader) (lForm: Format) (flip: bool) (compNorm: bool) = 
        let a = int (readNum lForm reader flip)
        let b = int (readNum lForm reader flip)
        let c = int (readNum lForm reader flip)
        let d = int (readNum lForm reader flip)
        let tri1 = (a, b, c)
        let tri2 = (b, c, d)
        if compNorm then addToNormal a b c verts
                         addToNormal b c d verts else ()
        Quad(T(tri1), T(tri2))
    
    /// Reads a face from a binary stream and optionally computes the normal vector of the nearby vertices.
    /// Relies on the reader to be at the correct spot in the stream at the time of calling. (NB: moves cursor of reader)
    let rec readFace (verts: Vertex[]) (cForm: Format) (lForm: Format) (reader: BinaryReader) (flip: bool) (compNorm: bool) = 
        // Read check digit and parse accordingly
        match int (readNum cForm reader flip) with
        | 3 -> readTris verts reader lForm flip compNorm
        | 4 -> readQuad verts reader lForm flip compNorm
        | _ -> let next = byte (reader.PeekChar())
               if next = (byte) 3 || next = (byte) 4 then readFace verts cForm lForm reader flip compNorm  //Signals that there should be a new attempt one byte ahead
               else failwith "readFace: unsupported amount of edges in face"
               
    /// Reads the filestream to the end of the ply header and returns the byte index (NB: Stream cursor is changed during scan)
    let scanToPayload (stream: FileStream) =
        let mutable seeking = true
        let buffer = new ResizeArray<char>()
        
        let reader = new BinaryReader(stream, Text.Encoding.UTF8)
        reader.ReadBytes(120) |> ignore   //This is the average minimum of bytes always present in header
        while seeking do 
            buffer.Add (char (reader.Read()))
            if (new String(buffer.ToArray())).Contains("end_header") then seeking <- false
            else ()
        let pos = (reader.BaseStream.Position + (int64) 1)
        reader.Dispose() 
        pos
    

    (********************************************)
    (*   END OF BINARY PARSER FUNCTIONS         *)
    (********************************************)

    /// SubParser for Binary ply
    let parseBinary (filename: string) (Vertices(i,ps), Face(k,cf,lf)) (flip: bool) (smooth: bool) = 
        let stream = File.Open(filename, FileMode.Open, FileAccess.Read, FileShare.Read)
        let reader = scanToPayload (stream) |> fun i ->
                     let binStream = File.OpenRead(filename) //Binary reader reads on a new stream to avoid errors due to the other being disposed
                     binStream.Seek(i, SeekOrigin.Begin) |> ignore
                     new BinaryReader(binStream)
        
        // Read vertices
        let verts = Array.init i (fun i -> readVertex ps reader flip)
       
        let dymAr = new ResizeArray<(int*int*int)>()
        for i in 1 .. k do
            match readFace verts cf lf reader flip smooth with
            | Tri(a)    -> dymAr.Add(getIDx a)
            | Quad(a,b) -> dymAr.Add(getIDx a)
                           dymAr.Add(getIDx b)
        
        let faces = dymAr.ToArray()

        //return vertices and faces as Tris
        if (smooth) then normalizeAll verts |> assembleFaces <| faces 
        else verts |> assembleFaces <| faces 
        

    /// SubParser for ASCII ply
    let parseASCII (Vertices(i,ps), Face(k,cf,lf)) smooth = 
        let vParser = pVertex ps //precompute vertex parser
        parray i (vParser) >>= fun (vArray) -> 
        pFace k vArray cf lf smooth |>> fun faces -> 
        if (smooth) then normalizeAll vArray |> assembleFaces <| faces 
        else vArray |> assembleFaces <| faces 
        //return vertices and faces as Tris

    /// Parses a ply file to an array of Tris (option)
    let parsePLY (filename: string) (smooth: bool) =
        let parser = pHeader >>= fun headRes ->
            match headRes with
            | (ASCII, vs, fs)     -> parseASCII (vs, fs) smooth
            | (BINARY(b), vs, fs) -> (getPosition .>> skipRestOfLine false) |>> fun pos ->
                                     parseBinary (filename) (vs, fs) b smooth
            | (_, _, _) -> failwith "header parsing failed"

        match runParserOnFile parser () filename Text.Encoding.UTF8 with
        | Success (res,_,_) -> printfn "Parsing succes"
                               Some res
        | Failure (err,_,_) -> printfn "Parsing failed! %s" err
                               None