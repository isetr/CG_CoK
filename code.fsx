open System

module Model =
    type Action =
        | Wait
        | Move of (int * int)
        
        static member Do (a: Action) =
            match a with
            | Wait          -> printfn "WAIT"
            | Move (x,y)    -> printfn "MOVE %d %d" x y
            
    type CellType =
        | Wall
        | WandererSpawn
        | Empty
        
        static member fromChar (c: char) =
            match c with
                | '#' -> Wall
                | 'w' -> WandererSpawn
                | '.' -> Empty
                | _ -> failwith "owo what's this"
                
    type ExplorerData =
        {
            Id      : int
            X       : int
            Y       : int
            Sanity  : int
            Param1  : int
            Param2  : int
        }
        
        static member fromLineData (s: string array) =
            {
                Id      = int s.[1]
                X       = int s.[2]
                Y       = int s.[3]
                Sanity  = int s.[4]
                Param1  = int s.[5]
                Param2  = int s.[6]
            }
        
    type WandererData =
        {
            Id      : int
            X       : int
            Y       : int
            Life    : int
            Spawned : bool
            Target  : int
        }
        
        static member fromLineData (s: string array) =
            {
                Id      = int s.[1]
                X       = int s.[2]
                Y       = int s.[3]
                Life    = int s.[4]
                Spawned = s.[5] = "1"
                Target  = int s.[6]
            }
                
    type Entity =
        | Explorer of ExplorerData
        | Wanderer of WandererData
            
    type Data =
        {
            Width               : int
            Height              : int
            Table               : Map<(int * int), CellType>
            SanityLossLonely    : int
            SanityLossGroup     : int
            WandererSpawnTime   : int
            WandererLifeTime    : int
            MyPlayer            : ExplorerData option
            Explorers           : ExplorerData list
            Wanderers           : WandererData list
        }
        
        static member init () : Data =
            let width = int(Console.In.ReadLine())
            let height = int(Console.In.ReadLine())
            let data =
                Seq.init height (fun i ->
                    let line = Console.In.ReadLine()
                    
                    line
                    |> Seq.mapi (fun j c ->
                        (i, j), CellType.fromChar c
                    )
                )
                |> Seq.concat
                |> Map<(int * int), CellType>
        
            (* sanityLossLonely: how much sanity you lose every turn when alone, always 3 until wood 1 *)
            (* sanityLossGroup: how much sanity you lose every turn when near another player, always 1 until wood 1 *)
            (* wandererSpawnTime: how many turns the wanderer take to spawn, always 3 until wood 1 *)
            (* wandererLifeTime: how many turns the wanderer is on map after spawning, always 40 until wood 1 *)
            let token = (Console.In.ReadLine()).Split [|' '|]
            let sanityLossLonely = int(token.[0])
            let sanityLossGroup = int(token.[1])
            let wandererSpawnTime = int(token.[2])
            let wandererLifeTime = int(token.[3])
            
            {
                Width               = width
                Height              = height
                Table               = data
                SanityLossLonely    = sanityLossLonely
                SanityLossGroup     = sanityLossGroup
                WandererSpawnTime   = wandererSpawnTime
                WandererLifeTime    = wandererLifeTime
                MyPlayer            = None
                Explorers           = []
                Wanderers           = []
            } : Data
            
        member this.Step() : Data =
            let entityCount = int (Console.In.ReadLine())
            
            let myPlayer = ExplorerData.fromLineData ((Console.In.ReadLine()).Split [|' '|])
            
            let (explorers, wanderers) =
                Seq.init (entityCount - 1) id
                |> Seq.fold (fun (explorers, wanderers) _ ->
                    let line = (Console.In.ReadLine()).Split [|' '|]
                    match line.[0] with
                        | "EXPLORER" -> ((ExplorerData.fromLineData line) :: explorers, wanderers)
                        | "WANDERER" -> (explorers, (WandererData.fromLineData line) :: wanderers)
                        | _ -> failwith "owo wut's dis"
                ) ([myPlayer], [])
                
            let newState =
                {this with 
                    MyPlayer    = Some myPlayer
                    Explorers   = explorers
                    Wanderers   = wanderers
                }
            
            printfn "WAIT"
            newState
            
    
module Program =
    open Model
    
    let rec run (d: Data) =
        let next = d.Step()
        run next
        
Program.run (Model.Data.init())
()