open System

module Helpers =
    type Coordinate =
        {
            X   : int
            Y   : int
        }

    type Vector =
        {
            Coordinates : Coordinate
            Length      : int
        }

    let distance (a: Coordinate) (b: Coordinate) : int =
        abs (a.X - b.X) + abs (a.Y + b.Y)

    let direction (a: Coordinate) (b: Coordinate) : Vector =
        {
            Coordinates = {X = a.X - b.X; Y = a.Y - b.Y}
            Length  = distance a b
        }

    let reverseDirection (coord: Coordinate) : Coordinate =
        {
            X = -coord.X
            Y = -coord.Y
        }

    let addDistance (a: Coordinate) (b: Coordinate) : Coordinate =
        {
            X = a.X + b.X
            Y = a.Y + b.Y
        }    

module Model =
    open Helpers

    type Action =
        | Wait
        | Move of Coordinate
        
        member this.Do () =
            match this with
            | Wait          -> printfn "WAIT"
            | Move coord    -> printfn "MOVE %d %d" coord.X coord.Y
            
    type CellType =
        | Wall
        | WandererSpawn
        | Empty
        
        static member FromChar (c: char) =
            match c with
                | '#' -> Wall
                | 'w' -> WandererSpawn
                | '.' -> Empty
                | _ -> failwith "owo what's this"
                
    type ExplorerData =
        {
            Id      : int
            Position: Coordinate
            Sanity  : int
            Param1  : int
            Param2  : int
        }
        
        static member FromLineData (s: string array) =
            {
                Id      = int s.[1]
                Position= {X = int s.[2]; Y = int s.[3]}
                Sanity  = int s.[4]
                Param1  = int s.[5]
                Param2  = int s.[6]
            }
        
    type WandererData =
        {
            Id      : int
            Position: Coordinate
            Life    : int
            Spawned : bool
            Target  : int
        }
        
        static member FromLineData (s: string array) =
            {
                Id      = int s.[1]
                Position= {X = int s.[2]; Y = int s.[3]}
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
            Table               : Map<Coordinate, CellType>
            SanityLossLonely    : int
            SanityLossGroup     : int
            WandererSpawnTime   : int
            WandererLifeTime    : int
            MyPlayer            : ExplorerData option
            Explorers           : ExplorerData list
            Wanderers           : WandererData list
        }
        
        static member Init () : Data =
            let width = int(Console.In.ReadLine())
            let height = int(Console.In.ReadLine())
            let data =
                Seq.init height (fun i ->
                    let line = Console.In.ReadLine()
                    
                    line
                    |> Seq.mapi (fun j c ->
                        {X = i; Y = j}, CellType.FromChar c
                    )
                )
                |> Seq.concat
                |> Map<Coordinate, CellType>
        
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
            
            let myPlayer = ExplorerData.FromLineData ((Console.In.ReadLine()).Split [|' '|])
            
            let (explorers, wanderers) =
                Seq.init (entityCount - 1) id
                |> Seq.fold (fun (explorers, wanderers) _ ->
                    let line = (Console.In.ReadLine()).Split [|' '|]
                    match line.[0] with
                        | "EXPLORER" -> ((ExplorerData.FromLineData line) :: explorers, wanderers)
                        | "WANDERER" -> (explorers, (WandererData.FromLineData line) :: wanderers)
                        | _ -> failwith "owo wut's dis"
                ) ([myPlayer], [])
                
            let newState =
                {this with 
                    MyPlayer    = Some myPlayer
                    Explorers   = explorers
                    Wanderers   = wanderers
                }

            let decideAction (oldData: Data) (newData: Data) : Action =
                let actions =
                    newData.Wanderers
                    |> List.map (fun wanderer ->
                        match newData.MyPlayer with
                        | None -> 100, Wait
                        | Some myPlayer ->
                            if wanderer.Target = myPlayer.Id then
                                let vec =
                                    direction wanderer.Position myPlayer.Position
                                let toMove =
                                    addDistance (myPlayer.Position) (reverseDirection vec.Coordinates)                  
                                vec.Length, Move toMove
                            else
                                100, Wait
                    )
                match actions with
                | [] -> Wait
                | actions ->    
                    actions            
                    |> List.minBy fst
                    |> snd

            let action = decideAction this newState
            action.Do ()
            newState
            
    
module Program =
    open Model
    
    let rec run (d: Data) =
        let next = d.Step()
        run next
        
Program.run (Model.Data.Init())
()