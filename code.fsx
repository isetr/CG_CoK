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

    let directions =
        [
            {X = 0; Y = 1}  // N
            //{X = 1; Y = 1}  // NE
            {X = 1; Y = 0}  // E
            //{X = 1; Y = -1} // SE
            {X = 0; Y = -1} // S
            //{X = -1; Y = -1}// SW
            {X = -1; Y = 0} // W
            //{X = -1; Y = 1} // NW
        ]

    let distance (a: Coordinate) (b: Coordinate) : int =
        (abs (a.X - b.X)) + (abs (a.Y - b.Y))

    let normalize (coord: Coordinate) : Coordinate =
        {
            X = if coord.X > 0 then 1 elif coord.X < 0 then -1 else 0
            Y = if coord.Y > 0 then 1 elif coord.Y < 0 then -1 else 0
        }

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

    let subDistance (a: Coordinate) (b: Coordinate) : Coordinate =
        {
            X = a.X - b.X
            Y = a.Y - b.Y
        }   
        
    let rotate90Degrees (coord: Coordinate) : Coordinate =
        {
            X = -coord.Y
            Y = coord.X
        }

module Model =
    open Helpers

    type Action =
        | Wait
        | Move of Coordinate
        | Plan
        | Light
        | Yell
        
        member this.Do () =
            match this with
            | Wait          -> printfn "WAIT"
            | Move coord    -> printfn "MOVE %d %d" coord.X coord.Y
            | Plan          -> printfn "PLAN"
            | Light         -> printfn "LIGHT"
            | Yell          -> printfn "YELL"
            
    type CellType =
        | Wall
        | WandererSpawn
        | Empty
        | Shelter
        
        static member FromChar (c: char) =
            match c with
                | '#' -> Wall
                | 'w' -> WandererSpawn
                | '.' -> Empty
                | 'U' -> Shelter
                | _ -> failwith "owo what's this"
                
    type ExplorerData =
        {
            Id      : int
            Position: Coordinate
            Sanity  : int
            Plans   : int
            Lights  : int
        }
        
        static member FromLineData (s: string array) =
            {
                Id      = int s.[1]
                Position= {X = int s.[2]; Y = int s.[3]}
                Sanity  = int s.[4]
                Plans   = int s.[5]
                Lights  = int s.[6]
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
        
    type SlasherData =
        {
            Id      : int
            Position: Coordinate
            State   : int
            Param1  : int
            Target  : int
        }
        
        static member FromLineData (s: string array) =
            {
                Id      = int s.[1]
                Position= {X = int s.[2]; Y = int s.[3]}
                State   = int s.[4]
                Param1  = int s.[5]
                Target  = int s.[6]
            }

    type EffectData =
        {
            Id          : int
            Position    : Coordinate
            Life        : int
            Parent      : int
            Param2      : int
        }
        
        static member FromLineData (s: string array) =
            {
                Id      = int s.[1]
                Position= {X = int s.[2]; Y = int s.[3]}
                Life    = int s.[4]
                Parent  = int s.[5]
                Param2  = int s.[6]
            }

    type Effect =
        | PlanEffect of EffectData
        | LightEffect of EffectData
        | YellEffect of EffectData
        | ShelterEffect of EffectData
        
        static member FromLineData (s: string array) =
            match s.[0] with
            | "EFFECT_PLAN" -> PlanEffect (EffectData.FromLineData s)
            | "EFFECT_LIGHT" -> LightEffect (EffectData.FromLineData s)
            | "EFFECT_SHELTER" -> ShelterEffect (EffectData.FromLineData s)
            | "EFFECT_YELL" -> YellEffect (EffectData.FromLineData s)
            | _ -> failwith "owo wuts dat"
                
    type Entity =
        | Explorer of ExplorerData
        | Wanderer of WandererData
            
    type State =
        {
            Width               : int
            Height              : int
            Table               : Map<Coordinate, CellType>
            WeightTable         : Map<Coordinate, float32 option>
            SanityLossLonely    : int
            SanityLossGroup     : int
            WandererSpawnTime   : int
            WandererLifeTime    : int
            MyPlayer            : ExplorerData option
            Explorers           : ExplorerData list
            Wanderers           : WandererData list
            Effects             : Effect list
            Slashers            : SlasherData list
        }
        member this.GetTile (c: Coordinate) =
            match this.Table.TryFind c with
            | Some v -> Some v
            | None -> failwith (sprintf "No Tile on: %A" c)

        member this.GetWeight (c: Coordinate) =
            match this.GetTile c with
            | Some Wall -> None
            | _ ->
                match this.WeightTable.TryFind c with
                | Some v -> v
                | None -> failwith (sprintf "No weight on: %A" c)
            
module AStar =
    open Helpers
    open Model

    let neighbors (state: State) (c: Coordinate) =
        directions
        |> List.map (fun dir ->
            let hasWanderer =
                state.Wanderers
                |> List.exists (fun wanderer ->
                    wanderer.Position = (addDistance c dir)
                )
            if hasWanderer then
                None
            else
                match state.GetTile (addDistance c dir) with
                | Some Wall 
                | None -> None
                | Some v -> Some v
        )
        |> List.choose id

    let heuristic = distance

module Calculations =
    open Model
    open Helpers

    let weightTable (state: Map<Coordinate, CellType>) (width: int) (height: int) (wanderers: WandererData list) (slashers: SlasherData list) (explorers: ExplorerData list) (effects: Effect list) =
        state
        |> Map.map(fun coord _ ->
            let weight =
                match state.TryFind coord with
                | Some WandererSpawn -> Some 150.f
                | Some Empty -> Some 100.f
                | Some Shelter -> Some 0.f
                | Some Wall
                | _ -> None
            let shelter =
                effects
                |> List.exists (fun effect ->
                    match effect with
                    | ShelterEffect d -> 
                        d.Life > 0 && (d.Position = coord ||
                            directions
                            |> List.exists (fun dir ->
                                d.Position = addDistance coord dir
                            )
                        )
                    | _ -> false
                )
            let explorer =
                explorers
                |> List.exists (fun wan -> 
                    wan.Position = coord
                )
            let explorerNeighbor =
                explorers
                |> List.exists (fun wan -> 
                    let rec has n had innercoord = 
                        if n = 0 then
                            had
                        else
                            directions
                            |> List.exists (fun dir ->
                                let hasIt = (addDistance innercoord dir) <> coord && wan.Position = addDistance innercoord dir
                                has (n-1) (hasIt || had) (addDistance innercoord dir)
                            )
                    has 2 false coord
                )
            let wanderer =
                wanderers
                |> List.exists (fun wan -> 
                    wan.Position = coord
                )
            let wandererNeighbor =
                wanderers
                |> List.exists (fun wan -> 
                    let rec has n had innercoord = 
                        if n = 0 then
                            had
                        else
                            directions
                            |> List.exists (fun dir ->
                                let hasIt = (addDistance innercoord dir) <> coord && wan.Position = addDistance innercoord dir
                                has (n-1) (hasIt || had) (addDistance innercoord dir)
                            )
                    has 3 false coord
                )
            let slasher =
                slashers
                |> List.exists (fun wan -> 
                    wan.Position = coord
                )
            let slasherNeighbor =
                slashers
                |> List.exists (fun wan -> 
                    if wan.Param1 <> 3 then
                        false
                    elif coord.X = wan.Position.X then
                        let left =
                            Array.init (wan.Position.Y) (fun coordY ->
                                let pos = {X = wan.Position.X; Y = wan.Position.Y - coordY}
                                match state.TryFind pos with
                                | Some cell -> pos, cell
                                | None -> failwith "lul"
                            )
                            |> Array.takeWhile (fun (pos, cell) ->
                                match cell with
                                | Wall -> false
                                | _ -> 
                                    match explorers with
                                    | [] -> true
                                    | [_] -> true
                                    | xs ->
                                        xs.Tail
                                        |> List.exists (fun exp ->
                                            exp.Position = pos
                                        )
                                        |> not
                            )
                            |> Array.map fst
                            |> Array.contains coord
                        let right =
                            Array.init (height - wan.Position.Y) (fun coordY ->
                                let pos = {X = wan.Position.X; Y = wan.Position.Y + coordY}
                                match state.TryFind pos with
                                | Some cell -> pos, cell
                                | None -> failwith (sprintf "lul %A" pos)
                            )
                            |> Array.takeWhile (fun (pos, cell) ->
                                match cell with
                                | Wall -> false
                                | _ -> 
                                    match explorers with
                                    | [] -> true
                                    | [_] -> true
                                    | xs ->
                                        xs.Tail
                                        |> List.exists (fun exp ->
                                            exp.Position = pos
                                        )
                                        |> not
                            )
                            |> Array.map fst
                            |> Array.contains coord
                        right || left
                    elif coord.Y = wan.Position.Y then
                        let up =
                            Array.init (wan.Position.X) (fun coordX ->
                                let pos = {X = wan.Position.X - coordX; Y = wan.Position.Y}
                                match state.TryFind pos with
                                | Some cell -> pos, cell
                                | None -> failwith "lul"
                            )
                            |> Array.takeWhile (fun (pos, cell) ->
                                match cell with
                                | Wall -> false
                                | _ -> 
                                    match explorers with
                                    | [] -> true
                                    | [_] -> true
                                    | xs ->
                                        xs.Tail
                                        |> List.exists (fun exp ->
                                            exp.Position = pos
                                        )
                                        |> not
                            )
                            |> Array.map fst
                            |> Array.contains coord
                        let down =
                            Array.init (width - wan.Position.X) (fun coordX ->
                                let pos = {X = wan.Position.X + coordX; Y = wan.Position.Y}
                                match state.TryFind pos with
                                | Some cell -> pos, cell
                                | None -> failwith "lul"
                            )
                            |> Array.takeWhile (fun (pos, cell) ->
                                match cell with
                                | Wall -> false
                                | _ -> 
                                    match explorers with
                                    | [] -> true
                                    | [_] -> true
                                    | xs ->
                                        xs.Tail
                                        |> List.exists (fun exp ->
                                            exp.Position = pos
                                        )
                                        |> not
                            )
                            |> Array.map fst
                            |> Array.contains coord
                        up || down
                    else
                        false
                )
            match weight with
            | None -> None
            | Some weight ->
                let weight = 
                    if shelter then
                        weight - 50.f
                    else
                        weight
                let weight =
                    if slasherNeighbor then
                        weight + 500.f
                    else
                        weight
                let weight =
                    if wandererNeighbor then
                        weight + 300.f
                    else
                        weight
                let weight =
                    if slasher then
                        weight + 300.f
                    else
                        weight
                let weight =
                    if wanderer then
                        weight + 500.f
                    else
                        weight
                let weight =
                    if explorer then
                        weight - 250.f
                    else
                        weight
                let weight =
                    if explorerNeighbor then
                        weight - 200.f
                    else
                        weight
                Some weight
        )

    let InitState () : State =
        let width = int(Console.In.ReadLine())
        let height = int(Console.In.ReadLine())
        let state =
            Seq.init height (fun i ->
                let line = Console.In.ReadLine()
                
                line
                |> Seq.mapi (fun j c ->
                    {X = j; Y = i}, CellType.FromChar c
                )
            )
            |> Seq.concat
            |> Map<Coordinate, CellType>
        let token = (Console.In.ReadLine()).Split [|' '|]
        let sanityLossLonely = int(token.[0])
        let sanityLossGroup = int(token.[1])
        let wandererSpawnTime = int(token.[2])
        let wandererLifeTime = int(token.[3])

        {
            Width               = width
            Height              = height
            Table               = state
            WeightTable         = weightTable state width height [] [] [] []
            SanityLossLonely    = sanityLossLonely
            SanityLossGroup     = sanityLossGroup
            WandererSpawnTime   = wandererSpawnTime
            WandererLifeTime    = wandererLifeTime
            MyPlayer            = None
            Explorers           = []
            Wanderers           = []
            Effects             = []
            Slashers            = []
        } : State

    let ConstructNewState (old: State) : State =
        let entityCount = int (Console.In.ReadLine())
        
        let myPlayer = ExplorerData.FromLineData ((Console.In.ReadLine()).Split [|' '|])
        
        let (explorers, wanderers, effects, slashers) =
            Seq.init (entityCount - 1) id
            |> Seq.fold (fun (explorers, wanderers, effects, slashers) _ ->
                let line = (Console.In.ReadLine()).Split [|' '|]
                match line.[0] with
                    | "EXPLORER" -> ((ExplorerData.FromLineData line) :: explorers, wanderers, effects, slashers)
                    | "WANDERER" -> (explorers, (WandererData.FromLineData line) :: wanderers, effects, slashers)
                    | "SLASHER"  -> (explorers, wanderers, effects, (SlasherData.FromLineData line) :: slashers)
                    | "EFFECT_YELL"
                    | "EFFECT_PLAN"
                    | "EFFECT_SHELTER"
                    | "EFFECT_LIGHT" -> (explorers, wanderers, (Effect.FromLineData line) :: effects, slashers)
                    | _ -> failwith (sprintf "owo wut's dis %A" line.[0])
            ) ([], [], [], [])

        {old with 
            MyPlayer    = Some myPlayer
            Explorers   = myPlayer :: explorers
            Wanderers   = wanderers
            Effects     = effects
            WeightTable = weightTable old.Table old.Width old.Height wanderers slashers explorers effects 
            Slashers    = slashers
        }

    let decideAction (oldState: State) (newState: State) : Action =
        let currentPlans, currentLights =
            newState.Effects
            |> List.partition (fun effect ->
                match effect with
                | PlanEffect _ -> true
                | _ -> false
            )
        let availableEffects =
            newState.Explorers
            |> List.map (fun player ->
                player.Plans, player.Lights
            )
        let myPlan, myLight = availableEffects.Head
        match newState.MyPlayer with
        | None -> Wait
        | Some myPlayer ->
            let usePlan =
                let sanity =
                    myPlayer.Sanity < 200
                let current =
                    currentPlans
                    |> List.exists (fun ef ->
                        match ef with
                        | PlanEffect d -> d.Parent = myPlayer.Id
                        | _ -> false
                    )
                let others =
                    availableEffects
                    |> List.forall (fun (p, _) -> p <= myPlan)
                not current && sanity && myPlan > 0//&& others
            let useLight =
                let current =
                    currentLights
                    |> List.exists (fun ef ->
                        match ef with
                        | LightEffect d -> d.Parent = myPlayer.Id
                        | _ -> false
                    )
                let others =
                    availableEffects
                    |> List.forall (fun (_, l) -> l <= myLight)
                let hasWandererClose =
                    newState.Wanderers
                    |> List.exists (fun wan ->
                        let closeToMe = distance wan.Position myPlayer.Position 
                        let closeToOther =
                            newState.Explorers.Tail
                            |> List.exists (fun exp ->
                                distance exp.Position wan.Position <= closeToMe
                            )
                        closeToOther && closeToMe < 5
                    )
                not current && myLight > 0 && hasWandererClose//&& others
            let useYell =
                //false
                let explorersCloseToMe =
                    newState.Explorers.Tail
                    |> List.exists (fun exp ->
                        let close = distance exp.Position myPlayer.Position < 3
                        // let hasWandererClose =
                        //     newState.Wanderers
                        //     |> List.exists (fun wan -> 
                        //         distance wan.Position exp.Position < 3
                        //     )
                        let hasSlasherInLine =
                            newState.Slashers
                            |> List.exists (fun wan ->
                                wan.State = 3 && (wan.Position.X = exp.Position.X || wan.Position.Y = exp.Position.Y)
                            )
                        close && hasSlasherInLine
                    )
                explorersCloseToMe
            let closestExplorer =
                let explorers =
                    newState.Explorers.Tail
                    |> List.map (fun explorer ->
                        distance explorer.Position myPlayer.Position, explorer
                    )
                match explorers with
                | [] -> None
                | explorers ->
                    explorers
                    |> List.minBy fst
                    |> snd
                    |> Some
            let weightAround =
                directions
                |> List.map (fun dir ->
                    let step = addDistance dir myPlayer.Position
                    match newState.GetWeight step with
                    | Some v -> 
                        let v =
                            directions
                            |> List.fold (fun (weight, count) ndir ->
                                let nstep = addDistance ndir step
                                if nstep <> myPlayer.Position then
                                    match newState.GetWeight nstep with
                                    | Some v -> v + weight, count + 1.f
                                    | None -> weight, count
                                else
                                    weight, count
                            ) (v, 0.f)
                        Some (v, step)
                        //eprintfn "%A" (v, step)
                        // let dirs =
                        //     directions
                        //     |> List.map (fun ndir ->
                        //         let step = addDistance step ndir
                        //         match newState.GetWeight step with
                        //         | Some nv -> 
                        //             //eprintfn "%A" (nv, step)
                        //             Some (nv, step)
                        //         | None -> None
                        //     )
                        //     |> List.choose id
                        // Some ((v, step) :: dirs)
                    | None -> None
                )
                |> List.choose id
                // |> List.concat
            let weightAround =
                weightAround
                |> List.map (fun ((v, c), coord) -> v / c, coord)
            let noDanger = 
                weightAround 
                |> List.forall (fun (i, _) -> 
                    i < 150.f
                )
            let bestPath = 
                weightAround
                |> List.filter (fun (_, d) ->
                    d <> myPlayer.Position
                )
                |> List.minBy (fun (weight, coord) -> 
                    weight
                )
                |> snd
            let idle =
                match closestExplorer with
                | Some closestExplorer ->
                    if useYell then
                        Yell
                    elif usePlan then
                        Plan
                    elif useLight then
                        Light
                    else
                        Move bestPath
                        //Move (closestExplorer.Position)
                | None ->
                    if usePlan then
                        Plan
                    elif useLight then
                        Light
                    else
                        Move bestPath
            let action =
                let myPos = 
                    match newState.GetWeight myPlayer.Position with
                    | Some v -> v
                    | None -> 0.f
                if myPos < 120.f then
                    Wait
                elif noDanger then
                    Wait
                elif useYell then
                    Yell
                else
                    Move bestPath
            match action with
            | Move v -> Move v
            | Wait ->
                match currentPlans, currentLights with
                | [], [] ->
                    match usePlan, useLight with
                    | true, false -> if usePlan then Plan else idle
                    | false, true -> if useLight then Light else idle
                    | true, true -> if myPlan > myLight && usePlan then Plan elif useLight then Light else idle
                    | false, false -> Wait
                | _, [] -> 
                    if useLight then Light else idle
                | [], _ -> 
                    if usePlan then Plan else idle
                | _, _ -> 
                    idle
            | _ -> idle    

        
    let Step (d: State) : State =
        let newState = ConstructNewState d
        let action = decideAction d newState
        action.Do ()
        newState
            
module Program =
    open Model
    
    let rec run (d: State) =
        let next = Calculations.Step d
        run next
        
Program.run (Calculations.InitState())
()