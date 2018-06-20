open System

let T = System.DateTime.Now

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

module MonteCarloModel =
    open Helpers

    type Entity =
        | Wall
        | Empty
        | MyPlayer
        | Explorer
        | Wanderer
        | Spawner
        | Slasher
        | Shelter
        | LightSource
        | PlanSource
        | YellSource

        static member fromChar (c: char) =
            match c with
            | '#' -> Wall
            | '.' -> Empty
            | 'U' -> Shelter
            | 'w' -> Spawner
            | _ -> failwith "owo wuts dis"

    type State = 
        (Coordinate * Entity) list

    let toKey (s: State) =
        s.Length

    type Reward = float32

    type Action =
        | Up
        | Down
        | Left
        | Right
        //| Plan
        //| Light
        //| Yell
        | Wait

        static member Count = 5

        static member toInt (a: Action) =
            match a with
            | Up    -> 1
            | Down  -> 2
            | Left  -> 3
            | Right -> 4
            //| Plan  -> 5
            //| Light -> 6
            //| Yell  -> 7
            | Wait  -> 0

        static member fromInt (a: int) =
            match a with
            | 1  -> Up   
            | 2  -> Down 
            | 3  -> Left 
            | 4  -> Right
            //| 5  -> Plan 
            //| 6  -> Light
            //| 7  -> Yell 
            | 0  -> Wait
            | d  -> failwith (sprintf "owo wuts dis: %d" d)
        
        member this.Do () =
            match this with
            | Up    -> printfn "UP"
            | Down  -> printfn "DOWN"
            | Left  -> printfn "LEFT"
            | Right -> printfn "RIGHT"
            //| Plan  -> printfn "PLAN"
            //| Light -> printfn "LIGHT"
            //| Yell  -> printfn "YELL"
            | Wait  -> printfn "WAIT"

    type MonteCarlo =
        {
            Q           : Map<int, Map<State, float32 list>>
            C           : Map<int, Map<State, float32 list>>
            Episodes    : int
            Behavior    : State -> float32 list
            Gamma       : float32
        }

module GameSimModel =
    open MonteCarloModel
    open Helpers

    type Settings = 
        {
            Width               : int
            Height              : int
            SanityLossLonely    : int
            SanityLossGroup     : int
            WandererSpawnTime   : int
            WandererLifeTime    : int
        }

    type ExplorerData =
        {
            ID          : int
            Position    : Coordinate
            Sanity      : int
            Plans       : int
            Lights      : int
            Stunned     : int
        }

        static member FromLineData (s: string array) =
            {
                ID      = int s.[1]
                Position= {X = int s.[2]; Y = int s.[3]}
                Sanity  = int s.[4]
                Plans   = int s.[5]
                Lights  = int s.[6]
                Stunned = 0
            }

        static member defData () =
            {
                ID = -1
                Position = {X = 0; Y = 0}
                Sanity = 250
                Plans = 2
                Lights = 2
                Stunned = 0
            }

    type MinionState =
        | Spawning
        | Wandering
        | Stalking
        | Rushing
        | Stunned

        static member FromInt (i: int) =
            match i with
            | 0 -> Spawning
            | 1 -> Wandering
            | 2 -> Stalking
            | 3 -> Rushing
            | 4 -> Stunned
            | _ -> failwith "owowowow"

    type SpawnedWandererData =
        {
            Id          : int
            Position    : Coordinate
            Recall      : int
            State       : MinionState
            Target      : int
        }

        static member FromLineData (s: string array) =
            {
                Id      = int s.[1]
                Position= {X = int s.[2]; Y = int s.[3]}
                Recall  = int s.[4]
                State   = MinionState.FromInt (int s.[5])
                Target  = int s.[6]
            }


    type SpawningWandererData =
        {
            Id          : int
            Position    : Coordinate
            Spawn       : int
            State       : MinionState
            Target      : int
        }

        static member FromLineData (s: string array) =
            {
                Id      = int s.[1]
                Position= {X = int s.[2]; Y = int s.[3]}
                Spawn   = int s.[4]
                State   = MinionState.FromInt (int s.[5])
                Target  = int s.[6]
            }

    type WandererData =
        | Spawned of SpawnedWandererData
        | Spawning of SpawningWandererData

    type PlanData =
        {
            Id          : int
            Position    : Coordinate
            Life        : int
            Parent      : int
        }

        static member FromLineData (s: string array) =
            {
                Id      = int s.[1]
                Position= {X = int s.[2]; Y = int s.[3]}
                Life    = int s.[4]
                Parent  = int s.[5]
            }

    type LightData =
        {
            Id          : int
            Position    : Coordinate
            Life        : int
            Parent      : int
        }

        static member FromLineData (s: string array) =
            {
                Id      = int s.[1]
                Position= {X = int s.[2]; Y = int s.[3]}
                Life    = int s.[4]
                Parent  = int s.[5]
            }

    type YellData =
        {
            Id          : int
            Position    : Coordinate
            Life        : int
            Parent      : int
            Target      : int
        }

        static member FromLineData (s: string array) =
            {
                Id      = int s.[1]
                Position= {X = int s.[2]; Y = int s.[3]}
                Life    = int s.[4]
                Parent  = int s.[5]
                Target  = int s.[6]
            }

    type ShelterData =
        {
            Id          : int
            Position    : Coordinate
            Energy      : int
        }

        static member FromLineData (s: string array) =
            {
                Id      = int s.[1]
                Position= {X = int s.[2]; Y = int s.[3]}
                Energy  = int s.[4]
            }

    type EffectData =
        | Plan of PlanData
        | Light of LightData
        | Yell of YellData
        | Shelter of ShelterData

    type SlasherData =
        {
            Id          : int
            Position    : Coordinate
            StateChange : int
            State       : MinionState
            Target      : int
        }

        static member FromLineData (s: string array) =
            {
                Id          = int s.[1]
                Position    = {X = int s.[2]; Y = int s.[3]}
                StateChange = int s.[4]
                State       = MinionState.FromInt (int s.[5])
                Target      = int s.[6]
            }

    type GameSimState =
        {
            InitTable   : (Coordinate * Entity) list
            Table       : (Coordinate * Entity) list
            Settings    : Settings
            MyPlayer    : ExplorerData
            Explorers   : ExplorerData list
            Wanderers   : WandererData list
            Effects     : EffectData list
            Slashers    : SlasherData list
            Yells       : (int*int) list
            Turn        : int
            ShouldEnd   : bool
            MonteCarlo  : MonteCarlo
        }

module GameSim =
    open MonteCarloModel
    open GameSimModel
    open Helpers
    //1. All players get the game state and print an action.
    //2. YELLs are handled first: any non-YELL (within 1 range) is converted to WAIT.
    //3. New minions spawn on the map.
    //4. Players move according to their command.
    //5. Effects are applied.
    //6. Minions move.
    //7. Minions spook players that share a cell with them.
    //8. Players lose sanity. The wanderers' time left until recall counts down.
    //9. Players with 0 sanity, wanderers with no time left (or having spooked a player), and faded out effects get removed from the game.
    
    let yellAction (action: Action) (gameSim: GameSimState) : GameSimState =
        //match action with
        //| Action.Yell ->
        //    let explorers =
        //        gameSim.Explorers
        //        |> List.map (fun exp ->
        //            let canStun = 
        //                gameSim.Yells
        //                |> List.contains (gameSim.MyPlayer.ID, exp.ID)
        //            if canStun then
        //                {exp with
        //                    Stunned = 2
        //                }
        //            else
        //                exp
        //        )
        //    let newTable =
        //        gameSim.Table
        //        |> List.map (fun (c, e) -> if c = gameSim.MyPlayer.Position then c, YellSource else c, e)
        //    {gameSim with 
        //        Explorers = explorers
        //        Table = newTable
        //    }
        //| _ ->
        //    gameSim
        gameSim
    let spawnMinions (gameSim: GameSimState) : GameSimState =
        if gameSim.Turn % 5 = 0 then
            let newWanderers =
                gameSim.InitTable
                |> List.filter (fun (_, e) -> 
                    match e with
                    | Entity.Spawner -> true
                    | _ -> false
                )
                |> List.map (fun (coord, _) ->
                    Spawning {
                        Id = -1
                        Position = coord
                        Spawn = gameSim.Settings.WandererSpawnTime
                        State = MinionState.Spawning
                        Target = -1
                    }
                )
            let newTable =
                gameSim.Table 
                |> List.map (fun (c,e) ->
                    match e with
                    | Entity.Spawner ->
                        c, Entity.Wanderer
                    | _ -> c,e
                )
            {gameSim with
                Wanderers = List.append gameSim.Wanderers newWanderers
                Table = newTable
            }
        else
            gameSim
    let movePlayers (action: Action) (gameSim: GameSimState) : GameSimState =
        let target =
            match action with
            | Action.Up ->
                {X = gameSim.MyPlayer.Position.X - 1; Y = gameSim.MyPlayer.Position.Y}
            | Action.Down ->
                {X = gameSim.MyPlayer.Position.X + 1; Y = gameSim.MyPlayer.Position.Y}
            | Action.Left ->
                {X = gameSim.MyPlayer.Position.X; Y = gameSim.MyPlayer.Position.Y - 1}
            | Action.Right ->
                {X = gameSim.MyPlayer.Position.X; Y = gameSim.MyPlayer.Position.Y + 1}
            | Action.Wait 
            | _ ->
                gameSim.MyPlayer.Position
        let targetCell = 
            gameSim.Table
            |> List.tryFind (fun (coord, _) -> coord = target)
        match targetCell with
        | Some (_, Wall) -> 
            {gameSim with ShouldEnd = true}
        | Some _ -> 
            let myPlayer = {gameSim.MyPlayer with Position = target}
            let newTable =
                gameSim.Table 
                |> List.map (fun (c,e) ->
                    if c = target then 
                        c, Entity.MyPlayer
                    elif c = gameSim.MyPlayer.Position then
                        gameSim.Table |> List.find (fun (coord, _) -> coord = gameSim.MyPlayer.Position)
                    else
                        c, e
                )
            {gameSim with 
                MyPlayer = myPlayer
                Table = newTable
            }
        | None -> failwith (sprintf "owo wheres dis %A" target)
    let applyEffects (action: Action) (gameSim: GameSimState) : GameSimState =
        gameSim
    let moveMinions (gameSim: GameSimState) : GameSimState =
        let wanderers =
            gameSim.Wanderers
            |> List.map (fun wanderer ->
                match wanderer with
                | Spawning wan -> 
                    wan.Position, Spawning wan
                | Spawned wan ->
                    let closestExplorer =
                        gameSim.Explorers
                        |> List.map (fun explorer ->
                            distance explorer.Position wan.Position, explorer
                        )
                        |> List.minBy fst
                        |> snd
                    let stepToExplorer =
                        directions
                        |> List.map (fun coord ->
                            let stepCoord = addDistance coord wan.Position
                            let stepCell =
                                gameSim.Table
                                |> List.tryFind (fun (c, _) -> c = stepCoord)
                            match stepCell with
                            | Some (_, Wall) -> None
                            | Some _ -> Some (distance closestExplorer.Position stepCoord, stepCoord)
                            | None -> failwith (sprintf "owo wheres dis %A" stepCoord)
                        )
                        |> List.choose id
                        |> List.minBy fst
                        |> snd
                    let newWan = 
                        Spawned {
                            wan with
                                Target = closestExplorer.ID
                                Position = stepToExplorer
                        }
                    wan.Position, newWan
            )
        let newTable =
            gameSim.Table 
            |> List.map (fun (c,e) ->
                let hasWan = 
                    wanderers
                    |> List.exists (fun (pos, wan) ->
                        match wan with
                        | Spawning wan -> false
                        | Spawned wan -> wan.Position = c
                    )
                let changeBack =
                    wanderers
                    |> List.exists (fun (pos, wan) ->
                        match wan with
                        | Spawning wan -> false
                        | Spawned wan -> pos = c
                    )
                if hasWan then
                    c, Wanderer
                elif changeBack then
                    gameSim.Table |> List.find (fun (coord, _) -> coord = gameSim.MyPlayer.Position)
                else
                    c, e
            )
        {gameSim with
            Wanderers = wanderers |> List.map snd
            Table = newTable
        }
    let spookPlayers (gameSim: GameSimState) : GameSimState =
        let explorers =
            gameSim.Explorers
            |> List.map (fun explorer ->
                let shouldSpook =
                    gameSim.Wanderers
                    |> List.exists (fun wan ->
                        match wan with
                        | Spawned wan -> wan.Position = explorer.Position
                        | _ -> false
                    )
                if shouldSpook then
                    {explorer with Sanity = explorer.Sanity - 20}
                else
                    explorer
            )
        let myPlayer = explorers.Head
        {gameSim with 
            Explorers = explorers
            MyPlayer = myPlayer
        }
    let loseSanity (gameSim: GameSimState) : GameSimState =
        let wanderers =
            gameSim.Wanderers
            |> List.map (fun wanderer ->
                match wanderer with
                | Spawned wan ->
                    Spawned {wan with Recall = wan.Recall - 1}
                | Spawning wan -> 
                    Spawning wan
            )
        let explorers =
            gameSim.Explorers
            |> List.map (fun explorer ->
                let group =
                    gameSim.Explorers
                    |> List.exists (fun exp ->
                        distance exp.Position explorer.Position < 3
                    )
                if group then
                    {explorer with Sanity = explorer.Sanity - gameSim.Settings.SanityLossGroup}
                else
                    {explorer with Sanity = explorer.Sanity - gameSim.Settings.SanityLossLonely}
            )
        let myPlayer =
            let group =
                gameSim.Explorers
                |> List.exists (fun exp ->
                    distance exp.Position gameSim.MyPlayer.Position < 3
                )
            if group then
                {gameSim.MyPlayer with Sanity = gameSim.MyPlayer.Sanity - gameSim.Settings.SanityLossGroup}
            else
                {gameSim.MyPlayer with Sanity = gameSim.MyPlayer.Sanity - gameSim.Settings.SanityLossLonely}
        {gameSim with
            Wanderers = wanderers
            Explorers = explorers
            MyPlayer  = myPlayer
            ShouldEnd = gameSim.ShouldEnd || myPlayer.Sanity <= 0
        }

    let removeDead (gameSim: GameSimState) : GameSimState =
        let explorers =
            gameSim.Explorers
            |> List.filter (fun explorer ->
                explorer.Sanity > 0
            )
        let wanderers =
            gameSim.Wanderers
            |> List.filter (fun wanderer ->
                match wanderer with
                | Spawned wan ->
                    let spooked =
                        gameSim.Explorers
                        |> List.exists (fun exp -> exp.Position = wan.Position)
                    wan.Recall > 0 || spooked
                | _ -> true
            )
        let effects =
            gameSim.Effects
            |> List.filter (fun effect ->
                match effect with
                | EffectData.Light d -> d.Life > 0
                | EffectData.Plan d -> d.Life > 0
                | EffectData.Yell d -> d.Life > 0
                | EffectData.Shelter d -> d.Energy > 0
            )
        {gameSim with
            Explorers = explorers
            Wanderers = wanderers
            Effects   = effects
            Turn      = gameSim.Turn + 1
        }

    let stepGame (action: Action) (gameSim: GameSimState) (maxTurn: int) : (State * Reward * bool * GameSimState) =
        let newGameSimState =
            gameSim
            |> yellAction action
            |> spawnMinions
            |> movePlayers action
            |> applyEffects action
            |> moveMinions
            |> spookPlayers
            |> loseSanity
            |> removeDead
        let won =
            match newGameSimState.Explorers with
            | [] -> false
            | explorers ->
                explorers.Tail
                |> List.forall (fun exp -> 
                    exp.Sanity = 0
                )
        let lost =
            newGameSimState.ShouldEnd
        let reward =
            if won then
                1000.f
            elif lost then
                if newGameSimState.MyPlayer.Sanity <= 0 then
                    -1000.f
                else
                    -250.f + float32 newGameSimState.MyPlayer.Sanity
            else
                1.f
        newGameSimState.Table, atan reward, won || lost || maxTurn = newGameSimState.Turn, newGameSimState

module MonteCarlo =
    open MonteCarloModel
    open GameSimModel
    open GameSim

    let nA = Action.Count

    let createRandomPolicy (n: int) =
            let ones = List.init n (fun _ -> 1.f / float32 n)
            fun (s: State) -> ones

    let createEpsilonGreedyPolicy (Q: Map<int, Map<State, float32 list>>) (nA: int) (epsilon: float32) =
        let policy (s: State) =
            let len = (toKey s)
            match Q.TryFind len with
            | Some maps -> 
                match maps.TryFind s with
                | Some values ->
                    let prob = if nA > 1 then epsilon / float32 (nA - 1) else 1.f
                    let A = List.init (List.length values) (fun _ -> prob)
                    let best = 
                        values
                        |> List.indexed
                        |> List.maxBy snd
                        |> fst
                    A
                    |> List.indexed
                    |> List.map (fun (i, v) -> 
                        if i = best then 1.f - epsilon else v
                    )
                | None ->
                    List.init nA (fun _ -> if nA > 1 then 1.f / float32 nA else 1.f)
            | None -> 
                List.init nA (fun _ -> if nA > 1 then 1.f / float32 nA else 1.f)
        policy

    let createGreedyPolicy (Q: Map<int, Map<State, float32 list>>) (n: int) =
        createEpsilonGreedyPolicy Q n 0.f

    let genEpisode (initState: State) (initSim: GameSimState) (behavior: State -> float32 list) (maxTurn: int) : (State * Action * Reward) list=
        let random = System.Random()
        let rec genEpisode (state: State) (gameSim: GameSimState) (finished: bool) (ep: (State * Action * Reward) list) =
            if finished then
                ep
            else
                let probs = behavior state 
                let action = 
                    let rand = random.Next(0, 100)
                    probs
                    |> List.indexed
                    |> List.fold (fun (prob, probs) (i, p) ->
                        let ap = p * 100.f
                        let probU = prob + ap
                        probU, ((prob, probU, i) :: probs)
                    ) (0.f, [])
                    |> snd
                    |> List.skipWhile (fun (p, _pU, _) ->
                        int p > rand
                    )
                    |> List.head
                    |> fun (_, _, a) -> a
                let actionType = Action.fromInt action
                let nextState, reward, finished, gameSim =
                    stepGame actionType gameSim maxTurn
                let episode = List.append ep [(state, actionType, reward)]
                genEpisode nextState gameSim finished episode 
        genEpisode initState initSim false []
        
    let mcControlImportanceSampling (episodes: int) 
                                    (behavior: State -> float32 list) 
                                    (Q : Map<int, Map<State, float32 list>>) 
                                    (C : Map<int, Map<State, float32 list>>) 
                                    (discountFactor: float32)
                                    (state: State)
                                    (gameSim: GameSimState) 
                                    (maxTurn: int) =
        let startT = System.DateTime.Now
        eprintfn "MC starts, %A" (startT - T)
        let Q, C =
            Array.init episodes (fun _ ->
                genEpisode state gameSim behavior maxTurn
            )
            |> List.ofArray
            |> List.fold (fun s episode ->
                let Q, C = s
            
                let G = 0.f
                let W = 1.f

                let G, C, Q, W =
                    (G, C, Q, W)
                    |> Seq.foldBack (fun v s ->
                        let state, actionType, reward = v
                        let action = Action.toInt actionType
                        let G, (C: Map<int, Map<State, float32 list>>), (Q: Map<int, Map<State, float32 list>>), W = s
                        let G = discountFactor * G + reward
                        let len = toKey state
                        let C = 
                            let newCVal = 
                                match C.TryFind len with
                                | Some maps ->
                                    match maps.TryFind state with
                                    | Some s -> 
                                        s
                                        |> List.indexed
                                        |> List.map (fun (i, p) ->
                                            if i = action then p + W else p
                                        )
                                    | None -> 
                                        List.init nA (fun _ -> 0.f)
                                        |> List.indexed
                                        |> List.map (fun (i, p) ->
                                            if i = action then p + W else p
                                        )
                                | None -> 
                                    List.init nA (fun _ -> 0.f)
                                    |> List.indexed
                                    |> List.map (fun (i, p) ->
                                        if i = action then p + W else p
                                    )
                            match C.TryFind len with
                            | Some maps ->
                                C.Add (len, maps.Add (state, newCVal))
                            | None -> 
                                C.Add (len, Map [(state, newCVal)])
                        let cVal =
                            match C.TryFind len with
                            | Some maps -> 
                                match maps.TryFind state with
                                | Some s ->
                                    s |> List.item action
                                | None -> W
                            | None -> W
                        let Q = 
                            let newQVal =
                                match Q.TryFind len with
                                | Some maps -> 
                                    match maps.TryFind state with
                                    | Some s ->
                                        s
                                        |> List.indexed
                                        |> List.map (fun (i, p) ->
                                            if i = action then p + (W / cVal) * (G - p) else p
                                        )
                                    | None -> 
                                        List.init nA (fun _ -> 0.)
                                        |> List.indexed
                                        |> List.map (fun (i, p) ->
                                            if i = action then (W / cVal) * G else 0.f
                                        )
                                | None -> 
                                    List.init nA (fun _ -> 0.)
                                    |> List.indexed
                                    |> List.map (fun (i, p) ->
                                        if i = action then (W / cVal) * G else 0.f
                                    )
                            match Q.TryFind len with
                            | Some maps ->
                                Q.Add (len, maps.Add (state, newQVal))
                            | None -> 
                                Q.Add (len, Map [(state, newQVal)])
                        let denom = 
                            behavior state
                            |> List.item action
                        let W = W * 1.f/denom
                        G, C, Q, W
                    ) episode
                Q, C
            ) (Q, C)
        let endT = System.DateTime.Now
        eprintfn "MC ends, duration: %A" (endT - T)
        Q, C, createGreedyPolicy Q nA

module Calculations =
    open Helpers   
    open GameSimModel
    open GameSim
    open MonteCarloModel
    open MonteCarlo

    let ConstructNewState (old: GameSimState) : GameSimState =
        let entityCount = int (Console.In.ReadLine())
        
        let myPlayer = ExplorerData.FromLineData ((Console.In.ReadLine()).Split [|' '|])
        
        let startReadT = System.DateTime.Now
        let (explorers, wanderers, effects, slashers, newTable) =
            Seq.init (entityCount - 1) id
            |> Seq.fold (fun (explorers, wanderers, effects, slashers, newTable) _ ->
                let line = (Console.In.ReadLine()).Split [|' '|]
                match line.[0] with
                    | "EXPLORER" -> 
                        let newEntity =
                            (ExplorerData.FromLineData line)
                        let newTable =
                            newTable
                            |> List.map (fun (c, e) ->
                                if c = newEntity.Position then
                                    c, Entity.Explorer
                                else
                                    c, e
                            )
                        (newEntity :: explorers, wanderers, effects, slashers, newTable)
                    | "WANDERER" -> 
                        let wanderer =
                            match MinionState.FromInt (int line.[5]) with
                            | MinionState.Spawning ->
                                Spawning (SpawningWandererData.FromLineData line)
                            | _ ->
                                Spawned (SpawnedWandererData.FromLineData line)
                        let pos = 
                            match wanderer with
                            | Spawning wan -> wan.Position
                            | Spawned wan -> wan.Position
                        let newTable =
                            newTable
                            |> List.map (fun (c, e) ->
                                if c = pos then
                                    c, Entity.Wanderer
                                else
                                    c, e
                            )
                        (explorers, wanderer :: wanderers, effects, slashers, newTable)
                    | "SLASHER"  -> 
                        let newEntity =
                            (SlasherData.FromLineData line)
                        let newTable =
                            newTable
                            |> List.map (fun (c, e) ->
                                if c = newEntity.Position then
                                    c, Entity.Slasher
                                else
                                    c, e
                            )
                        (explorers, wanderers, effects, newEntity :: slashers, newTable)
                    | "EFFECT_YELL" -> 
                        let newEntity =
                            (EffectData.Yell (YellData.FromLineData line))
                        let newTable =
                            newTable
                            |> List.map (fun (c, e) ->
                                match newEntity with
                                | EffectData.Yell newEntity->
                                    if c = newEntity.Position then
                                        c, Entity.YellSource
                                    else
                                        c, e
                                | _ -> c, e
                            )
                        (explorers, wanderers, newEntity :: effects, slashers, newTable)
                    | "EFFECT_PLAN" -> 
                        let newEntity =
                            (EffectData.Plan (PlanData.FromLineData line))
                        let newTable =
                            newTable
                            |> List.map (fun (c, e) ->
                                match newEntity with
                                | EffectData.Plan newEntity->
                                    if c = newEntity.Position then
                                        c, Entity.PlanSource
                                    else
                                        c, e
                                | _ -> c, e
                            )
                        (explorers, wanderers, newEntity :: effects, slashers, newTable)
                    | "EFFECT_SHELTER" -> 
                        let newEntity =
                            (EffectData.Shelter (ShelterData.FromLineData line))
                        let newTable =
                            newTable
                            |> List.map (fun (c, e) ->
                                match newEntity with
                                | EffectData.Shelter newEntity->
                                    if c = newEntity.Position then
                                        c, Entity.Shelter
                                    else
                                        c, e
                                | _ -> c, e
                            )
                        (explorers, wanderers, newEntity :: effects, slashers, newTable)
                    | "EFFECT_LIGHT" -> 
                        let newEntity =
                            (EffectData.Light (LightData.FromLineData line))
                        let newTable =
                            newTable
                            |> List.map (fun (c, e) ->
                                match newEntity with
                                | EffectData.Light newEntity->
                                    if c = newEntity.Position then
                                        c, Entity.LightSource
                                    else
                                        c, e
                                | _ -> c, e
                            )
                        (explorers, wanderers, newEntity :: effects, slashers, newTable)
                    | _ -> failwith (sprintf "owo wut's dis %A" line.[0])
            ) ([], [], [], [], old.Table)
        
        let newState =
            {old with 
                Table       = newTable
                MyPlayer    = myPlayer
                Explorers   = myPlayer :: explorers
                Wanderers   = wanderers
                Effects     = effects
                Slashers    = slashers
            }

        let startT = System.DateTime.Now
        eprintfn "ConstructNewState has the new state, %A\tduration: %A" (startT - T) (startT - startReadT)
        
        let Q, C, _ = MonteCarlo.mcControlImportanceSampling 
                            ((newState.MonteCarlo.Episodes)  / 100)
                            (newState.MonteCarlo.Behavior) 
                            (newState.MonteCarlo.Q) 
                            (newState.MonteCarlo.C) 
                            (newState.MonteCarlo.Gamma) 
                            newState.Table
                            newState 
                            5
                            
        let startT = System.DateTime.Now
        eprintfn "ConstructNewState finished the mc, %A" (startT - T)
        
        {newState with
            MonteCarlo = 
                {newState.MonteCarlo with
                    Q = Q
                    C = C
                    Behavior = createEpsilonGreedyPolicy Q Action.Count 0.1f
                }
        }

    let InitState () : GameSimState =
        let width = int(Console.In.ReadLine())
        let height = int(Console.In.ReadLine())
        let state =
            List.init height (fun i ->
                let line = Console.In.ReadLine()
                
                line
                |> Seq.mapi (fun j c ->
                    {X = j; Y = i}, Entity.fromChar c
                )
                |> List.ofSeq
            )
            |> List.concat
        let token = (Console.In.ReadLine()).Split [|' '|]
        let sanityLossLonely = int(token.[0])
        let sanityLossGroup = int(token.[1])
        let wandererSpawnTime = int(token.[2])
        let wandererLifeTime = int(token.[3])

        let mc =
            {
                Episodes = 400
                Behavior = createRandomPolicy MonteCarloModel.Action.Count
                Q = Map.empty
                C = Map.empty
                Gamma = 0.9f
            }
        let settings = 
            {
                Width               = width
                Height              = height
                SanityLossLonely    = sanityLossLonely
                SanityLossGroup     = sanityLossGroup
                WandererSpawnTime   = wandererSpawnTime
                WandererLifeTime    = wandererLifeTime
            } : Settings
        let gameSim =
            {
                InitTable = state
                Table = state
                Settings = settings
                MyPlayer = ExplorerData.defData ()
                Explorers = []
                Wanderers = []
                Effects = []
                Slashers = []
                Yells = []
                Turn = 0
                ShouldEnd = false
                MonteCarlo = mc
            } : GameSimState

        let startT = System.DateTime.Now
        eprintfn "initState has the state, %A" (startT - T)
        
        let newState = ConstructNewState gameSim
        
        let startT = System.DateTime.Now
        eprintfn "initState constructed the state, %A" (startT - T)
        
        let Q, C, _ = MonteCarlo.mcControlImportanceSampling 
                            (newState.MonteCarlo.Episodes) 
                            (newState.MonteCarlo.Behavior) 
                            (newState.MonteCarlo.Q) 
                            (newState.MonteCarlo.C) 
                            (newState.MonteCarlo.Gamma) 
                            newState.Table
                            newState 
                            5

        let startT = System.DateTime.Now
        eprintfn "initState finished the mc, %A" (startT - T)
        
        {newState with
            MonteCarlo = 
                {newState.MonteCarlo with
                    Q = Q
                    C = C
                    Behavior = createEpsilonGreedyPolicy Q Action.Count 0.1f
                }
        }

    let decideAction (newState: GameSimState) : Action =
        let startT = System.DateTime.Now
        eprintfn "decideAction starts the test"
        
        let sarsa = genEpisode newState.Table newState newState.MonteCarlo.Behavior 2
        let state, action, reward = sarsa.Head

        let endT = System.DateTime.Now
        eprintfn "decideAction finished the test\tduration: %A\taction: %A" (endT - T) action
        action
        
    let Step (d: GameSimState) : GameSimState =
        let startT = System.DateTime.Now
        eprintfn "step started %A" (startT - T)
        let action = decideAction d
        let endT = System.DateTime.Now
        eprintfn "step decided action, duration: %A" (endT - T)
        action.Do ()
        let newState = ConstructNewState d
        newState
            
module Program =
    open GameSimModel
    
    let rec run (d: GameSimState) =
        let startT = System.DateTime.Now
        eprintfn "run start: %A" (startT - T)
        let next = Calculations.Step d
        let endT = System.DateTime.Now
        eprintfn "run duration: %A" (endT - startT)
        run next
        
[<EntryPoint>]
Program.run (Calculations.InitState())
()