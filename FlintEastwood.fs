namespace FlintEastwood

open System
open System.Windows
open System.Windows.Controls
open System.Windows.Input
open System.Windows.Media
open System.Windows.Media.Imaging
open System.Windows.Shapes

[<AutoOpen>]
module Game = 
    let width, height = 512,384
    let move(shape,x,y) = Canvas.SetLeft(shape,float x); Canvas.SetTop(shape,float y)
    let read(shape) = Canvas.GetLeft(shape) |> int, Canvas.GetTop(shape) |> int
    let run rate update =
        let rate = TimeSpan.FromSeconds(rate)
        let lastUpdate = ref DateTime.Now
        let residual = ref (TimeSpan())
        CompositionTarget.Rendering.Subscribe (fun _ -> 
            let now = DateTime.Now
            residual := !residual + (now - !lastUpdate)
            while !residual > rate do
                update(); residual := !residual - rate
            lastUpdate := now
        )
    let toImage (bitmap:#BitmapSource) =
        let w, h = float bitmap.PixelWidth, float bitmap.PixelHeight  
        Image(Source=bitmap,Stretch=Stretch.Fill,Width=w,Height=h) 
    let loadImage path =
        #if SILVERLIGHT
        let stream = Application.GetResourceStream(new Uri(path, UriKind.Relative)).Stream
        let image = BitmapImage()
        image.SetSource(stream)
        #else
        let image = BitmapImage(Uri(path, UriKind.Relative))
        #endif
        image |> toImage

type Facing = Left | Right


[<AutoOpen>]
module Levels =
    let rexs () = 16, [loadImage "RexLeft1.png";loadImage "RexLeft2.png"], [loadImage "RexRight1.png";loadImage "RexRight2.png"]
    let dimes () = 32, [loadImage "DimetradonLeft1.png";loadImage "DimetradonLeft2.png"], [loadImage "DimetradonRight1.png";loadImage "DimetradonRight2.png"]
    let hons () = 32, [loadImage "HornosaurusLeft1.png";loadImage "HornosaurusLeft2.png"], [loadImage "HornosaurusRight1.png";loadImage "HornosaurusRight2.png"]

//23456789ABCDEF
    let level3 = (224,176,Left),"
TTTTTTTTTTTTTTTT
T               
TT     TTT    TT
TTT       TTTTTT
TTTTTTTT       T
TTTTTTTTTTTTTT T
TTT       TTT  T
T     TT      TT
T TTTTTTT  TTTTT
T TTTTTTTTTTTTTT
T              T
TTTTTTTTTTTTTTTT", [(192,32), Left, dimes; (128,80), Right, rexs; (16,176), Right, hons]

    let level1 = (0,32,Right),"
CCCCCCCCCCCCCCCC
               C
C       B      C
CCC      BCCCCCC
CCCCC          C
CCCCC          C
CCCCCCCCC      C
C              C
C              C
C              C
               C
CCCCCCCCCCCCCCCC", [(12*16,3*16), Left, rexs; (11*16,6*16), Right, hons; (13*16,8*16), Left, hons; (12*16,10*16), Right, dimes]

    let level2 = (224,176,Left),"
BBBBBBBBBBBBBBBB
               B
BBBBBBB        B
BBBBBBBBBB     B
BBBBBBB        B
B              B
B              B
BBBBBBBB   BBBBB
B              B
B     BBBBBBBBBB
B               
BBBBBBBBBBBBBBBB", [(192,32), Left, rexs; (128,80), Right, rexs; (16,176), Right, hons; (9*16,7*16), Right, dimes]

type Keys (control:Control) =
    let mutable keysDown = Set.empty
    do  control.KeyDown.Add (fun e -> keysDown <- keysDown.Add e.Key)
    do  control.KeyUp.Add (fun e -> keysDown <- keysDown.Remove e.Key)
    member keys.IsKeyDown key = keysDown.Contains key

type GameControl() as control = 
    inherit UserControl(Width=256.0,Height=192.0+16.0)

    let uri = Uri("/FlintEastwood;component/GameControl.xaml", UriKind.Relative)
    do  Application.LoadComponent(control, uri)
    let grid = Grid()
    do  grid.RenderTransform <- ScaleTransform(ScaleX=2.0,ScaleY=2.0)
    do  control.Content <- grid

    let canvas = Canvas()
    do  canvas.Background <- SolidColorBrush (Color.FromArgb(255uy ,255uy, 201uy, 14uy))
    do  grid.Children.Add canvas

    let scoreText = TextBlock(Text="Score 0")
    do  grid.Children.Add scoreText

    let add (x:#UIElement) = canvas.Children.Add x
    let remove (x:#UIElement) = canvas.Children.Remove x |> ignore

    let fl1, fl2 = loadImage "FlintLeft1.png", loadImage "FlintLeft2.png"
    let fr1, fr2 = loadImage "FlintRight1.png", loadImage "FlintRight2.png"

    let keys = Keys control
    let mutable x = 224
    let mutable y = 176
    let mutable flint = fl1
    let mutable facing = Left
    let mutable stepCount = 0
    let mutable reloadCountdown = 0
    let mutable bullets = []
    let mutable score = 0
    let mutable count = 0

    let mutable rows = [||]

    let addImage name (x,y) =
        let image = loadImage name
        move(image,x*16,y*16)
        image |> add

    let drawLevel (level:string) =
        rows <- level.Split('\n')
        rows |> Array.iteri (fun y row ->
            row.ToCharArray() |> Array.iteri (fun x col ->
                match col with
                | 'T' -> addImage "Bush.png" (x,y)
                | 'C' -> addImage "Cactus.png" (x,y)
                | 'R' -> addImage "Rock.png" (x,y)
                | 'B' -> addImage "Boulder.png" (x,y)
                | _ -> ()
            )
        )

    let isBlocked (x',y') =
        if x' <0 || x' >= 256 then true
        else 
            let ox, oy = x'/16, y'/16
            let c = rows.[oy].PadRight(16,' ').[ox] 
            c = ' ' |> not

    let playMedia name =
        let me = MediaElement(AutoPlay=true)
        me.Source <- Uri(name, UriKind.Relative)
        add me
    let onFire () = playMedia "/Gun_Shot.mp3"

    let mutable dinosaurs = []

    let initLevel ((fx,fy,face),level:string,coords) =
        x <- fx
        y <- fy
        drawLevel level
        facing <- face
        flint <- face |> function Left -> fl1 | Right -> fr1
        add flint
        move(flint,x,y)
        dinosaurs <-
            coords |> List.map (fun ((x',y'),face,(images:unit -> int * Image list * Image list)) ->
                let width, lefts, rights = images ()
                let image = face |> function Left -> lefts.[0] | Right -> rights.[0]
                move(image,x',y')
                add image
                image, width, lefts, rights, (x',y'), face, 1
            )

    let dinosaurHit (x',y', width) =
        let index =
            bullets |> List.tryFindIndex (fun ((x,y,dx), shape) ->
                x' < x && (x'+width) > x && y' < y && (y'+16) > y
            )
        match index with
        | Some index ->
            let _, shape = bullets.[index]
            remove shape
            bullets <- bullets |> List.mapi (fun i v -> (i,v)) |> List.filter (fun (i,v) -> i <> index) |> List.map snd
            true
        | None -> false

    let flintHit () =
        dinosaurs |> List.exists (fun (image, width, lefts, rights, (x', y'), face, life) ->
            let a = ((x+2) > x' && (x+2) < x' + width) || ((x+14) > x' && (x+14) < (x'+width))
            let b = (y >= y' && y < (y'+16)) || ((y+16) > y') && (y+16) < (y'+16)
            a && b
        )

    let updateScore () =
        scoreText.Text <- sprintf "Score %d" score

    let updateDinosaurs () =
        dinosaurs |> List.map (fun (image, width, lefts, rights, (x', y'), face, life) ->
            remove image
            let images = face |> function Left -> lefts | Right -> rights
            let image = 
                let index = (count/5)% images.Length
                images.[index]
            let ox,dx = 
                match face with
                | Left -> 0, -1
                | Right -> width, 1
            let x' = x' + dx
            let face =
                if isBlocked ((x'+ox),y') then
                    match face with
                    | Left -> Right
                    | Right -> Left
                else face
            move(image,x',y')
            let isHit = dinosaurHit (x',y',width)
            if isHit then score <- score + 100
            updateScore ()
            let life = if isHit then life - 1 else life
            if life > 0 then add image
            image, width, lefts, rights, (x',y'), face, life
        )
        |> List.filter (fun (image, width, lefts, rights, (x', y'), face, life) -> life > 0)

    let updateBullets () =
        bullets |> List.map (fun ((x,y,dx), shape) ->
            move(shape,x,y)
            (x+dx,y,dx), shape
        )

    let isDead = Event<_>()

    let update () =
        dinosaurs <- updateDinosaurs ()
        count <- count + 1

        let dir, dx, ox = 
            if keys.IsKeyDown Key.X then Right, 1, 12
            elif keys.IsKeyDown Key.Z then Left,-1, 2
            else facing, 0, 0
        if (isBlocked (x+ox,y+2) || isBlocked (x+ox,y+15)) |> not then x <- x + dx
        stepCount <- stepCount + dx
        facing <- dir
        remove flint
        flint <- 
            match facing, (abs stepCount/5)%2 with 
            | Left, 0 -> fl1
            | Left, 1 -> fl2
            | Right, 0 -> fr2
            | Right, 1 -> fr1
            | _, _ -> flint
        add flint
        if dx = 0 then
            let dy, oy = 
                if keys.IsKeyDown Key.Q then -1, 0
                elif keys.IsKeyDown Key.A then 1, 16
                else 0, 0
            stepCount <- stepCount + dy
            if (isBlocked (x+2,y+oy) || isBlocked (x+14,y+oy)) |> not then y <- y + dy
        move (flint,x,y)
        if reloadCountdown = 0 && keys.IsKeyDown Key.Space then
            onFire ()
            let ox,dx = 
                match facing with
                | Left -> 0, -3
                | Right -> 12, 3
            let pos, shape = (x+ox,y+9,dx), Rectangle(Width=4.0,Height=1.0, Fill = SolidColorBrush Colors.Black)
            add shape
            bullets <- (pos,shape) :: bullets 
            reloadCountdown <- 12
        if reloadCountdown > 0 then reloadCountdown <- reloadCountdown - 1
        bullets <- updateBullets ()
        if flintHit() then isDead.Trigger([||])

    let createMessage text =
        let t = TextBlock(Text=text)
        t.HorizontalAlignment <- HorizontalAlignment.Center
        t.VerticalAlignment <- VerticalAlignment.Center
        t

    let rec loop () =  async {
        score <- 0
        updateScore()
        initLevel level2
        
        let t = createMessage "Click to Start"
        grid.Children.Add t
        do! control.MouseLeftButtonDown |> Async.AwaitEvent |> Async.Ignore
        grid.Children.Remove t |> ignore

        let disposable = run (1.0/25.0) update
        do! isDead.Publish |> Async.AwaitEvent |> Async.Ignore
        disposable.Dispose()

        let t = createMessage "Game Over"
        grid.Children.Add t
        do! Async.Sleep 2500
        grid.Children.Remove t |> ignore
        canvas.Children.Clear()
        return! loop ()
        }
    do loop () |> Async.StartImmediate