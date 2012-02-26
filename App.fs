namespace FlintEastwood

open System.Windows
open System.Windows.Controls

type App() as this = 
    inherit Application()
    do  this.Startup.AddHandler(fun o e -> 
            if this.IsRunningOutOfBrowser then
                this.MainWindow.Width <- 512.0
                this.MainWindow.Height <- 416.0
            this.RootVisual <- GameControl()
        )
