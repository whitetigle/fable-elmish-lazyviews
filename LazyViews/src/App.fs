module LazyViews

open Elmish
open Elmish.React
open Global

module Types =
  type Order =
    Asc | Desc

  type Model =
    {
      Records: (Doc list) option
      ErrorMessage: string option
      Checks: Map<string,bool>
      DateOrder:Order
      CtOrder:Order
    }
    static member Empty =
        {
          Records=None
          ErrorMessage = None
          Checks= Map []
          DateOrder=Asc
          CtOrder=Asc
        }


  type LoadIndex = int
  type Msg =
    | Select of Doc
    | DoQuery
    | LoadList
    | DoReset
    | DoFailedQuery
    | Print of Doc
    | Edit of Doc
    | Duplicate of Doc
    | ToggleCheck of string
    | OrderDate
    | OrderClient


module View =
  open Types
  open Fable.Helpers.React
  open Fable.Helpers.React.Props

  open Fulma
  open Fulma.FontAwesome
  open Fulma.Extensions

  open Util.React.Inside.Helpers
  module Inside = Util.React.Inside

  let error id msg =
    printfn "%s Please check #%s" msg id
    //failwith <| sprintf "%s Please check certificate #%s" msg id

  let root (model:Model) dispatch =

    let ToColumn color elements =
      elements
      |> Inside.Column
      => Inside.Columns
      => Inside.Notification color
      => Inside.Table.Column

    let prepareChoice (data:Doc) =

        let ident =
          match data.Ident with
          | Some spec ->
            sprintf "Id : %s" spec
            |> Inside.Span
            => ToColumn IsLight

          | None ->
            sprintf "Id : NA"
            |> Inside.Span
            => ToColumn IsLight

        let modele =
          match data.Model with
          | Some m ->
            m
            |> Inside.Span
            => ToColumn IsLight

          | None ->
            "OMG!!" |> error data.Id
            span [] []

        let ct =
          match data.Cl with
          | Some c ->

            //sprintf "%s à %s" c.Name c.City
            c.Name
            |> Inside.Span
            => ToColumn IsDark

          | None ->
            "OMG!!" |> error data.Id
            span [] []


        let date =
          let date =
            match data.Dte with
            | Some local ->
              let t = System.TimeSpan(2,0,0)
              let d = local.Subtract( t )
              let minutes =
                if d.Minute < 10 then
                  sprintf "0%i" d.Minute
                else
                  sprintf "%i" d.Minute

              let hours =
                if d.Hour < 10 then
                  sprintf "0%i" d.Hour
                else
                  sprintf "%i" d.Hour

              sprintf "le %i/%i/%i à %s:%s" d.Day d.Month d.Year hours minutes
            | None -> "" //"A certificate was saved without any date!!" |> error data.Id;


          date
          |> Inside.Span
          => ToColumn IsInfo

        let duplicate =
          "DoSomething"
          |> Inside.Button.Small IsInfo (fun _ -> Duplicate data |> dispatch) Fa.I.Copy
          => Inside.Column

        let edit =
          "DoSomething"
          |> Inside.Button.Small IsWarning (fun _ -> Edit data |> dispatch) Fa.I.Pencil
          => Inside.Column

        let buttons =
          [duplicate;edit]
          |> Inside.Columns
          => Inside.Notification IsLight
          => Inside.Table.Column

        let action =
          let pdfName =
            data
            |> Doc.getPDFName
            |> sprintf "%s.pdf"

          Checkradio.checkbox
            [ Checkradio.Props [ TabIndex 2.]
              Checkradio.Checked (model.Checks |> Map.exists(fun k v -> System.String.Equals(k, data.Number, System.StringComparison.CurrentCultureIgnoreCase)))
              Checkradio.Color IsWhite
              Checkradio.Props [ ClassName "is-checkradio" ]
              Checkradio.OnChange (fun _ -> (ToggleCheck data.Number ) |> dispatch) ]
            [ str pdfName ]
          => ToColumn IsLight

//        [ action; modele; numSerie; ident; ct; date; buttons ]
        [ action; modele; ct; ident; date; buttons ]
        |> Inside.Table.Row

(*
    let  fakeRecords =
      [ for i in 0..100 do
        yield Certificate.Fake()  ] |> Seq.toList
*)

    let error =
      function
      | Some err ->
        err
        |> Inside.Str
        => Inside.Notification IsDanger

      | None -> str ""

//    Util.React.HeroVCentered IsSuccess
    match model.Records with
    | Some records ->

      let button =
        let label =
          " Do Smething"
          |> Inside.Span

        let icon =
          Inside.Icon Fa.I.Print

        [icon;label]
        |> Inside.Heading.h6
        => Inside.Notification IsSuccess
        => Inside.BoxWithClick (fun _ -> printfn "click")
        => Inside.Column

      let dateOrder =
        let label =
          " Order by Date"
          |> Inside.Span

        let icon =
          Inside.Icon Fa.I.CalendarCheckO

        [icon;label]
        |> Inside.Heading.h6
        => Inside.Notification IsInfo
        => Inside.BoxWithClick (fun _ -> OrderDate |> dispatch)
        => Inside.Column


      let ctOrder =
        let label =
          " Order by Ct"
          |> Inside.Span

        let icon =
          Inside.Icon Fa.I.UserCircleO

        [icon;label]
        |> Inside.Heading.h6
        => Inside.Notification IsPrimary
        => Inside.BoxWithClick (fun _ -> OrderClient |> dispatch)
        => Inside.Column

      let buttons =
        [button;ctOrder;dateOrder]
        |> Inside.Columns

      [
        "OMG It's awesome!" |> Inside.H3ColoredText "black"

        buttons

        records
        |> List.map  ( prepareChoice)
        |> Inside.Table.TBody
        => Inside.Table.FullWidth []
      ]
      |> Inside.Container.Fluid
      => Inside.Hero.FullHeightAndEscapeNavbar IsLight

    | None ->

      [
        "OMG It's awesome!" |> Inside.H3ColoredText "white"
        model.ErrorMessage |> error
      ]
      |> Inside.Notification IsSuccess
      => Inside.Container.Standard
      => Inside.Hero.FullHeightAndEscapeNavbar IsLight


module State =

  open Types

  let init () : Model * Cmd<Msg> =
    Model.Empty ,Cmd.ofMsg LoadList

  let update msg (model:Model) : Model * Cmd<Msg> =
    match msg with
    | OrderClient ->
      match model.CtOrder with
      | Asc ->
        match model.Records with
        | Some records ->
          let sorted =
            records
            |> List.sortWith( fun x1 x2 ->
              let ct1 =
                match x1.Cl with
                | Some ct -> ct.Name.Trim()
                | None -> ""

              let ct2 =
                match x2.Cl with
                | Some ct -> ct.Name.Trim()
                | None -> ""

              System.String.Compare(ct1, ct2, true))

          { model with Records = Some sorted; CtOrder=Desc}
          ,Cmd.none

        | None ->
          model
          ,Cmd.none

      | Desc ->
        match model.Records with
        | Some records ->
          let sorted =
            records
            |> List.sortWith( fun x1 x2 ->
              let ct1 =
                match x1.Cl with
                | Some ct -> ct.Name.Trim()
                | None -> ""

              let ct2 =
                match x2.Cl with
                | Some ct -> ct.Name.Trim()
                | None -> ""

              -System.String.Compare(ct1, ct2, true))

          { model with Records = Some sorted; CtOrder=Asc}
          ,Cmd.none

        | None ->
          model
          ,Cmd.none

    | OrderDate ->
      match model.DateOrder with
      | Asc ->
        match model.Records with
        | Some records ->
          let sorted =
            records
            |> List.sortWith( fun x1 x2 ->
              let date1 =
                match x1.Dte with
                | Some date -> date
                | None -> System.DateTime.Now

              let date2 =
                match x2.Dte with
                | Some date -> date
                | None -> System.DateTime.Now

              System.DateTime.Compare(date1, date2))

          { model with Records = Some sorted; DateOrder=Desc}
          ,Cmd.none

        | None ->
          model
          ,Cmd.none

      | Desc ->
        match model.Records with
        | Some records ->
          let sorted =
            records
            |> List.sortWith( fun x1 x2 ->
              let date1 =
                match x1.Dte with
                | Some date -> date
                | None -> System.DateTime.Now

              let date2 =
                match x2.Dte with
                | Some date -> date
                | None -> System.DateTime.Now

              -System.DateTime.Compare(date1, date2))

          { model with Records = Some sorted; DateOrder=Asc}
          ,Cmd.none

        | None ->
          model
          ,Cmd.none

    | ToggleCheck number ->

        let updated =

          if model.Checks |> Map.exists( fun k _ -> k = number ) then
            model.Checks |> Map.remove number
          else
            model.Checks |> Map.add number true

        {model with Checks = updated }
        ,Cmd.none

    | LoadList ->

      let records = 
        [ 
          for i in 0..500 do 
            yield Doc.Fake()
        ]
      {model with Records= Some records}, Cmd.none


let init() =
    Program.mkProgram State.init State.update View.root
    |> Program.withReact"elmish-app"
    |> Program.run

init()