namespace Util

open Fable.Core
open Fable.Core.JsInterop
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fulma.FontAwesome
open Fulma.BulmaClasses
open Fulma.Extensions
open Fable.Import
open Fulma

[<RequireQualifiedAccess>]
module React =


  module Inside =

    module Helpers =

      let inline (=>) (el: React.ReactElement) (el2: React.ReactElement list -> React.ReactElement) = [el] |> el2
      let inline (<=>) (el: React.ReactElement) (el2: React.ReactElement) = [el] @ [el2]

    module ColumnWithSize =
      let Is1 elements = Column.column [ Column.Width (Column.All, Column.Is1) ] elements
      let Is2 elements = Column.column [ Column.Width (Column.All, Column.Is2) ] elements
      let Is3 elements = Column.column [ Column.Width (Column.All, Column.Is3) ] elements
      let Is4 elements = Column.column [ Column.Width (Column.All, Column.Is4) ] elements
      let Is5 elements = Column.column [ Column.Width (Column.All, Column.Is5) ] elements
      let Is6 elements = Column.column [ Column.Width (Column.All, Column.Is6) ] elements
      let Is7 elements = Column.column [ Column.Width (Column.All, Column.Is7) ] elements
      let Is8 elements = Column.column [ Column.Width (Column.All, Column.Is8) ] elements
      let Is9 elements = Column.column [ Column.Width (Column.All, Column.Is9) ] elements
      let Is10 elements = Column.column [ Column.Width (Column.All, Column.Is10) ] elements
      let Is11 elements = Column.column [ Column.Width (Column.All, Column.Is11) ] elements
      let IsTwoThirds elements = Column.column [ Column.Width (Column.All, Column.IsTwoThirds) ] elements
      let IsHalf elements = Column.column [ Column.Width (Column.All, Column.IsHalf) ] elements
      let IsOneThird elements = Column.column [ Column.Width (Column.All, Column.IsOneThird) ] elements

    let Label (elements:React.ReactElement list) = Label.label [] elements
    let ColumnsWithClick action (elements:React.ReactElement list) = Columns.columns [ Columns.Props [OnClick action] ] elements
    let Columns (elements:React.ReactElement list) = Columns.columns [] elements
    let ColumnHalf elements = Column.column [ Column.Width (Column.All, Column.IsHalf) ] elements
    let Column34 elements = Column.column [ Column.Width (Column.All, Column.IsThreeQuarters) ] elements
    let ColumnWithClick action  elements = Column.column [ Column.Props [OnClick action] ] elements
    let Column elements = Column.column [] elements
    let BoxWithClick action elements = Box.box' [ Props[OnClick action] ] elements
    let Box elements = Box.box' [ Props [ Style [ Height "100%" ] ] ] elements

    let Icon name = Icon.faIcon [ ] [ Fa.icon name ]
    let Span text = span [] [ str text ]

    module Table =
      let Standard elements = Table.table [] elements
      let WithProps props elements = Table.table props  elements
      let Hoverable (props:Table.TableOption list) elements = elements |> WithProps ([ Table.IsHoverable ] @ props)
      let FullWidth (props:Table.TableOption list) elements = elements |> WithProps ([ Table.IsFullWidth ] @ props)
      let Striped (props:Table.TableOption list) elements = elements |> WithProps ([ Table.IsStriped ] @ props)
      let Narrow (props:Table.TableOption list) elements = elements |> WithProps ([ Table.IsNarrow ] @ props)
      let Thead elements = thead [] elements
      let TR elements = tr [] elements
      let Row elements = TR elements
      let TH elements = th [] elements
      let TD elements = td [] elements
      let HeadColumn elements = TH elements
      let Column elements = TD elements
      let TBody elements = tbody [] elements

      module Selected =
        let TR elements = tr [ ClassName "is-selected"] elements
        let Row elements = TR elements

    module Heading =
      let h1 elements = Heading.h1 [] elements
      let h2 elements = Heading.h2 [] elements
      let h3 elements = Heading.h3 [] elements
      let h4 elements = Heading.h4 [] elements
      let h5 elements = Heading.h5 [] elements
      let h6 elements = Heading.h6 [] elements

    module Button =

      module WithColor =
        let Create action icon title color =  Button.button [ Button.Color color; Button.Props[ OnClick action] ] [ Icon icon; Span title ]
        let Info action icon title = IsInfo |> Create action icon title
        let Warning action icon title = IsWarning |> Create action icon title
        let Success action icon title = IsSuccess |> Create action icon title
        let Danger action icon title = IsDanger |> Create action icon title
        let Light action icon title = IsLight |> Create action icon title
        let Primary action icon title = IsPrimary |> Create action icon title

      let Small color action icon title =  Button.button [ Button.Color color; Button.Size IsSmall; Button.Props[ OnClick action] ] [ Icon icon; Span title ]

    module Container =

      let Standard elements = Container.container [ ] elements
      let Fluid elements = Container.container[ Container.IsFluid ] elements
      let VCenteredFluid elements = Container.container [ Container.IsFluid; Container.CustomClass Bulma.Properties.Alignment.HasTextCentered ] elements
      let VCentered elements = Container.container [ Container.CustomClass Bulma.Properties.Alignment.HasTextCentered ] elements

    let Img width src =  img [ Style [ Width width ]; Src src ]
    let Tag color text = Tag.tag [ Tag.Color color ] [ str text ]

    let NotificationWithClick color action elements =
      Notification.notification
        [
          Notification.Color color
          Notification.Props [OnClick action]
        ] elements

    let Notification color elements = Notification.notification [ Notification.Color color ] elements

    let Str text = str text
    let P elements = p[] elements
    let UL elements = ul[] elements
    let LI elements = li [] elements
    let Div elements = div [] elements
    let Content elements = Content.content [] elements

    let getColor =
      function
      | IsWhite -> "is-white"
      | IsBlack -> "is-black"
      | IsLight -> "is-light"
      | IsDark -> "is-dark"
      | IsPrimary -> "is-primary"
      | IsLink -> "is-link"
      | IsInfo -> "is-info"
      | IsSuccess -> "is-success"
      | IsWarning -> "is-warning"
      | IsDanger -> "is-danger"

    module Hero =
      let FullHeight color elements = Hero.hero [ Hero.Color color; Hero.IsFullHeight ] elements
      let Body elements = Hero.body [] elements
      let Standard elements = Hero.hero [] elements
      let EscapeNavbar color elements =
        let color = color |> getColor
        Hero.hero [ Hero.Props[ClassName <| sprintf "add-sticky-header hero %s" color] ] elements
