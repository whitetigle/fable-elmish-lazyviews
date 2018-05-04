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

    [<StringEnum>]
    type Tooltip =
      | Top
      | Bottom
      | Left
      | Right

    let TooltipClass ccss=
      function
      | Top -> ccss (Tooltip.ClassName ++ Tooltip.IsTooltipTop)
      | Bottom -> ccss (Tooltip.ClassName ++ Tooltip.IsTooltipBottom)
      | Right -> ccss (Tooltip.ClassName ++ Tooltip.IsTooltipRight)
      | Left -> ccss (Tooltip.ClassName ++ Tooltip.IsTooltipLeft)


    let ExistingList list1 list2 = list1 @ list2
    let ReactList element = [element]

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

    module Tile =
      let WithBox size elements =  Tile.parent [ Tile.Size size ]  [ Tile.child [] [ Box elements] ]
      let Ancestor elements =  Tile.ancestor[] elements
      let ParentWithSize size elements =  Tile.parent [ Tile.Size size ]  [ Tile.child [] elements ]
      let Parent elements =  Tile.parent []  [ Tile.child [] elements ]
      let Vertical size elements =  Tile.parent [ Tile.Size size; Tile.IsVertical ]  [ Tile.child [] elements ]
      let Child size elements =  Tile.child [Tile.Size size ] elements

    let H1Text text = Heading.h1 [] [ str text ]
    let H2Text text = Heading.h2 [] [ str text ]
    let H3Text text = Heading.h3 [] [ str text ]
    let H4Text text = Heading.h4 [] [ str text ]
    let H5Text text = Heading.h5 [] [ str text ]
    let H6Text text = Heading.h6 [] [ str text ]
    let H1ColoredText color text = Heading.h1 [ Heading.Props[ Style [ Color color ] ] ] [ str text ]
    let H2ColoredText color text = Heading.h2 [ Heading.Props[ Style [ Color color ] ]] [ str text ]
    let H3ColoredText color text = Heading.h3 [ Heading.Props[ Style [ Color color ] ]] [ str text ]
    let H4ColoredText color text = Heading.h4 [ Heading.Props[ Style [ Color color ] ]] [ str text ]
    let H5ColoredText color text = Heading.h5 [ Heading.Props[ Style [ Color color ] ]] [ str text ]
    let H6ColoredText color text = Heading.h6 [ Heading.Props[ Style [ Color color ] ]] [ str text ]
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


    module Control =
      let Div elements = Control.div [] elements
      let DivIconLeft elements = Control.div [ Control.HasIconLeft] elements
      let DivIconRight elements = Control.div [ Control.HasIconRight] elements

    module Form =
      let form elements = form [] elements
      let Label elements = Fulma.Label.label [] elements

    module Field =
      let Div elements = Field.div [] elements

    module Level =
      let Parent elements = Level.level [] elements
      let Left elements = Level.left [] elements
      let Item elements = Level.item [] elements
      let ItemCentered elements = Level.item [ Level.Item.HasTextCentered ] elements
      let Right elements = Level.right [] elements

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
      let SmallWithTooltip  tip (tipkind:Tooltip) color action icon title =
        let props = Button.Props [ OnClick action; Tooltip.dataTooltip tip ]
        Button.button [ Button.Color color; Button.Size IsSmall; props; TooltipClass Button.CustomClass tipkind ] [ Icon icon; Span title ]

    let SwitchWithColor color isChecked action text =
      Switch.switch [ Switch.Checked isChecked; Switch.Color color; Switch.IsRounded;  Switch.Props [OnClick action] ] [ str text ]
    let ButtonInfoSmall action icon title =  Button.button [ Button.Color IsInfo; Button.Size IsSmall; Button.Props[ OnClick action] ] [ Icon icon; Span title ]
    let Section elements = Section.section [] elements

    module Container =

      let Standard elements = Container.container [ ] elements
      let Fluid elements = Container.container[ Container.IsFluid ] elements
      let VCenteredFluid elements = Container.container [ Container.IsFluid; Container.CustomClass Bulma.Properties.Alignment.HasTextCentered ] elements
      let VCentered elements = Container.container [ Container.CustomClass Bulma.Properties.Alignment.HasTextCentered ] elements

    [<RequireQualifiedAccess>]
    module Navbar =
      let ItemWithClick action elements = Navbar.Item.div [  Navbar.Item.Props[ OnClick action ] ] elements
      let Item elements = Navbar.Item.div [] elements
      let End elements = Navbar.End.div [] elements
      let Menu elements = Navbar.menu [] elements
      let Brand elements = Navbar.Brand.div [] elements
      let ItemAnchor page elements = Navbar.Item.div [Navbar.Item.Props [ Href page] ] elements
      let Bar color elements = Navbar.navbar [  Navbar.IsFixedTop; Navbar.Color color; Navbar.Props [ Style [ Height "1.5em" ]] ] elements
      let DropdownStart isActive elements =
        match isActive with
        | true -> Navbar.Item.a [ Navbar.Item.HasDropdown;  Navbar.Item.IsActive true] elements
        | false -> Navbar.Item.a [ Navbar.Item.HasDropdown] elements

      let link action elements = Navbar.Link.a [ Navbar.Link.Props[ OnClick action ] ] elements
      let DropdownDiv elements = Navbar.Dropdown.div [] elements
      let Divider = Navbar.divider [] []

    let Img width src =  img [ Style [ Width width ]; Src src ]
    let Tag color text = Tag.tag [ Tag.Color color ] [ str text ]

    let NotificationWithClick color action elements =
      Notification.notification
        [
          Notification.Color color
          Notification.Props [OnClick action]
        ] elements

    let Notification color elements = Notification.notification [ Notification.Color color ] elements

    module Message =
      let Box color elements = Message.message [ Message.Color color ] elements
      let Header elements = Message.header [ ] elements
      let Body elements = Message.body [ ] elements

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

      let FullHeightAndEscapeNavbar color elements =
        let color = color |> getColor
        Hero.hero [ Hero.Props[ClassName <| sprintf "add-sticky-header is-fullheight %s" color] ] elements

  let confirmBox active title actionlabel action closeAction =
    Modal.modal [ Modal.IsActive active]
        [ Modal.background [ Props [ OnClick closeAction ] ] [ ]
          Modal.Card.card [ ]
            [ Modal.Card.head [ ]
                [ Modal.Card.title [ ]
                    [ str title ]
                  Delete.delete [ Delete.OnClick closeAction ] [ ] ]
              Modal.Card.foot [ ]
                [ Button.button [ Button.Props [ OnClick action];Button.Color IsSuccess ]
                    [ str actionlabel ]
                  Button.button [  Button.Props [ OnClick closeAction ] ]
                    [ str "Annuler" ] ] ] ]

  let tilesContainer tiles =
    tiles
      |> Inside.Container.Fluid
      |> Inside.ReactList
      |> Inside.Section
      |> Inside.ReactList
      |> Inside.Hero.FullHeightAndEscapeNavbar IsWarning

  let prepareTile data onClick =
    let props =
      Tile.Props[
        OnClick onClick
      ]

    Tile.parent [props]  [
      Tile.child [ ] [
        Box.box' [ Props [ Style [ Height "100%" ] ] ]
             data
        ]
      ]

  let prepareActionMenu name addAction backAction =
    Tile.parent [ Tile.Size Tile.Is3 ]  [
      Tile.child [] [
        Box.box' [ Props [ Style [ Height "100%" ] ] ] [
          name |> Inside.H5ColoredText "black"
          Columns.columns [] [
//                  Column.column [ Column.Width.isOneThird ] [ p.logo ]
            Column.column [ Column.Width (Column.All, Column.IsHalf) ] [
                Button.button [
                    Button.Color IsSuccess
                    Button.Props[ OnClick addAction ]
                  ]
                  [
                    Icon.faIcon [ ] [ Fa.icon Fa.I.Plus ]
                    span [] [ str "Nouveau"]
                  ]
            ]
            Column.column [Column.Width (Column.All, Column.IsHalf)] [
                Button.button
                  [
                    Button.Color IsInfo
                    Button.Props[ OnClick backAction ]
                  ]
                  [
                    span [] [ str "Retour"]
                  ]
                ]
              ]
            ]
          ]
        ]

  let prepareForm inputs preview =
      [
        Hero.body [] [
          Columns.columns [] [
            Column.column [ Column.Width (Column.All, Column.IsHalf) ] [
              Box.box'[] [
                div [ ] inputs
            ]]
            Column.column[] [
                Card.card [] [
                  Card.content [] [
                    Columns.columns [] [
                      Column.column [ Column.Width (Column.All, Column.IsTwoThirds)] preview
                    ]
                  ]
                ]
            ]
          ]
        ]
      ] |> Inside.Hero.FullHeightAndEscapeNavbar IsLight

  let createField tabIndex label (currentValue:string) (errors:string list) (icon:Fa.I.FontAwesomeIcons option) onChange=
    let hasErrors = errors.Length >0
    let hasSomething = currentValue.Length > 0
    let isSuccess = not hasErrors && hasSomething
    let isFirstField = tabIndex <= 1.

    let props =
      if isFirstField then
        [
            //Input.Type TypeIsText
//            Input.defaultValue currentValue
            Input.Type Input.IInputType.Text
            Input.Placeholder "?"
            Input.Value currentValue
            Input.Props[
              TabIndex tabIndex
              OnChange (fun e -> onChange !!e.target?value )
              AutoFocus true
            ]
        ]
      else
        [
            //Input.typeIsText
//            Input.defaultValue currentValue
            Input.Type Input.IInputType.Text
            Input.Placeholder "?"
            Input.Value currentValue
            Input.Props[
              TabIndex tabIndex
              OnChange (fun e -> onChange !!e.target?value )
            ]
        ]

    let props = props @ [(if hasErrors then Input.Color IsDanger elif hasSomething then Input.Color IsSuccess else Input.Color IsSuccess)]

    let field =
      [
        Input.input props
        (if hasErrors then Icon.faIcon [ Icon.Size IsSmall; Icon.IsRight ] [ Fa.icon Fa.I.Warning ]
          elif isSuccess then Icon.faIcon [ Icon.Size IsSmall; Icon.IsRight  ] [ Fa.icon Fa.I.Check ] else str "")
        (if icon.IsSome then Icon.faIcon [ Icon.Size IsSmall; Icon.IsRight  ] [ Fa.icon icon.Value ] else str "")
        (if hasErrors then
          let firstError = errors.Head
          Help.help [ Help.Color IsDanger ][ str firstError ]
          else
            str "")
      ]

    let controlProps =
      if icon.IsSome then
        [Control.HasIconLeft; Control.HasIconRight]
      else
        [Control.HasIconLeft; Control.HasIconRight]

    //let controlProps = if isSuccess || hasErrors then controlProps @ [Control.HasIconRight] else controlProps
    Field.div []
      [
        Label.label [][ str label ]
        Control.div controlProps field
      ]

  (*
  let createImageFileInput id label onChange=

    let props = [
      File.Props[
        OnChange (fun e ->
          let t : HTMLInputElement = e.target :?> HTMLInputElement
          let list : FileList = t.files
          let file = list.Item(0)
          let name = file.name
          let reader = FileReader
          let r =reader.Create()
          r.onload <- (fun _ -> onChange(name,!!r.result) )
          r.readAsDataURL(file)
          )
      ]
    ]

    Field.div [ ]
        [
          Form.File.file
            [
              //Form.File.IsActive
              Form.File.Size IsMedium
              Form.File.IsBoxed
              Form.File.Props [ Id id]
            ]
            [ Form.File.label [ ]
                [ Form.File.input !!props
                  Form.File.cta [ ]
                    [ Form.File.icon [ ]
                        [ Icon.faIcon [ ] [ Fa.icon Fa.I.Upload ] ]
                      Form.File.label [ ]
                        [ str "Logo ?" ] ]
        ] ] ]
*)
  let HeroVCentered color elements =
    Hero.hero [
        Hero.Color color
        Hero.IsFullHeight ] [
          Hero.body [ ] [
            Container.container [ Container.IsFluid; Container.CustomClass Bulma.Properties.Alignment.HasTextCentered ]
              elements
            ]
          ]

  module List =

    let ActionMenu addAction cancelAction actionLabel =
        prepareActionMenu
          (string actionLabel)
          addAction
          cancelAction
