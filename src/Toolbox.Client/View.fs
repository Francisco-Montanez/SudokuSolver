module Toolbox.Client.View

open Feliz
open Router
open Elmish
open Feliz.UseElmish
open Feliz.DaisyUI

type private Msg =
    | UrlChanged of Page

type private State = {
    Page : Page
}

let private init () =
    let nextPage = Router.currentPath() |> Page.parseFromUrlSegments
    { Page = nextPage }, Cmd.navigatePage nextPage

let private update (msg:Msg) (state:State) : State * Cmd<Msg> =
    match msg with
    | UrlChanged page -> { state with Page = page }, Cmd.none

[<ReactComponent>]
let AppView () =
    let state,dispatch = React.useElmish(init, update)
    let navigation =
        Daisy.navbar [
            prop.className "mb-2 shadow-lg bg-neutral text-neutral-content rounded-box"
            prop.children [
                Daisy.navbarStart [
                    Daisy.dropdown [
                        dropdown.hover
                        prop.children [
                            Daisy.button.button [ button.ghost; prop.text "tools" ]
                            Daisy.dropdownContent [
                                prop.className "p-2 shadow menu bg-base-100 rounded-box w-52"
                                prop.tabIndex 0
                                prop.children [
                                    Html.li [Html.a("About", Page.About)]
                                    Html.li [Html.a("Index", Page.Index)]
                                    Html.li [Html.a("Sudoku Solver", Page.SudokuSolver)]
                                ]
                            ]
                        ]
                    ]
                ]
                Daisy.navbarCenter [Html.span "TOOLBOX"]
            ]
        ]
    let render =
        match state.Page with
        | Page.Index -> Pages.Index.IndexView ()
        | Page.About -> Html.text "SAFEr Template"
        | Page.SudokuSolver -> Pages.SudokuSolver.SudokuSolverView ()
    React.router [
        router.pathMode
        router.onUrlChanged (Page.parseFromUrlSegments >> UrlChanged >> dispatch)
        router.children [ navigation; render ]
    ]