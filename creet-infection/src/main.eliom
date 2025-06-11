(* main.eliom *)

open Eliom_content.Html.D

let () =
  Eliom_registration.Html.register
    ~service:Eliom_service.Service.create
    (fun () ->
      let title = "Creet Infection" in
      let body =
        div [class_ "game-container"] [
          h1 [class_ "game-title"] [pcdata title];
          div [id "game-area"] [];
          div [id "game-status"] [];
        ]
      in
      let html_content = html (head (title (pcdata title)) []) (body body) in
      Lwt.return html_content
    )