(** This is the main file if you are using static linking without config file.
    It is not used if you are using a config file and ocsigenserver *)

module%shared H42n42 = H42n42

let%server _ =
  Ocsigen_server.start
    [ Ocsigen_server.host
        [Staticmod.run ~dir:"local/var/www/h42n42" (); Eliom.run ()] ]
