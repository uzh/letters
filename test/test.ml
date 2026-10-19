open Letters

let stream_to_string s =
  let b = Buffer.create 4096 in
  let rec go () =
    match s () with
    | Some (buf, off, len) ->
      Buffer.add_substring b buf off len;
      go ()
    | None -> Buffer.contents b
  in
  go ()
;;

let test_create_plain_text_email _ () =
  let recipients = [ To "dave@example.com" ] in
  let subject = "Hello" in
  let body = Plain "Hello Dave" in
  let mail = create_email ~from:"harry@example.com" ~recipients ~subject ~body () in
  let stream =
    match mail with
    | Ok mail -> Mrmime.Mt.to_stream mail
    | Error reason -> failwith reason
  in
  let message = stream_to_string stream in
  Lwt.return (print_string message)
;;

let test_create_html_email _ () =
  let recipients = [ To "dave@example.com" ] in
  let subject = "Hello" in
  let body = Html "<i>Hello Dave</i>" in
  let mail = create_email ~from:"harry@example.com" ~recipients ~subject ~body () in
  let stream =
    match mail with
    | Ok mail -> Mrmime.Mt.to_stream mail
    | Error reason -> failwith reason
  in
  let message = stream_to_string stream in
  Lwt.return (print_string message)
;;

let test_create_mixed_body_email _ () =
  let recipients = [ To "dave@example.com" ] in
  let subject = "Hello" in
  let body = Mixed ("Hello Dave", "<i>Hello Dave</i>", Some "blaablaa") in
  let mail = create_email ~from:"harry@example.com" ~recipients ~subject ~body () in
  let stream =
    match mail with
    | Ok mail -> Mrmime.Mt.to_stream mail
    | Error reason -> failwith reason
  in
  let message = stream_to_string stream in
  Lwt.return (print_string message)
;;

let test_create_config () =
  (* check for config compatibility *)
  let username, password, hostname, with_starttls =
    "SomeUser", "password", "localhost", true
  in
  let[@warning "-3"] (_ : Config.t) =
    Config.make ~username ~password ~hostname ~with_starttls
  in
  let (_ : Config.t) = Config.create ~username ~password ~hostname ~with_starttls () in
  let (_ : Config.t) =
    Config.create
      ~mechanism:Sendmail.LOGIN
      ~username
      ~password
      ~hostname
      ~with_starttls
      ()
  in
  ()
;;

let open_fds () =
  if Sys.file_exists "/proc/self/fd"
  then Some (Array.length (Sys.readdir "/proc/self/fd"))
  else None
;;

let free_local_port () =
  let socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.bind socket (Unix.ADDR_INET (Unix.inet_addr_loopback, 0));
  let port =
    match Unix.getsockname socket with
    | Unix.ADDR_INET (_, port) -> port
    | Unix.ADDR_UNIX _ -> assert false
  in
  Unix.close socket;
  port
;;

let failing_starttls_sends ?timeout ~hostname ~port () =
  let config =
    Config.create ~username:"" ~password:"" ~hostname ~with_starttls:true ()
    |> Config.set_port (Some port)
  in
  let message =
    match
      create_email
        ~from:"harry@example.com"
        ~recipients:[ To "dave@example.com" ]
        ~subject:"Hello"
        ~body:(Plain "Hello Dave")
        ()
    with
    | Ok message -> message
    | Error reason -> failwith reason
  in
  let send () =
    let send () =
      send
        ~config
        ~sender:"harry@example.com"
        ~recipients:[ To "dave@example.com" ]
        ~message
    in
    match timeout with
    | Some timeout -> Lwt_unix.with_timeout timeout send
    | None -> send ()
  in
  let rec go n =
    if n = 0
    then Lwt.return ()
    else
      Lwt.bind
        (Lwt.catch
           (fun () -> Lwt.map (fun () -> true) (send ()))
           (fun _ -> Lwt.return false))
        (fun sent ->
           if sent then Alcotest.fail "sending email should fail" else go (n - 1))
  in
  go 50
;;

let check_no_fd_leak sends =
  match open_fds () with
  | None -> Lwt.return (Alcotest.skip ())
  | Some before ->
    Lwt.bind (sends ()) (fun () ->
      Alcotest.(check (option int)) "open file descriptors" (Some before) (open_fds ());
      Lwt.return ())
;;

let test_starttls_connection_refused_does_not_leak _ () =
  let port = free_local_port () in
  check_no_fd_leak (failing_starttls_sends ~hostname:"localhost" ~port)
;;

let test_starttls_connect_timeout_does_not_leak _ () =
  (* Needs a hostname, since [Config] rejects IP addresses; nip.io resolves it to the
     unroutable 10.255.255.1 *)
  let hostname = "10.255.255.1.nip.io" in
  Lwt.bind
    (Lwt.catch
       (fun () ->
          Lwt.map
            (fun res -> Array.to_list res.Lwt_unix.h_addr_list)
            (Lwt_unix.gethostbyname hostname))
       (fun _ -> Lwt.return []))
    (function
      | addr :: _ when Unix.string_of_inet_addr addr = "10.255.255.1" ->
        check_no_fd_leak (failing_starttls_sends ~timeout:0.2 ~hostname ~port:25)
      | _ -> Lwt.return (Alcotest.skip ()))
;;

let () =
  let () =
    Alcotest.(
      run
        ~and_exit:false
        "model"
        [ "config", [ test_case "create config" `Quick test_create_config ] ])
  in
  Lwt_main.run
    (Alcotest_lwt.run
       "Email creation"
       [ ( "Generating body"
         , [ Alcotest_lwt.test_case
               "email with plain text body"
               `Quick
               test_create_plain_text_email
           ; Alcotest_lwt.test_case
               "email with HTML text body"
               `Quick
               test_create_html_email
           ; Alcotest_lwt.test_case
               "email with mixed plain text and HTML body"
               `Quick
               test_create_mixed_body_email
           ] )
       ; ( "Sending"
         , [ Alcotest_lwt.test_case
               "STARTTLS connection refused does not leak file descriptors"
               `Quick
               test_starttls_connection_refused_does_not_leak
           ; Alcotest_lwt.test_case
               "STARTTLS connect timeout does not leak file descriptors"
               `Slow
               test_starttls_connect_timeout_does_not_leak
           ] )
       ])
;;
