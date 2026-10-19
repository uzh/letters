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

let open_fds () = Array.length (Sys.readdir "/proc/self/fd")

(* Number of sockets on this host in state SYN_SENT towards local [port] *)
let connecting_sockets port =
  let suffix = Printf.sprintf ":%04X" port in
  let ic = open_in "/proc/net/tcp" in
  let rec go acc =
    match input_line ic with
    | exception End_of_file -> acc
    | line ->
      (match String.split_on_char ' ' line |> List.filter (( <> ) "") with
       | _ :: _ :: remote :: "02" :: _ when String.ends_with ~suffix remote -> go (acc + 1)
       | _ -> go acc)
  in
  Fun.protect ~finally:(fun () -> close_in ic) (fun () -> go 0)
;;

let local_socket ?backlog () =
  let socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.bind socket (Unix.ADDR_INET (Unix.inet_addr_loopback, 0));
  Option.iter (Unix.listen socket) backlog;
  match Unix.getsockname socket with
  | Unix.ADDR_INET (_, port) -> socket, port
  | Unix.ADDR_UNIX _ -> assert false
;;

let free_local_port () =
  let socket, port = local_socket () in
  Unix.close socket;
  port
;;

(* Listens on a local port without ever accepting, and fills the accept queue. The
   kernel then drops further SYNs, so connecting to the port stays pending. Returns the
   port and the sockets to close afterwards. *)
let pending_connect_listener () =
  let listener, port = local_socket ~backlog:0 () in
  let rec fill sockets n =
    if n = 0
    then (
      List.iter Unix.close sockets;
      Alcotest.fail "could not fill the accept queue")
    else (
      let socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
      Unix.set_nonblock socket;
      (try Unix.connect socket (Unix.ADDR_INET (Unix.inet_addr_loopback, port)) with
       | Unix.Unix_error (Unix.EINPROGRESS, _, _) -> ());
      Lwt.bind (Lwt_unix.sleep 0.05) (fun () ->
        if connecting_sockets port > 0
        then (
          (* The queue is full, keep only the connections that made it in *)
          Unix.close socket;
          Lwt.return (port, sockets))
        else fill (socket :: sockets) (n - 1)))
  in
  fill [ listener ] 16
;;

let starttls_send ~hostname ~port () =
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
  send ~config ~sender:"harry@example.com" ~recipients:[ To "dave@example.com" ] ~message
;;

(* Runs [send] [n] times, each must fail with an error accepted by [expected]. Any other
   error means [send] failed before connecting, so the fd counts would not test
   anything. *)
let failing_sends ~expected send n =
  let rec go n =
    if n = 0
    then Lwt.return ()
    else
      Lwt.bind
        (Lwt.catch
           (fun () -> Lwt.map (fun () -> None) (send ()))
           (fun exn -> Lwt.return (Some exn)))
        (function
          | None -> Alcotest.fail "sending email should fail"
          | Some exn when expected exn -> go (n - 1)
          | Some exn -> Alcotest.failf "unexpected error: %s" (Printexc.to_string exn))
  in
  go n
;;

(* Cancels [send] only once it has a new socket connecting to [port], so after the DNS
   lookup and while the connection is being established. Counting relative to the
   start keeps sockets leaked by earlier sends from triggering the cancellation. *)
let cancel_while_connecting ~port send () =
  let connecting = connecting_sockets port in
  let sending = send () in
  let rec wait n =
    match Lwt.state sending with
    | Lwt.Return () | Lwt.Fail _ -> Lwt.return ()
    | Lwt.Sleep when connecting_sockets port > connecting ->
      Lwt.return (Lwt.cancel sending)
    | Lwt.Sleep when n = 0 -> Alcotest.fail "sending email never started connecting"
    | Lwt.Sleep -> Lwt.bind (Lwt_unix.sleep 0.005) (fun () -> wait (n - 1))
  in
  Lwt.bind (wait 1000) (fun () -> sending)
;;

let check_no_fd_leak sends =
  (* Warm up, so fds that stay open for the whole process (resolver, CA certs) are not
     counted as leaks *)
  Lwt.bind (sends 1) (fun () ->
    let before = open_fds () in
    Lwt.bind (sends 50) (fun () ->
      Alcotest.(check int) "open file descriptors" before (open_fds ());
      Lwt.return ()))
;;

let test_starttls_connection_refused_does_not_leak _ () =
  let port = free_local_port () in
  check_no_fd_leak
    (failing_sends
       ~expected:(function
         | Unix.Unix_error (err, _, _) -> err = Unix.ECONNREFUSED
         | _ -> false)
       (fun () -> Lwt_unix.with_timeout 5. (starttls_send ~hostname:"localhost" ~port)))
;;

let test_starttls_connect_cancelled_does_not_leak _ () =
  Lwt.bind (pending_connect_listener ()) (fun (port, sockets) ->
    Lwt.finalize
      (fun () ->
         check_no_fd_leak
           (failing_sends
              ~expected:(fun exn -> exn = Lwt.Canceled)
              (cancel_while_connecting ~port (starttls_send ~hostname:"localhost" ~port))))
      (fun () -> Lwt.return (List.iter Unix.close sockets)))
;;

(* The fd checks read /proc, so they only run where it is available (Linux) *)
let fd_leak_tests =
  if Sys.file_exists "/proc/self/fd" && Sys.file_exists "/proc/net/tcp"
  then
    [ Alcotest_lwt.test_case
        "STARTTLS connection refused does not leak file descriptors"
        `Quick
        test_starttls_connection_refused_does_not_leak
    ; Alcotest_lwt.test_case
        "STARTTLS connect cancelled does not leak file descriptors"
        `Quick
        test_starttls_connect_cancelled_does_not_leak
    ]
  else []
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
       ; "Sending", fd_leak_tests
       ])
;;
