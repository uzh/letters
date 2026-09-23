val run_with_starttls
  :  hostname:'a Domain_name.t
  -> ?port:int
  -> domain:Colombe.Domain.t
  -> ?authentication:Sendmail.authentication
  -> tls_authenticator:X509.Authenticator.t
  -> from:Colombe.Reverse_path.t
  -> recipients:Colombe.Forward_path.t list
  -> mail:Mrmime.Mt.buffer Mrmime.Mt.stream
  -> unit
  -> (unit, Sendmail_with_starttls.error) Lwt_result.t

val run
  :  hostname:'a Domain_name.t
  -> ?port:int
  -> domain:Colombe.Domain.t
  -> ?authentication:Sendmail.authentication
  -> tls_authenticator:X509.Authenticator.t
  -> from:Colombe.Reverse_path.t
  -> recipients:Colombe.Forward_path.t list
  -> mail:Mrmime.Mt.buffer Mrmime.Mt.stream
  -> unit
  -> (unit, Sendmail.error) Lwt_result.t

(** @deprecated Use {!run_with_starttls} with a trailing [unit] argument. *)
val run_with_starttls_legacy
  :  hostname:'a Domain_name.t
  -> ?port:int
  -> domain:Colombe.Domain.t
  -> ?authentication:Sendmail.authentication
  -> tls_authenticator:X509.Authenticator.t
  -> from:Colombe.Reverse_path.t
  -> recipients:Colombe.Forward_path.t list
  -> mail:Mrmime.Mt.buffer Mrmime.Mt.stream
  -> (unit, Sendmail_with_starttls.error) Lwt_result.t
[@@deprecated "Use run_with_starttls with a trailing unit argument"]
[@@migrate
  { repl =
      (fun ~hostname
        ?port
        ~domain
        ?authentication
        ~tls_authenticator
        ~from
        ~recipients
        ~mail ->
        run_with_starttls
          ~hostname
          ?port
          ~domain
          ?authentication
          ~tls_authenticator
          ~from
          ~recipients
          ~mail
          ())
  }]

(** @deprecated Use {!run} with a trailing [unit] argument. *)
val run_legacy
  :  hostname:'a Domain_name.t
  -> ?port:int
  -> domain:Colombe.Domain.t
  -> ?authentication:Sendmail.authentication
  -> tls_authenticator:X509.Authenticator.t
  -> from:Colombe.Reverse_path.t
  -> recipients:Colombe.Forward_path.t list
  -> mail:Mrmime.Mt.buffer Mrmime.Mt.stream
  -> (unit, Sendmail.error) Lwt_result.t
[@@deprecated "Use run with a trailing unit argument"]
[@@migrate
  { repl =
      (fun ~hostname
        ?port
        ~domain
        ?authentication
        ~tls_authenticator
        ~from
        ~recipients
        ~mail ->
        run
          ~hostname
          ?port
          ~domain
          ?authentication
          ~tls_authenticator
          ~from
          ~recipients
          ~mail
          ())
  }]
