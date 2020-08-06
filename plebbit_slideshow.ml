open Core
open Lwt
open Cohttp
open Cohttp_lwt_unix
let (let+) x f = Lwt.(>>=) x f 

let current_version = "0.0.1"
let default_config_dir = "~/.config/plebbit_slideshow/"
let default_save_dir = "~/Pictures/"
let default_auth_pass_entry = "web/reddit.com"
let default_client_pass_entry = "web/client-reddit.com"
let default_change_bg_command = "gsettings set org.gnome.desktop.background  picture-uri  \"file://%s\""


let home_tilde = Fpath.v "~"
let home_path = Fpath.v (Sys.home_directory ())

module Database = struct
  type file_info = { path: string; url: string option; hash: int; date: float; } [@@deriving bin_io, show]

  module Filetbl = struct
    include Hashtbl.Make_binable (Int)
    let pp f fmt vl =
      Hashtbl.to_alist vl |>
      (fun ls ->
         Format.pp_print_string fmt "{";
         Format.pp_open_hvbox fmt 2;
         Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.pp_print_string fmt ";"; Format.pp_print_space fmt ())
           (fun fmt (key, vl) ->
              Format.pp_open_hbox fmt ();
              Format.pp_print_string fmt "#"; Format.pp_print_int fmt key;
              Format.pp_print_string fmt "=";
              f fmt vl;
              Format.pp_close_box fmt ();
           ) fmt ls;
         Format.pp_close_box fmt ();
         Format.pp_print_string fmt "}"      )
  end

  type t = {
    file_map: file_info list Filetbl.t; (** maps image hashes to lists of metadata *)
  } [@@deriving bin_io, show]

  let init () = {file_map= Filetbl.create ()}

  let add_file ?date ?(avoid_duplicates=false) ?url db ~hash ~path  =
    let date = match date with
      | Some date -> date
      | None ->
        let now  = Time.now () in
        let time_since_epoch = Time.to_span_since_epoch now |> Time.Span.to_sec in
        time_since_epoch in
    let inserted = ref `Inserted in
    Hashtbl.update db.file_map hash ~f:(function
        | None ->
          inserted := `Inserted; [{hash; path; url; date}]
        (* when avoiding duplicates, do not insert in case of hash colission *)
        | Some prior_list when avoid_duplicates ->
          inserted := `Duplicate; prior_list
        | Some prior_list when
            List.exists prior_list ~f:(fun info ->
                String.(info.path = path) || (match info.url, url with
                    | Some url, Some url' -> String.(url = url') 
                    | _ -> false
                  )) ->
          inserted := `UrlDuplicate;
          prior_list
        | Some prior_list ->
          inserted := `Inserted;
          {hash; path; url; date} :: prior_list
      );
    !inserted

  let update_db ?remove_older_than ?(only_remove_downloaded=false) db =
    let removed_files = ref [] in
    let current_time = Time.now () in
    Hashtbl.filter_map_inplace db.file_map ~f:(fun files ->
        let files = List.filter files ~f:(fun file_info ->
            let not_expired () =
              if Option.is_none file_info.url && only_remove_downloaded then true
              else match remove_older_than with
                  None -> true
                | Some max_age ->
                  let insertion_date = file_info.date |> Time.Span.of_sec |> Time.of_span_since_epoch in
                  let age = Time.diff current_time insertion_date in
                  Time.Span.(age < max_age) in
            match Sys.file_exists ~follow_symlinks:true file_info.path with
            | `Yes -> true && (not_expired ())
            | `Unknown ->
              removed_files := file_info.path :: !removed_files;
              false
            |`No ->
              removed_files := file_info.path :: !removed_files;
              false
          ) in
        match files with
        | [] -> None
        | _ -> Some files
      );
    !removed_files

  (** selects a random image from the database  *)
  let random_image db =
    match Hashtbl.data db.file_map |> List.concat |> List.random_element with
    | None -> raise (Failure "Can not select image from empty database.")
    | Some file -> file.path

  (** write db to a file *)
  let write_to_file file db =
    try
      Unix.with_file ~mode:Unix.[O_RDWR ; O_CREAT; O_TRUNC] file ~f:(fun fd ->
          let size = bin_size_t db in
          let buf = Bigstring.map_file ~shared:true fd size in
          let write_result = bin_write_t buf ~pos:0 db in
          Int.(write_result = size)
        )
    with
    | Unix.Unix_error (err, _, _) ->
      raise (Failure (Printf.sprintf "When reading file %s - %s" file (Unix.Error.message err)))

  (** read db from file *)
  let read_from_file file =
    try
      Unix.with_file ~mode:Unix.[O_RDONLY] file ~f:(fun fd ->
          let fstat = Unix.fstat fd in
          let size = fstat.st_size in
          let pos_ref = ref 0 in
          let buf = Bigstring.map_file ~shared:false fd (Int.of_int64_exn size) in
          let read_result = bin_read_t ~pos_ref buf in
          read_result
        )
    with
    | Unix.Unix_error (err, _, _) ->
      raise (Failure (Printf.sprintf "When reading file %s - %s" file (Unix.Error.message err)))

end

(** construct a path from a user string  *)
let path_of_str ~name str =
  match Fpath.of_string str with
  | Ok path -> path
  | Error `Msg reason -> raise (Failure (Printf.sprintf "Bad %s path - %s" name reason))

(** expands paths to be relative to root  *)
let expand_path path =
  if Fpath.is_prefix home_tilde path
  then begin
    let segments = Fpath.segs path |> fun ls -> List.drop ls 1 in
    List.fold ~init:home_path segments ~f:(Fpath.(/))
  end else path

(** retrieves a username & password from pass
    assumes first line contains the password,
    and one of the subsequent lines is of the form "login: <username>" *)
let from_pass name () =
  let (let*) x f = Option.bind x ~f in
  let* password, username =
    Unix.open_process_in (Printf.sprintf "pass %s" name)
    |> In_channel.input_all
    |> String.split_lines
    |> List.filter ~f:(fun v -> not @@ String.is_empty v)
    |> (function pass :: rest -> Some (pass, rest) | _ -> None)
    |> (function None -> None
               | Some (pass, rest) ->
                 let* rest = List.map ~f:(fun s -> s |> String.split ~on:':' |> List.map ~f:String.strip) rest
                             |> List.filter_map ~f:(function h :: t -> Some (h,String.concat ~sep:":" t) | _ -> None)
                             |> (fun ls -> List.Assoc.find ~equal:String.equal ls "login") in
                 Some (pass, rest)) in
  Some (username, password)


(** ensures directory exists
    @param dir the directory specified by the user
    @param name the name of the directory (for use in error messages)
    @raises Failure if we do not have read permissions for the directory
    @raises Failure if file exists, but is not a directory
*)
let ensure_dir ?(name="Config") dir =
  let invalid_read_permissions () =
    raise (Failure
             (Printf.sprintf "Bad %s Directory - Do not have read permissions for %s" name dir)) in
  (* Fpath.exists_ext *)
  let build_dir () = Unix.mkdir_p dir in
  match Sys.file_exists ~follow_symlinks:true dir with
  | `No -> build_dir ()
  | `Unknown -> invalid_read_permissions ()
  | `Yes ->
    match Sys.is_directory ~follow_symlinks:true dir with
    | `Unknown -> invalid_read_permissions ()
    | `No -> raise (Failure
                      (Printf.sprintf
                         "Bad %s Directory - Specified path %s already exists and is not a directory"
                         name dir))
    | `Yes -> ()

(** reads file without failing if does not exist
    @param file the directory specified by the user
    @param name the name of the directory (for use in error messages)
    @raises Failure if we do not have read permissions for the directory
    @raises Failure if file exists, but is not a directory
*)
let ensure_read_file ~name file =
  let invalid_read_permissions () =
    raise (Failure
             (Printf.sprintf "Bad %s Path - Do not have read permissions for %s" name file)) in
  (* Fpath.exists_ext *)
  match Sys.file_exists ~follow_symlinks:true file with
  | `No -> ""
  | `Unknown -> invalid_read_permissions ()
  | `Yes ->
    match Sys.is_file ~follow_symlinks:true file with
    | `Unknown -> invalid_read_permissions ()
    | `No -> raise (Failure
                      (Printf.sprintf
                         "Bad %s Path - Specified path %s already exists and is not a file"
                         name file))
    | `Yes -> In_channel.read_all file


(** ensures config directory exists and is setup correctly
    @param config_dir config directory specified by user *)
let ensure_config_dir config_dir = ensure_dir ~name:"Config" config_dir

(** determines whether a given path is to a supported image file  *)
let is_image_file path =
  match Fpath.get_ext path with
  | ".png" | ".jpg" | ".gif" | ".gifv" | ".webp" | ".jpeg" -> true
  | _ -> false

(** builds a thread safe counter (not sure about if OCaml has a atomic
   int equivalent, so going for the safe route) *)
let build_safe_counter total () =
  let counter_mutex = Lwt_mutex.create () in
  let counter = ref 0 in
  let incr_counter () =
    Lwt_mutex.with_lock counter_mutex (fun () -> counter := !counter + 1;
                                        Lwt.return (Printf.sprintf "[%d/%d]" !counter total) ) in
  incr_counter  

(** loads tracking database from configuration directory  *)
let load_database_from_config_dir config_dir =
  let db_file = Fpath.(config_dir / "database.binio") |> Fpath.to_string in
  let invalid_read_permissions () =
    raise (Failure
             (Printf.sprintf "Bad Config Dir - not have read permissions for %s" db_file)) in
  match Sys.file_exists ~follow_symlinks:true db_file with
  | `Unknown -> invalid_read_permissions ()
  | `No -> Database.init ()
  | `Yes -> Database.read_from_file db_file

(** saves tracking database to configuration directory *)
let save_database_to_config_dir config_dir db =
  let db_file = Fpath.(config_dir / "database.binio") |> Fpath.to_string in
  Database.write_to_file db_file db

(** wraps a value behind a thread-safe getter  *)
let thread_safe_wrapper value =
  let mutex = Lwt_mutex.create () in
  let with_mutex (type a) (f: _ -> a t) : a t = Lwt_mutex.with_lock mutex (fun () -> f value) in
  with_mutex

let retrieve_change_bg_command config_dir =
  let config_file = Fpath.(config_dir / "change_cmd.txt") in
  let config_file_name = Fpath.to_string config_file in
  let invalid_read_permissions () =
    raise (Failure
             (Printf.sprintf "Bad Config Dir - not have read permissions for %s" config_file_name)) in
  match Sys.file_exists ~follow_symlinks:true config_file_name with
  | `Unknown -> invalid_read_permissions ()
  | `Yes -> In_channel.read_all config_file_name
  | `No -> default_change_bg_command

let format_change_bg_command_with_file ~cmd ~file =
  Stringext.replace_all cmd ~pattern:"%s" ~with_:file

let download_subreddits ?(sort=`Hot) ?(count=20)
    ?(avoid_duplicates=false)
    ?(config_dir = default_config_dir)
    ?(save_dir = default_save_dir)
    ?(client_username) ?(client_password) ?(reddit_username) ?(reddit_password)
    ~(show_all:bool) ~(auth_from_pass:bool) ?(auth_pass_entry = default_auth_pass_entry)
    ~(client_from_pass:bool) ?(client_pass_entry="web/client-reddit.com")
    ~(verbose: bool) ~(allow_nsfw:bool)
    (subreddits: string list) : unit =
  let config_dir = path_of_str ~name:"config" config_dir |> expand_path in
  let save_dir = path_of_str ~name:"save" save_dir |> expand_path in
  ensure_config_dir (config_dir |> Fpath.to_string);
  ensure_dir ~name:"Save" (save_dir |> Fpath.to_string);
  let with_db = load_database_from_config_dir config_dir |> thread_safe_wrapper in
  let subreddits =
    let subreddits = 
      match subreddits with
      | [] -> ensure_read_file ~name:"Config file"
                (Fpath.(config_dir / "subreddits.txt") |> Fpath.to_string)
              |> String.split_lines |> List.map ~f:String.strip |> List.map ~f:String.lowercase
      | _ -> subreddits in
    let subreddits = List.map subreddits ~f:(fun subreddit ->
        if String.(String.prefix subreddit 2 = "r/")
        then String.drop_prefix subreddit 2
        else if  String.(String.prefix subreddit 3 = "/r/")
        then String.drop_prefix subreddit 3
        else subreddit ) in
    if Int.(List.length subreddits = 0) then raise (Failure "No subreddits provided.");
    subreddits in
  let retrieve_from_components ~name ~env_user_name ~env_pass_name ~username ~password ~pass_entry =
    match client_from_pass, username, password,
          Lazy.from_fun (fun () -> Sys.getenv env_user_name),
          Lazy.from_fun (fun () -> Sys.getenv env_pass_name) with
    | _, Some username, Some password, _, _  -> username, password
    | _, _, _, lazy (Some username), lazy (Some password) -> username, password
    | true, _, _, _, _ | _, None, None, lazy None, lazy None ->
      begin match from_pass pass_entry () with
        | None -> raise (Failure (Printf.sprintf "Could not load %s details from password entry." name))
        | Some vl -> vl
      end
    | _ -> raise (Failure (Printf.sprintf "Could not retrieve reddit %s details." name)) in
  let client_username, client_password =
    retrieve_from_components
      ~name:"client"
      ~env_user_name:"REDDIT_CLIENT_ID" ~env_pass_name:"REDDIT_CLIENT_SECRET"
      ~username:client_username ~password:client_password ~pass_entry:client_pass_entry in
  let reddit_details =
    try
      if auth_from_pass then
        Some (retrieve_from_components ~name:"user"
                ~env_user_name:"REDDIT_USER_NAME" ~env_pass_name:"REDDIT_USER_PASSWORD"
                ~username:reddit_username ~password:reddit_password ~pass_entry:auth_pass_entry)
      else
        None
    with Failure _ -> None in
  let authentication_context = match reddit_details with
    | None ->
      Reddit.API.build_unauth_context ~client_username ~client_pass:client_password ()
    | Some (reddit_username, reddit_password) ->
      let+ ctx = Reddit.API.build_auth_context
          ~reddit_username ~reddit_password
          ~client_username ~client_pass:client_password () in
      let ctx = Reddit.API.downcast_context ctx in
      Lwt.return (module (val ctx) : Reddit.API.PUBLIC_CONTEXT) in
  let get_image_urls =
    let+ ctx = authentication_context in
    let safe_load_subreddit subreddit =
      let module Context = (val ctx : Reddit.API.PUBLIC_CONTEXT) in
      let module RedditAPI = Reddit.API.Unauthenticated (Context) in
      let+ results = Lwt.catch
          (fun () -> RedditAPI.Subreddit.get_multi ~verbose ~show:show_all ~sort subreddit count)
          (function
              Failure err_msg ->
              let+ () =
                if verbose then Lwt_io.eprintf "Err: Could not load subreddit %s - reason: %s\n" subreddit err_msg
                else Lwt.return_unit in
              Lwt.return []
            | e ->
              let+ () =
                if verbose then
                  Lwt_io.eprintf "Err: Could not load subreddit %s - reason: %s\n" subreddit (Exn.to_string e)
                else Lwt.return_unit in
              Lwt.return [] ) in
      let results = if not allow_nsfw then List.filter results ~f:(fun link -> not link.over_18) else results in
      let results = List.map ~f:(fun link -> link.url) results in
      let+ () = if verbose
        then Lwt_io.printf "Info: Retrieved %d entries from %s\n" (List.length results) subreddit
        else Lwt.return_unit  in
      Lwt.return results in
    let+ results = Lwt_list.map_p safe_load_subreddit subreddits in
    let results = List.concat results in
    Lwt.return results in
  let download_urls urls =
    let incr_counter = build_safe_counter (List.length urls) () in
    let download_url i_url =
      let url = Uri.of_string i_url in
      match Uri.path url |> Fpath.of_string with
      | Error _ ->
        let+ () = if verbose then Lwt_io.eprintf "Error: invalid url %s\n" i_url else Lwt.return_unit in
        Lwt.return_unit
      | Ok path when not @@ is_image_file path ->
        let+ () = if verbose
          then Lwt_io.printf "Info: Found invalid image url %s\n" (Fpath.to_string path)
          else Lwt.return_unit in
        Lwt.return_unit
      | Ok path ->
        let file = Fpath.basename path in
        let save_path = Fpath.(save_dir / file) |> Fpath.to_string in
        let+ (resp, body) = Client.get url in
        let is_success = Response.status resp |> Code.code_of_status |> Code.is_success in
        match is_success with
        | false ->
          let+ () = if verbose
            then Lwt_io.eprintf "Error: Could not download image %s due to error %s\n"
                (Fpath.to_string path)
                (Response.status resp |> Code.string_of_status)
            else Lwt.return_unit in
          Lwt.return_unit
        | true ->
          let+ data = Cohttp_lwt.Body.to_string body in
          let+ should_save = with_db (fun db ->
              let hash = String.hash data in
              let url = url |> Uri.to_string in
              let path = save_path in
              Lwt.return @@ match Database.add_file ~avoid_duplicates ~url db ~hash ~path with
              | `Inserted -> true
              | `Duplicate -> false
              | `UrlDuplicate -> false
            ) in
          let+ success =
            if should_save
            then try
                Lwt_io.with_file ~mode:Lwt_io.Output save_path (fun out_channel ->
                    let+ () = Lwt_io.write out_channel data in
                    let+ () = Lwt_io.flush out_channel in
                    Lwt.return_true
                  )
              with
              | _ ->
                if verbose
                then begin
                  let+ () =
                    Lwt_io.eprintf "Err: Could not save image %s to %s\n" (Fpath.to_string path) save_path in
                  Lwt.return_false
                end
                else Lwt.return_false
            else Lwt.return_false
          in
          if verbose then begin
            let+ proportion = incr_counter () in
            let+ () =
              if success
              then
                Lwt_io.printf "Info: %s Downloaded %s to %s\n" proportion (Fpath.to_string path) save_path
              else
                Lwt_io.printf "Info: %s Did NOT download %s to %s\n" proportion (Fpath.to_string path) save_path
            in
            Lwt.return_unit 
          end else Lwt.return_unit in
    let+ () = Lwt_list.iter_p download_url urls in
    let+ _ = with_db (fun db -> Lwt.return @@ save_database_to_config_dir config_dir db ) in
    if verbose then
      Lwt_io.printf "Info: Donwloaded all urls and updated local database\n"
    else Lwt.return_unit in
  let main_command =
    let+ urls = get_image_urls in
    let+ () = download_urls urls in
    Lwt.return_unit 
  in
  Lwt_main.run main_command

let import_images ?avoid_duplicates ?(verbose=false) ?(config_dir=default_config_dir) (import_dir: string list) =
  let config_dir = Filename.realpath config_dir in
  let config_dir = path_of_str ~name:"config" config_dir |> expand_path in
  let import_dir =
    List.map ~f:Filename.realpath import_dir
    |> List.map ~f:(fun path -> path_of_str ~name:"import" path |> expand_path) in
  let check_valid_file file_name =
    match Sys.file_exists ~follow_symlinks:true file_name with
    | `Unknown ->
      if verbose
      then Printf.printf "Info: Ignoring image file %s as insufficient read \
                          permissions\n" file_name;
      None
    |`Yes -> Some [file_name]
    |`No ->
      if verbose
      then Printf.printf "Info: Ignoring image file %s as it is \
                          not a file\n" file_name;
      None in
  (** converts path to a list of images files *)
  let file_to_images path =
    let file_name = (Fpath.to_string path) in
    match is_image_file path with
    | true ->                   (* is an image file - check if exists *)
      check_valid_file file_name
    | false ->                  (* is not an image file - check if directory *)
      match Sys.is_directory ~follow_symlinks:true file_name with
      | `Unknown ->
        if verbose
        then Printf.printf "Info: Ignoring image directory %s as insufficient read \
                            permissions\n" file_name;
        None
      |`Yes -> Some (Sys.ls_dir file_name
                     |> List.filter_map ~f:(fun dir_name ->
                         let full_path = Fpath.(path / dir_name) in
                         let full_path_name = Fpath.to_string full_path in
                         if is_image_file full_path && Option.is_some (check_valid_file full_path_name)
                         then Some full_path_name
                         else None))
      |`No ->
        if verbose
        then Printf.printf "Info: Ignoring import option %s as it is \
                            not a directory nor has a supported \
                            image extension\n" file_name;
        None in
  let images = List.filter_map ~f:file_to_images import_dir |> List.concat in
  let image_to_hash_data image = In_channel.read_all image |> String.hash in
  let db = load_database_from_config_dir config_dir in
  let insert_image_to_db path =
    let hash = image_to_hash_data path in
    match Database.add_file ?avoid_duplicates db ~path ~hash with
    | `Inserted -> ()
    | `Duplicate ->
      if verbose
      then Printf.printf "Info: Did not insert image %s due to hash colission\n" path
      else ()
    |`UrlDuplicate ->
      if verbose
      then Printf.printf "Info: Did not insert image %s due to url colission\n" path
      else () in
  (** insert each one of the images into the db *)
  List.iter images ~f:insert_image_to_db;
  (** save the database back into the configuration directory  *)
  if save_database_to_config_dir config_dir db
  then begin
    if verbose then Printf.printf "Info: Succesfully updated database.\n";
  end
  else raise (Failure "Failed to write changes to database")
  
(** changes the background to a random image stored in the database  *)
let change_bg ?(verbose=false) ?(config_dir=default_config_dir) () =
  let config_dir = Filename.realpath config_dir in
  let config_dir = path_of_str ~name:"config" config_dir |> expand_path in
  let command =
    let cmd = retrieve_change_bg_command config_dir in
    let db = load_database_from_config_dir config_dir in
    let file = Database.random_image db in
    if verbose then Printf.printf "Info: Setting background to image %s\n" file;
    format_change_bg_command_with_file ~cmd ~file in
  if verbose then Printf.printf "Info: Executing command %s\n" command;
  let process_output = Unix.open_process_in command |> In_channel.input_all in
  if verbose then Printf.printf "Info: Executing command outputs %s" process_output;
  ()

let update_db
    ?(verbose=false)
    ?remove_older_than ?(only_remove_downloaded_pictures=false)
    ?(config_dir=default_config_dir) () =
  let config_dir = Filename.realpath config_dir in
  let config_dir = path_of_str ~name:"config" config_dir |> expand_path in
  ensure_config_dir (config_dir |> Fpath.to_string);
  let db = load_database_from_config_dir config_dir in
  let removed_files =
    Database.update_db ?remove_older_than
      ~only_remove_downloaded:only_remove_downloaded_pictures db in
  if verbose
  then begin
    List.iter removed_files ~f:(fun file_name ->
        Printf.printf "Info: Removed missing file %s from database.\n" file_name
      )
  end;
  if save_database_to_config_dir config_dir db
  then begin
    if verbose then Printf.printf "Info: Succesfully updated database.\n";
  end
  else raise (Failure "Failed to write changes to database");
  ()

let run_with_pp_err main () =
  try
    main ();
  with
  | Failure err_msg ->
    Printf.eprintf "Error: %s\n" err_msg;
    exit (-1)
  | e ->
    let e_str = Exn.to_string e in
    Printf.eprintf "Uncaught Error: %s\n" e_str;
    exit (-1)

let sort_of_string str =
  let fail () = raise (Failure (Printf.sprintf "invalid sort specification %s" str)) in
  let period_of_string str = match str with
    | "all" -> `All    | "day" -> `Day | "hour" -> `Hour | "month" -> `Month
    | "week" -> `Week | "year" -> `Year | _ -> fail () in
  match str |> String.strip |> String.split ~on:':' |> List.map ~f:String.strip
        |> List.map ~f:String.lowercase with
  | ["hot"] ->  `Hot
  | ["new"] ->  `New
  | ["random"] ->  `Random
  | ["rising"] ->  `Rising
  | ["controversial"; range] ->  `Controversial (period_of_string range)
  | ["top"; range] ->  `Top (period_of_string range)
  | _ -> fail ()


let () =
  let download_command =
    let open Command.Let_syntax in
    Command.basic
      ~summary:"Download images from Reddit to a local folder."
      [%map_open
        let config_dir = flag
            ~doc:"Directory where configuration of plebbit-slideshow \
                  is stored (defaults to ~/.config/plebbit_slideshow/)."
            "config-directory" (optional string)
        and verbose = flag
            ~doc:"Print verbose outputs."
            "verbose" no_arg
        and save_dir = flag
            ~doc:"Directory where images should be saved (defaults to ~/Pictures/)."
            "save-directory" (optional string)
        and allow_nsfw = flag
            ~doc:"Allow NSFW images?."
            "allow-nsfw" no_arg
        and show_all = flag
            ~doc:"Show hidden posts."
            "show-hidden" no_arg
        and sort = flag
            ~doc:"Sorting strategy (one of hot, new, random, rising, \
                  or (top/controversial with a time period (all, day, \
                  month, hour, year, week) separated by a colon - i.e \
                  all:top))"
            "sort" (optional (Command.Param.Arg_type.create sort_of_string))
        and count = flag
            ~doc:"Number of posts per subreddit"
            "count" (optional int)
        and auth_from_pass =
          flag ~doc:"Authenticate with Reddit using credentials from pass."
            "auth-from-pass" no_arg
        and avoid_duplicates =
          flag ~doc:"Over-approximately avoid duplicate records."
            "avoid-duplicates" no_arg
        and auth_pass_entry =
          flag ~doc:"NAME Name of pass entry containing authentication \
                     details (expects first line to be password, and a \
                     subsequent line starting with \"login:\" that \
                     contains the login credentials - defaults to \
                     web/reddit.com)."
            "auth-pass-entry" (optional string)
        and client_from_pass =
          flag ~doc:"Retrieve client credentials from pass."
            "client-from-pass" no_arg
        and client_pass_entry =
          flag ~doc:"NAME Name of pass entry containing client \
                     credentials (expects first line to be secret, and \
                     a subsequent line starting with login: that \
                     contains the client id - defaults to \
                     web/client-reddit.com)."
            "client-pass-entry" (optional string)
        and client_username =
          flag ~doc:"CLIENT-ID Provide Client ID from Reddit Developer API directly through CLI interface"
            "client-id" (optional string)
        and client_password =
          flag ~doc:"CLIENT-SECRET Provide Client secret from Reddit Developer API directly through CLI interface"
            "client-secret" (optional string)
        and reddit_username =
          flag ~doc:"USERNAME Provide Reddit Username directly through CLI interface"
            "reddit-username" (optional string)
        and reddit_password =
          flag ~doc:"PASSWORD Provide Reddit Password directly through CLI interface"
            "reddit-password" (optional string)
        and subreddits = anon (sequence ("subreddit" %: string)) in
        run_with_pp_err @@
        fun () ->
        download_subreddits ~avoid_duplicates
          ?count ~show_all
          ?config_dir ?save_dir ?sort
          ~verbose ~allow_nsfw
          ~client_from_pass ?client_pass_entry
          ~auth_from_pass ?auth_pass_entry
          ?client_username ?client_password
          ?reddit_username ?reddit_password
          subreddits
      ] in
  let update_db_command =
    let open Command.Let_syntax in
    Command.basic
      ~summary:"Update stored database to accurately track pictures."
      [%map_open
        let config_dir = flag
            ~doc:"DIRECTORY Directory where configuration of plebbit-slideshow \
                  is stored (defaults to ~/.config/plebbit_slideshow/)."
            "config-directory" (optional string)
        and remove_older_than =
          flag ~doc:"Maximum age of pictures to keep (expressed in \
                     format ([0-9]d)?([0-9]h)?([0-9]m)?([0-9]s)?)"
            "max-age" (optional (Command.Param.Arg_type.create Time.Span.of_string))
        and only_remove_downloaded_pictures =
          flag ~doc:"Relates to the max-age parameter - if this flag \
                     is provided, then only images that were \
                     downloaded from an external source will be \
                     elligible for expiration." "only-downloaded" no_arg in
        run_with_pp_err @@
        fun () ->
        update_db ?remove_older_than ~only_remove_downloaded_pictures  ?config_dir () ] in
  let import_command =
    let open Command.Let_syntax in
    Command.basic
      ~summary:"Import saved images into database."
      [%map_open
        let config_dir = flag
            ~doc:"DIRECTORY Directory where configuration of plebbit-slideshow \
                  is stored (defaults to ~/.config/plebbit_slideshow/)."
            "config-directory" (optional string)
        and avoid_duplicates =
          flag ~doc:"Over-approximately avoid duplicate records."
            "avoid-duplicates" no_arg
        and verbose =
          flag ~doc:"Print verbose output."
            "verbose" no_arg
        and import_dir = anon (sequence ("dir-or-img" %: string)) in
        run_with_pp_err @@
        fun () ->
        import_images  ~avoid_duplicates ~verbose ?config_dir import_dir
      ] in
  let change_bg_command =
    let open Command.Let_syntax in
    Command.basic
      ~summary:"Change background image using images in database."
      [%map_open
        let config_dir = flag
            ~doc:"DIRECTORY Directory where configuration of plebbit-slideshow \
                  is stored (defaults to ~/.config/plebbit_slideshow/)."
            "config-directory" (optional string)
        and verbose =
          flag ~doc:"Print verbose output."
            "verbose" no_arg in
        run_with_pp_err @@
        fun () ->
        change_bg ~verbose ?config_dir ()
      ] in
  let main_command =
    Command.group
      ~preserve_subcommand_order:()
      ~summary:"Script to automatically set desktop backgrounds from Reddit images"
      [
        "download", download_command;
        "import", import_command;
        "change-bg", change_bg_command;
        "update-db", update_db_command;
      ] in
  Command.run
    ~version:current_version
    ~verbose_on_parse_error:(match Sys.getenv "DEBUG" with Some _ ->true|None->false)
    main_command
