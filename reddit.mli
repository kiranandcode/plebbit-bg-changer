open Core
open Cohttp_lwt_unix

val handle_response : Response.t -> unit
val js_date_to_date : float -> Date.t

(** interface for Reddit data types that have been created *)
module type Created = sig
  (** type of data  *)
  type t
  (** accessor to retrieve creation date  *)
  val created_utc : t -> Date.t
end

(** interface for Reddit data types that are votable  *)
module type Votable  = sig
  (** type of data  *)
  type t

  (** accessor to retrieve the number of upvotes (includes own)  *)
  val ups: t -> int

  (** accessor to retrieve the number downvotes upvotes (includes own)  *)
  val downs: t -> int

  (** true if liked by user, false if disliked, None if user has not voted or not logged in  *)
  val likes: t -> bool option
end

module Account : sig

  type t = {
    comment_karma: int;                (** user's comment karma *)
    has_mail: bool option;             (** user has unread mail - null if not your account *)
    has_mod_mail: bool option;         (** user has unread mod mail - null if not your account *)
    has_verified_mail: bool option;    (** user has provided an email address and got it verified  *)
    id: string;                        (** id of account, prepend t2_ to get fullname  *)
    inbox_count: int option;           (** number of unread messages in inbox; not present if not your account  *)
    is_friend: bool option;            (** whether the logged in user has this user set as a friend  *)
    is_gold: bool;                     (** reddit gold status  *)
    is_mod: bool;                      (** whether this account moderates any subreddits  *)
    link_karma: int;                   (** user's link karma  *)
    modhash: string option;            (** current modhash, not present if not your account  *)
    name: string;                      (** username of account in question - not equal to fullname  *)
    over_18: bool;                     (** whether this account is set to be over 18  *)

    created_utc: Date.t;               (** (??) date of creation  *)
  }
  [@@deriving show, ord, eq]  

  val fullname : t -> string
  val created_utc : t -> Date.t
  val from_json_opt : Yojson.Basic.t -> t option
  val from_json : Yojson.Basic.t -> t
end
module Link :
sig
  type t = {
    author : string option;
    author_flair_css_class : string option;
    author_flair_text : string option;
    domain : string;
    hidden : bool;
    is_self : bool;
    link_flair_css_class : string option;
    link_flair_text : string option;
    locked : bool;
    media : unit;
    media_embed : unit;
    num_comments : int;
    over_18 : bool;
    permalink : Uri.t;
    saved : bool option;
    score : int;
    selftext : string option;
    selftext_html : string option;
    subreddit : string;
    subreddit_id : string;
    thumbnail : [ `Default | `Image | `Self | `Url of Uri.t ];
    title : string;
    url : string;
    edited : Date.t option;
    distinguished : [ `Admin | `Moderator | `Special ] option;
    stickied : bool;
    num_crossposts : int;
    hide_score : bool;
    quarantine : bool;
    is_reddit_media_domain : bool;
    is_meta : bool;
    is_video : bool;
    created_utc : Date.t;
    ups : int;
    downs : int;
    likes : bool option;
  }
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val created_utc : t -> Date.t
  val ups : t -> int
  val downs : t -> int
  val likes : t -> bool option
  val from_json_opt : Yojson.Basic.t -> t option
  val from_json : Yojson.Basic.t -> t
end
module Subreddit :
sig
  type t = {
    accounts_active : int option;
    accounts_active_is_fuzzed : bool;
    allow_discovery : bool;
    allow_galleries : bool;
    allow_images : bool;
    allow_polls : bool;
    allow_videogifs : bool;
    allow_videos : bool;
    comment_score_hide_mins : int;
    description : string;
    description_html : string;
    display_name : string;
    header_img : Uri.t option;
    header_size : (float * float) option;
    header_title : string option;
    over18 : bool;
    public_description : string;
    public_description_html : string;
    public_traffic : bool;
    subscribers : int;
    submission_type : [ `Any | `Link | `Self ];
    submission_link_label : string option;
    submit_text_label : string option;
    subreddit_type :
      [ `Archived | `GoldRestricted | `Private | `Public | `Restricted ];
    title : string;
    url : Uri.t;
    user_is_banned : bool option;
    user_is_contributor : bool option;
    user_is_moderator : bool option;
    user_is_subscriber : bool option;
  }
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val from_json_opt : Yojson.Basic.t -> t option
  val from_json : Yojson.Basic.t -> t
end
module Thing :
sig

  type 'a tag = Account : Account.t tag
    | Link : Link.t tag
    | Subreddit : Subreddit.t tag
  type 'a data = 'a tag * 'a
  val pp_data : 'b -> Format.formatter -> 'a data -> unit
  val compare_data : 'b -> 'a data -> 'a data -> int
  val equal_data : 'b -> 'a data -> 'a data -> bool

  type 'a t = { id : string; name : string; data : 'a data; }

  val pp :
    (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
  val show : (Format.formatter -> 'a -> unit) -> 'a t -> string
  val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
  val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  val from_json : 'a tag -> Yojson.Basic.t -> 'a t
  val from_json_opt : 'a tag -> Yojson.Basic.t -> 'a t option

  type 'a s = 'a t

  module Wrapped : sig
    type t = MkThing : 'a s -> t
    val from_json : Yojson.Basic.t -> t
    val from_json_opt : Yojson.Basic.t -> t option
  end
  
end

module API : sig

  val base_api : string

  (** context required to access public Reddit APIs *)
  module type PUBLIC_CONTEXT

  (** context required to access authenticated Reddit APIs *)
  module type AUTH_CONTEXT

  (** downcast the more secure authenticated context to the public context (for convenience primarily, the authenticated API has a superset of the interface of the unauthenticated API) *)
  val downcast_context : (module AUTH_CONTEXT) -> (module PUBLIC_CONTEXT)

  (** construct an unauthenticated API Context
      @param user_agent string to be used as user agent in request
      @param client_username id for client from reddit developer page
      @param client_pass secret for client from reddit developer page
  *)
  val build_unauth_context :
    ?user_agent:string ->
    client_username:string ->
    client_pass:string -> unit -> (module PUBLIC_CONTEXT) Lwt.t

  (** construct an Authenticated API Context  *)
  val build_auth_context :
    ?user_agent:string ->
    client_username:string ->
    client_pass:string ->
    reddit_password:string ->
    reddit_username:string -> unit -> (module AUTH_CONTEXT) Lwt.t

  (** Interface for Public Facing reddit Api  *)
  module type PUBLIC_API = sig
    module Subreddit :
    sig

      type period = [ `All | `Day | `Hour | `Month | `Week | `Year ]

      type sort =
        [ `Controversial of period
        | `Hot
        | `New
        | `Random
        | `Rising
        | `Top of period ]

      type anchor = [ `After of string | `Before of string ]

      type pagination_state

      val get :
        ?verbose:bool ->
        ?anchor:[< `After of string | `Before of string ] ->
        ?count:int ->
        ?limit:int ->
        ?show:bool ->
        sort:sort -> string -> (Link.t list * pagination_state) Lwt.t

      val get_more :
        ?verbose:bool ->
        ?limit:int ->
        ?show:bool ->
        pagination_state -> (Link.t list * pagination_state) Lwt.t

      val get_multi:
        ?verbose:bool ->
        ?show:bool -> sort:sort -> string -> int -> Link.t list Lwt.t

    end
  end

  (** Unauthenticated API Constructor  *)
  module Unauthenticated : functor (Context : PUBLIC_CONTEXT) -> PUBLIC_API

  (** Authenticated API Constructor  *)
  module Authenticated : functor (Context : AUTH_CONTEXT) -> sig
    include PUBLIC_API
    module Me : sig val get : Account.t Lwt.t end
  end
end
