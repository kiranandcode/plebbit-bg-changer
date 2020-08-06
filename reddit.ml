open Core
open Cohttp
open Cohttp_lwt_unix
let (let+) x f = Lwt.(>>=) x f 
let (let*) x f = Option.bind x ~f
let handle_response resp =
  let code = resp |> Response.status |> Code.code_of_status in
  if not (Code.is_success code)
  then raise (Failure (Printf.sprintf "auth-api request failed: error code %d" code))

let js_date_to_date date = date
                           |> Int.of_float |>  Time.Span.of_int_sec
                           |> Time.of_span_since_epoch
                           |> Time.to_date ~zone:Time.Zone.utc  



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

(** datatype encoding Reddit Accounts *)
module Account = struct

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

  let fullname acct = "t2_" ^ acct.id

  let created_utc acct = acct.created_utc

  let from_json_opt json = 
    let open Yojson.Basic in
    let* comment_karma = Util.member "comment_karma" json |> Util.to_int_option in
    let* created_utc = Util.member "created_utc" json
                       |> Util.to_number_option in
    let created_utc = created_utc |> js_date_to_date in
    let has_mail = Util.member "has_mail" json |> Util.to_bool_option in
    let has_mod_mail = Util.member "has_mod_mail" json |> Util.to_bool_option in
    let has_verified_mail = Util.member "has_verified_mail" json |> Util.to_bool_option in
    let* id = Util.member "id" json |> Util.to_string_option in
    let* is_gold = Util.member "is_gold" json |> Util.to_bool_option in
    let* is_mod = Util.member "is_mod" json |> Util.to_bool_option in
    let* link_karma = Util.member "link_karma" json |> Util.to_int_option in
    let* name = Util.member "name" json |> Util.to_string_option in
    let* over_18 = Util.member "over_18" json |> Util.to_bool_option in
    let is_friend = Util.member "is_friend" json |> Util.to_bool_option in
    let inbox_count = Util.member "inbox_count" json |> Util.to_int_option in
    let modhash = Util.member "modhash" json |> Util.to_string_option in
    Some { comment_karma; created_utc; has_mail; has_mod_mail;
           has_verified_mail; id; is_gold; is_mod; link_karma; name;
           over_18; is_friend; inbox_count; modhash }



  let from_json json = 
    let open Yojson.Basic in
    let comment_karma = Util.member "comment_karma" json |> Util.to_int in
    let created_utc = Util.member "created_utc" json
                      |> Util.to_number |> Int.of_float |> Time_ns.of_int_ns_since_epoch
                      |> Time_ns.to_date ~zone:Time.Zone.utc in
    let has_mail = Util.member "has_mail" json |> Util.to_bool_option in
    let has_mod_mail = Util.member "has_mod_mail" json |> Util.to_bool_option in
    let has_verified_mail = Util.member "has_verified_mail" json |> Util.to_bool_option in
    let id = Util.member "id" json |> Util.to_string in
    let is_gold = Util.member "is_gold" json |> Util.to_bool in
    let is_mod = Util.member "is_mod" json |> Util.to_bool in
    let link_karma = Util.member "link_karma" json |> Util.to_int in
    let name = Util.member "name" json |> Util.to_string in
    let over_18 = Util.member "over_18" json |> Util.to_bool in
    let is_friend = Util.member "is_friend" json |> Util.to_bool_option in
    let inbox_count = Util.member "inbox_count" json |> Util.to_int_option in
    let modhash = Util.member "modhash" json |> Util.to_string_option in
    { comment_karma; created_utc; has_mail; has_mod_mail;
      has_verified_mail; id; is_gold; is_mod; link_karma; name;
      over_18; is_friend; inbox_count; modhash }

end

(** datatype encoding Reddit Links *)
module Link = struct

  type t = {
    author: string option;                 (** account anme of the poster, None if this is a promotional link *)
    author_flair_css_class: string option; (** CSS class of the author's flair (if present) - subreddit specific  *)
    author_flair_text: string option;      (** text of the author's flair (if present) - subreddit specific. *)
    domain: string;                        (** domain of the link - self posts will be self.subreddit*)
    hidden: bool;                          (** true if the post is hidden by the logged in user, false if not logged in or not hidden*)
    is_self: bool;                         (** true if this link is a selfpost  *)
    link_flair_css_class: string option;   (** CSS class of the link's flair *)
    link_flair_text: string option;        (** text of the link's flair *)
    locked: bool;                          (** whether the link is locked (closed to new comments) or not*)
    media: unit;                           (** used for streaming video - detailed information about the video placed here  *)
    media_embed: unit;                    (** used for streaming video - detailed embed specific information found here  *)
    num_comments: int;                    (** the number of comments that belong to this link - include removed comments  *)
    over_18: bool;                        (** true if the post is tagged as NSFW, false if otherwise *)
    permalink: Uri.t;                     (** relative URL of the permanent url for this link *)
    saved: bool option;                   (** true if this post is saved by the logged in user  *)
    score: int;                           (** the net-score of the link *)
    selftext: string option;              (** the raw text - this is the unformatted text which includes the raw markup characters; empty if not present *)
    selftext_html: string option;         (** the formatted unescaped text *)
    subreddit: string;                    (** subreddit of link excluding the r/ prefix  *)
    subreddit_id: string;                 (** id of the subreddit of the link   *)
    thumbnail: [`Self | `Image | `Default | `Url of Uri.t ];
    (** full URL to the thumbnail for this link; Self if this is a self-post, `Image if this is a link to an image but has no thumbnail, `Default if no thumnails are available   *)
    title: string;                        (** the title of the link  *)
    url: string;                          (** the link of this post - will be a permalink if this is a self-post  *)

    edited: Date.t option;                (** indicates if the link has been edited - will be the edit timestamp if the link has been edited and return false otherwise  *)

    distinguished: [`Moderator | `Admin | `Special ] option;
    (** to allow determining whether the post has been distinguished by moderators/admins  *)
    stickied: bool;                       (** true if the post is set as the sticky in its subreddit  *)

    num_crossposts: int;                  (** number of crossposts  *)
    hide_score: bool;                     (** (??) whether the score should be hidden  *)
    quarantine: bool;                     (** (??) whether the link is quarantined  *)

    is_reddit_media_domain: bool;         (** (??) whether the URL is on a reddit media domain  *)
    is_meta: bool;                        (** (??) whether the post is a meta post *)
    is_video: bool;                       (** (??) whether the post is a video post post  *)

    created_utc: Date.t;                  (** (??) date of creation  *)

    ups: int;
    downs: int;
    likes: bool option;         (** how the logged in user has voted on the link - true means upvoted, false downvoted, None means no vote *)
  } [@@deriving show, eq, ord]

  let created_utc act = act.created_utc

  let ups act = act.ups

  let downs act = act.downs

  let likes act = act.likes

  let from_json_opt data : t option =
    let open Yojson.Basic in
    let module U = Util in
    let* title = U.member "title" data |> U.to_string_option in
    let* url = U.member "url" data |> U.to_string_option in
    let author = U.member "author" data |> U.to_string_option in
    let author_flair_css_class = U.member "author_flair_css_class" data |> U.to_string_option in
    let author_flair_text = U.member "author_flair_text" data |> U.to_string_option in

    let link_flair_css_class = U.member "link_flair_css_class" data |> U.to_string_option in
    let link_flair_text = U.member "link_flair_text" data |> U.to_string_option in

    let* domain = U.member "domain" data |> U.to_string_option in
    let saved = U.member "saved" data |> U.to_bool_option in
    let* subreddit = U.member "subreddit" data |> U.to_string_option in
    let* subreddit_id = U.member "subreddit_id" data |> U.to_string_option in
    let* hidden = U.member "hidden" data |> U.to_bool_option in
    let* downs = U.member "downs" data |> U.to_int_option in
    let* ups = U.member "ups" data |> U.to_int_option in
    let* hide_score = U.member "hide_score" data |> U.to_bool_option in
    let* quarantine = U.member "quarantine" data |> U.to_bool_option in
    let* is_reddit_media_domain = U.member "is_reddit_media_domain" data |> U.to_bool_option in
    let* is_meta = U.member "is_meta" data |> U.to_bool_option in
    let* is_self = U.member "is_self" data |> U.to_bool_option in
    let* over_18 = U.member "over_18" data |> U.to_bool_option in
    let* num_comments = U.member "num_comments" data |> U.to_int_option in
    let selftext = U.member "selftext" data |> U.to_string_option in
    let selftext_html = U.member "selftext_html" data |> U.to_string_option in
    let* locked = U.member "locked" data |> U.to_bool_option in
    let* score = U.member "score" data |> U.to_int_option in
    let* is_video = U.member "is_video" data |> U.to_bool_option in
    let* stickied = U.member "stickied" data |> U.to_bool_option in
    let* created_utc = U.member "created_utc" data
                       |> Util.to_number_option in
    let edited = U.member "edited" data |> (function
        | `Float n -> Some (n |> js_date_to_date)
        | `Int n -> Some (n |> Float.of_int |> js_date_to_date)
        | _ -> None ) in
    let distinguished = U.member "distinguished" data |> U.to_string_option
                        |> (function
                            | Some "moderator" -> Some `Moderator
                            | Some "admin" -> Some `Admin
                            | Some "special" -> Some `Special
                            | _ -> None ) in
    let created_utc = js_date_to_date created_utc in
    let thumbnail = U.member "thumbnail" data |> U.to_string_option
                    |> (function
                          None -> `Default
                        | Some "self" -> `Self
                        | Some "image" -> `Image
                        | Some s -> `Url (Uri.of_string s)
                      ) in
    let* num_crossposts = U.member "num_crossposts" data |> U.to_int_option in
    let likes = U.member "likes" data |> U.to_bool_option in
    let* permalink = U.member "permalink" data |> U.to_string_option in
    let permalink = permalink |> Uri.of_string in
    Some { title; url; author; saved; subreddit; subreddit_id; hidden;  downs; ups;
           hide_score; quarantine; is_reddit_media_domain; is_meta; score;
           is_video; created_utc; permalink; author_flair_css_class; author_flair_text;
           domain; is_self; link_flair_text; link_flair_css_class; locked; over_18;
           num_comments; selftext; selftext_html; thumbnail; edited; distinguished;
           stickied; num_crossposts; likes; media=(); media_embed=(); }

  let from_json json : t =
    let open Yojson.Basic in
    let module U = Util in
    let data = U.member "data" json in
    let title = U.member "title" data |> U.to_string in
    let url = U.member "url" data |> U.to_string in
    let author = U.member "author" data |> U.to_string_option in
    let author_flair_css_class = U.member "author_flair_css_class" data |> U.to_string_option in
    let author_flair_text = U.member "author_flair_text" data |> U.to_string_option in

    let link_flair_css_class = U.member "link_flair_css_class" data |> U.to_string_option in
    let link_flair_text = U.member "link_flair_text" data |> U.to_string_option in

    let domain = U.member "domain" data |> U.to_string in
    let saved = U.member "saved" data |> U.to_bool_option in
    let subreddit = U.member "subreddit" data |> U.to_string in
    let subreddit_id = U.member "subreddit_id" data |> U.to_string in
    let hidden = U.member "hidden" data |> U.to_bool in
    let downs = U.member "downs" data |> U.to_int in
    let ups = U.member "ups" data |> U.to_int in
    let hide_score = U.member "hide_score" data |> U.to_bool in
    let quarantine = U.member "quarantine" data |> U.to_bool in
    let is_reddit_media_domain = U.member "is_reddit_media_domain" data |> U.to_bool in
    let is_meta = U.member "is_meta" data |> U.to_bool in
    let is_self = U.member "is_self" data |> U.to_bool in
    let over_18 = U.member "over_18" data |> U.to_bool in
    let num_comments = U.member "num_comments" data |> U.to_int in
    let selftext = U.member "selftext" data |> U.to_string_option in
    let selftext_html = U.member "selftext_html" data |> U.to_string_option in
    let locked = U.member "locked" data |> U.to_bool in
    let score = U.member "score" data |> U.to_int in
    let is_video = U.member "is_video" data |> U.to_bool in
    let stickied = U.member "stickied" data |> U.to_bool in
    let created_utc = U.member "created_utc" data
                      |> Util.to_number in
    let edited = U.member "edited" data |> (function
        | `Float n -> Some (n |> js_date_to_date)
        | `Int n -> Some (n |> Float.of_int |> js_date_to_date)
        | _ -> None ) in
    let distinguished = U.member "distinguished" data |> U.to_string_option
                        |> (function
                            | Some "moderator" -> Some `Moderator
                            | Some "admin" -> Some `Admin
                            | Some "special" -> Some `Special
                            | _ -> None ) in
    let created_utc = js_date_to_date created_utc in
    let thumbnail = U.member "thumbnail" data |> U.to_string_option
                    |> (function
                          None -> `Default
                        | Some "self" -> `Self
                        | Some "image" -> `Image
                        | Some s -> `Url (Uri.of_string s)
                      ) in
    let num_crossposts = U.member "num_crossposts" data |> U.to_int in
    let likes = U.member "likes" data |> U.to_bool_option in
    let permalink = U.member "permalink" data |> U.to_string in
    let permalink = permalink |> Uri.of_string in
    { title; url; author; saved; subreddit; subreddit_id; hidden;  downs; ups;
      hide_score; quarantine; is_reddit_media_domain; is_meta; score;
      is_video; created_utc; permalink; author_flair_css_class; author_flair_text;
      domain; is_self; link_flair_text; link_flair_css_class; locked; over_18;
      num_comments; selftext; selftext_html; thumbnail; edited; distinguished;
      stickied; num_crossposts; likes; media=(); media_embed=(); }

end

module Subreddit = struct

  type t = {
    accounts_active: int option;         (** number of users active in the last 15 minutes *)
    accounts_active_is_fuzzed: bool;     (** (??) whether the number of active users is fuzzed  *)

    allow_discovery: bool;               (** whether the subreddit allows discovery  *)
    allow_galleries: bool;               (** whether the subreddit allows galleries  *)
    allow_images: bool;                  (** whether the subreddit allows images  *)
    allow_polls: bool;                   (** whether the subreddit allows polls  *)
    allow_videogifs: bool;               (** whether the subreddit allows gifs  *)
    allow_videos: bool;                  (** whether the subreddit allows videos  *)

    comment_score_hide_mins: int;        (** number of minutes the subreddit initially hides comment scores  *)
    description: string;                 (** sidebar text  *)
    description_html: string;            (** sidebare text in escaped html format  *)
    display_name: string;                (** human name of the subreddit  *)
    header_img: Uri.t option;            (** full URL to the header image or Nonen   *)
    header_size: (float * float) option; (** height and width of header image or None *)
    header_title: string option;         (** description of header image shown on hover or None  *)
    over18: bool;                        (** whether the subreddit is marked as NSFW  *)
    public_description: string;          (** description shown in search results  *)
    public_description_html: string;     (** html-unescaped description shown in search results  *)
    public_traffic: bool;                (** whether the subreddits traffic page is publicly accessible  *)
    subscribers: int;                    (** the number of redditors subscribed to this subreddit  *)
    submission_type:
      [`Any | `Link | `Self ];           (** type of submissions the subreddit allows  *)
    submission_link_label: string option;(** subreddit's custom label for the submit link button if any  *)
    submit_text_label: string option;    (** subreddit's custom label for the submit text button if any  *)
    subreddit_type:
      [`Public | `Private | `Restricted
      | `GoldRestricted | `Archived ];  (** subreddit's type  *)
    title: string;                      (** title of main page  *)
    url: Uri.t;                         (** relative URL of the subreddit  *)
    user_is_banned: bool option;        (** whether the logged in user is banned from the subredit  *)
    user_is_contributor: bool option;   (** whether the logged in user is subscribed to the subreddit  *)
    user_is_moderator: bool option;     (** whether the logged in user is a moderator of the subreddit  *)
    user_is_subscriber: bool option;    (** whether the logged in user is a subcriber of the subreddit  *)
  } [@@deriving show, eq, ord]

  let from_json_opt json =
    let open Yojson.Basic in
    let module U = Util in
    let accounts_active = U.member "accounts_active" json |> U.to_int_option in
    let* accounts_active_is_fuzzed = U.member "accounts_active_is_fuzzed" json |> U.to_bool_option in

    let* allow_discovery = U.member "allow_discovery" json |> U.to_bool_option in
    let* allow_galleries = U.member "allow_galleries" json |> U.to_bool_option in
    let* allow_images = U.member "allow_images" json |> U.to_bool_option in
    let* allow_polls = U.member "allow_polls" json |> U.to_bool_option in
    let* allow_videogifs = U.member "allow_videogifs" json |> U.to_bool_option in
    let* allow_videos = U.member "allow_videos" json |> U.to_bool_option in

    let* comment_score_hide_mins = U.member "comment_score_hide_mins" json |> U.to_int_option in
    let* description = U.member "description" json |> U.to_string_option in
    let* description_html = U.member "description_html" json |> U.to_string_option in
    let* display_name = U.member "display_name" json |> U.to_string_option in
    let header_img = U.member "header_img" json |> U.to_string_option |> Option.map ~f:Uri.of_string in
    let header_size = U.member "header_size" json |> U.to_list |> function
      | [v1; v2] ->
        let* v1 = U.to_number_option v1 in
        let* v2 = U.to_number_option v2 in
        Some (v1,v2)
      | _ -> None in
    let header_title = U.member "header_title" json |> U.to_string_option in
    let* over18 = U.member "over18" json |> U.to_bool_option in
    let* public_description = U.member "public_description" json |> U.to_string_option in
    let* public_description_html = U.member "public_description_html" json |> U.to_string_option in
    let* public_traffic = U.member "public_traffic" json |> U.to_bool_option in
    let* subscribers = U.member "subscribers" json |> U.to_int_option in
    let* submission_type = U.member "submission_type" json |> U.to_string_option |> function
      | Some "any" -> Some `Any
      | Some "link" -> Some `Link
      | Some "self" -> Some `Self
      | _ -> None in
    let submission_link_label = U.member "submission_link_label" json |> U.to_string_option in
    let submit_text_label = U.member "submit_text_label" json |> U.to_string_option in
    let* subreddit_type = U.member "subreddit_type" json |> U.to_string_option |> function
      | Some "public" -> Some `Public
      | Some "private" -> Some `Private
      | Some "restricted" -> Some `Restricted
      | Some "gold_restricted" -> Some `GoldRestricted
      | Some "archived" -> Some `Archived
      | _ -> None in
    let* title = U.member "title" json |> U.to_string_option in
    let* url = U.member "url" json |> U.to_string_option |> Option.map ~f:Uri.of_string in
    let user_is_banned = U.member "user_is_banned" json |> U.to_bool_option in
    let user_is_contributor = U.member "user_is_contributor" json |> U.to_bool_option in
    let user_is_moderator = U.member "user_is_moderator" json |> U.to_bool_option in
    let user_is_subscriber = U.member "user_is_subscriber" json |> U.to_bool_option in
    Some {
      accounts_active; accounts_active_is_fuzzed; allow_discovery;
      allow_galleries; allow_images; allow_polls; allow_videogifs;
      allow_videos; comment_score_hide_mins; description; description_html;
      display_name; header_img; header_size; header_title; over18; public_description;
      public_description_html; public_traffic; subscribers; submission_type;
      submission_link_label; submit_text_label; subreddit_type; title; url;
      user_is_banned; user_is_contributor; user_is_moderator; user_is_subscriber; 
    }  

  let from_json json =
    let open Yojson.Basic in
    let module U = Util in
    let accounts_active = U.member "accounts_active" json |> U.to_int_option in
    let accounts_active_is_fuzzed = U.member "accounts_active_is_fuzzed" json |> U.to_bool in

    let allow_discovery = U.member "allow_discovery" json |> U.to_bool in
    let allow_galleries = U.member "allow_galleries" json |> U.to_bool in
    let allow_images = U.member "allow_images" json |> U.to_bool in
    let allow_polls = U.member "allow_polls" json |> U.to_bool in
    let allow_videogifs = U.member "allow_videogifs" json |> U.to_bool in
    let allow_videos = U.member "allow_videos" json |> U.to_bool in

    let comment_score_hide_mins = U.member "comment_score_hide_mins" json |> U.to_int in
    let description = U.member "description" json |> U.to_string in
    let description_html = U.member "description_html" json |> U.to_string in
    let display_name = U.member "display_name" json |> U.to_string in
    let header_img = U.member "header_img" json |> U.to_string_option |> Option.map ~f:Uri.of_string in
    let header_size = U.member "header_size" json |> U.to_list |> function
      | [v1; v2] ->
        let* v1 = U.to_number_option v1 in
        let* v2 = U.to_number_option v2 in
        Some (v1,v2)
      | _ -> None in
    let header_title = U.member "header_title" json |> U.to_string_option in
    let over18 = U.member "over18" json |> U.to_bool in
    let public_description = U.member "public_description" json |> U.to_string in
    let public_description_html = U.member "public_description_html" json |> U.to_string in
    let public_traffic = U.member "public_traffic" json |> U.to_bool in
    let subscribers = U.member "subscribers" json |> U.to_int in
    let submission_type = U.member "submission_type" json |> U.to_string_option |> function
      | Some "any" -> `Any
      | Some "link" -> `Link
      | Some "self" -> `Self
      | _ ->  assert false in
    let submission_link_label = U.member "submission_link_label" json |> U.to_string_option in
    let submit_text_label = U.member "submit_text_label" json |> U.to_string_option in
    let subreddit_type = U.member "subreddit_type" json |> U.to_string_option |> function
      | Some "public" -> `Public
      | Some "private" -> `Private
      | Some "restricted" -> `Restricted
      | Some "gold_restricted" -> `GoldRestricted
      | Some "archived" -> `Archived
      | _ -> assert false in
    let title = U.member "title" json |> U.to_string in
    let url = U.member "url" json |> U.to_string |> Uri.of_string in
    let user_is_banned = U.member "user_is_banned" json |> U.to_bool_option in
    let user_is_contributor = U.member "user_is_contributor" json |> U.to_bool_option in
    let user_is_moderator = U.member "user_is_moderator" json |> U.to_bool_option in
    let user_is_subscriber = U.member "user_is_subscriber" json |> U.to_bool_option in
    {
      accounts_active; accounts_active_is_fuzzed; allow_discovery;
      allow_galleries; allow_images; allow_polls; allow_videogifs;
      allow_videos; comment_score_hide_mins; description; description_html;
      display_name; header_img; header_size; header_title; over18; public_description;
      public_description_html; public_traffic; subscribers; submission_type;
      submission_link_label; submit_text_label; subreddit_type; title; url;
      user_is_banned; user_is_contributor; user_is_moderator; user_is_subscriber; 
    }

end

module Thing  = struct

  module Internal = struct


    type 'a tag =
      | Account : Account.t tag
      | Link : Link.t tag
      | Subreddit : Subreddit.t tag

    type 'a data = ('a tag * 'a)

    let pp_data (type a) _ fmt (vl: a data) = match vl with
      | Account, v -> Account.pp fmt v
      | Link, v -> Link.pp fmt v
      | Subreddit, v -> Subreddit.pp fmt v

    let compare_data (type a) _ (a: a data) (b: a data) = match a,b with
      | (Account, a), (Account, b) -> Account.compare a b
      | (Link, a), (Link, b) -> Link.compare a b
      | (Subreddit, a), (Subreddit, b) -> Subreddit.compare a b

    let equal_data (type a) _ (a: a data) (b: a data) = match a,b with
      | (Account, a), (Account, b) -> Account.equal a b
      | (Link, a), (Link, b) -> Link.equal a b
      | (Subreddit, a), (Subreddit, b) -> Subreddit.equal a b

    type 'a t = { id: string; name: string; data: 'a data; }
    [@@deriving show, ord, eq]

  end

  type 'a s = 'a Internal.t

  module Wrapped = struct

    type t = MkThing : 'a s -> t

    let from_json json =
      let open Yojson.Basic in
      let module U = Util in
      let id = U.member "id" json |> U.to_string in
      let name = U.member "name" json |> U.to_string in
      let kind = U.member "kind" json |> U.to_string in
      let data = U.member "data" json in
      match kind with
      | "t2" -> let account = Account.from_json data in
        MkThing ({id; name;  data=(Account,account)})
      | "t3" -> let link = Link.from_json data in
        MkThing ({id; name;  data=(Link, link)})
      | "t5" -> 
        let subreddit = Subreddit.from_json data in
        MkThing ({id; name;  data=(Subreddit, subreddit)})
      | _ -> assert false

    let from_json_opt json =
      let open Yojson.Basic in
      let module U = Util in
      let* id = U.member "id" json |> U.to_string_option in
      let* name = U.member "name" json |> U.to_string_option in
      let* kind = U.member "kind" json |> U.to_string_option in
      let data = U.member "data" json in
      match kind with
      | "t2" -> let* account = Account.from_json_opt data in
        Some (MkThing ({id; name;  data=(Account, account)}))
      | "t3" -> let* link = Link.from_json_opt data in
        Some (MkThing ({id; name;  data=(Link, link)}))
      | "t5" ->
        let* subreddit = Subreddit.from_json_opt data in
        Some (MkThing ({id; name;  data=(Subreddit, subreddit)}))
      | _ -> None

  end

  include Internal

  let from_json (type a) (tag: a tag) json : a t =
    let open Yojson.Basic in
    let module U = Util in
    let id = U.member "id" json |> U.to_string in
    let name = U.member "name" json |> U.to_string in
    let kind = U.member "kind" json |> U.to_string in
    let data = U.member "data" json in

    match tag,kind with
    | Account, "t2" -> let account = Account.from_json data in
      {id; name;  data=(Account,account)}
    | Link, "t3" ->
      let link = Link.from_json data in
      {id; name;  data=(Link, link)}
    | Subreddit, "t5" -> 
      let subreddit = Subreddit.from_json data in
      {id; name;  data=(Subreddit, subreddit)}
    | _ -> assert false

  let from_json_opt (type a) (tag: a tag) json : a t option =
    let build id name kind data : a t option =
      match tag,kind with
      | Account, "t2" -> let* account = Account.from_json_opt data in
        Some {id; name;  data=(Account,account)}
      | Link, "t3" ->
        let* link = Link.from_json_opt data in
        Some {id; name;  data=(Link, link)}
      | Subreddit, "t5" -> 
        let* subreddit = Subreddit.from_json_opt data in
        Some {id; name;  data=(Subreddit, subreddit)}
      | _ -> None
    in
    let open Yojson.Basic in
    let module U = Util in
    let* id = U.member "id" json |> U.to_string_option in
    let* name = U.member "name" json |> U.to_string_option in
    let* kind = U.member "kind" json |> U.to_string_option in
    let data = U.member "data" json in
    build id name kind data

end

module API = struct

  let base_api = "https://oauth.reddit.com"

  let add_auth user_agent ctx header =
    Printf.printf "user_agent: %s\n" Header.user_agent;
    let header = Header.add header "User-Agent" user_agent in
    match ctx with
    | `Unauthenticated access_token
    | `Authenticated access_token ->
      Header.add header "Authorization" (Printf.sprintf "bearer %s" access_token)

  module type PUBLIC_CONTEXT = sig
    val user_agent: string
    (** access token to be passed in api calls *)
    val access_token: [ `Authenticated of string |
                        `Unauthenticated of string ]
  end

  module type AUTH_CONTEXT = sig
    val user_agent: string
    (** access token to be passed in api calls *)
    val access_token: [ `Authenticated of string ]
  end

  let downcast_context (module Ctx: AUTH_CONTEXT) =
    let module M = (struct
      let user_agent = Ctx.user_agent

      let access_token : [ `Authenticated of string | `Unauthenticated of string ] =
        (Ctx.access_token :> [ `Authenticated of string |
                               `Unauthenticated of string ] )
    end) in
    (module M : PUBLIC_CONTEXT)

  let build_unauth_context ?(user_agent="OCaml-Reddit-Wrapper-Script") ~client_username ~client_pass () =
    let (let+) x f = Lwt.(>>=) x f  in
    Printf.printf "client_username %s, client_pass %s\n" client_username client_pass;
    let access_token =
      let client_credentials : Auth.credential = `Basic (client_username, client_pass) in
      let+ (resp,body) =
        let header = Header.add_authorization (Header.init ()) client_credentials in
        let body = Uri.encoded_of_query
            ["grant_type", ["client_credentials"];] in
        Client.post
          ~headers:header
          ~body:(Cohttp_lwt.Body.of_string body)
          (Uri.of_string "https://www.reddit.com/api/v1/access_token") in
      let code = resp |> Response.status |> Code.code_of_status in
      if not (Code.is_success code)
      then raise (Failure (Printf.sprintf "auth-api request failed: error code %d" code));
      let+ body = body |> Cohttp_lwt.Body.to_string in
      let response = Yojson.Basic.from_string body in
      Lwt.return (Yojson.Basic.Util.to_assoc response
                  |> (fun ls -> List.Assoc.find_exn ~equal:String.equal ls "access_token")
                  |> Yojson.Basic.Util.to_string) in
    let+ access_token = access_token in
    let module M = struct let user_agent = user_agent let access_token = `Unauthenticated access_token end in
    Lwt.return (module M : PUBLIC_CONTEXT)



  let build_auth_context
      ?(user_agent="OCaml-Reddit-Wrapper-Script")
      ~client_username ~client_pass ~reddit_password ~reddit_username () =
    (* Retrieve access token from Reddit API *)
    let (let+) x f = Lwt.(>>=) x f  in
    let access_token =
      let client_credentials : Auth.credential = `Basic (client_username, client_pass) in
      let+ (resp,body) =
        let header = Header.add_authorization (Header.init ()) client_credentials in
        let body = Uri.encoded_of_query
            ["grant_type", ["password"]; "username", [reddit_username]; "password", [reddit_password]] in
        Client.post
          ~headers:header
          ~body:(Cohttp_lwt.Body.of_string body)
          (Uri.of_string "https://www.reddit.com/api/v1/access_token") in
      let code = resp |> Response.status |> Code.code_of_status in
      if not (Code.is_success code)
      then raise (Failure (Printf.sprintf "auth-api request failed: error code %d" code));
      let+ body = body |> Cohttp_lwt.Body.to_string in
      let response = Yojson.Basic.from_string body in
      Lwt.return (Yojson.Basic.Util.to_assoc response
                  |> (fun ls -> List.Assoc.find_exn ~equal:String.equal ls "access_token")
                  |> Yojson.Basic.Util.to_string) in
    let+ access_token = access_token in
    let module M = struct let user_agent = user_agent let access_token = `Authenticated access_token end in
    Lwt.return (module M : AUTH_CONTEXT)

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



  module Unauthenticated (Context: PUBLIC_CONTEXT)  = struct
    let add_auth = add_auth Context.user_agent Context.access_token

    module Subreddit = struct
      type period = [ `Hour | `Day | `Week | `Month | `Year | `All ]
      type sort = [ `Hot | `New | `Random | `Rising | `Top of period | `Controversial of period ]
      type anchor = [`Before of string | `After of string]
      type pagination_state = { anchor: anchor; count: int; subreddit: string; sort: sort; }

      let get ?verbose:(_verbose=false) ?anchor ?count ?limit ?(show=false) ~(sort: sort) subreddit =
        let sort_str = match sort with
            `Controversial _ -> "controversial" | `Random -> "random" | `Top _ -> "top"
          | `Rising -> "rising" | `New -> "new" | `Hot -> "hot" in
        let url = Printf.sprintf "%s/r/%s/%s" base_api subreddit sort_str in
        let headers = Header.init () |> add_auth
                      |> (fun header -> match anchor with None -> header |
                          Some (`Before (fullname: string)) -> Header.add header "before" fullname |
                          Some (`After (fullname: string)) -> Header.add header "after" fullname )
                      |> (fun header -> match count with None -> header |
                          Some count -> Header.add header "count" (Printf.sprintf "%d" count))
                      |> (fun header -> match limit with None -> header |
                          Some limit -> Header.add header "limit" (Printf.sprintf "%d" limit))
                      |> (fun header -> if show then Header.add header "show" "all" else header)
                      |> (fun header -> (match sort with
                            `Top period | `Controversial period ->
                            let period_str = match period with
                              | `Month -> "month" | `All -> "all" | `Week -> "week"
                              | `Day -> "day" | `Hour -> "hour" | `Year -> "year" in
                            Header.add header "t" period_str | _ -> header)) in
        let+ (resp, body) = Client.get ~headers (Uri.of_string url) in
        handle_response resp;
        let module U = Yojson.Basic.Util in
        let+ body = Cohttp_lwt.Body.to_string body in
        let body = Yojson.Basic.from_string body in
        (* sanity check *)
        assert String.((Yojson.Basic.Util.member "kind" body |> Yojson.Basic.Util.to_string) = "Listing");
        let data = U.member "data" body in
        let new_anchor: anchor = U.member "after" data |> U.to_string |> (fun s -> `After s) in
        let new_count: int = U.member "dist" data |> U.to_int in
        let children = U.member "children" data |> U.to_list
                       |> List.filter ~f:(fun json -> String.equal "t3" @@ (U.member "kind" json |> U.to_string))
                       |> List.map ~f:Link.from_json in
        Lwt.return (children, {anchor=new_anchor; count=new_count; subreddit; sort})

      let get_more ?(verbose=false) ?limit ?show prev_state =
        get ~verbose ?limit ?show
          ~anchor:prev_state.anchor ~count:prev_state.count ~sort:prev_state.sort prev_state.subreddit

      let get_multi ?(verbose=false) ?(show=false) ~(sort: sort) subreddit count =
        let+ (subreddits, state) = get ~verbose ~show ~sort subreddit in
        let rec loop acc curr_count state =
          let+ (next_subreddits, state) = get_more ~verbose ~show state in
          match List.length next_subreddits with
          | 0 -> Lwt.return @@ List.concat (List.rev acc)
          | n when curr_count + n >= count -> Lwt.return @@ List.concat (List.rev acc)
          | _ -> loop (next_subreddits :: acc) (curr_count + List.length next_subreddits) state in
        loop [subreddits] (List.length subreddits) state

    end
  end

  module Authenticated (Context: AUTH_CONTEXT) = struct

    include Unauthenticated (val (downcast_context (module Context)))

    module Me = struct
      let get =
        let headers = Header.init () |> add_auth in
        let+ (resp, body) =
          Client.get ~headers (Uri.of_string (Printf.sprintf "%s/api/v1/me" base_api)) in
        handle_response resp;
        let+ body = Cohttp_lwt.Body.to_string body in
        let body = Yojson.Basic.from_string body in
        Lwt.return (Account.from_json body)

    end
  end

end
