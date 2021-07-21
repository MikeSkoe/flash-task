let (>>) f1 f2 arg = f2 (f1 arg)

module Id = struct
  let counter = ref(0)
      
  let of_string =
    String.to_seq
    >> Seq.map Char.code 
    >> List.of_seq 
    >> List.fold_left (+) 0 
      
  let get_next () =
    let next_id = counter.contents + 1 in
    counter.contents <- next_id;
    next_id
      
  type t =
    | Tag of int
    | Item of int 
end 

module Tag = struct
  type t = {
    id: Id.t;
    title: string;
  }
  
  let make title = 
    let id = Id.(Tag(of_string title)) in
    { id; title }
end

module Item = struct
  type t = {
    id: Id.t;
    title: string;
    tags: string list;
    body: string;
  }
  
  let make title tags body = 
    let id = Id.(Item (get_next ())) in
    { id; title; tags; body }
end

module Folder = struct
  type t = {
    items: Item.t list;
    tags: Tag.t list;
  }
  
  let make () = { items=[]; tags=[] } 
  
  let add_item (item: Item.t) t =
    let tags = List.map Tag.make item.tags in
    { items=item::t.items; tags=t.tags @ tags }
end

let first_item = Item.make
    "my title of the thing"
    ["first_tag"]
    "body of the\nthing btw"

let second_item = Item.make
    "my title of the thing"
    ["first_tag"; "second_tag"]
    "body of the\nthing btw"

let folder =
  Folder.make ()
  |> Folder.add_item first_item
  |> Folder.add_item second_item
