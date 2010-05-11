structure Id = struct
type id = int
val compare = Int.compare

structure StringMap = BinaryMapFn(type ord_key = string val compare = String.compare)
structure IdMap = BinaryMapFn(type ord_key = id val compare = compare)
structure IdSet = BinarySetFn(type ord_key = id val compare = compare)

val nextId = ref 0
val ids : id StringMap.map ref = ref StringMap.empty
val names : string IdMap.map ref = ref IdMap.empty

fun id name =
    case StringMap.find (!ids, name) of
      SOME id => id
    | NONE => let
        val id = !nextId
        val _ = nextId := id + 1
        val _ = ids := StringMap.insert (!ids, name, id)
        val _ = names := IdMap.insert (!names, id, name)
      in id end

fun name id =
    case IdMap.find (!names, id) of
      SOME name => name
    | NONE => raise Fail "No such symbol"

fun dump (idmap, toStr) =
    print (String.concatWith "\n"
                             (map (fn (id, item) =>
                                      name id ^ ": " ^ (toStr item))
                                  (IdMap.listItemsi idmap)))

end
