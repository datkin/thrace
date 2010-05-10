structure Id = struct
type id = int

structure StringMap = BinaryMapFn(type ord_key = string val compare = String.compare)
structure IntMap = BinaryMapFn(type ord_key = int val compare = Int.compare)

val nextId = ref 0
val ids : id StringMap.map ref = ref StringMap.empty
val names : string IntMap.map ref = ref IntMap.empty

fun id name =
    case StringMap.find (!ids, name) of
      SOME id => id
    | NONE => let
        val id = !nextId
        val _ = nextId := id + 1
        val _ = ids := StringMap.insert (!ids, name, id)
        val _ = names := IntMap.insert (!names, id, name)
      in id end

fun name id =
    case IntMap.find (!names, id) of
      SOME name => name
    | NONE => raise Fail "No such symbol"

end
