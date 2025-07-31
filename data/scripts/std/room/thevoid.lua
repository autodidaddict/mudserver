function short()
    tellPlayer("std.player#kevin", "You're in the void, bruh")
    return "This is the void"
end

-- Called when an object enters this room's inventory
-- 'ob' is a table with `ref` and `name` fields
-- `from` is a table with `ref` and `name` fields, and refers to the room from which the object came
function on_entered_inv(ob, from)
    tellPlayer(ob.ref, "Welcome to the void, " .. ob.name)
end

-- Called when an object leaves this room's inventory
-- `ob` is a table with `ref` and `name`
-- `to` is the room to which the object has gone, also having a `name` and `ref` field
function on_exited_inv(ob, to)
   tellPlayer(ob.ref, "Come back soon, " .. ob.name)
end

