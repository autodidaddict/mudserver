function short()
    tellPlayer("kevin", "You're in the shed!")
    return "This is a fairly empty storage shed"
end

function long()
    return "This is a basic storage shed. There should probably be something here but there isn't."
end

-- Called when an object enters this room's inventory
-- 'ob' is a table with `ref` and `name` fields
-- `from` is a table with `ref` and `name` fields, and refers to the room from which the object came
function on_entered_inv(ob, from)
    tellPlayer(ob.ref, "Welcome to the shed, " .. ob.name)
end


