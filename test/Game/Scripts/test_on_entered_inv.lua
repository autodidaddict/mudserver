-- Global variable to track if on_entered_inv was called
entered_inv_called = false

-- Function to check if the entered_inv_called variable was set to true
function was_entered_inv_called()
    return entered_inv_called
end

-- Function that will be called by notifyEnteredInvAction
-- 'ob' is a table with `ref` and `name` fields (the object that entered)
-- 'from' is a table with `ref` and `name` fields (the source object)
function on_entered_inv(ob, from)
    -- Set the global variable to true to indicate the function was called
    entered_inv_called = true
    
    -- Optionally, we could validate the parameters here
    -- For example, check that ob and from are tables with ref and name fields
    
    -- Return true to indicate success (though the return value isn't used by notifyEnteredInvAction)
    return true
end