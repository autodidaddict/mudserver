-- Global variables to track if on_entered_inv was called and if parameters are correct
entered_inv_called = false
correct_ob_ref = false
expected_ob_ref = "test.player#player1"

-- Function to check if the entered_inv_called variable was set to true
function was_entered_inv_called()
    return entered_inv_called
end

-- Function to check if the ob parameter's ref field matches the expected value
function has_correct_ob_ref()
    return correct_ob_ref
end

-- Function that will be called by notifyEnteredInvAction
-- 'ob' is a table with `ref` and `name` fields (the object that entered)
-- 'from' is a table with `ref` and `name` fields (the source object)
function on_entered_inv(ob, from)
    -- Set the global variable to true to indicate the function was called
    entered_inv_called = true
    
    -- Validate that the ob parameter has the expected ref field
    if ob and ob.ref and ob.ref == expected_ob_ref then
        correct_ob_ref = true
    else
        correct_ob_ref = false
    end
    
    -- Return true to indicate success (though the return value isn't used by notifyEnteredInvAction)
    return true
end