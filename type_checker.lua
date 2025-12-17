local Node = require('node')
local util = require('util')
local serpent = require('serpent')
local Type = require('type')
local Type_Checker = {}


function Type_Checker:type_check(ast, symbol_table)
    local base = Type.base
    local pointer = Type.pointer
    local array = Type.array
    local func = Type.func
    local struct = Type.struct
    local union = Type.union
    local to_string = Type.to_string
    local to_string_pretty = Type.to_string_pretty
    local same_type_chain = Type.same_type_chain
    local is_base_type = Type.is_base_type
    local get_symbol = symbol_table.get_symbol
    local add_symbol = symbol_table.add_symbol
    local new_scope = symbol_table.new_scope
    local exit_scope = symbol_table.exit_scope

    local NODE_TYPES = Node.NODE_TYPES
    local node_check = Node.node_check

    function check_program(n)
        for _, child in ipairs(n) do
            build_type(child)
        end

        return n
    end

    function fill_array_dimensions(target_type, initializer_type)
        while(target_type ~= nil and initializer_type ~= nil) do
            if(target_type.kind == Type.KINDS["ARRAY"] and initializer_type.kind == Type.KINDS["ARRAY"] and target_type.length == -1) then
                target_type.length = initializer_type.length
            end

            target_type = target_type.points_to
            initializer_type = initializer_type.points_to
        end
    end

    function check_struct_or_union_type(n)

        if(n.declaration) then
            local type = nil
            if(n.is_struct) then
                type = struct(n.id.id)
            else
                type = union(n.id.id)
            end
            n.value_type = type
            add_symbol(n.id.id, {type = type}, symbol_table.tag)
            for _, child in ipairs(n.declaration) do
                local base_type = nil
                    if(node_check(child.type_specifier.kind, "STRUCT_OR_UNION_SPECIFIER")) then
                        base_type = get_symbol(child.type_specifier.kind.id.id, symbol_table.tag).type
                    else
                        base_type = base(child.type_specifier.kind)
                    end
                for _, element in ipairs(child) do
                    assert(node_check(element, "DECLARATOR"), "Expected DECLARATOR")
                    
                    local member_type = {type=build_declarator(element, base_type)}
                    type.members[element.direct_declarator.id.id] = member_type
                    table.insert(type.members, member_type)
                end
            end
        else
            n.value_type = get_symbol(n.id.id, symbol_table.tag).type
        end

        n.handle = get_symbol(n.id.id, symbol_table.tag)
    end

    function build_type(n)
        local base_type = nil
        if(node_check(n.specifier.type_specifier.kind, "STRUCT_OR_UNION_SPECIFIER")) then
            check_struct_or_union_type(n.specifier.type_specifier.kind)
            base_type = n.specifier.type_specifier.kind.value_type
        else
            base_type = base(n.specifier.type_specifier.kind)
        end

        if(n.declarator) then
            n.value_type = build_declarator(n.declarator, base_type)
        else
            n.value_type = base_type
        end

        if(n.initializer) then
            if(n.value_type.kind == Type.KINDS["ARRAY"] and n.value_type.length == -1) then
                n.value_type.length = #n.initializer
            end
            if(node_check(n.initializer, "INITIALIZER_LIST")) then
                if(not check_initializer_list(n.initializer, n.value_type)) then

                    error("Initializer list does not match the declared type")
                end
                n.initializer.value_type = n.value_type
            else
                if(not can_coerce(check_initializer(n.initializer), n.value_type)) then
                    error("Initializer does not match the declared type")
                end
            end
        end

        if(n.id) then
            add_symbol(n.id.id, {type = n.value_type}, symbol_table.ordinary)
            n.handle = get_symbol(n.id.id, symbol_table.ordinary)
        else
            n.handle = get_symbol(n.specifier.type_specifier.kind.id.id, symbol_table.tag)
        end

        if(n.block) then
            new_scope(n.declarator.direct_declarator.id.id)
            for _, child in ipairs(n.declarator.direct_declarator.parameter_list) do
                add_symbol(child.id.id, {type = child.value_type}, symbol_table.ordinary)
                child.handle = get_symbol(child.id.id, symbol_table.ordinary)
            end
            if(n.handle.type.parameter_types.is_variadic) then
                add_variadic_parameter(n.declarator.direct_declarator.parameter_list)
            end
            check_block(n.block)
            exit_scope()
        end


        return n.value_type
    end

    function add_variadic_parameter(parameter_list)
        local identifier_node = Node:new("IDENTIFIER")
        identifier_node.id = "va_args"
        add_symbol(identifier_node.id, {type=array(0, base("VOID"))}, symbol_table.ordinary)
        table.insert(parameter_list, {id=identifier_node, handle=get_symbol(identifier_node.id, symbol_table.ordinary)})
    end

    function can_coerce_array_to_struct(initializer, n)
        assert(initializer.value_type.kind == Type.KINDS["ARRAY"] and n.value_type.kind == Type.KINDS["STRUCT"], "Can only coerce array to struct")
        -- coerce array to struct
        for i, member in ipairs(n.value_type.members) do
            if(i > initializer.value_type.length) then
                break
            else
                if(not can_coerce(initializer[i].value_type, member.type, true)) then
                    print("cannot coerce array to struct: " .. to_string_pretty(initializer[i].value_type) .. " to " .. to_string_pretty(member.type))
                    return false
                end
            end
        end
        return true
    end

    local block_id = 0
    function next_block_id()
        local temp = block_id
        block_id = block_id + 1
        return temp
    end

    function check_block(n)
        new_scope("block_" .. next_block_id())
        for _, child in ipairs(n) do
            check_statement(child)
        end
        exit_scope()
    end


    function check_statement(n)
        if(node_check(n.child, "DECLARATION")) then
            build_type(n.child)
        elseif(node_check(n.child, "IF")) then
            check_if(n.child)
        elseif(node_check(n.child, "RETURN")) then
            check_return(n.child)
        elseif(node_check(n.child, "BLOCK")) then
            check_block(n.child)
        elseif(node_check(n.child, "FOR")) then
            check_for(n.child)
        elseif(node_check(n.child, "BREAK")) then
            -- nothing
        elseif(node_check(n.child, "CONTINUE")) then
            -- nothing
        elseif(node_check(n.child, "WHILE")) then
            check_while(n.child)
        elseif(node_check(n.child, "EXPRESSION")) then
            check_expression(n.child)
        end
    end

    function check_for(n)
        if(node_check(n.initialization, "DECLARATION")) then
            build_type(n.initialization)
        else
            check_expression(n.initialization)
        end
        if(not can_coerce(check_expression(n.condition), base("INT"))) then
            error("The condition for a for statement must be an int")
        end
        check_statement(n.statement)
        check_expression(n.update)
    end

    function check_while(n)
        if(not can_coerce(check_expression(n.condition), base("INT"))) then
            error("The condition for a while statement must be an int")
        end
        check_statement(n.statement)
    end

    function check_if(n)
        if(not can_coerce(check_expression(n.condition), base("INT"))) then
            error("The condition in an if statement must be an int")
        end
        check_statement(n.true_case)
        if(n.false_case) then
            check_statement(n.false_case)
        end
    end



    function check_return(n)
        return check_expression(n.value)
    end

    function array_coerce(type1, type2)
        -- builds a new synthetic type that is a result of the coersion of the array elements
        local result = array(0, nil)
        local start = result
        while(type1 ~= nil and type2 ~= nil) do
            if(type1.kind == Type.KINDS["ARRAY"] and type2.kind == Type.KINDS["ARRAY"]) then
                result.points_to = array(math.max(type1.length, type2.length),nil)
                type1 = type1.points_to
                type2 = type2.points_to
            elseif(can_coerce(type2, type1)) then
                result.points_to = type1
                break
            elseif(can_coerce(type1, type2)) then
                result.points_to = type2
                break
            else
                print("array coerce failed: " .. to_string(type1) .. " to " .. to_string(type2))
                return nil
            end

            result = result.points_to
        end
        return start.points_to
    end

    function get_type_chain_length(type)
        local length = 0
        while(type ~= nil) do
            length = length + 1
            type = type.points_to
        end

        return length
    end


    function can_coerce(type, target, allow_greater_target_length)
        allow_greater_target_length = allow_greater_target_length or false

        -- array to pointer decay
        while(type ~= nil and target ~= nil) do

            if(target.kind == Type.KINDS["STRUCT"]) then
                if(type.kind == Type.KINDS["STRUCT"]) then
                    if(type.id ~= target.id) then
                        return false
                    end
                    return true
                elseif(type.kind == Type.KINDS["ARRAY"]) then
                    return can_coerce_array_to_struct(type, target)
                else
                    return false
                end
            -- can coerce when both types are base types or if the coerced type is void at any point in the type chain
            elseif(type.kind == Type.KINDS["FUNCTION"] and target.kind == Type.KINDS["FUNCTION"]) then
                if(#type.parameter_types ~= #target.parameter_types) then
                    return false
                end

                for i, param_type in ipairs(type.parameter_types) do
                    if(not can_coerce(param_type, target.parameter_types[i])) then
                        return false
                    end
                end

                return true
            elseif(is_base_type(type) and is_base_type(target) or type.kind == Type.KINDS["VOID"]) then
                return true
            elseif(type.kind ~= target.kind and not (type.kind == Type.KINDS["ARRAY"] and target.kind == Type.KINDS["POINTER"])) then
                break
            elseif(type.length ~= nil and target.length ~= nil and (type.length > target.length or (target.length > type.length and not allow_greater_target_length))) then
                if(target.length ~= -1) then -- -1 means the target array has unknown dimensions that will later be filled
                    break
                end
            end
            type = type.points_to
            target = target.points_to
        end

        return false
    end

    function check_initializer_list(n, target_type)
        assert(target_type.kind == Type.KINDS["ARRAY"] or target_type.kind == Type.KINDS["STRUCT"] or target_type.kind == Type.KINDS["UNION"], "Declared type must be an aggregate type")
        if((target_type.kind == Type.KINDS["ARRAY"] and target_type.length < #n) or (target_type.kind == Type.KINDS["STRUCT"] and #target_type.members < #n)) then
            return false
        end

        if(target_type.kind == Type.KINDS["UNION"]) then
            for i, member in ipairs(target_type.members) do
                if(node_check(n[1], "INITIALIZER_LIST")) then
                    if(check_initializer_list(n[1], member.type)) then
                        return true
                    end
                else
                    if(can_coerce(check_initializer(n[1]), member.type)) then
                        return true
                    end
                end
            end
            return false
        end

        for i, child in ipairs(n) do
            local same_type = nil
            if(node_check(child, "INITIALIZER_LIST")) then
                if(target_type.kind == Type.KINDS["STRUCT"]) then
                    same_type = check_initializer_list(child, target_type.members[i].type)
                else
                    same_type = check_initializer_list(child, target_type.points_to)
                end

            elseif(node_check(child, "INITIALIZER")) then
                if(target_type.kind == Type.KINDS["STRUCT"]) then
                    same_type = can_coerce(check_initializer(child), target_type.members[i].type)
                else
                    same_type = can_coerce(check_initializer(child), target_type.points_to)
                end
            else
                error()
            end

            if(not same_type) then
                return false
            end
        end

        return true
    end

        

    function check_initializer(n)

        n.value_type = check_assignment_expression(n.value)

        return n.value_type
    end

    function check_expression(n)
        for _, child in ipairs(n) do
            check_assignment_expression(child)
        end

        n.value_type = n[#n].value_type
        return n.value_type
    end

    function check_assignment_expression(n)
        -- May not have a lhs or rhs
        if(node_check(n, "ASSIGNMENT")) then
            local lhs_type = check_ternary_expression(n.lhs)
            local rhs_type = check_assignment_expression(n.rhs)
            if(not same_type_chain(lhs_type, rhs_type, true)) then
                error("Assignment types do not match")
            end
            n.value_type = lhs_type
        else
            n.value_type = check_ternary_expression(n)
        end

        return n.value_type
    end

    function check_ternary_expression(n)
        if(node_check(n, "TERNARY")) then
            local condition_type = check_logical_or_expression(n.condition)
            local true_case_type = check_assignment_expression(n.true_case)
            local false_case_type = check_logical_or_expression(n.false_case)
            if(not same_type_chain(false_case_type, true_case_type, true)) then
                error("Ternary false and true case types do not match")
            end
            n.value_type = true_case_type
        else
            n.value_type = check_logical_or_expression(n)
        end

        return n.value_type
    end

    function check_logical_or_expression(n)
        if(node_check(n, "LOGICAL_OR_EXPRESSION")) then
            for i=1, #n do
        
                if(not can_coerce(check_logical_and_expression(n[i]), base("INT"))) then
                    error("Logical or expression terms must be ints")
                end
            end
            n.value_type = base({is_signed(n) and "SIGNED" or "UNSIGNED", "INT"})
            return n.value_type
        else
            return check_logical_and_expression(n)
        end
    end

    function check_logical_and_expression(n)
        if(node_check(n, "LOGICAL_AND_EXPRESSION")) then
            for i=1, #n do
                if(not can_coerce(check_inclusive_or_expression(n[i]), base("INT"))) then
                    error("Logical and expression terms must be ints")
                end
            end
            n.value_type = base({is_signed(n) and "SIGNED" or "UNSIGNED", "INT"})
            return n.value_type
        else
            return check_inclusive_or_expression(n)
        end
    end

    function check_inclusive_or_expression(n)
        if(node_check(n, "INCLUSIVE_OR_EXPRESSION")) then
            for i=1, #n do
                if(not can_coerce(check_inclusive_xor_expression(n[i]), base("INT"))) then
                    error("Bitwise inclusive or expression terms must be ints")
                end
            end
            n.value_type = base({is_signed(n) and "SIGNED" or "UNSIGNED", "INT"})
            return n.value_type
        else
            return check_inclusive_xor_expression(n)
        end
    end

    function check_inclusive_xor_expression(n)
        if(node_check(n, "INCLUSIVE_XOR_EXPRESSION")) then
            for i=1, #n do
                if(not can_coerce(check_inclusive_and_expression(n[i]), base("INT"))) then
                    error("Bitwise inclusive xor expression terms must be ints")
                end
            end
            n.value_type = base({is_signed(n) and "SIGNED" or "UNSIGNED", "INT"})
            return n.value_type
        else
            return check_inclusive_and_expression(n)
        end
    end

    function check_inclusive_and_expression(n)
        if(node_check(n, "INCLUSIVE_AND_EXPRESSION")) then
            for i=1, #n do
                if(not can_coerce(check_equality_expression(n[i]), base("INT"))) then
                    error("Bitwise inclusive and expression terms must be ints")
                end
            end
            n.value_type = base({is_signed(n) and "SIGNED" or "UNSIGNED", "INT"})
            return n.value_type
        else
            return check_equality_expression(n)
        end
    end

    function check_equality_expression(n)
        if(node_check(n, "EQUALITY_EXPRESSION")) then
            for i=1, #n, 2 do
                -- might extend equality to non-int types later
                if(not can_coerce(check_relational_expression(n[i]), base("INT"))) then
                    error("Equality expression terms must be ints")
                end
            end
            n.value_type = base({is_signed(n) and "SIGNED" or "UNSIGNED", "INT"})
            return n.value_type
        else
            return check_relational_expression(n)
        end
    end

    function check_relational_expression(n)
        if(node_check(n, "RELATIONAL_EXPRESSION")) then
            for i=1, #n, 2 do
                if(not can_coerce(check_shift_expression(n[i]), base("INT"))) then
                    error("Relational expression terms must be ints")
                end
            end
            n.value_type = base({is_signed(n) and "SIGNED" or "UNSIGNED", "INT"})
            return n.value_type
        else
            return check_shift_expression(n)
        end
    end

    function check_shift_expression(n)
        if(node_check(n, "SHIFT_EXPRESSION")) then
            for i=1, #n, 2 do
                if(not can_coerce(check_sum_expression(n[i]), base("INT"))) then
                    error("Shift expression terms must be ints")
                end
            end
            n.value_type = base({is_signed(n) and "SIGNED" or "UNSIGNED", "INT"})
            return n.value_type
        else
            return check_sum_expression(n)
        end
    end


    function check_sum_expression(n)
        if(node_check(n, "SUM_EXPRESSION")) then
            local pointer_type = nil
            for i=1, #n, 2 do
                local term = n[i]
                local term_type = check_term(term)
                if(term_type.kind == Type.KINDS["POINTER"]) then
                    pointer_type = term_type
                elseif(term_type.kind ~= Type.KINDS["INT"]) then
                    error("Sum expression term types must be ints or pointers")
                end
            end
            if(pointer_type == nil) then
                n.value_type = base({is_signed(n) and "SIGNED" or "UNSIGNED", "INT"})
            else
                n.value_type = pointer_type
            end
        else
            n.value_type = check_term(n)
        end

        return n.value_type
    end

    function is_signed(n)
        for _, child in ipairs(n) do
            -- value type is skipped if it is nil (in the case of operators like +/-)
            if(child.value_type and child.value_type.signed == true) then
                return true
            end
        end
        return false
    end

    function check_term(n)
        if(node_check(n, "MULTIPLICATIVE_EXPRESSION")) then
            for i=1, #n, 2 do
                local factor = n[i]
                local factor_type = check_cast_expression(factor)
                if(factor_type.kind == Type.KINDS["POINTER"]) then
                    if(#n > 1) then
                        error("Cannot multiply or divide a pointer")
                    end
                elseif(factor_type.kind ~= Type.KINDS["INT"]) then
                    error("Can only multiply or divide an int by an int")
                end
            end

            n.value_type = n[1].value_type
        else
            n.value_type = check_cast_expression(n)
        end
        
        return n.value_type
    end

    function check_cast_expression(n)
        if(node_check(n, "CAST_EXPRESSION")) then
            n.value_type = check_type_specifier(n.type_specifier)
            for i=1, n.pointer_level do
                n.value_type = pointer(n.value_type)
            end
            check_cast_expression(n.cast_expression)
        else
            n.value_type = check_unary_expression(n)
        end
        return n.value_type
    end

    function check_type_specifier(n)
        if(node_check(n.kind, "STRUCT_OR_UNION_SPECIFIER")) then
            n.value_type = get_symbol(n.kind.id.id, symbol_table.tag).type
        else
            n.value_type = base(n.kind)
        end
        return n.value_type
    end

    function check_unary_expression(n)
        if(node_check(n, "UNARY_EXPRESSION")) then
            check_unary_expression(n.child)
            if(n.operator == "++" or n.operator == "--") then
                if(n.child.value_type.kind == Type.KINDS["INT"] or n.child.value_type.kind == Type.KINDS["POINTER"]) then
                    n.value_type = n.child.value_type
                else
                    error("pre-increment/decrement is only valid for ints or pointers")
                end
            elseif(n.operator == "SIZEOF") then
                n.value_type = base("INT")
            elseif(n.operator == "&") then
                n.value_type = pointer(n.child.value_type)
            elseif(n.operator == "*") then
                n.value_type = n.child.value_type.points_to
            elseif(n.operator == "!") then
                n.value_type = base("INT")
            elseif(n.operator == "~") then
                n.value_type = base("INT")
            elseif(n.operator == "-") then
                if(n.child.value_type.kind == Type.KINDS["INT"] or n.child.value_type.kind == Type.KINDS["POINTER"]) then
                    n.value_type = n.child.value_type
                else
                    error("unary minus is only valid for ints or pointers")
                end
            elseif(n.operator == "+") then
                if(n.child.value_type.kind == Type.KINDS["INT"] or n.child.value_type.kind == Type.KINDS["POINTER"]) then
                    n.value_type = n.child.value_type
                else
                    error("unary plus is only valid for ints or pointers")
                end
            else
                error()
            end
        else
            check_postfix_expression(n)
        end

        return n.value_type
    end

    function check_postfix_expression(n)
        if(node_check(n, "POSTFIX_EXPRESSION")) then
            local primary_expression_type = check_primary_expression(n.primary_expression)
            n.value_types = {}
            n.value_types[0] = primary_expression_type
            for i, operation in ipairs(n) do
                if(operation.type == "[") then
                    local index_type = check_expression(operation.value)
                    if(can_coerce(index_type, base("INT"))) then
                        table.insert(n.value_types, n.value_types[i-1].points_to) -- use the previous type to generate a new type
                    else
                        error("Array index must be an int")
                    end
                elseif(operation.type == "(") then
                    n.value_types[i-1] = n.value_types[i-1].kind == Type.KINDS["POINTER"] and n.value_types[i-1].points_to or n.value_types[i-1]
                    if(n.value_types[i-1].kind == Type.KINDS["FUNCTION"]) then
                        
                        check_argument_list(operation.value, n.value_types[i-1].parameter_types)
                        table.insert(n.value_types, n.value_types[i-1].return_type)
                    else
                        error("Function call can only be performed on a function")
                    end
                elseif(operation.type == "++" or operation.type == "--") then
                    if(n.value_types[i-1].kind ~= Type.KINDS["INT"] and n.value_types[i-1].kind ~= Type.KINDS["POINTER"]) then
                        error("Post-increment/decrement can only be performed on integers or pointers")
                    end
                    table.insert(n.value_types, n.value_types[i-1])
                elseif(operation.type == ".") then
                    if(n.value_types[i-1].kind ~= Type.KINDS["STRUCT"] and n.value_types[i-1].kind ~= Type.KINDS["UNION"]) then
                        error("Can only perform the member access operation on a struct or union")
                    end
                    local member_type = n.value_types[i-1].members[operation.value.id].type
                    if(member_type.kind == Type.KINDS["ARRAY"]) then
                        member_type = pointer(member_type.points_to)
                    end
                    table.insert(n.value_types, member_type)
                elseif(operation.type == "->") then
                    if(n.value_types[i-1].kind ~= Type.KINDS["POINTER"] or n.value_types[i-1].points_to.kind ~= Type.KINDS["STRUCT"] and n.value_types[i-1].points_to.kind ~= Type.KINDS["UNION"]) then
                        error("Can only perform the arrow operation on a pointer to a struct or union")
                    end
                    table.insert(n.value_types, n.value_types[i-1].points_to.members[operation.value.id].type) -- might add array handling
                else
                    error()
                end
            end
            -- shift value type array left so that value_types[i] is the type expected after the operation i has taken place
            --n.value_types[0] = nil
            n.value_type = n.value_types[#n.value_types]
        else
            check_primary_expression(n)
        end

        return n.value_type
    end

    function check_argument_list(arguments, parameter_types)
        if((parameter_types.is_variadic and #arguments < #parameter_types) or (not parameter_types.is_variadic and #arguments ~= #parameter_types)) then
            error("Argument list length does not match the parameter list length")
        end

        for i, argument in ipairs(arguments) do
            local argument_type = check_assignment_expression(argument)
            if(not (parameter_types.is_variadic or can_coerce(argument_type, parameter_types[i]))) then
                error("Argument type does not match parameter type")
            end
        end
    end

    function check_primary_expression(n)
        if(node_check(n, "INT")) then
            n.value_type = base("INT")
        elseif(node_check(n, "IDENTIFIER")) then
            n.handle = get_symbol(n.value, symbol_table.ordinary)
            n.value_type = n.handle.type
            if(n.value_type.kind == Type.KINDS["ARRAY"]) then
                n.value_type = pointer(n.value_type.points_to)
            elseif(n.value_type.kind == Type.KINDS["FUNCTION"]) then
                n.value_type = pointer(n.value_type)
            end
        elseif(node_check(n, "STRING_LITERAL")) then
            n.value_type = array(#n.value + 1, base("CHAR"))
        elseif(node_check(n, "CHARACTER")) then
            n.value_type = base("CHAR")
            -- TODO:Finish the other primary expression types
        elseif(node_check(n, "EXPRESSION")) then
            n.value_type = check_expression(n)
        else
            print(n.type)
            error()
        end
        return n.value_type
    end




    function build_declarator(n, base_type)
        local type = base_type
        for i=1, n.pointer_level do
            type = pointer(type)
        end

        type = build_direct_declarator(n.direct_declarator, type)

        return type
    end

    function build_direct_declarator(n, type)
        for i=#n.dimensions, 1, -1 do
            type = array(n.dimensions[i], type)
        end
        if(n.parameter_list) then -- FIX THIS
            type = func(type, build_parameter_list(n.parameter_list))
        end

        if(n.declarator) then
            type = build_declarator(n.declarator, type)
        end

        return type
    end

    function build_parameter_list(n)
        local parameter_types = {}
        parameter_types.is_variadic = n.is_variadic
        for _, child in ipairs(n) do
            if(child.declarator) then
                local base_type = nil
                if(node_check(child.type_specifier.kind, "STRUCT_OR_UNION_SPECIFIER")) then
                    base_type = get_symbol(child.type_specifier.kind.id.id, symbol_table.tag).type
                else
                    base_type = base(child.type_specifier.kind)
                end
                table.insert(parameter_types, build_declarator(child.declarator, base_type))
                if(parameter_types[#parameter_types].kind == Type.KINDS["ARRAY"]) then
                    parameter_types[#parameter_types] = pointer(parameter_types[#parameter_types].points_to)
                end
            else
                table.insert(parameter_types, base(child.type_specifier.kind))
            end
            child.value_type = parameter_types[#parameter_types]
        end

        return parameter_types
    end

    return check_program(ast)
end


return Type_Checker